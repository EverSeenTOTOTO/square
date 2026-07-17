//! 由宿主事件循环驱动的异步运行时。没有 Rust `Future`——任务就是一段被 unwind
//! 出来的 VM 栈快照（`crate::vm::UnwindFrame`），由 `tick`（run）/`step_tick`（step）
//! rewind 回 VM 继续跑。

#![cfg(target_family = "wasm")]

extern crate alloc;

use alloc::{
    collections::{BTreeMap, VecDeque},
    rc::Rc,
    vec::Vec,
};
use core::cell::{Cell, RefCell};

use crate::vm::{ExecResult, RtCx, Task, UnwindFrame, VM};

mod host {
    #[link(wasm_import_module = "host")]
    extern "C" {
        pub fn js_sleep(id: u32, ms: u32);
        pub fn js_queue_microtask(id: u32);
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Mode {
    Run,
    Step,
}

/// 待唤醒入队的任务
struct Entry {
    task: Rc<Task>,
}

pub struct Runtime {
    queue: RefCell<VecDeque<Rc<Task>>>,
    registry: RefCell<BTreeMap<u32, Entry>>,
    next_id: Cell<u32>,
    vm_ptr: *mut VM,
    insts_ptr: *const Vec<crate::vm_insts::Inst>,
    mode: Cell<Mode>,
}

impl Runtime {
    const fn new() -> Self {
        Self {
            queue: RefCell::new(VecDeque::new()),
            registry: RefCell::new(BTreeMap::new()),
            next_id: Cell::new(1),
            vm_ptr: core::ptr::null_mut(),
            insts_ptr: core::ptr::null(),
            mode: Cell::new(Mode::Run),
        }
    }

    fn spawn_frame(&self, frame: UnwindFrame) {
        let task = Rc::new(Task {
            frame: RefCell::new(Some(frame)),
        });
        self.queue.borrow_mut().push_back(task);
    }

    fn alloc_id(&self) -> u32 {
        let v = self.next_id.get();
        self.next_id.set(v.wrapping_add(1));
        v
    }
}

/// run 驱动器：逐个 rewind 队头 task 并 `vm.run` 到 park/完成，直到队列空。
fn tick(vm: &mut VM, insts: &Vec<crate::vm_insts::Inst>) -> ExecResult {
    let rt = rt();
    loop {
        // 先取出再 run：不能在持有队列 borrow 时 run，否则 run 里再碰队列会 RefCell 重入 panic。
        let Some(task) = rt.queue.borrow_mut().pop_front() else {
            return Ok(());
        };
        let Some(frame) = task.frame.borrow_mut().take() else {
            continue; // 已完成
        };
        vm.restore_context(frame.context);
        vm.pc = frame.ra;
        let mut cx = RtCx { task: task.clone(), rt };
        vm.run(insts, &mut cx)?; // 出错即向上抛（run 导出会 panic）
    }
}

/// step 驱动器：推进队头 task 一条指令，然后构造“执行下一条指令”的延续
fn step_tick(vm: &mut VM, insts: &Vec<crate::vm_insts::Inst>) -> ExecResult {
    let rt = rt();
    let Some(front) = rt.queue.borrow().front().cloned() else {
        return Ok(()); // 无就绪 task（都在等外部事件）
    };
    let frame = front.frame.borrow_mut().take().expect("ready task has frame");
    vm.restore_context(frame.context);
    vm.pc = frame.ra;
    vm.current_frame().borrow_mut().ra = insts.len();
    let mut cx = RtCx { task: front.clone(), rt };
    let result = if vm.pc < insts.len() {
        vm.step(insts, &mut cx)
    } else {
        Ok(())
    };

    if result.is_err() || cx.is_parked() || vm.pc >= insts.len() {
        rt.queue.borrow_mut().pop_front();
    } else {
        *front.frame.borrow_mut() = Some(UnwindFrame {
            ra: vm.pc,
            context: vm.save_context(),
        });
    }
    result
}

static mut RUNTIME: Runtime = Runtime::new();

fn rt() -> &'static Runtime {
    // SAFETY：单线程 wasm。
    unsafe { &RUNTIME }
}

impl RtCx {
    /// sleep：登记外侧延续到等待表、排 setTimeout、把延续存入 task.frame 并 park。
    pub fn park_sleep(&mut self, frame: UnwindFrame, ms: u32) {
        let id = self.rt.alloc_id();
        *self.task.frame.borrow_mut() = Some(frame);
        self.rt
            .registry
            .borrow_mut()
            .insert(id, Entry { task: self.task.clone() });
        unsafe { host::js_sleep(id, ms) };
    }

    /// spawn：闭包续延直接进就绪队列，不停当前任务。
    pub fn spawn(&mut self, frame: UnwindFrame) {
        self.rt.spawn_frame(frame);
    }

    /// defer：闭包续延挂到等待表，microtask 回调 wake_by_id 时才进就绪队列，不停当前任务（主延续）
    pub fn defer(&mut self, frame: UnwindFrame) {
        let id = self.rt.alloc_id();
        let task = Rc::new(Task {
            frame: RefCell::new(Some(frame)),
        });
        self.rt.registry.borrow_mut().insert(id, Entry { task });
        unsafe { host::js_queue_microtask(id) };
    }
}

/// 宿主 `setTimeout`/`queueMicrotask` 到点时回调：按 id 入队 → 按 mode 推进。
#[no_mangle]
pub extern "C" fn wake_by_id(id: u32) {
    let Some(e) = rt().registry.borrow_mut().remove(&id) else {
        return;
    };
    rt().queue.borrow_mut().push_back(e.task); // 唤醒 = 入队
    let (vm, insts) = current_run_targets();
    let result = if rt().mode.get() == Mode::Step {
        step_tick(vm, insts)
    } else {
        tick(vm, insts)
    };
    if let Err(err) = result {
        panic!("{}", err);
    }
}

fn set_run_targets(vm: *mut VM, insts: *const Vec<crate::vm_insts::Inst>) {
    unsafe {
        RUNTIME.vm_ptr = vm;
        RUNTIME.insts_ptr = insts;
    }
}

/// run 导出：主程序续延 spawn 进队列、置 Run 模式、tick 到所有任务结束。
pub fn start(vm: *mut VM, insts: *const Vec<crate::vm_insts::Inst>) -> ExecResult {
    set_run_targets(vm, insts);
    reset_runtime_state();
    rt().mode.set(Mode::Run);
    unsafe {
        let vm_ref = &mut *vm;
        let frame = UnwindFrame {
            ra: 0,
            context: vm_ref.save_context(),
        };
        rt().spawn_frame(frame);
        tick(vm_ref, &*insts)?;
    }
    Ok(())
}

/// step 导出首次调用（幂等）：登记句柄、置 Step 模式、主程序续延 spawn 进队列。
pub fn ensure_started(vm: *mut VM, insts: *const Vec<crate::vm_insts::Inst>) {
    set_run_targets(vm, insts);
    if rt().mode.get() == Mode::Step {
        return;
    }
    reset_runtime_state();
    rt().mode.set(Mode::Step);
    unsafe {
        let vm_ref = &mut *vm;
        let frame = UnwindFrame {
            ra: 0,
            context: vm_ref.save_context(),
        };
        rt().spawn_frame(frame);
    }
}

pub fn step_one(vm: &mut VM, insts: &Vec<crate::vm_insts::Inst>) -> ExecResult {
    step_tick(vm, insts)
}

pub fn reset() {
    unsafe {
        reset_runtime_state();
        RUNTIME.vm_ptr = core::ptr::null_mut();
        RUNTIME.insts_ptr = core::ptr::null();
    }
}

fn reset_runtime_state() {
    let rt = rt();
    rt.queue.borrow_mut().clear();
    rt.registry.borrow_mut().clear();
    rt.next_id.set(1);
    rt.mode.set(Mode::Run);
}

fn current_run_targets() -> (&'static mut VM, &'static Vec<crate::vm_insts::Inst>) {
    // SAFETY：单线程 wasm；VM/insts 由导出在调用期间保证存活。
    unsafe { (&mut *rt().vm_ptr, &*rt().insts_ptr) }
}
