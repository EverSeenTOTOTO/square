//! 由宿主事件循环驱动的异步运行时。没有 Rust `Future`——任务就是一段被 unwind
//! 出来的 VM 栈快照（`crate::vm::UnwindFrame`），由 `tick`（run）/`step_tick`（step）
//! rewind 回 VM 继续跑。

#![cfg(target_family = "wasm")]

extern crate alloc;

use alloc::{
    collections::{BTreeMap, VecDeque},
    rc::Rc,
    string::{String, ToString},
    vec,
    vec::Vec,
};
use core::cell::{Cell, RefCell};

use crate::code_frame::SourceMap;
use crate::println;
use crate::errors::SquareError;
use crate::ffi;
use crate::vm::{ExecResult, Pending, RtCx, Task, TryHandler, UnwindFrame, VM};
use crate::vm_value::{Function, Value};

/// 把闭包包装成可唤醒任务：`ra = ip + 1`（闭包 ip 指向入口前的 JMP，tick 直接
/// `vm.pc = ra` 进 run，没有 step 循环的那步 +1，手动对齐）。
pub(crate) fn new_closure_unwind(
    info: &alloc::rc::Rc<crate::vm_value::ClosureInfo>,
    ip: usize,
    ups: &alloc::rc::Rc<alloc::vec::Vec<alloc::rc::Rc<core::cell::RefCell<Value>>>>,
) -> UnwindFrame {
    let mut frame = crate::vm::CallFrame::new();
    crate::vm::bind_params(&mut frame, info, &[]);
    frame.ups = ups.clone();
    // 闭包体 RET 的去向：程序外大值使 run 循环立即结束——
    // 若回落到哨兵帧默认 ra=0，任务会从程序头重跑并级联注册（无限循环）
    frame.ra = usize::MAX >> 1;
    let sentinel = crate::vm::CallFrame::new();

    UnwindFrame {
        ra: ip + 1,
        context: vec![
            alloc::rc::Rc::new(core::cell::RefCell::new(sentinel)),
            alloc::rc::Rc::new(core::cell::RefCell::new(frame)),
        ],
        handlers: Vec::new(),
    }
}

/// 闭包首参槽位：Fixed 取首参；Pack 取参数包槽（投递整包）；无参 u16::MAX
fn first_param_slot(params: &crate::vm_value::ParamLayout) -> u16 {
    match params {
        crate::vm_value::ParamLayout::Fixed(slots) => {
            slots.first().copied().unwrap_or(u16::MAX)
        }
        crate::vm_value::ParamLayout::Pack(slot) => *slot,
    }
}

/// 闭包值注册为可唤醒任务（js 传参跨界 / 事件回调），返回句柄 id
pub fn register_closure(f: &Value) -> Option<u32> {
    let func = f.as_fn()?;
    let closure = func.borrow();
    if let Function::Closure(info, ip, ups) = &*closure {
        let task = Rc::new(Task {
            frame: core::cell::RefCell::new(Some(new_closure_unwind(info, *ip, ups))),
            pending: core::cell::RefCell::new(None),
            arg_slot: RefCell::new(first_param_slot(&info.params)),
        });
        Some(rt().alloc_id_and_insert(task))
    } else {
        None
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
    source_map: RefCell<SourceMap>,
    /// 用户代码首指令 pc（prelude 拼接后），compile 时设置；导出 `user_start()` 供宿主快进
    user_start: Cell<u32>,
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
            source_map: RefCell::new(SourceMap::new()),
            user_start: Cell::new(0),
        }
    }

    fn spawn_frame(&self, frame: UnwindFrame) {
        let task = Rc::new(Task {
            frame: RefCell::new(Some(frame)),
            pending: RefCell::new(None),
            arg_slot: RefCell::new(u16::MAX),
        });
        self.queue.borrow_mut().push_back(task);
    }

    fn alloc_id(&self) -> u32 {
        let v = self.next_id.get();
        self.next_id.set(v.wrapping_add(1));
        v
    }

    fn alloc_id_and_insert(&self, task: Rc<Task>) -> u32 {
        let id = self.alloc_id();
        self.registry.borrow_mut().insert(id, Entry { task });
        id
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
        vm.handlers = frame.handlers;
        vm.pc = frame.ra;
        deliver_pending(vm, &task);
        let mut cx = RtCx { task: task.clone(), rt };
        vm.run(insts, &mut cx)?; // 出错即向上抛（run 导出打印后返回状态）
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
    vm.handlers = frame.handlers;
    vm.pc = frame.ra;
    deliver_pending(vm, &front);
    // 同 run()：哨兵 ra 只设根帧（恢复中的闭包帧 ra 是活的返回地址）
    if vm.call_frames.len() == 1 {
        vm.current_frame().borrow_mut().ra = insts.len();
    }
    let mut cx = RtCx { task: front.clone(), rt };
    let result = if vm.pc < insts.len() {
        vm.step(insts, &mut cx)
    } else {
        Ok(())
    };

    if result.is_err() || cx.is_parked() || vm.pc >= insts.len() {
        rt.queue.borrow_mut().pop_front();
    } else {
        let mut f = vm.unwind_snapshot();
        f.ra = vm.pc;
        *front.frame.borrow_mut() = Some(f);
    }
    result
}

/// 恢复时刻投递 call_cb 写入的回调结果：值压栈（await/回调实参），
/// 错误走 try handler（无 handler 打印后以 nil 继续，保持栈形）。
fn deliver_pending(vm: &mut VM, task: &Rc<Task>) {
    let slot = *task.arg_slot.borrow();
    match task.pending.borrow_mut().take() {
        // 闭包任务：实参写参数槽（Fixed 首参 / Pack 包槽），而非操作数栈
        Some(Pending::Value(v)) if slot != u16::MAX => {
            vm.current_frame().borrow_mut().store_slot(slot, v)
        }
        Some(Pending::Value(v)) => vm.current_frame().borrow_mut().push(v),
        Some(Pending::Error(msg)) => {
            let e = SquareError::RuntimeError(msg);
            if !vm.handle_error(&e) {
                println!("{}", format_error(&e));
                vm.current_frame().borrow_mut().push(Value::Nil);
            }
        }
        None => {}
    }
}

static mut RUNTIME: Runtime = Runtime::new();

pub(crate) fn rt() -> &'static Runtime {
    // SAFETY：单线程 wasm。
    unsafe { &RUNTIME }
}

impl RtCx {
    /// 注册可唤醒任务句柄（宿主回调经 call_cb 按 id 唤醒）
    pub fn register(&self, task: Rc<Task>) -> u32 {
        let id = self.rt.alloc_id();
        self.rt.registry.borrow_mut().insert(id, Entry { task });
        id
    }

    /// park 当前任务：延续（含活动 try handler）存入 task.frame，run 循环检测到即停
    pub fn park_self(&self, vm: &mut VM) {
        let mut frame = vm.unwind_snapshot();
        frame.ra = vm.pc + 1;
        *self.task.frame.borrow_mut() = Some(frame);
    }
}

/// 宿主唯一唤醒入口：`call_cb(id, args_json_ptr, len)`。零参即旧裸唤醒；
/// 带参则作为回调实参/await 结果投递（`{"__sq_err": msg}` 对象 → try 可捕获的错误）。
#[no_mangle]
pub extern "C" fn call_cb(id: u32, ptr: u32, len: u32) {
    let Some(e) = rt().registry.borrow_mut().remove(&id) else {
        return;
    };
    if len > 0 {
        let bytes =
            unsafe { core::slice::from_raw_parts(ptr as *const u8, len as usize) };
        // 宿主传来实参数组：按元数解包（0 → nil，1 → 该值，n → vec），
        // 再识别 {"__sq_err": msg}（Promise 拒绝/宿主异常 → try 可捕获）
        let value = match ffi::json_to_value(bytes) {
            Some(Value::Vec(arr)) => {
                let v = {
                    let a = arr.borrow();
                    match a.len() {
                        0 => Value::Nil,
                        1 => a[0].clone(),
                        _ => Value::Vec(arr.clone()),
                    }
                };
                v
            }
            Some(v) => v,
            None => Value::Nil,
        };
        let pending = if let Value::Obj(obj) = &value {
            if obj.borrow().contains_key("__sq_err") {
                let msg = match obj.borrow().get("__sq_err") {
                    Some(Value::Str(s)) => s.to_string(),
                    _ => "rejected".to_string(),
                };
                Pending::Error(msg)
            } else {
                Pending::Value(value)
            }
        } else {
            Pending::Value(value)
        };
        *e.task.pending.borrow_mut() = Some(pending);
    }
    rt().queue.borrow_mut().push_back(e.task); // 唤醒 = 入队
    let (vm, insts) = current_run_targets();
    let result = if rt().mode.get() == Mode::Step {
        step_tick(vm, insts)
    } else {
        tick(vm, insts)
    };
    if let Err(err) = result {
        // 异步任务里的错误：打印后丢弃该任务（已出队），其余任务继续
        println!("{}", format_error(&err));
    }
}

fn set_run_targets(vm: *mut VM, insts: *const Vec<crate::vm_insts::Inst>) {
    unsafe {
        RUNTIME.vm_ptr = vm;
        RUNTIME.insts_ptr = insts;
    }
}

/// compile 时存入当前程序的 source map，供错误定位反查。
pub fn set_source_map(sm: SourceMap) {
    *rt().source_map.borrow_mut() = sm;
}

/// compile 时存入用户代码首指令 pc（见 [`crate::prelude::user_start_pc`]）。
/// 属编译产物而非运行状态，reset 不清除。
pub fn set_user_start(pc: usize) {
    rt().user_start.set(pc as u32);
}

/// 宿主查询用户代码起点：单步模式下宿主可先快进 prelude 再交互单步
pub fn user_start() -> u32 {
    rt().user_start.get()
}

/// 用当前 source map 把错误的 pc 反查为源码位置后格式化。
pub fn format_error(e: &SquareError) -> String {
    let sm = rt().source_map.borrow();
    e.enrich(Some(&sm))
}

/// run 导出：主程序续延 spawn 进队列、置 Run 模式、tick 到所有任务结束。
pub fn start(vm: *mut VM, insts: *const Vec<crate::vm_insts::Inst>) -> ExecResult {
    set_run_targets(vm, insts);
    reset_runtime_state();
    rt().mode.set(Mode::Run);
    unsafe {
        let vm_ref = &mut *vm;
        let mut frame = vm_ref.unwind_snapshot();
        frame.ra = 0;
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
        let mut frame = vm_ref.unwind_snapshot();
        frame.ra = 0;
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
