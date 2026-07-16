//! # 简易异步运行时：由宿主事件循环（`setTimeout`/`queueMicrotask`）驱动
//!
//! 移植自 `rustdemo/wasm-demo`（设计见 `docs/CS/Snippets/Rust-Wasm-Async.md`），适配 square
//! 的执行单元：**这里没有 Rust `Future`，任务就是一段被 unwind 出来的 VM 栈快照**
//! （`crate::vm::UnwindFrame`）。
//!
//! ## 核心思想：unwind / rewind
//!
//! wasm 一次导出调用必然同步跑到底，没有「挂起再恢复执行栈」的能力。所以真正的「暂停」只
//! 发生在 VM 续延这一层——我们用 [`VM::save_context`] 把当前调用栈 **unwind** 成一个
//! `UnwindFrame` 快照存起来；下一次 `tick` 再用 [`VM::restore_context`] + `vm.pc = ra`
//! 把 VM **rewind** 回那个状态继续跑。和 JS 用异步回调消解调用栈是同一回事，只是 wasm
//! 里得自己把执行上下文序列化下来。
//!
//! ## 三种 syscall，同一种 unwind 套路
//!
//! `sleep`/`defer`/`spawn` 都「捕获一段续延 + 入队」，区别只在「入哪个队 / 当前任务停不停」：
//!
//! | syscall | 捕获的续延 | 去向 | 当前任务 |
//! |---|---|---|---|
//! | `sleep(ms)` | 外侧续延（sleep 之后的语句） | 等待表 + 宿主 `setTimeout` | **停**（让出线程） |
//! | `defer(fn)` | `fn` 自身的闭包续延 | 等待表 + 宿主 `queueMicrotask` | 不停（继续往下跑） |
//! | `spawn(fn)` | `fn` 自身的闭包续延 | 立即进就绪队列 | 不停 |
//!
//! `sleep` 要「停」：它把 `vm.pc` 拨到 `insts.len()-1`，`step` 末尾的 `pc+=1` 让循环
//! 自然退出（`run` 不需要任何额外信号/标志）。`defer`/`spawn` 不停：捕获的是**另一个新任务**
//! 的续延，当前任务照常执行下一条指令。
//!
//! ## 时间线（以 `sleep` 为例）
//!
//! `[sleep 1000]` syscall：
//!   1. `alloc_id()` → unwind 捕获外侧续延（`ra = vm.pc`，即 `CALL` 位置——恢复时 `step`
//!      的 `pc+=1` 会跳过整个 `[sleep ...]` 调用，从下一条指令继续）
//!   2. `REGISTRY.insert(id, Entry { waker, task, frame })`（task = 当前正在跑的 task）
//!   3. 调 `host::js_sleep(id, ms)`（宿主排 `setTimeout`）
//!   4. `vm.pc = insts.len() - 1` → `run` 循环退出，当前 tick 把控制权交还事件循环
//!
//! 宿主 `setTimeout` 到点 → 回调导出的 `wake_by_id(id)`：
//!   1. 摘 `REGISTRY.remove(id)` → `waker.wake_by_ref()`（task 重新进就绪队列）
//!   2. `tick()`：rewind 恢复该 task 的续延，从 sleep 下一条指令继续执行

#![cfg(target_family = "wasm")]

extern crate alloc;

use alloc::{
    collections::{BTreeMap, VecDeque},
    rc::Rc,
    vec::Vec,
};
use core::{
    cell::{Cell, RefCell},
    mem,
    task::{RawWaker, RawWakerVTable, Waker},
};

use crate::vm::{UnwindFrame, VM};

// ════════════════════════ host 导入：JS 注入的能力 ═════════════════════════════════
// 客机没有定时器/事件循环，全部由宿主提供。回调统一走导出的 `wake_by_id(id)`。
mod host {
    #[link(wasm_import_module = "host")]
    extern "C" {
        /// 排一个 `setTimeout`：到点宿主回调导出的 `wake_by_id(id)`。`id` 由本模块分配。
        pub fn js_sleep(id: u32, ms: u32);
        /// 排一个 `queueMicrotask`：宿主在当前同步栈清空后回调 `wake_by_id(id)`。
        pub fn js_queue_microtask(id: u32);
    }
}

// ════════════════════════ Task / Runtime ═══════════════════════════════════════════
// 一个任务 = 一段待 rewind 的 `UnwindFrame`。运行时就一条就绪队列；`tick` 逐个 rewind 续延
// 跑到任务结束。

/// 一个任务：持有一段待恢复的栈快照。被唤醒（重新入队）后，`tick` 会 rewind 它并继续 `vm.run()`。
struct Task {
    frame: RefCell<Option<UnwindFrame>>, // None = 已完成；Some = 待 rewind
}

struct Runtime {
    /// 就绪队列：`tick` 从队头取，唤醒/`spawn` 往队尾塞。
    queue: RefCell<VecDeque<Rc<Task>>>,
}

impl Runtime {
    const fn new() -> Self {
        Self {
            queue: RefCell::new(VecDeque::new()),
        }
    }

    /// 把一段栈快照包成 `Task` 丢进就绪队列队尾。
    fn spawn_frame(&self, frame: UnwindFrame) {
        let task = Rc::new(Task {
            frame: RefCell::new(Some(frame)),
        });
        self.queue.borrow_mut().push_back(task);
    }

    /// 把就绪队列里的任务逐个 rewind 并跑到结束，直到空。
    /// `run` 自然结束（`pc >= insts.len()`）即任务完成，`Task` 随引用归零 drop。
    fn tick(&self, vm: &mut VM, insts: &Vec<crate::vm_insts::Inst>) {
        loop {
            // 先「取出来」再 run，绝不在持着队列 borrow 的时候 run（否则 run 里一旦再碰
            // 队列就会 RefCell 重入 panic）。
            let task = self.queue.borrow_mut().pop_front();
            let task = match task {
                Some(t) => t,
                None => break, // 队列空 = 当前没有可推进的任务，把控制权交还事件循环
            };
            let frame = match task.frame.borrow_mut().take() {
                Some(f) => f,
                None => continue, // 已完成（无续延），跳过
            };
            // rewind：恢复续延（复用 `Function::Continuation` 的恢复语义）。
            vm.restore_context(frame.context);
            vm.pc = frame.ra;
            // 记下「现在在跑谁」——run 期间的 sleep syscall 借它登记 Entry。
            set_current(task);
            let _ = vm.run(insts);
            clear_current();
            // run 自然结束 = 任务完成（sleep 中断时 pc 已被 syscall 拨到末尾，同样结束）。
        }
    }
}

/// 全局运行时单例。`const fn new` 让它能直接常量初始化（无需 lazy init / `Option`）。
static mut RUNTIME: Runtime = Runtime::new();

/// 取全局运行时引用。SAFETY 封在内部：单线程 wasm，对 `static mut` 取共享引用无并发风险。
fn rt() -> &'static Runtime {
    unsafe { &RUNTIME }
}

// ───────────────────────── 任务 Waker：RawWaker over Rc<Task> ─────────────────────────
// 和 wasm-demo 一模一样：唤醒 = 把任务重新塞回就绪队列。这里它由 `wake_by_id`
// （外部宿主事件）触发。

fn waker_for(task: &Rc<Task>) -> Waker {
    let ptr = Rc::into_raw(task.clone()) as *const ();
    unsafe { Waker::from_raw(RawWaker::new(ptr, &VTABLE)) }
}

const VTABLE: RawWakerVTable = RawWakerVTable::new(clone_raw, wake_raw, wake_by_ref_raw, drop_raw);

unsafe fn clone_raw(ptr: *const ()) -> RawWaker {
    Rc::increment_strong_count(ptr as *const Task); // 复制 Waker：引用计数 +1
    RawWaker::new(ptr, &VTABLE)
}
unsafe fn drop_raw(ptr: *const ()) {
    Rc::decrement_strong_count(ptr as *const Task); // 丢弃 Waker：引用计数 -1
}
unsafe fn wake_raw(ptr: *const ()) {
    // wake 消费 Waker：先调度，再 -1。
    wake_by_ref_raw(ptr);
    Rc::decrement_strong_count(ptr as *const Task);
}
unsafe fn wake_by_ref_raw(ptr: *const ()) {
    // 借用 Waker 的引用来调度，不消费它。
    let rc = Rc::from_raw(ptr as *const Task);
    rt().queue.borrow_mut().push_back(rc.clone()); // 唤醒 = 重新塞回就绪队列
    mem::forget(rc); // 不减引用：保持 Waker 持有的那份不变
}

// ════════════════════════ id 注册表 ═════════════════════════════════════════════════
// JS 只认整数 id（拿不到 Rust 的 `Waker`/`Task`），所以任务在挂起时把自己的 waker +
// task 挂到某个 id 下；`wake_by_id(id)` 靠它找回并重新入队。

/// 一个挂起项：唤醒器 + 它所属的 task（task 保活，否则栈快照随 task 一起被释放）。
struct Entry {
    waker: Waker,
    task: Rc<Task>,
}

static mut REGISTRY: RefCell<BTreeMap<u32, Entry>> = RefCell::new(BTreeMap::new());

static mut NEXT_ID: Cell<u32> = Cell::new(1);
fn alloc_id() -> u32 {
    unsafe {
        let v = NEXT_ID.get();
        NEXT_ID.set(v.wrapping_add(1));
        v
    }
}

// 当前正在跑的那个 `Task`。`tick` rewind 续延前写入、`run` 返回后清空；syscall 借它拿到
// 「我自己是谁」。生命周期仅限一次 `vm.run()` 期间。
static mut CURRENT_TASK: Option<Rc<Task>> = None;

fn set_current(task: Rc<Task>) {
    unsafe {
        CURRENT_TASK = Some(task);
    }
}
fn clear_current() {
    unsafe {
        CURRENT_TASK = None;
    }
}

// ════════════════════════ 公开给 syscall 用的注册接口 ═════════════════════════════════
// `builtin.rs` 里的 `sleep`/`defer`/`spawn` syscall 调用这些。栈快照由调用方（syscall）负责
// 捕获并传入。

/// `sleep` syscall 调用：把当前任务的**外侧续延**（sleep 之后的语句）登记到等待表，并让宿主
/// 排一个 `setTimeout`。调用后 syscall 会把 `vm.pc` 拨到末尾终止当前 tick——见 [`park_current`]。
pub fn schedule_sleep(frame: UnwindFrame, ms: u32) {
    let id = alloc_id();
    register(id, frame);
    unsafe {
        host::js_sleep(id, ms);
    }
}

/// `defer` syscall 调用：把 `fn` 自身的闭包续延登记成「延迟 spawn」——不立刻入队，而是让宿主
/// 用 `queueMicrotask` 在当前同步栈清空后回调 `wake_by_id`，届时再进就绪队列。当前任务**不停**。
pub fn schedule_defer(frame: UnwindFrame) {
    let id = alloc_id();
    // 造一个新 task 持有这段续延，挂到等待表（不入就绪队列）；microtask 唤醒时再入队。
    unsafe {
        let task = Rc::new(Task {
            frame: RefCell::new(Some(frame)),
        });
        let waker = waker_for(&task);
        REGISTRY.borrow_mut().insert(id, Entry { waker, task });
        host::js_queue_microtask(id);
    }
}

/// `spawn` syscall 调用：把 `fn` 的闭包续延直接塞进就绪队列（不跨事件循环、不停当前任务）。
pub fn spawn(frame: UnwindFrame) {
    rt().spawn_frame(frame);
}

/// 把 `id` → 当前 task + 续延登记进 REGISTRY。当前 task 取自 [`CURRENT_TASK`]。
fn register(id: u32, frame: UnwindFrame) {
    unsafe {
        let task = CURRENT_TASK.clone().expect("no current task while parking");
        let waker = waker_for(&task);
        // 注意：sleep 捕获的是「外侧续延」，但它还没到就绪队列——它被 Entry 临时保管，
        // 等 wake_by_id 时由 waker 重新入队。所以这里把续延放回 task.frame。
        *task.frame.borrow_mut() = Some(frame);
        REGISTRY.borrow_mut().insert(id, Entry { waker, task });
    }
}

// ════════════════════════ JS 回调入口 ══════════════════════════════════════════════

/// `setTimeout`/`queueMicrotask` 到点时宿主调这个。摘掉 id → `waker.wake_by_ref()`（task
/// 重新进就绪队列，续延已在 task.frame 里）→ 跑 `tick`（rewind 续延从上次停点续跑）。
#[no_mangle]
pub extern "C" fn wake_by_id(id: u32) {
    let entry = unsafe { REGISTRY.borrow_mut().remove(&id) }; // 摘掉 = 标记完成
    if let Some(e) = entry {
        e.waker.wake_by_ref(); // 重新入队 task
    }
    // rewind 续延并推进。VM/insts 句柄由入口（`run` 导出）提前存好。
    let (vm, insts) = current_run_targets();
    rt().tick(vm, insts);
}

// ════════════════════════ 入口：启动 + 句柄存取 ══════════════════════════════════════
// `tick`/`wake_by_id` 要驱动 VM，而 VM/insts 由 wasm 导出 `run` 拿进来（跨独立调用栈），
// 所以这里存一份裸指针句柄。VM 是 `Box` 在堆上、以裸指针回传宿主（见 `main.rs::init/run`），
// 在 wasm 实例存活期间地址稳定，单线程下解引用安全。

static mut VM_PTR: *mut VM = core::ptr::null_mut();
static mut INSTS_PTR: *const Vec<crate::vm_insts::Inst> = core::ptr::null();

/// 由 `run` 导出在启动时登记当前 VM / insts 句柄。
pub fn set_run_targets(vm: *mut VM, insts: *const Vec<crate::vm_insts::Inst>) {
    unsafe {
        VM_PTR = vm;
        INSTS_PTR = insts;
    }
}

/// 把主程序续延 spawn 进就绪队列，然后 `tick` 跑到所有任务结束。供 `run` 导出调用。
pub fn start(vm: *mut VM, insts: *const Vec<crate::vm_insts::Inst>) {
    set_run_targets(vm, insts);
    // 主程序续延：从 pc=0 开始（VM 已 reset）。context 用 VM 当前的调用栈。
    unsafe {
        let vm_ref = &mut *vm;
        let insts_ref = &*insts;
        let frame = UnwindFrame {
            ra: 0,
            context: vm_ref.save_context(),
        };
        rt().spawn_frame(frame);
        rt().tick(vm_ref, insts_ref);
    }
}

fn current_run_targets() -> (&'static mut VM, &'static Vec<crate::vm_insts::Inst>) {
    unsafe {
        // SAFETY：单线程 wasm；VM/insts 由导出 `run` 在调用期间保证存活。
        (&mut *VM_PTR, &*INSTS_PTR)
    }
}
