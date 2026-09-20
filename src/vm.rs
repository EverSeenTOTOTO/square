use alloc::{format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use core::{cell::RefCell, fmt};

use hashbrown::HashMap;

use crate::{
    builtin::Builtin,
    errors::SquareError,
    vm_insts::Inst,
    vm_value::{CalcResult, CaptureSrc, ClosureInfo, FxHashMap, Function, ParamLayout, Value},
};

#[cfg(test)]
use crate::code_frame::Position;
#[cfg(test)]
use crate::emit::{emit, EmitContext};
#[cfg(test)]
use crate::parse::parse;

pub type ExecResult = Result<(), SquareError>;

#[cfg(target_family = "wasm")]
use crate::println;

/// 被 unwind 出来的 VM 栈快照：`ra`（下一条指令）+ 调用栈 `context`。由 runtime 的
/// `tick`/`step_tick` rewind 回 VM 继续跑。与语言层续延值 `Function::Continuation` 区分：
/// 后者是暴露给用户程序的一等续延，`UnwindFrame` 是调度器持有的 task 快照。
#[derive(Clone)]
pub struct UnwindFrame {
    pub ra: usize,
    pub context: Vec<Rc<RefCell<CallFrame>>>,
    /// park 时刻的活动 try handler——任务交错时各自的 handler 互不串扰
    pub handlers: Vec<TryHandler>,
}

/// try 安装的错误处理器：帧栈深度（回退目标）、操作数栈 sp（handler 闭包位置）、
/// catch 段入口。错误发生时帧栈截到 depth、sp 复位、错误值（Str 消息）入栈后跳 target。
#[derive(Clone)]
pub struct TryHandler {
    pub depth: usize,
    pub sp: usize,
    pub target: usize,
    /// handler 闭包：TRY 安装时弹出暂存，catch 时压回（成功路径不占操作数栈）
    pub handler: Value,
}

/// call_cb 投递的待交付结果：值（await 结果/回调实参）或错误（Promise 拒绝，try 可捕获）
pub enum Pending {
    Value(Value),
    Error(String),
}

/// 运行时任务。`frame` 存在性可当作 park 信号：`Some` = 正在 park（延续已存、不在 VM 里活），
/// `None` = live（状态在 VM 里）。定义在 `vm.rs` 是因为 native 单测也要构造（runtime.rs 整个 `#[cfg(wasm)]`）。
pub struct Task {
    pub frame: RefCell<Option<UnwindFrame>>,
    /// 宿主回调写入、恢复时刻投递（tick 取出后压栈或走 handle_error）
    pub pending: RefCell<Option<Pending>>,
    /// 闭包跨界任务的实参槽位（Fixed 首参 / Pack 的参数包槽）；
    /// u16::MAX = 非闭包任务（await park，投递走操作数栈）
    pub arg_slot: RefCell<u16>,
}

/// 运行时上下文，对标 `Future::poll` 的 `cx`：把当前 task 顺调用栈（`run→step→exec→syscall`）
/// 传下去，让 syscall 知道「当前任务是谁」
pub struct RtCx {
    pub task: Rc<Task>,
    #[cfg(target_family = "wasm")]
    pub rt: &'static crate::runtime::Runtime,
}

impl RtCx {
    #[cfg(not(target_family = "wasm"))]
    pub fn new(task: Rc<Task>) -> Self {
        Self { task }
    }

    #[cfg(test)]
    pub fn test() -> Self {
        Self::new(Rc::new(Task {
            frame: RefCell::new(None),
            pending: RefCell::new(None),
            arg_slot: RefCell::new(u16::MAX),
        }))
    }

    pub fn is_parked(&self) -> bool {
        self.task.frame.borrow().is_some()
    }
}

type OpFn = dyn Fn(&Value, &Value) -> CalcResult;

/// 比较指令共享实现（op 用 Inst::id 的 EQ..GE）
fn cmp_imm(lhs: &Value, op: u8, rhs: &Value) -> bool {
    match op {
        11 => lhs == rhs,
        12 => lhs != rhs,
        13 => lhs < rhs,
        14 => lhs <= rhs,
        15 => lhs > rhs,
        16 => lhs >= rhs,
        _ => unreachable!(),
    }
}

/// 算术指令共享实现（op 用 Inst::id 的 ADD..REM）
fn arith_imm(lhs: &Value, op: u8, rhs: &Value) -> CalcResult {
    match op {
        2 => lhs + rhs,
        3 => lhs - rhs,
        4 => lhs * rhs,
        5 => lhs / rhs,
        6 => lhs % rhs,
        _ => unreachable!(),
    }
}

impl Inst {
    fn exec(&self, vm: &mut VM, cx: &mut RtCx, insts: &Vec<Inst>) -> ExecResult {
        match self {
            Inst::PUSH(value) => {
                vm.current_frame().borrow_mut().push(value.clone());
                Ok(())
            }
            Inst::POP => {
                vm.current_frame().borrow_mut().pop();
                Ok(())
            }

            Inst::STORE_LOCAL(i) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();

                if let Some(val) = frame.top() {
                    let cloned = val.clone();
                    frame.store_slot(*i, cloned);
                    Ok(())
                } else {
                    Err(SquareError::InstructionError(
                        "bad store, operand stack empty".to_string(),
                        self.clone(),
                        vm.pc,
                    ))
                }
            }
            Inst::STORE_UP(i) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();

                let Some(cell) = frame.ups.get(*i as usize).cloned() else {
                    return Err(SquareError::InstructionError(
                        format!("bad store_up, no upvalue {}", i),
                        self.clone(),
                        vm.pc,
                    ));
                };
                if let Some(val) = frame.top() {
                    *cell.borrow_mut() = val.clone();
                    Ok(())
                } else {
                    Err(SquareError::InstructionError(
                        "bad store_up, operand stack empty".to_string(),
                        self.clone(),
                        vm.pc,
                    ))
                }
            }
            Inst::LOAD_LOCAL(i) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let value = frame.load_slot(*i);
                frame.push(value);
                Ok(())
            }
            Inst::LOAD_UP(i) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let Some(cell) = frame.ups.get(*i as usize).cloned() else {
                    return Err(SquareError::InstructionError(
                        format!("bad load_up, no upvalue {}", i),
                        self.clone(),
                        vm.pc,
                    ));
                };
                let value = cell.borrow().clone();
                frame.push(value);
                Ok(())
            }
            Inst::LOAD_GLOBAL(name) => {
                // 全局表（`=` 动态定义）→ builtin → 未定义报错
                if let Some(value) = vm.globals.get(name).cloned() {
                    vm.current_frame().borrow_mut().push(value);
                    Ok(())
                } else if let Some(value) = vm.buildin.resolve_builtin(name) {
                    vm.current_frame().borrow_mut().push(value);
                    Ok(())
                } else {
                    Err(SquareError::InstructionError(
                        format!("undefined variable: {}", name),
                        self.clone(),
                        vm.pc,
                    ))
                }
            }
            Inst::STORE_GLOBAL(name) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                if let Some(val) = frame.top() {
                    vm.globals.insert(name.clone(), val.clone());
                    Ok(())
                } else {
                    Err(SquareError::InstructionError(
                        "bad store, operand stack empty".to_string(),
                        self.clone(),
                        vm.pc,
                    ))
                }
            }

            Inst::ADD => self.binop(vm, &|a, b| a + b),
            Inst::SUB => self.binop(vm, &|a, b| a - b),
            Inst::MUL => self.binop(vm, &|a, b| a * b),
            Inst::DIV => self.binop(vm, &|a, b| a / b),
            Inst::REM => self.binop(vm, &|a, b| a % b),
            Inst::BITAND => self.binop(vm, &|a, b| a & b),
            Inst::BITOR => self.binop(vm, &|a, b| a | b),
            Inst::BITXOR => self.binop(vm, &|a, b| a ^ b),
            Inst::EQ => self.binop(vm, &|a, b| Ok(Value::Bool(a == b))),
            Inst::NE => self.binop(vm, &|a, b| Ok(Value::Bool(a != b))),
            Inst::LT => self.binop(vm, &|a, b| Ok(Value::Bool(a < b))),
            Inst::LE => self.binop(vm, &|a, b| Ok(Value::Bool(a <= b))),
            Inst::GT => self.binop(vm, &|a, b| Ok(Value::Bool(a > b))),
            Inst::GE => self.binop(vm, &|a, b| Ok(Value::Bool(a >= b))),
            Inst::SHL => self.binop(vm, &|a, b| a << b),
            Inst::SHR => self.binop(vm, &|a, b| a >> b),
            Inst::BITNOT => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let result = (!frame.top().unwrap()).map_err(|e| match e {
                    SquareError::RuntimeError(msg) => {
                        SquareError::InstructionError(msg, self.clone(), vm.pc)
                    }
                    _ => e,
                });

                let sp = frame.sp;
                frame.stack[sp - 1] = result?;
                Ok(())
            }

            Inst::CMP_JNE(op, value) => {
                // 比较与条件跳转融合：免中间 Bool 压栈/弹栈与一次派发。
                // JNE 语义保持：条件为假才跳转
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let sp = frame.sp;
                let cond = cmp_imm(&frame.stack[sp - 2], *op, &frame.stack[sp - 1]);
                frame.sp = sp - 2;
                if !cond {
                    drop(frame);
                    return self.jump(insts, &mut vm.pc, *value);
                }
                Ok(())
            }
            Inst::LOAD2_LOCAL(a, b) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let va = frame.load_slot(*a);
                let vb = frame.load_slot(*b);
                frame.push(va);
                frame.push(vb);
                Ok(())
            }
            Inst::LOADP_LOCAL(a, v) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let val = frame.load_slot(*a);
                frame.push(val);
                frame.push(v.clone());
                Ok(())
            }
            Inst::LOADC_JNE(a, op, imm, off) => {
                // 整条循环条件一条指令：槽位与立即数比较，为假跳转
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let cond = cmp_imm(&frame.load_slot(*a), *op, imm);
                if !cond {
                    drop(frame);
                    return self.jump(insts, &mut vm.pc, *off);
                }
                Ok(())
            }
            Inst::LOADC_JNZ(a, op, imm, off) => {
                // LOADC_JNE 的为真跳转孪生：回边穿线产物，谓词相同极性相反
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let cond = cmp_imm(&frame.load_slot(*a), *op, imm);
                if cond {
                    drop(frame);
                    return self.jump(insts, &mut vm.pc, *off);
                }
                Ok(())
            }
            Inst::LOAD_ARITH_IMM(a, op, imm) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let result = arith_imm(&frame.load_slot(*a), *op, imm).map_err(|e| match e {
                    SquareError::RuntimeError(msg) => {
                        SquareError::InstructionError(msg, self.clone(), vm.pc)
                    }
                    _ => e,
                })?;
                frame.push(result);
                Ok(())
            }
            Inst::LOAD2_ARITH(a, b, op) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let va = frame.load_slot(*a);
                let vb = frame.load_slot(*b);
                let result = arith_imm(&va, *op, &vb).map_err(|e| match e {
                    SquareError::RuntimeError(msg) => {
                        SquareError::InstructionError(msg, self.clone(), vm.pc)
                    }
                    _ => e,
                })?;
                frame.push(result);
                Ok(())
            }
            Inst::BINOP_IMM(op, imm) => {
                // 立即数为右操作数：栈顶是左操作数，原地替换
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let sp = frame.sp;
                let lhs = &frame.stack[sp - 1];
                let result = match *op {
                    2 => lhs + imm,
                    3 => lhs - imm,
                    4 => lhs * imm,
                    5 => lhs / imm,
                    6 => lhs % imm,
                    _ => unreachable!(),
                }
                .map_err(|e| match e {
                    SquareError::RuntimeError(msg) => {
                        SquareError::InstructionError(msg, self.clone(), vm.pc)
                    }
                    _ => e,
                })?;
                frame.stack[sp - 1] = result;
                Ok(())
            }
            Inst::TRY(off) => {
                let handler = {
                    let binding = vm.current_frame();
                    let mut frame = binding.borrow_mut();
                    let h = frame.pop();
                    (h, frame.sp)
                };
                vm.handlers.push(TryHandler {
                    depth: vm.call_frames.len(),
                    sp: handler.1,
                    target: (vm.pc as i32 + 1 + *off) as usize,
                    handler: handler.0,
                });
                Ok(())
            }
            Inst::POP_HANDLER => {
                vm.handlers.pop();
                Ok(())
            }
            Inst::JMP(value) => self.jump(insts, &mut vm.pc, *value),
            Inst::JNE(value) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let top = frame.pop();

                if !top.as_bool() {
                    self.jump(insts, &mut vm.pc, *value)
                } else {
                    Ok(())
                }
            }

            Inst::CALL(argc) => {
                let n = *argc as usize;
                let callee = {
                    let frame = vm.current_frame();
                    let frame = frame.borrow();
                    let sp = frame.sp;
                    if sp < n + 1 {
                        return Err(SquareError::InstructionError(
                            "bad call, operand stack underflow".to_string(),
                            self.clone(),
                            vm.pc,
                        ));
                    }
                    frame.stack[sp - 1 - n].as_fn()
                };

                let Some(func) = callee else {
                    // 错误路径才付克隆的代价
                    let target = {
                        let frame = vm.current_frame();
                        let frame = frame.borrow();
                        frame.stack[frame.sp - 1 - n].clone()
                    };
                    return Err(SquareError::InstructionError(
                        format!("bad call, cannot call with {}", target),
                        self.clone(),
                        vm.pc,
                    ));
                };

                let is_tail_call = vm.pc + 1 < insts.len() && insts[vm.pc + 1] == Inst::RET;

                // 先把被调者数据克隆出来（Rc 自增），避免函数体 RefCell 借用跨 VM 操作
                enum Callee {
                    Closure(Rc<ClosureInfo>, usize, Rc<Vec<Rc<RefCell<Value>>>>),
                    Syscall(&'static str),
                    Continuation(usize, Vec<Rc<RefCell<CallFrame>>>),
                }
                let callee = match &*func.borrow() {
                    Function::Closure(info, ip, ups) => {
                        Callee::Closure(info.clone(), *ip, ups.clone())
                    }
                    Function::Syscall(name) => Callee::Syscall(name),
                    Function::Continuation(ra, context) => {
                        Callee::Continuation(*ra, context.clone())
                    }
                    Function::ClosureMeta(..) => unreachable!(),
                };
                drop(func);

                match callee {
                    Callee::Closure(info, ip, ups) => {
                        // 参数不走 Vec 打包：直接从调用方栈拷进被调方槽位
                        let caller_rc = vm.current_frame();
                        let mut caller = caller_rc.borrow_mut();
                        let sp = caller.sp;

                        if is_tail_call {
                            // 同帧重置：清槽位后把栈顶 n 个参数逆序弹出直填
                            caller.slots.clear();
                            caller.slots.resize(info.n_slots as usize, Value::Nil);
                            caller.names = info.names.clone();
                            match &info.params {
                                ParamLayout::Fixed(param_slots) => {
                                    for k in (0..param_slots.len()).rev() {
                                        let v = if k < n {
                                            caller.pop()
                                        } else {
                                            Value::Nil
                                        };
                                        caller.slots[param_slots[k] as usize] = v;
                                    }
                                }
                                ParamLayout::Pack(slot) => {
                                    // pop 从栈顶来（末参在前），收集后反转恢复入参顺序
                                    let mut packed: Vec<Value> =
                                        (0..n).map(|_| caller.pop()).collect();
                                    packed.reverse();
                                    caller.slots[*slot as usize] =
                                        Value::Vec(Rc::new(RefCell::new(packed)));
                                }
                            }
                            caller.ups = ups;
                            caller.sp = 0;
                        } else {
                            let callee_rc = vm.take_frame();
                            {
                                let mut callee = callee_rc.borrow_mut();
                                bind_params(&mut callee, &info, &caller.stack[sp - n..sp]);
                                callee.ups = ups;
                                callee.ra = vm.pc;
                            }
                            caller.sp = sp - n - 1;
                            drop(caller);
                            vm.push_frame(callee_rc);
                        }

                        vm.pc = ip;
                        Ok(())
                    }
                    Callee::Syscall(name) => {
                        let syscall = vm.buildin.get_syscall(name);
                        let params = {
                            let frame = vm.current_frame();
                            let frame = frame.borrow();
                            let sp = frame.sp;
                            Rc::new(RefCell::new(frame.stack[sp - n..sp].to_vec()))
                        };
                        vm.current_frame().borrow_mut().sp -= n + 1;
                        syscall(vm, params, cx, self)
                    }
                    Callee::Continuation(ra, context) => {
                        let first = {
                            let frame = vm.current_frame();
                            let frame = frame.borrow();
                            let sp = frame.sp;
                            if n > 0 {
                                frame.stack[sp - n].clone()
                            } else {
                                Value::Nil
                            }
                        };
                        vm.current_frame().borrow_mut().sp -= n + 1;
                        vm.pc = ra;
                        vm.restore_context(context);
                        vm.current_frame().borrow_mut().push(first);
                        Ok(())
                    }
                }
            }
            Inst::RET => {
                // 借而不克隆 Rc：取 ra 与返回值后立即释放借用，使 pop 出的帧 Rc 唯一、可回池。
                let (ra, top) = {
                    let frame = vm.call_frames.last().unwrap().borrow();
                    (frame.ra, frame.top().unwrap_or(&Value::Nil).clone())
                };

                // jump back
                vm.pc = ra;

                if let Some(rc) = vm.pop_frame() {
                    vm.recycle_frame(rc);
                }

                // always return the top value（cur 已被 pop_frame 同步，免去 Vec::last）
                vm.current_frame().borrow_mut().push(top);
                Ok(())
            }
            Inst::PUSH_CLOSURE(meta) => {
                let Function::ClosureMeta(info) = meta else {
                    unreachable!()
                };
                let ip = info.abs_ip(vm.pc);

                // 无捕获：共享 VM 级空表，零分配
                let ups = if info.captures.is_empty() {
                    vm.empty_ups.clone()
                } else {
                    let binding = vm.current_frame();
                    let mut frame = binding.borrow_mut();
                    Rc::new(
                        info.captures
                            .iter()
                            .map(|src| match src {
                                CaptureSrc::Local(i) => frame.slot_cell(*i),
                                CaptureSrc::Upvalue(j) => frame
                                    .ups
                                    .get(*j as usize)
                                    .cloned()
                                    .unwrap_or_else(|| Rc::new(RefCell::new(Value::Nil))),
                                // 每个闭包实例独立的 this 单元，存入 obj 时回填
                                CaptureSrc::This => Rc::new(RefCell::new(Value::Nil)),
                            })
                            .collect(),
                    )
                };

                vm.current_frame().borrow_mut().push(Value::Function(Rc::new(
                    RefCell::new(Function::Closure(info.clone(), ip, ups)),
                )));
                Ok(())
            }

            Inst::PACK(len) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let mut result = vec![];

                let new_sp = frame.sp - len;
                for i in 0..*len {
                    let val = frame.stack[new_sp + i].clone();
                    result.push(val);
                }

                frame.sp = new_sp;

                frame.push(Value::Vec(Rc::new(RefCell::new(result))));
                Ok(())
            }
            Inst::PEEK(offset, i) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let top = frame.stack[frame.sp - 1].as_vec();

                if let Some(val) = top {
                    // 只借用 pack 计算下标、clone 单个元素；整包 clone 会让
                    // 多参函数的参数绑定退化为每参一次 O(n) 拷贝
                    let pack = val.borrow();
                    let len = pack.len();

                    let index = if *i > 0 {
                        *i as usize
                    } else {
                        if *offset >= len {
                            return Err(SquareError::InstructionError(
                                format!(
                                    "bad peek_vec, offset {} out of range, pack length is {}",
                                    offset,
                                    len
                                ),
                                self.clone(),
                                vm.pc,
                            ));
                        }

                        let rest = (len - offset) as i32;

                        ((*i + rest) % rest) as usize + offset
                    };

                    if index < len {
                        let val = pack[index].clone();
                        drop(pack);
                        frame.push(val);
                        Ok(())
                    } else {
                        Err(SquareError::InstructionError(
                            format!(
                                "bad peek_vec, index {} out of range, pack length is {}",
                                index,
                                len
                            ),
                            self.clone(),
                            vm.pc,
                        ))
                    }
                } else {
                    Err(SquareError::InstructionError(
                        format!(
                            "bad peek_vec, top value is not a vector, got {}",
                            frame.top().unwrap_or(&Value::Nil).clone()
                        ),
                        self.clone(),
                        vm.pc,
                    ))
                }
            }

            Inst::GET(key) => self.get_field(vm, cx, key),
            Inst::LOADGET_LOCAL(a, key) => {
                // 槽位读 + 属性访问融合：先压目标再走 GET 共享路径（含 proxy 回退）
                {
                    let binding = vm.current_frame();
                    let mut frame = binding.borrow_mut();
                    let target = frame.load_slot(*a);
                    frame.push(target);
                }
                self.get_field(vm, cx, key)
            }
            Inst::LOADP_UP(u, v) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let Some(cell) = frame.ups.get(*u as usize).cloned() else {
                    return Err(SquareError::InstructionError(
                        format!("bad loadp_up, no upvalue {}", u),
                        self.clone(),
                        vm.pc,
                    ));
                };
                let val = cell.borrow().clone();
                frame.push(val);
                frame.push(v.clone());
                Ok(())
            }
            Inst::LOADU_ARITH(u, op, imm) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                let Some(cell) = frame.ups.get(*u as usize).cloned() else {
                    return Err(SquareError::InstructionError(
                        format!("bad loadu_arith, no upvalue {}", u),
                        self.clone(),
                        vm.pc,
                    ));
                };
                let val = cell.borrow().clone();
                let result = arith_imm(&val, *op, imm).map_err(|e| match e {
                    SquareError::RuntimeError(msg) => {
                        SquareError::InstructionError(msg, self.clone(), vm.pc)
                    }
                    _ => e,
                })?;
                frame.push(result);
                Ok(())
            }
            Inst::SET(key) => {
                let value = vm.current_frame().borrow_mut().pop();
                let target = vm.current_frame().borrow_mut().pop();

                if let Some(obj) = target.as_obj() {
                    Builtin::try_capture_this(&value, &obj);
                    {
                        // 热路径：键已存在则原地覆写，免去每次 insert 的 String 分配
                        let mut map = obj.borrow_mut();
                        if let Some(slot) = map.get_mut(key.as_str()) {
                            *slot = value;
                        } else {
                            map.insert(Rc::from(key.as_str()), value);
                        }
                    }
                    vm.current_frame().borrow_mut().push(Value::Obj(obj));
                    Ok(())
                } else {
                    let set = vm.buildin.get_syscall("set");
                    set(
                        vm,
                        Rc::new(RefCell::new(vec![
                            target,
                            Value::Str(Rc::from(key.as_str())),
                            value,
                        ])),
                        cx,
                        self,
                    )
                }
            }

            Inst::DELIMITER(mindex) => {
                if *mindex < vm.mpc {
                    // TODO: optimize
                    for (i, item) in insts.iter().enumerate().skip(vm.pc) {
                        if let Inst::DELIMITER(index) = item {
                            if *index == vm.mpc {
                                vm.pc = i;
                                break;
                            }
                        }
                    }
                }

                vm.mpc += 1;
                Ok(())
            }

            Inst::NAMES(names) => {
                vm.current_frame().borrow_mut().names = names.clone();
                Ok(())
            }
        }
    }

    /// 属性访问共享路径（GET / LOADGET_LOCAL）：Obj 直访，其余回退 get 内建
    fn get_field(&self, vm: &mut VM, cx: &mut RtCx, key: &String) -> ExecResult {
        let target = vm.current_frame().borrow_mut().pop();

        if let Some(obj) = target.as_obj() {
            let val = obj.borrow().get(key.as_str()).cloned().unwrap_or(Value::Nil);
            vm.current_frame().borrow_mut().push(val);
            Ok(())
        } else {
            // proxy 等目标回退 get 内建，保持唯一拦截路径
            let get = vm.buildin.get_syscall("get");
            get(
                vm,
                Rc::new(RefCell::new(vec![
                    target,
                    Value::Str(Rc::from(key.as_str())),
                ])),
                cx,
                self,
            )
        }
    }

    fn binop(&self, vm: &mut VM, op_fn: &OpFn) -> ExecResult {
        let binding = vm.current_frame();
        let mut frame = binding.borrow_mut();
        let result =
            op_fn(&frame.stack[frame.sp - 2], &frame.stack[frame.sp - 1]).map_err(|e| match e {
                SquareError::RuntimeError(msg) => {
                    SquareError::InstructionError(msg, self.clone(), vm.pc)
                }
                _ => e,
            });

        let sp = frame.sp;
        frame.stack[sp - 2] = result?;
        frame.sp -= 1;
        Ok(())
    }

    fn jump(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) -> ExecResult {
        let new_pc = *pc as i32 + offset;
        if new_pc < 0 || new_pc >= insts.len() as i32 {
            return Err(SquareError::InstructionError(
                "bad jump".to_string(),
                self.clone(),
                *pc,
            ));
        }

        *pc = new_pc as usize;
        Ok(())
    }

    /// 内建侧调用入口（proxy trap / callcc iife）：参数已是打包好的 Vec。
    /// CALL 指令的快路径不经过这里。
    pub fn call(
        &self,
        vm: &mut VM,
        cx: &mut RtCx,
        closure: Rc<RefCell<Function>>,
        params: Rc<RefCell<Vec<Value>>>,
        is_tail_call: bool,
    ) -> ExecResult {
        match &*closure.borrow() {
            Function::ClosureMeta(..) => unreachable!(),
            Function::Closure(info, ip, ups) => {
                let args = params.borrow();

                if is_tail_call {
                    let binding = vm.current_frame();
                    let mut frame = binding.borrow_mut();
                    bind_params(&mut frame, info, &args);
                    frame.ups = ups.clone();
                    frame.sp = 0;
                } else {
                    let frame_rc = vm.take_frame();
                    {
                        let mut new_frame = frame_rc.borrow_mut();
                        bind_params(&mut new_frame, info, &args);
                        new_frame.ups = ups.clone();
                        new_frame.ra = vm.pc;
                    }
                    vm.push_frame(frame_rc);
                }

                vm.pc = *ip;
                Ok(())
            }
            Function::Syscall(name) => {
                let syscall = vm.buildin.get_syscall(name);
                syscall(vm, params, cx, self)
            }
            Function::Continuation(ra, context) => {
                vm.pc = *ra;
                vm.restore_context(context.clone());

                vm.current_frame()
                    .borrow_mut()
                    .push(params.borrow().first().unwrap_or(&Value::Nil).clone());
                Ok(())
            }
        }
    }
}

/// 绑定参数并重置帧布局：槽位清到 info.n_slots（定参直拷，展开参数整包），
/// 装载名表。调用方负责随后设置 ups / ra / sp。
pub(crate) fn bind_params(frame: &mut CallFrame, info: &ClosureInfo, args: &[Value]) {
    frame.slots.clear();
    frame.slots.resize(info.n_slots as usize, Value::Nil);
    frame.names = info.names.clone();

    match &info.params {
        ParamLayout::Fixed(param_slots) => {
            for (k, slot) in param_slots.iter().enumerate() {
                frame.slots[*slot as usize] = args.get(k).cloned().unwrap_or(Value::Nil);
            }
        }
        ParamLayout::Pack(slot) => {
            frame.slots[*slot as usize] = Value::Vec(Rc::new(RefCell::new(args.to_vec())));
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    /// 槽位化局部变量（编译期静态布局）。被捕获的槽位以 Value::UpValue 共享单元存储，
    /// 读写自动解包/写穿。
    pub slots: Vec<Value>,
    /// 本帧对应闭包的 upvalue 单元表（与闭包共享 Rc，CALL 只做引用自增）
    pub ups: Rc<Vec<Rc<RefCell<Value>>>>,
    /// 槽位名表（快照/调试用），与 ClosureInfo 共享同一 Rc
    pub names: Rc<Vec<String>>,

    // operand stack
    pub stack: Vec<Value>,
    // fake stack pointer, avoid frequent operand stack push/pop
    pub sp: usize,

    pub ra: usize, // return address
}

impl fmt::Display for CallFrame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "-----------------------------")?;
        writeln!(f, "Return Addr: {}", self.ra)?;
        writeln!(f, "Locals:")?;
        for (i, value) in self.slots.iter().enumerate() {
            let name = self.names.get(i).map(|s| s.as_str()).unwrap_or("");
            writeln!(f, "{:>8}: {}", name, value)?;
        }

        writeln!(f, "Operand Stack:")?;
        for index in 0..self.sp {
            let value = &self.stack[index];
            writeln!(f, "{:>8}: {}", index, value)?;
        }

        Ok(())
    }
}

impl Default for CallFrame {
    fn default() -> Self {
        Self::new()
    }
}

impl CallFrame {
    pub fn new() -> Self {
        Self {
            slots: Vec::new(),
            ups: Rc::new(Vec::new()),
            names: Rc::new(Vec::new()),
            stack: vec![Value::Nil; 8],
            sp: 0,
            ra: 0,
        }
    }

    pub fn push(&mut self, value: Value) {
        if self.sp >= self.stack.len() {
            self.stack.resize(self.stack.len() * 2 + 1, Value::Nil);
        }

        self.stack[self.sp] = value;
        self.sp += 1;
    }

    #[inline]
    pub fn pop(&mut self) -> Value {
        let top = self.stack.swap_remove(self.sp - 1);
        self.sp -= 1;
        top
    }

    #[inline]
    pub fn top(&self) -> Option<&Value> {
        self.stack.get(self.sp - 1)
    }

    /// 读槽位：捕获槽位解包 UpValue 单元。根帧槽位惰性增长，未赋值读 Nil。
    #[inline]
    pub fn load_slot(&self, i: u16) -> Value {
        match self.slots.get(i as usize) {
            Some(Value::UpValue(cell)) => cell.borrow().clone(),
            Some(v) => v.clone(),
            None => Value::Nil,
        }
    }

    /// 写槽位：捕获槽位写穿共享单元，保持捕获的可变性。
    #[inline]
    pub fn store_slot(&mut self, i: u16, value: Value) {
        let i = i as usize;
        if i >= self.slots.len() {
            self.slots.resize(i + 1, Value::Nil);
        }
        if let Value::UpValue(cell) = &self.slots[i] {
            *cell.borrow_mut() = value;
        } else {
            self.slots[i] = value;
        }
    }

    /// 取槽位的共享单元：未升级则原地升级为 UpValue（闭包捕获用）。
    #[inline]
    pub fn slot_cell(&mut self, i: u16) -> Rc<RefCell<Value>> {
        let i = i as usize;
        if i >= self.slots.len() {
            self.slots.resize(i + 1, Value::Nil);
        }
        match &self.slots[i] {
            Value::UpValue(cell) => cell.clone(),
            v => {
                let cell = Rc::new(RefCell::new(v.clone()));
                self.slots[i] = Value::UpValue(cell.clone());
                cell
            }
        }
    }

    /// 按名反查槽位（测试断言用）：同名多槽位时取最内层（后定义者遮蔽前者），
    /// 捕获槽位解包 UpValue。
    #[cfg(test)]
    pub fn local_by_name(&self, name: &str) -> Option<Value> {
        let i = self.names.iter().position(|n| n == name)?;
        match self.slots.get(i)? {
            Value::UpValue(cell) => Some(cell.borrow().clone()),
            v => Some(v.clone()),
        }
    }
}

pub struct VM {
    pub call_frames: Vec<Rc<RefCell<CallFrame>>>,
    pub pc: usize,
    pub mpc: usize,

    buildin: Builtin,

    /// `= x v` 动态定义的全局（编译期无法归属槽位的名字）
    pub globals: FxHashMap<String, Value>,

    /// 捕获空闭包/帧共享的空 upvalue 表
    empty_ups: Rc<Vec<Rc<RefCell<Value>>>>,

    /// 活动的 try 错误处理器（内层在后）
    pub handlers: Vec<TryHandler>,

    /// 当前帧缓存：与 call_frames 栈顶同步，current_frame() 免去 Vec::last + Rc clone
    cur: Rc<RefCell<CallFrame>>,

    /// 回收的调用帧：CALL 复用而非重新分配（含 `vec![Nil; 8]` 操作数栈）。
    /// 仅在 `Rc` 唯一（无 callcc 续延共享）时回收，见 [`recycle_frame`]。
    frame_pool: Vec<Rc<RefCell<CallFrame>>>,

    /// 逐指令剖析：(rdtsc 周期累计, 执行次数)。profiling 开启时由 step 记录
    pub inst_cycles: [(u64, u64); 49],
    pub profiling: bool,
}


impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        let root = Rc::new(RefCell::new(CallFrame::new()));
        Self {
            call_frames: vec![root.clone()],
            buildin: Builtin::new(),
            globals: FxHashMap::default(),
            empty_ups: Rc::new(Vec::new()),
            handlers: Vec::new(),
            cur: root.clone(),
            pc: 0,
            mpc: 0,
            frame_pool: Vec::new(),
            inst_cycles: [(0, 0); 49],
            profiling: false,
        }
    }

    /// 错误恢复：弹最内层有效 handler，帧栈截回其深度、sp 复位（handler 闭包在栈顶），
    /// 错误消息以 Str 入栈，pc 跳 catch 段。跨续延跳出的过期 handler（深度大于当前
    /// 帧栈）直接丢弃。返回 false 表示无 handler，错误继续向上抛。
    pub(crate) fn handle_error(&mut self, e: &SquareError) -> bool {
        loop {
            let Some(h) = self.handlers.pop() else {
                return false;
            };
            if self.call_frames.len() < h.depth {
                continue; // 过期 handler（续延跳出 try 域）
            }
            while self.call_frames.len() > h.depth {
                self.pop_frame();
            }
            // handler 收原始消息：InstructionError/RuntimeError 都是裸 msg，
            // 其余类型才走 Display 格式化
            let msg = match e {
                SquareError::InstructionError(msg, ..)
                | SquareError::RuntimeError(msg) => msg.clone(),
                other => format!("{}", other),
            };
            {
                let binding = self.current_frame();
                let mut frame = binding.borrow_mut();
                frame.sp = h.sp;
                frame.push(h.handler.clone());
                frame.push(Value::Str(Rc::from(msg.as_str())));
            }
            self.pc = h.target;
            return true;
        }
    }

    pub fn reset(&mut self) {
        self.pc = 0;
        self.mpc = 0;
        self.globals.clear();
        self.handlers.clear();
        self.inst_cycles = [(0, 0); 49];
        let root = Rc::new(RefCell::new(CallFrame::new()));
        self.cur = root.clone();
        self.call_frames.splice(0.., vec![root]);
    }

    #[inline]
    pub fn current_frame(&mut self) -> Rc<RefCell<CallFrame>> {
        self.cur.clone()
    }

    #[inline]
    pub fn push_frame(&mut self, rc: Rc<RefCell<CallFrame>>) {
        self.cur = rc.clone();
        self.call_frames.push(rc)
    }

    #[inline]
    pub fn pop_frame(&mut self) -> Option<Rc<RefCell<CallFrame>>> {
        let popped = self.call_frames.pop();
        if let Some(last) = self.call_frames.last() {
            self.cur = last.clone();
        }
        popped
    }

    /// 取一个调用帧供新 CALL 使用：优先复用池中帧（连 Rc 壳一起，复位槽位与 sp，
    /// 保留操作数栈与槽位容量），池空才新建。CALL 全程零堆分配。
    fn take_frame(&mut self) -> Rc<RefCell<CallFrame>> {
        if let Some(rc) = self.frame_pool.pop() {
            let mut frame = rc.borrow_mut();
            frame.slots.clear();
            frame.sp = 0;
            drop(frame);
            rc
        } else {
            Rc::new(RefCell::new(CallFrame::new()))
        }
    }

    /// RET 退栈时回收帧。仅当 `Rc` 唯一（无 callcc 续延共享该帧）时连壳入池；
    /// 否则丢弃，由其最后一个引用释放。池设上限，避免极端递归深度下无限增长。
    fn recycle_frame(&mut self, rc: Rc<RefCell<CallFrame>>) {
        const POOL_CAP: usize = 256;
        if self.frame_pool.len() < POOL_CAP && Rc::strong_count(&rc) == 1 {
            self.frame_pool.push(rc);
        }
    }

    #[inline]
    pub fn save_context(&self) -> Vec<Rc<RefCell<CallFrame>>> {
        self.call_frames.clone()
    }

    /// 连同活动 handler 一起快照（任务交错时 try 域互不串扰）
    pub fn unwind_snapshot(&self) -> UnwindFrame {
        UnwindFrame {
            ra: 0,
            context: self.save_context(),
            handlers: self.handlers.clone(),
        }
    }

    #[inline]
    pub fn restore_context(&mut self, context: Vec<Rc<RefCell<CallFrame>>>) {
        self.call_frames = context;
        if let Some(last) = self.call_frames.last() {
            self.cur = last.clone();
        }
    }

    /// 把当前 VM 状态序列化成一段字节，供宿主调试面板读取（走专用数据通道，与 `println`
    /// 程序输出分离）。布局（全部 little-endian u32 长度前缀 + UTF-8 字节）：
    ///   `[pc][n_frames] ( [ra] [n_locals] ( [k][v] )* [n_stack] ( [v] )* )*`
    pub fn snapshot(&self) -> Vec<u8> {
        fn push_u32(buf: &mut Vec<u8>, v: u32) {
            buf.extend_from_slice(&v.to_le_bytes());
        }
        fn push_str(buf: &mut Vec<u8>, s: &str) {
            push_u32(buf, s.len() as u32);
            buf.extend_from_slice(s.as_bytes());
        }

        let mut buf = Vec::new();
        push_u32(&mut buf, self.pc as u32);
        push_u32(&mut buf, self.call_frames.len() as u32);

        for frame in &self.call_frames {
            let frame = frame.borrow();
            push_str(&mut buf, &frame.ra.to_string());
            push_u32(&mut buf, frame.slots.len() as u32);
            for (i, value) in frame.slots.iter().enumerate() {
                let name = frame.names.get(i).map(|s| s.as_str()).unwrap_or("");
                push_str(&mut buf, name);
                push_str(&mut buf, &format!("{}", value));
            }
            push_u32(&mut buf, frame.sp as u32);
            for value in frame.stack.iter().take(frame.sp) {
                push_str(&mut buf, &format!("{}", value));
            }
        }

        buf
    }

    pub fn step(&mut self, insts: &Vec<Inst>, cx: &mut RtCx) -> ExecResult {
        let inst = &insts[self.pc];

        // rdtsc 读取约 10 周期；分支预测下不剖析时几乎零成本
        #[cfg(target_arch = "x86_64")]
        let t0 = if self.profiling {
            unsafe { core::arch::x86_64::_rdtsc() }
        } else {
            0
        };

        inst.exec(self, cx, insts)?;

        self.pc += 1;

        #[cfg(target_arch = "x86_64")]
        if self.profiling {
            let dt = unsafe { core::arch::x86_64::_rdtsc() } - t0;
            let e = &mut self.inst_cycles[inst.id()];
            e.0 += dt;
            e.1 += 1;
        }

        Ok(())
    }

    pub fn run(&mut self, insts: &Vec<Inst>, cx: &mut RtCx) -> ExecResult {
        // 程序结束哨兵 ra 只属于根帧；跨帧 park 恢复时当前帧的 ra 是活的返回地址，
        // 覆盖它会吞掉闭包调用的返回（await 在闭包内 park 即中招）
        if self.call_frames.len() == 1 {
            self.current_frame().borrow_mut().ra = insts.len();
        }

        while self.pc < insts.len() {
            match self.step(insts, cx) {
                Ok(()) => {}
                Err(e) => {
                    if !self.handle_error(&e) {
                        return Err(e);
                    }
                }
            }
            if cx.is_parked() {
                break; // sleep park：续延已存回 task.frame，停止本次推进。
                       // sleep 内设置 pc = #insts 也可实现中止，但是需要透传指令集总长
            }
        }

        Ok(())
    }

    pub fn print_times(&self) {
        let mut data: Vec<_> = self
            .inst_cycles
            .iter()
            .enumerate()
            .filter(|(_, (_, count))| *count > 0)
            .map(|(id, (total, count))| (id, *total, *total / *count, *count))
            .collect();

        println!("\n== 按总周期排序 ==");
        data.sort_by(|a, b| b.1.cmp(&a.1));
        for (id, total, _, count) in &data {
            println!("{:>12}: {:>14} cyc  {:>10} 次", Inst::name_of(*id), total, count);
        }

        println!("\n== 按平均周期排序 ==");
        data.sort_by(|a, b| b.2.cmp(&a.2));
        for (id, _, avg, count) in &data {
            println!("{:>12}: {:>8} cyc/次  {:>10} 次", Inst::name_of(*id), avg, count);
        }
    }
}

#[test]
fn test_grow_operand_stack() {
    let mut vm = VM::new();
    let mut cx = RtCx::test();
    let mut insts = vec![];

    for i in 0..100 {
        insts.push(Inst::PUSH(Value::Num(i as f64)))
    }

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert!(callframe.stack.len() >= 100);
    assert_eq!(callframe.sp, 100);
    assert_eq!(callframe.top(), Some(&Value::Num(99.0)));
}

/// 解码 [`VM::snapshot`] 的字节流，校验同构
#[test]
fn test_snapshot_roundtrip() {
    let mut vm = VM::new();
    let mut cx = RtCx::test();
    let insts = vec![Inst::PUSH(Value::Num(42.0)), Inst::POP];

    vm.run(&insts, &mut cx).unwrap(); // 跑完后 pc=2，栈空

    let bytes = vm.snapshot();

    let mut o = 0usize;
    let u32_at = |buf: &[u8], off: &mut usize| -> u32 {
        let v = u32::from_le_bytes(buf[*off..*off + 4].try_into().unwrap());
        *off += 4;
        v
    };
    let str_at = |buf: &[u8], off: &mut usize| -> String {
        let len = u32_at(buf, off) as usize;
        let s = String::from_utf8(buf[*off..*off + len].to_vec()).unwrap();
        *off += len;
        s
    };

    let pc = u32_at(&bytes, &mut o) as usize;
    assert_eq!(pc, vm.pc);

    let n_frames = u32_at(&bytes, &mut o) as usize;
    assert_eq!(n_frames, vm.call_frames.len());

    for frame in &vm.call_frames {
        let expected = frame.borrow();
        let ra: usize = str_at(&bytes, &mut o).parse().unwrap();
        assert_eq!(ra, expected.ra);

        let n_locals = u32_at(&bytes, &mut o) as usize;
        assert_eq!(n_locals, expected.slots.len());
        for _ in 0..n_locals {
            str_at(&bytes, &mut o);
            str_at(&bytes, &mut o);
        }

        let n_stack = u32_at(&bytes, &mut o) as usize;
        assert_eq!(n_stack, expected.sp);
        for _ in 0..n_stack {
            str_at(&bytes, &mut o);
        }
    }

    assert_eq!(o, bytes.len(), "trailing bytes / over-read mismatch");
}


#[test]
fn test_exec_try_success() {
    let code = "[let r [try 42 /[e] 0]]\nr";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
    assert!(vm.handlers.is_empty()); // 正常路径 handler 已撤销
}

#[test]
fn test_exec_try_catch() {
    // Num + Nil 类型错误 → handler 收到 Str 消息
    let code = "[let r [try [+ 1 nil] /[e] e]]\nr";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    match callframe.top() {
        Some(Value::Str(msg)) => assert!(msg.contains("cannot perform")),
        other => panic!("expect error Str, got {:?}", other),
    }
}

#[test]
fn test_exec_try_undefined() {
    let code = "[let r [try undefined_thing /[e] 'caught']]\nr";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top(), Some(&Value::Str(Rc::from("caught"))));
}

#[test]
fn test_exec_try_deep_unwind() {
    // 错误发生在三层函数调用深处：帧栈应截回 try 所在深度
    let code = "
[let f3 /[] undefined_deep]
[let f2 /[] [f3]]
[let f1 /[] [f2]]
[let r [try [f1] /[e] 7]]
r
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    assert_eq!(vm.call_frames.len(), 1); // 帧栈已回退到根
    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top(), Some(&Value::Num(7.0)));
}

#[test]
fn test_exec_try_nested() {
    // 内层 try 捕获，外层不受影响
    let code = "
[let r [try [try [+ 1 nil] /[e1] 'inner'] /[e2] 'outer']]
r
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top(), Some(&Value::Str(Rc::from("inner"))));
}

/// TCO + 展开参数（Pack）组合：尾调用复用帧时参数包顺序必须保持入参顺序
#[test]
fn test_exec_pack_args_tco() {
    let code = "[let cons /[x g] /[f] [f x g]]\n[let car /[p] [p /[x .] x]]\n[let r1 [car [cons 42 /[] nil]]]\n[let r2 [[cons 42 /[] nil] /[x .] x]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();
    vm.current_frame().borrow_mut().ra = insts.len();
    while vm.pc < insts.len() {
        vm.step(&insts, &mut cx).unwrap();
    }
    assert_eq!(vm.current_frame().borrow().local_by_name("r1"), Some(Value::Num(42.0)));
    assert_eq!(vm.current_frame().borrow().local_by_name("r2"), Some(Value::Num(42.0)));
}

#[test]
fn test_profile_fib() {
    let code = "
[let fib /[n] [if [<= n 2] 1 [+ [fib [- n 1]] [fib [- n 2]]]]]
[fib 22]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();
    vm.print_times();
}

#[test]
fn test_exec_token() {
    let code = "42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load() {
    let code = "[let x 42]\nx";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load_undefined() {
    let code = "x";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    assert_eq!(
        vm.run(&insts, &mut cx),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD_GLOBAL("x".to_string()),
            2,
        ))
    );
}

#[test]
fn test_exec_define() {
    let code = "[let x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_assign() {
    let code = "[let x nil] [= x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_assign_capture() {
    let code = "[let x nil] [begin [= x 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_define_expand() {
    let code = "[let [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand() {
    let code = "[let x nil] [= [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand_capture() {
    let code = "[let x nil] [begin [= [x] [vec 42]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot() {
    let code = "[let [. x] [vec 1 42 3]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot_error() {
    let code = "[let [. x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    assert_eq!(
        vm.run(&insts, &mut cx),
        Err(SquareError::InstructionError(
            "bad peek_vec, index 1 out of range, pack length is 1".to_string(),
            Inst::PEEK(0, 1),
            7,
        ))
    );
}

#[test]
fn test_exec_define_expand_greed() {
    let code = "[let [... x] [vec 1 2 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_greed_error() {
    let code = "[let [... x] [vec]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    assert_eq!(
        vm.run(&insts, &mut cx),
        Err(SquareError::InstructionError(
            "bad peek_vec, offset 0 out of range, pack length is 0".to_string(),
            Inst::PEEK(0, -1),
            4,
        ))
    );
}

#[test]
fn test_exec_define_expand_nested() {
    let code = "
[let [. [x] ... y] [vec 1 [vec 42] 3 4 5]] ; x = 42, y = 5
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_define_chain() {
    let code = "[let x [let y [let z 42]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0));
    assert_eq!(callframe.local_by_name("y").unwrap(), Value::Num(42.0));
    assert_eq!(callframe.local_by_name("z").unwrap(), Value::Num(42.0));
}

#[test]
fn test_exec_op() {
    let code = "[- 1 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(-1.0))
}

#[test]
fn test_exec_op_assign() {
    let code = "[let x 1]\n[+= x 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0));
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(3.0))
}

#[test]
fn test_exec_op_assign_dot() {
    let code = "[let x [obj 'y' [obj 'z' 0]]]
        [+= x.y.z 42]
        x.y.z";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_dot() {
    let code = "[let o [obj 'x' [obj 'y' 42]]]\no.x.y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_dot() {
    let code = "[let o [obj 'x' [obj 'y' 0]]]\n[= o.x.y 42]\no.x.y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_obj() {
    let code = "
[let o [obj 
        'x' 42
        'inc' /[] [+= o.x 1]]]

[println o]

[o.inc]
[= o.o o]
[o.o.o.o.o.o.inc]

o.x
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    insts.iter().for_each(|inst| println!("{}", inst));
    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(44.0))
}

#[test]
fn test_exec_begin() {
    let code = "[begin 1 2 3]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0))
}

#[test]
fn test_exec_scope() {
    let code = "
        [let x 1]
        [let y [begin [let x 2] x]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(1.0));
    assert_eq!(callframe.local_by_name("y").unwrap(), Value::Num(2.0));
}

#[test]
fn test_exec_tail_call() {
    let code = "[let f /[n] [if [>= n 100000] n [f [+ n 1]]]]\n[f 0]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();
    let mut max_depth = vm.call_frames.len();

    vm.current_frame().borrow_mut().ra = insts.len();

    while vm.pc < insts.len() {
        vm.step(&insts, &mut cx).unwrap();
        if vm.call_frames.len() > max_depth {
            max_depth = vm.call_frames.len();
        }
    }

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(100000.0));
    assert_eq!(max_depth, 2); // 10 万层尾递归只增 1 帧
}

#[test]
fn test_exec_if_true() {
    let code = "[if true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_if_false_nil() {
    let code = "[if false 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Nil)
}

#[test]
fn test_exec_if_false() {
    let code = "[if false 42 24]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(24.0))
}

#[test]
fn test_exec_while() {
    let code = "[let x 0]\n[while [< x 4] [+= x 1]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(4.0))
}

#[test]
fn test_exec_match() {
    let code = "[cond
        [false 24]
        [true 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_fn_call() {
    let code = "[/[] 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_call_with_params() {
    let code = "[/[x] x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_overwrite_params() {
    let code = "
    [let x 24]
    [/[x] [begin
            [= x 42]
            x] x]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(24.0))
}

#[test]
fn test_exec_fn_capture_assign() {
    let code = "
    [let fn /[] [begin 
        [let x 1]
        /[] [+= x 1]]]
    [let f [fn]]
    [let g [fn]]
    [f]
    [g]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert_eq!(callframe.top().unwrap(), &Value::Num(2.0));
    assert_eq!(callframe.stack[callframe.sp - 2], Value::Num(2.0));
}

#[test]
fn test_exec_fn_capture_nested() {
    let code = "
[let foo /[] [begin
               [let x 1]
               /[] [begin ; should capture x
                       [let y 1]
                       /[] [+ x y]]]]
[[[foo]]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert_eq!(callframe.top().unwrap(), &Value::Num(2.0));
}

#[test]
fn test_exec_fn_capture_scope_lift() {
    let code = "
[let fn /[] x]
[let x 42]
[fn]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_capture_error() {
    let code = "
[let fn /[] x]
[begin 
    [let x 42]
    [fn]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    // 姊妹作用域的 let 不会发布到全局表：前向引用在此处报未定义（旧实现静默 Nil）
    assert_eq!(
        vm.run(&insts, &mut cx),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD_GLOBAL("x".to_string()),
            3,
        ))
    )
}

#[test]
fn test_exec_fn_capture_shadow() {
    let code = "
[let fn /[] [begin ;this x should be shadow: ; x [let x 24] x]]
[let x 42]
[fn]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    // fn 体内 [let x 24] 遮蔽外层：返回 24（旧实现此处误报 undefined）
    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top(), Some(&Value::Num(24.0)));
}

#[test]
fn test_exec_fn_capture_lazy() {
    let code = "
[let foo /[f] /[] [f]]
[let bar [foo /[] x]]
[let x 42]
[bar]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_capture_self() {
    let code = "
[let foo /[x] [if [> x 0] 42 [foo 1]]]
[foo 0]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_builtin_value() {
    let code = "
[let p println]
[let t [typeof p]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(
        callframe.local_by_name("t").unwrap(),
        Value::Str(Rc::from("fn"))
    )
}

#[test]
fn test_exec_getter() {
    let code = "
[let o [obj]]

[= o.x 0]

[let p [proxy o 'get' /[t k] 42]]

p.x
";

    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_setter() {
    let code = "
[let o [obj]]

[= o.x 0]

[let p [proxy o 'set' /[t k v] [set t k [+ 1 v]]]]

[= p.x 41]

p.x
";

    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_flow() {
    let code = "
[let x [callcc /[cc] 42]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0))
}

#[test]
fn test_callcc_break() {
    let code = "
[let x [callcc /[cc] [begin [cc 42] 24]]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("x").unwrap(), Value::Num(42.0))
}

#[test]
fn test_callcc_cc1() {
    let code = "
[let cc [callcc /[cc] cc]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("cc").unwrap(), Value::Num(42.0))
}

#[test]
fn test_callcc_cc2() {
    let code = "
[let cc [callcc /[cc] [cc cc]]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("cc").unwrap(), Value::Num(42.0))
}

#[test]
fn test_callcc_cc3() {
    let code = "
[let cc [callcc /[cc] [callcc cc]]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("cc").unwrap(), Value::Num(42.0))
}

#[test]
fn test_callcc_cc4() {
    let code = "
[let cc [callcc callcc]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.local_by_name("cc").unwrap(), Value::Num(42.0))
}

#[test]
fn test_callcc_abort() {
    let code = "
[let cc [callcc /[cc] cc]]

[let x [begin [cc 42] 24]]

x
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    // abort 把执行带回 callcc 之后：cc 被覆写为 42，let x 从未执行
    // （DELIMITER 跳过已完成语句），x 槽位未初始化
    vm.run(&insts, &mut cx).unwrap();
    assert_eq!(vm.current_frame().borrow().local_by_name("cc"), Some(Value::Num(42.0)));
    assert_eq!(vm.current_frame().borrow().local_by_name("x"), None);
}

#[test]
fn test_profile() {
    let code = "
[let [a b c d e f g] [vec 1 2 3 4 5 6 7]]

[begin 
    [begin 
        [begin 
            [begin 
                [begin 
                    [begin 
                        [begin [println a b c d e f g]]]]]]]]

";
    let ast = parse(code, &mut Position::new()).unwrap();
    let (insts, _source_map) = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut cx = RtCx::test();

    vm.run(&insts, &mut cx).unwrap();
    vm.print_times();
}
