use alloc::{format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use core::{cell::RefCell, fmt};

use hashbrown::HashMap;

use crate::{
    builtin::{Builtin, GETTER_KEY, SETTER_KEY},
    errors::SquareError,
    vm_insts::Inst,
    vm_value::{CalcResult, Function, Value},
};

#[cfg(test)]
use crate::code_frame::Position;
#[cfg(test)]
use crate::emit::{emit, EmitContext};
#[cfg(test)]
use crate::parse::parse;
#[cfg(test)]
use std::time::Instant;

pub type ExecResult = Result<(), SquareError>;

type OpFn = dyn Fn(&Value, &Value) -> CalcResult;

impl Inst {
    fn exec(&self, vm: &mut VM, insts: &Vec<Inst>) -> ExecResult {
        match self {
            Inst::PUSH(value) => {
                vm.current_frame().borrow_mut().push(value.clone());
                Ok(())
            }
            Inst::POP => {
                vm.current_frame().borrow_mut().pop();
                Ok(())
            }

            Inst::STORE(name) => {
                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();

                if let Some(val) = frame.top() {
                    let cloned = val.clone();
                    frame.assign_local(name, cloned);
                    Ok(())
                } else {
                    Err(SquareError::InstructionError(
                        "bad store, operand stack empty".to_string(),
                        self.clone(),
                        vm.pc,
                    ))
                }
            }
            Inst::LOAD(name) => {
                if let Some(value) = vm.buildin.resolve_builtin(name) {
                    let cloned = value.clone();
                    return {
                        vm.current_frame().borrow_mut().push(cloned);
                        Ok(())
                    };
                }

                let binding = vm.current_frame();
                let mut frame = binding.borrow_mut();
                if let Some(value) = frame.resolve_local(name) {
                    let cloned = value.clone();
                    frame.push(cloned);
                    Ok(())
                } else {
                    Err(SquareError::InstructionError(
                        format!("undefined variable: {}", name),
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

            Inst::CALL => {
                // call() will borrow again, so don't borrow here
                // let binding = vm.current_frame();
                // let mut frame = binding.borrow_mut();

                // call closure
                let sp = vm.current_frame().borrow().sp;
                let params = vm.current_frame().borrow().stack[sp - 1].as_vec();
                let function = vm.current_frame().borrow().stack[sp - 2].as_fn();

                if let Some(func) = function {
                    if let Some(args) = params {
                        vm.current_frame().borrow_mut().sp -= 2;
                        let is_tail_call = vm.pc + 1 < insts.len() && insts[vm.pc + 1] == Inst::RET;
                        return self.call(vm, func, args, is_tail_call);
                    }
                }

                Err(SquareError::InstructionError(
                    format!(
                        "bad call, cannot call with {}",
                        vm.current_frame().borrow().stack[sp - 2]
                    ),
                    self.clone(),
                    vm.pc,
                ))
            }
            Inst::RET => {
                let binding = vm.current_frame();
                let frame = binding.borrow_mut();

                // jump back
                vm.pc = frame.ra;

                let top = frame.top().unwrap_or(&Value::Nil).clone();

                vm.pop_frame();

                // always return the top value
                vm.current_frame().borrow_mut().push(top);
                Ok(())
            }
            Inst::PUSH_CLOSURE(meta) => {
                if let Function::ClosureMeta(offset, captures) = meta {
                    // create closure
                    let mut upvalues = HashMap::new();
                    let ip = (vm.pc as i32) + offset;
                    let binding = vm.current_frame();
                    let mut frame = binding.borrow_mut();

                    let varnames = captures.iter().cloned().collect::<Vec<_>>();

                    // upgrade value to captured
                    for name in varnames {
                        if let Some(value) = frame.resolve_local(&name) {
                            let upvalue = value.upgrade();

                            upvalues.insert(name.clone(), upvalue.clone());
                            frame.insert_local(&name, upvalue);
                        } else {
                            // else undefined yet, if later be defined in same scope,
                            // the value will be assigned
                            let upvalue = Value::UpValue(Rc::new(RefCell::new(Value::Nil)));

                            upvalues.insert(name.clone(), upvalue.clone());
                            frame.insert_local(&name, upvalue);
                        }
                    }

                    return {
                        frame.push(Value::Function(Rc::new(RefCell::new(Function::Closure(
                            ip as usize,
                            upvalues,
                        )))));
                        Ok(())
                    };
                }

                unreachable!()
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
                let top = Builtin::get_internal_vec(&frame.stack[frame.sp - 1]);

                if let Some(val) = top {
                    let pack = val.borrow().clone();

                    let index = if *i > 0 {
                        *i as usize
                    } else {
                        if *offset >= pack.len() {
                            return Err(SquareError::InstructionError(
                                format!(
                                    "bad peek_vec, offset {} out of range, pack length is {}",
                                    offset,
                                    pack.len()
                                ),
                                self.clone(),
                                vm.pc,
                            ));
                        }

                        let len = (pack.len() - offset) as i32;

                        ((*i + len) % len) as usize + offset
                    };

                    if index < pack.len() {
                        return {
                            frame.push(pack[index].clone());
                            Ok(())
                        };
                    } else {
                        return Err(SquareError::InstructionError(
                            format!(
                                "bad peek_vec, index {} out of range, pack length is {}",
                                index,
                                pack.len()
                            ),
                            self.clone(),
                            vm.pc,
                        ));
                    }
                }

                Err(SquareError::InstructionError(
                    format!(
                        "bad peek_vec, top value is not a vector, got {}",
                        frame.top().unwrap_or(&Value::Nil).clone()
                    ),
                    self.clone(),
                    vm.pc,
                ))
            }

            Inst::GET(key) => {
                let target = vm.current_frame().borrow_mut().pop();

                if let Some(obj) = target.as_obj() {
                    // support proxy method
                    if let Some(getter) = obj
                        .borrow_mut()
                        .get(GETTER_KEY)
                        .unwrap_or(&Value::Nil)
                        .as_fn()
                    {
                        return self.call(
                            vm,
                            getter,
                            Rc::new(RefCell::new(vec![Value::Str(key.to_string())])),
                            false,
                        );
                    }

                    let get = vm.buildin.get_syscall("get");

                    get(
                        vm,
                        Rc::new(RefCell::new(vec![target, Value::Str(key.to_string())])),
                        self,
                    )
                } else {
                    Err(SquareError::InstructionError(
                        format!(
                            "bad peek_obj, top value is not an object, got {}",
                            vm.current_frame()
                                .borrow_mut()
                                .top()
                                .unwrap_or(&Value::Nil)
                                .clone()
                        ),
                        self.clone(),
                        vm.pc,
                    ))
                }
            }
            Inst::SET(key) => {
                let sp = vm.current_frame().borrow().sp;
                let value = vm.current_frame().borrow().stack[sp - 1].clone();
                let target = vm.current_frame().borrow().stack[sp - 2].clone();

                if let Some(obj) = target.as_obj() {
                    vm.current_frame().borrow_mut().sp -= 2;

                    if let Some(setter) = obj
                        .borrow_mut()
                        .get(SETTER_KEY)
                        .unwrap_or(&Value::Nil)
                        .as_fn()
                    {
                        return self.call(
                            vm,
                            setter,
                            Rc::new(RefCell::new(vec![Value::Str(key.to_string()), value])),
                            false,
                        );
                    }

                    let set = vm.buildin.get_syscall("set");

                    return set(
                        vm,
                        Rc::new(RefCell::new(vec![
                            target,
                            Value::Str(key.to_string()),
                            value,
                        ])),
                        self,
                    );
                }

                Err(SquareError::InstructionError(
                    format!(
                        "bad patch_obj, top value is not an object, got {}",
                        vm.current_frame().borrow_mut().stack[sp - 2].clone()
                    ),
                    self.clone(),
                    vm.pc,
                ))
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

    pub fn call(
        &self,
        vm: &mut VM,
        closure: Rc<RefCell<Function>>,
        params: Rc<RefCell<Vec<Value>>>,
        is_tail_call: bool,
    ) -> ExecResult {
        match *closure.borrow() {
            Function::ClosureMeta(..) => unreachable!(),
            Function::Closure(ip, ref upvalues) => {
                if is_tail_call {
                    let binding = vm.current_frame();
                    let mut frame = binding.borrow_mut();

                    // always push params as first operand
                    frame.stack[0] = Value::Vec(params);
                    frame.sp = 1;
                    frame.clear_locals();
                    // fill up captures
                    frame.extend_locals(upvalues.clone());
                } else {
                    let mut new_frame = CallFrame::new();
                    new_frame.ra = vm.pc;
                    // always push params as first operand
                    new_frame.stack[0] = Value::Vec(params);
                    new_frame.sp = 1;
                    // fill up captures
                    new_frame.extend_locals(upvalues.clone());

                    vm.push_frame(new_frame);
                }

                // jump to function
                vm.pc = ip;
                Ok(())
            }
            Function::Syscall(name) => {
                let syscall = vm.buildin.get_syscall(name);
                syscall(vm, params, self)
            }
            Function::Contiuation(ra, ref context) => {
                vm.pc = ra;
                vm.restore_context(context.clone());

                vm.current_frame()
                    .borrow_mut()
                    .push(params.borrow().first().unwrap_or(&Value::Nil).clone());
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub locals: HashMap<String, Value>,

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
        for (key, value) in &self.locals {
            writeln!(f, "{:>8}: {}", key, value)?;
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
            locals: HashMap::new(),
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

    #[inline]
    pub fn insert_local(&mut self, name: &str, value: Value) {
        self.locals.insert(name.to_string(), value);
    }

    #[inline]
    pub fn extend_locals(&mut self, upvalues: HashMap<String, Value>) {
        self.locals.extend(upvalues);
    }

    #[inline]
    pub fn resolve_local(&self, name: &str) -> Option<&Value> {
        self.locals.get(name)
    }

    #[inline]
    pub fn clear_locals(&mut self) {
        self.locals.clear();
    }

    pub fn assign_local(&mut self, name: &str, value: Value) {
        let new = if let Value::UpValue(upval) = value {
            upval.borrow().clone() // if ref appeared as right value, it should be cloned
        } else {
            value
        };

        if let Some(Value::UpValue(old)) = self.locals.get(name) {
            *old.borrow_mut() = new;
        } else {
            self.insert_local(name, new);
        }
    }
}

pub struct VM {
    pub call_frames: Vec<Rc<RefCell<CallFrame>>>,
    pub pc: usize,
    pub mpc: usize,

    buildin: Builtin,

    #[cfg(test)]
    inst_times: HashMap<&'static str, (u128, usize)>,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            call_frames: vec![Rc::new(RefCell::new(CallFrame::new()))],
            buildin: Builtin::new(),
            pc: 0,
            mpc: 0,

            #[cfg(test)]
            inst_times: HashMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.pc = 0;
        self.mpc = 0;
        self.call_frames
            .splice(0.., vec![Rc::new(RefCell::new(CallFrame::new()))]);

        #[cfg(test)]
        self.inst_times.clear();
    }

    #[inline]
    pub fn current_frame(&mut self) -> Rc<RefCell<CallFrame>> {
        self.call_frames.last().unwrap().clone()
    }

    #[inline]
    pub fn push_frame(&mut self, frame: CallFrame) {
        self.call_frames.push(Rc::new(RefCell::new(frame)))
    }

    #[inline]
    pub fn pop_frame(&mut self) -> Option<Rc<RefCell<CallFrame>>> {
        self.call_frames.pop()
    }

    #[inline]
    pub fn save_context(&self) -> Vec<Rc<RefCell<CallFrame>>> {
        self.call_frames.clone()
    }

    #[inline]
    pub fn restore_context(&mut self, context: Vec<Rc<RefCell<CallFrame>>>) {
        self.call_frames = context;
    }

    pub fn step(&mut self, insts: &Vec<Inst>) -> ExecResult {
        let inst = &insts[self.pc];

        #[cfg(test)]
        println!("{}: {}", self.pc, inst);

        #[cfg(test)]
        let start = Instant::now();

        inst.exec(self, insts)?;

        self.pc += 1;

        #[cfg(test)]
        {
            let duration = start.elapsed().as_nanos();
            let entry = self.inst_times.entry(inst.name()).or_insert((0, 0));
            *entry = (entry.0 + duration, entry.1 + 1);
        }

        #[cfg(test)]
        println!(
            "-------- CallFrame {} --------\n{}",
            self.call_frames.len() - 1,
            self.current_frame().borrow()
        );

        Ok(())
    }

    pub fn run(&mut self, insts: &Vec<Inst>) -> ExecResult {
        self.current_frame().borrow_mut().ra = insts.len();

        #[cfg(test)]
        self.inst_times.clear();

        while self.pc < insts.len() {
            self.step(insts)?;
        }

        Ok(())
    }

    #[cfg(test)]
    pub fn print_times(&self) {
        let mut data: Vec<_> = self
            .inst_times
            .iter()
            .map(|(name, (total_time, count))| {
                (
                    name,
                    *total_time,
                    (*total_time as f64) / (*count as f64),
                    *count,
                )
            })
            .collect();

        println!();
        // Sort and print by total time
        data.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        for (name, total_time, ..) in &data {
            println!("Total time for {}: {} ns", name, total_time);
        }

        println!();
        // Sort and print by average time
        data.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap());
        for (name, _, average_time, _) in &data {
            println!("Average time for {}: {:.2} ns", name, average_time);
        }

        println!();
        // Sort and print by count
        data.sort_by(|a, b| b.3.partial_cmp(&a.3).unwrap());
        for (name, .., count) in &data {
            println!("Count for {}: {} times", name, count);
        }
    }
}

#[test]
fn test_grow_operand_stack() {
    let mut vm = VM::new();
    let mut insts = vec![];

    for i in 0..100 {
        insts.push(Inst::PUSH(Value::Num(i as f64)))
    }

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert!(callframe.stack.len() >= 100);
    assert_eq!(callframe.sp, 100);
    assert_eq!(callframe.top(), Some(&Value::Num(99.0)));
}

#[test]
fn test_exec_token() {
    let code = "42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load() {
    let code = "x";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame()
        .borrow_mut()
        .assign_local("x", Value::Num(42.0));
    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load_undefined() {
    let code = "x";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD("x".to_string()),
            1,
        ))
    );
}

#[test]
fn test_exec_define() {
    let code = "[let x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign() {
    let code = "[let x nil] [= x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_capture() {
    let code = "[let x nil] [begin [= x 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand() {
    let code = "[let [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand() {
    let code = "[let x nil] [= [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand_capture() {
    let code = "[let x nil] [begin [= [x] [vec 42]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot() {
    let code = "[let [. x] [vec 1 42 3]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot_error() {
    let code = "[let [. x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts),
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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_greed_error() {
    let code = "[let [... x] [vec]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts),
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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_chain() {
    let code = "[let x [let y [let z 42]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
    assert_eq!(callframe.resolve_local("y").unwrap(), &Value::Num(42.0));
    assert_eq!(callframe.resolve_local("z").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_op() {
    let code = "[- 1 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(-1.0))
}

#[test]
fn test_exec_op_assign() {
    let code = "[+= x 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame()
        .borrow_mut()
        .assign_local("x", Value::Num(1.0));
    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0));
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(3.0))
}

#[test]
fn test_exec_op_assign_dot() {
    let code = "[let x [obj 'y' [obj 'z' 0]]]
        [+= x.y.z 42]
        x.y.z";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_dot() {
    let code = "o.x.y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    let mut o = HashMap::new();
    let mut x = HashMap::new();
    let y = Value::Num(42.0);
    x.insert("y".to_string(), y);
    o.insert("x".to_string(), Value::Obj(Rc::new(RefCell::new(x))));
    vm.current_frame()
        .borrow_mut()
        .assign_local("o", Value::Obj(Rc::new(RefCell::new(o))));
    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_dot() {
    let code = "[= o.x.y 42] o.x.y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    let mut o = HashMap::new();
    let mut x = HashMap::new();
    let y = Value::Num(24.0);
    x.insert("y".to_string(), y);
    o.insert("x".to_string(), Value::Obj(Rc::new(RefCell::new(x))));
    vm.current_frame()
        .borrow_mut()
        .assign_local("o", Value::Obj(Rc::new(RefCell::new(o))));
    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    insts.iter().for_each(|inst| println!("{}", inst));
    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(44.0))
}

#[test]
fn test_exec_begin() {
    let code = "[begin 1 2 3]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(1.0));
    assert_eq!(callframe.resolve_local("y").unwrap(), &Value::Num(2.0));
}

#[test]
fn test_exec_tail_call() {
    let code = "[begin [begin [begin 42]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut max_depth = vm.call_frames.len();

    vm.current_frame().borrow_mut().ra = insts.len();

    while vm.pc < insts.len() {
        vm.step(&insts).unwrap();
        if vm.call_frames.len() > max_depth {
            max_depth = vm.call_frames.len();
        }
    }

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
    assert_eq!(max_depth, 2);
}

#[test]
fn test_exec_if_true() {
    let code = "[if true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_if_false_nil() {
    let code = "[if false 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Nil)
}

#[test]
fn test_exec_if_false() {
    let code = "[if false 42 24]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(24.0))
}

#[test]
fn test_exec_while() {
    let code = "[while [< x 4] [begin [+= x 1]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame()
        .borrow_mut()
        .assign_local("x", Value::Num(0.0));
    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(4.0))
}

#[test]
fn test_exec_match() {
    let code = "[cond
        [false 24]
        [true 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_fn_call() {
    let code = "[/[] 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_call_with_params() {
    let code = "[/[x] x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(24.0))
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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap(); // FIXME: should report undefined variable: x
    assert_eq!(vm.current_frame().borrow().top(), Some(&Value::Nil))
}

#[test]
fn test_exec_fn_capture_shadow() {
    let code = "
[let fn /[] [begin ;this x should be shadow: ; x [let x 24] x]]
[let x 42]
[fn]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD("x".to_string()),
            4,
        )),
    );
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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(
        callframe.resolve_local("t").unwrap(),
        &Value::Str("fn".to_string())
    )
}

#[test]
fn test_exec_builtin_vec_methods() {
    let code = "
[let v [vec 1 2 3]]
[= v.at /[index] [at this index]]

[let x [v.at 0]]

[splice v 0 1 [vec 4]]

[let y [at v 0]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(1.0));
    assert_eq!(callframe.resolve_local("y").unwrap(), &Value::Num(4.0))
}

#[test]
fn test_exec_getter() {
    let code = format!(
        "
[let o [obj]]

[= o.x 0]

[= o.{} /[k] 42]

o.x
",
        GETTER_KEY
    );

    let ast = parse(&code, &mut Position::new()).unwrap();
    let insts = emit(&code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_setter() {
    let code = format!(
        "
[let o [obj]]

[= o.x 0]

[= o.{} /[k v] [set this k [+ 1 v]]]

[= o.x 41]

o.x
",
        SETTER_KEY
    );

    let ast = parse(&code, &mut Position::new()).unwrap();
    let insts = emit(&code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_break() {
    let code = "
[let x [callcc /[cc] [begin [cc 42] 24]]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_cc1() {
    let code = "
[let cc [callcc /[cc] cc]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("cc").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_cc2() {
    let code = "
[let cc [callcc /[cc] [cc cc]]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("cc").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_cc3() {
    let code = "
[let cc [callcc /[cc] [callcc cc]]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("cc").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_cc4() {
    let code = "
[let cc [callcc callcc]]

[cc 42]

cc
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();

    let binding = vm.current_frame();
    let callframe = binding.borrow_mut();
    assert_eq!(callframe.resolve_local("cc").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_abort() {
    let code = "
[let cc [callcc /[cc] cc]]

[let x [begin [cc 42] 24]]

x
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD("x".to_string()),
            26,
        )),
    );
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
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts).unwrap();
    vm.print_times();
}
