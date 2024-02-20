use alloc::{format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use core::{cell::RefCell, fmt};

use hashbrown::HashMap;

use crate::{
    builtin::Builtin,
    errors::SquareError,
    vm_insts::Inst,
    vm_value::{CalcResult, Closure, Value},
};

#[cfg(test)]
use crate::code_frame::Position;
#[cfg(test)]
use crate::emit::{emit, EmitContext};
#[cfg(test)]
use crate::parse::parse;

pub type ExecResult = Result<(), SquareError>;

type OpFn = dyn Fn(&Value, &Value) -> CalcResult;

impl Inst {
    fn exec(&self, vm: &mut VM, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        match self {
            Inst::PUSH(value) => Ok(vm.current_frame().push(value.clone())),
            Inst::POP => Ok(vm.current_frame().pop()),

            Inst::ADD => self.binop(vm, &|a, b| a + b, pc),
            Inst::SUB => self.binop(vm, &|a, b| a - b, pc),
            Inst::MUL => self.binop(vm, &|a, b| a * b, pc),
            Inst::DIV => self.binop(vm, &|a, b| a / b, pc),
            Inst::REM => self.binop(vm, &|a, b| a % b, pc),
            Inst::BITAND => self.binop(vm, &|a, b| a & b, pc),
            Inst::BITOR => self.binop(vm, &|a, b| a | b, pc),
            Inst::BITXOR => self.binop(vm, &|a, b| a ^ b, pc),
            Inst::EQ => self.binop(vm, &|a, b| Ok(Value::Bool(a == b)), pc),
            Inst::NE => self.binop(vm, &|a, b| Ok(Value::Bool(a != b)), pc),
            Inst::LT => self.binop(vm, &|a, b| Ok(Value::Bool(a < b)), pc),
            Inst::LE => self.binop(vm, &|a, b| Ok(Value::Bool(a <= b)), pc),
            Inst::GT => self.binop(vm, &|a, b| Ok(Value::Bool(a > b)), pc),
            Inst::GE => self.binop(vm, &|a, b| Ok(Value::Bool(a >= b)), pc),
            Inst::SHL => self.binop(vm, &|a, b| a << b, pc),
            Inst::SHR => self.binop(vm, &|a, b| a >> b, pc),
            Inst::BITNOT => {
                let frame = vm.current_frame();

                if frame.sp < 1 {
                    return Err(SquareError::InstructionError(
                        format!("bad binary operation, operand stack length is {}", frame.sp),
                        self.clone(),
                        *pc,
                    ));
                }

                let result = !&frame.stack[frame.sp - 1];

                if let Err(SquareError::RuntimeError(msg)) = result {
                    return Err(SquareError::InstructionError(msg, self.clone(), *pc));
                }

                frame.stack[frame.sp - 1] = result?;
                Ok(())
            }

            Inst::JMP(value) => self.jump(insts, pc, *value),
            Inst::JNE(value) => {
                let frame = vm.current_frame();

                if frame.sp < 1 {
                    return Err(SquareError::InstructionError(
                        "bad jne, operand stack empty".to_string(),
                        self.clone(),
                        *pc,
                    ));
                }

                if !frame.top().unwrap().as_bool() {
                    frame.pop();
                    self.jump(insts, pc, *value)
                } else {
                    Ok(frame.pop())
                }
            }

            Inst::STORE(name) => {
                let frame = vm.current_frame();

                if let Some(val) = frame.top() {
                    let cloned = val.clone();
                    frame.pop();
                    Ok(frame.assign_local(name, cloned))
                } else {
                    Err(SquareError::InstructionError(
                        "bad store, operand stack empty".to_string(),
                        self.clone(),
                        *pc,
                    ))
                }
            }
            Inst::LOAD(name) => {
                let frame = vm.current_frame();
                if let Some(value) = frame.resolve_local(name) {
                    let cloned = value.clone();
                    Ok(frame.push(cloned))
                } else {
                    Err(SquareError::InstructionError(
                        format!("undefined variable: {}", name),
                        self.clone(),
                        *pc,
                    ))
                }
            }

            Inst::CALL => {
                // call closure
                let frame = vm.current_frame();

                if frame.sp < 2 {
                    return Err(SquareError::InstructionError(
                        "bad call, closure and params required".to_string(),
                        self.clone(),
                        *pc,
                    ));
                }

                let params = frame.stack[frame.sp - 1].clone();
                let func = frame.stack[frame.sp - 2].clone();

                frame.sp -= 2;

                let error = Err(SquareError::InstructionError(
                    format!("bad call, cannot call with {}", func),
                    self.clone(),
                    *pc,
                ));
                let is_tail_call = *pc + 1 < insts.len() && insts[*pc + 1] == Inst::RET;

                match func {
                    Value::Closure(closure) => self.call(vm, closure, params, is_tail_call, pc),
                    Value::UpValue(val) => {
                        if let Value::Closure(ref closure) = *val.borrow() {
                            self.call(vm, closure.clone(), params, is_tail_call, pc)
                        } else {
                            error
                        }
                    }
                    _ => error,
                }
            }
            Inst::RET => {
                let frame = vm.current_frame();

                // jump back
                *pc = frame.ra;

                let top = frame.top().unwrap_or(&Value::Nil).clone();

                vm.pop_frame();

                // always return the top value
                Ok(vm.current_frame().push(top))
            }
            Inst::PUSH_CLOSURE(meta) => {
                // create closure
                let mut closure = meta.clone();
                closure.ip = (*pc as i32) + closure.ip;

                if closure.ip < 0 {
                    return Err(SquareError::InstructionError(
                        format!("invalid function address: {}", closure.ip),
                        self.clone(),
                        *pc,
                    ));
                }

                let frame = vm.current_frame();

                // upgrade value to captured
                for name in closure.captures.iter().cloned().collect::<Vec<_>>() {
                    if let Some(value) = frame.resolve_local(&name) {
                        let upvalue = value.upgrade();

                        closure.capture(&name, &upvalue);
                        frame.insert_local(&name, upvalue.clone());
                    } else {
                        // else undefined yet, if later be defined in same scope,
                        // the value will be updated
                        let upvalue = Value::UpValue(Rc::new(RefCell::new(Value::Nil)));

                        closure.capture(&name, &upvalue);
                        frame.insert_local(&name, upvalue.clone());
                    }
                }

                Ok(frame.push(Value::Closure(Rc::new(RefCell::new(closure)))))
            }

            Inst::PACK(len) => {
                let frame = vm.current_frame();

                if frame.sp < *len {
                    return Err(SquareError::InstructionError(
                        format!("bad pack, {} requied, {} provided", len, frame.sp),
                        self.clone(),
                        *pc,
                    ));
                }

                let mut result = vec![];
                for index in frame.sp - len..frame.sp {
                    result.push(frame.stack[index].clone());
                }
                frame.sp -= *len;
                Ok(frame.push(Value::Vec(Rc::new(RefCell::new(result)))))
            }
            Inst::PEEK(offset, i) => {
                let frame = vm.current_frame();
                let result = if let Some(Value::Vec(val)) = frame.top() {
                    let pack = val.borrow();

                    if *offset >= pack.len() {
                        return Err(SquareError::InstructionError(
                            format!(
                                "bad peek, offset {} out of range, pack length is {}",
                                offset,
                                pack.len()
                            ),
                            self.clone(),
                            *pc,
                        ));
                    }

                    let len = (pack.len() - offset) as i32;
                    let index = if *i > 0 { *i } else { (i + len) % len } as usize + offset;

                    if index < pack.len() {
                        Ok(pack.get(index).unwrap().clone())
                    } else {
                        Err(SquareError::InstructionError(
                            format!(
                                "bad peek, index {} out of range, pack length is {}",
                                index,
                                pack.len()
                            ),
                            self.clone(),
                            *pc,
                        ))
                    }
                } else {
                    Err(SquareError::InstructionError(
                        "bad peek, top value is not a pack".to_string(),
                        self.clone(),
                        *pc,
                    ))
                };

                Ok(frame.push(result?))
            }
        }
    }

    fn binop(&self, vm: &mut VM, op_fn: &OpFn, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();

        if frame.sp < 2 {
            return Err(SquareError::InstructionError(
                format!("bad binary operation, operand stack length is {}", frame.sp),
                self.clone(),
                *pc,
            ));
        }

        let result = op_fn(&frame.stack[frame.sp - 2], &frame.stack[frame.sp - 1]);

        if let Err(SquareError::RuntimeError(msg)) = result {
            return Err(SquareError::InstructionError(msg, self.clone(), *pc));
        }

        frame.stack[frame.sp - 2] = result?;
        frame.sp -= 1;
        Ok(())
    }

    fn jump(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) -> ExecResult {
        let new_pc = *pc as i32 + offset;
        if new_pc < 0 || new_pc >= insts.len() as i32 {
            return Err(SquareError::InstructionError(
                "bad pc".to_string(),
                self.clone(),
                *pc,
            ));
        }

        *pc = new_pc as usize;
        Ok(())
    }

    fn call(
        &self,
        vm: &mut VM,
        closure: Rc<RefCell<Closure>>,
        params: Value,
        is_tail_call: bool,
        pc: &mut usize,
    ) -> ExecResult {
        if closure.borrow().ip < 0 {
            return self.syscall(vm, closure.borrow().ip, params, pc);
        }

        if is_tail_call {
            let frame = vm.current_frame();
            // always push params as first operand
            frame.stack[0] = params;
            frame.sp = 1;
            frame.clear_local();
            // fill up captures
            frame.extend_locals(closure.borrow().upvalues.clone());
        } else {
            let mut frame = CallFrame::new();
            frame.ra = *pc;
            // always push params as first operand
            frame.stack[0] = params;
            frame.sp = 1;
            // fill up captures
            frame.extend_locals(closure.borrow().upvalues.clone());

            vm.push_frame(frame);
        }

        // jump to function
        *pc = closure.borrow().ip as usize;

        Ok(())
    }

    fn syscall(&self, vm: &mut VM, ip: i32, params: Value, pc: &mut usize) -> ExecResult {
        if let Some(syscall) = vm.buildin.syscalls.get(&ip) {
            syscall.clone()(vm, params, pc)
        } else {
            Err(SquareError::InstructionError(
                format!("undefined syscall {}", ip),
                self.clone(),
                *pc,
            ))
        }
    }
}

pub struct CallFrame {
    locals: HashMap<String, Value>,

    // operand stack
    stack: Vec<Value>,
    // fake stack pointer, avoid frequent operand stack push/pop
    sp: usize,

    ra: usize, // return address
}

impl fmt::Display for CallFrame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

impl CallFrame {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            stack: vec![Value::Nil; 8],
            sp: 0,
            ra: 0,
        }
    }

    pub fn push(&mut self, value: Value) {
        if self.sp >= self.stack.len() {
            self.stack.resize(self.stack.len() * 2, Value::Nil);
        }

        self.stack[self.sp] = value;
        self.sp += 1;
    }

    #[inline]
    pub fn pop(&mut self) {
        self.sp -= 1;
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
    pub fn resolve_local(&mut self, name: &str) -> Option<&Value> {
        self.locals.get(name)
    }

    #[inline]
    pub fn clear_local(&mut self) {
        self.locals.clear();
    }

    pub fn assign_local(&mut self, name: &str, value: Value) {
        if let Some(Value::UpValue(old)) = self.locals.get(name) {
            *old.borrow_mut() = if let Value::UpValue(new) = value {
                new.borrow().clone()
            } else {
                value
            };
        } else {
            self.insert_local(name, value);
        }
    }
}

pub struct VM {
    pub call_frames: Vec<CallFrame>,
    buildin: Builtin,
}

impl VM {
    pub fn new() -> Self {
        Self {
            call_frames: vec![CallFrame::new()],
            buildin: Builtin::new(),
        }
    }

    #[inline]
    pub fn current_frame(&mut self) -> &mut CallFrame {
        self.call_frames.last_mut().unwrap()
    }

    #[inline]
    pub fn push_frame(&mut self, frame: CallFrame) {
        self.call_frames.push(frame)
    }

    #[inline]
    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        self.call_frames.pop()
    }

    pub fn step(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        let inst = &insts[*pc];

        // #[cfg(test)]
        // println!("{}: {}", *pc, inst);

        inst.exec(self, insts, pc)?;

        // #[cfg(test)]
        // println!(
        //     "-------- CallFrame {} --------\n{}",
        //     self.call_frames.len() - 1,
        //     self.current_frame()
        // );

        *pc += 1;
        Ok(())
    }

    pub fn run(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        let buildin_values = self.buildin.values.clone();

        let frame = self.current_frame();

        frame.extend_locals(buildin_values);
        frame.ra = insts.len();

        while *pc < insts.len() {
            self.step(insts, pc)?;
        }

        Ok(())
    }
}

#[test]
fn test_grow_operand_stack() {
    let mut vm = VM::new();
    let mut insts = vec![];

    for i in 0..100 {
        insts.push(Inst::PUSH(Value::Num(i as f64)))
    }

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.stack.len() >= 100, true);
    assert_eq!(callframe.sp, 100);
    assert_eq!(callframe.top(), Some(&Value::Num(99.0)));
}

#[test]
fn test_exec_token() {
    let code = "42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load() {
    let code = "x";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().assign_local("x", Value::Num(42.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load_undefined() {
    let code = "x";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD("x".to_string()),
            0,
        ))
    );
}

#[test]
fn test_exec_define() {
    let code = "[let x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign() {
    let code = "[let x nil] [= x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand() {
    let code = "[let [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand() {
    let code = "[let x nil] [= [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot() {
    let code = "[let [. x] [vec 1 42 3]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot_error() {
    let code = "[let [. x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "bad peek, index 1 out of range, pack length is 1".to_string(),
            Inst::PEEK(0, 1),
            6,
        ))
    );
}

#[test]
fn test_exec_define_expand_greed() {
    let code = "[let [... x] [vec 1 2 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_greed_error() {
    let code = "[let [... x] [vec]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "bad peek, offset 0 out of range, pack length is 0".to_string(),
            Inst::PEEK(0, -1),
            3,
        ))
    );
}

#[test]
fn test_exec_define_expand_nested() {
    let code = "[let [. [x]] [vec 1 [vec 42] 3]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_op() {
    let code = "[- 1 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(-1.0))
}

#[test]
fn test_exec_op_assign() {
    let code = "[+= x 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().assign_local("x", Value::Num(1.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0));
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(3.0))
}

#[test]
fn test_exec_begin() {
    let code = "[begin 1 2 3]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
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

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(1.0));
    assert_eq!(callframe.resolve_local("y").unwrap(), &Value::Num(2.0));
}

#[test]
fn test_exec_tail_call() {
    let code = "[begin [begin 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut pc = 0;
    let mut max_depth = vm.call_frames.len();

    vm.current_frame().ra = insts.len();

    while pc < insts.len() {
        vm.step(&insts, &mut pc).unwrap();
        if vm.call_frames.len() > max_depth {
            max_depth = vm.call_frames.len();
        }
    }

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
    assert_eq!(max_depth, 2);
}

#[test]
fn test_exec_if_true() {
    let code = "[if true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_if_false_nil() {
    let code = "[if false 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Nil)
}

#[test]
fn test_exec_if_false() {
    let code = "[if false 42 24]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(24.0))
}

#[test]
fn test_exec_while() {
    let code = "[while [< x 4] [begin [+= x 1]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().assign_local("x", Value::Num(0.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(4.0))
}

#[test]
fn test_exec_fn_call() {
    let code = "[/[] 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_call_with_params() {
    let code = "[/[x] x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
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

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
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

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

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

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

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

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
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

    vm.run(&insts, &mut 0).unwrap(); // FIXME: should report undefined variable: x
    assert_eq!(vm.current_frame().top(), Some(&Value::Nil))
}

#[test]
fn test_exec_fn_capture_shadow() {
    let code = "
[let fn /[] [begin x [let x 24] x]]
[let x 42]
[fn]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD("x".to_string()),
            3,
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

    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.current_frame();

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

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_builtin() {
    let code = "
[let p println]
[p nil]
[let t [typeof p]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(
        callframe.resolve_local("t").unwrap(),
        &Value::Str("fn".to_string())
    )
}
