use alloc::{boxed::Box, format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use core::fmt;

use hashbrown::HashMap;

use crate::{
    errors::SquareError,
    vm_insts::Inst,
    vm_value::{Closure, Value},
};

type OpFn = dyn Fn(&Value, &Value) -> Value;
type ExecResult<'a> = Result<(), SquareError<'a>>;

impl Inst {
    fn exec<'a>(&self, vm: &mut VM, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult<'a> {
        match self {
            Inst::PUSH(value) => self.push(vm, value.clone(), pc),
            Inst::POP => self.pop(vm, pc),

            Inst::ADD => self.binop(vm, &|a, b| a + b, pc),
            Inst::SUB => self.binop(vm, &|a, b| a - b, pc),
            Inst::MUL => self.binop(vm, &|a, b| a * b, pc),
            Inst::DIV => self.binop(vm, &|a, b| a / b, pc),
            Inst::REM => self.binop(vm, &|a, b| a % b, pc),
            Inst::BITAND => self.binop(vm, &|a, b| a & b, pc),
            Inst::BITOR => self.binop(vm, &|a, b| a | b, pc),
            Inst::BITXOR => self.binop(vm, &|a, b| a ^ b, pc),
            Inst::EQ => self.binop(vm, &|a, b| Value::Bool(a == b), pc),
            Inst::NE => self.binop(vm, &|a, b| Value::Bool(a != b), pc),
            Inst::LT => self.binop(vm, &|a, b| Value::Bool(a < b), pc),
            Inst::LE => self.binop(vm, &|a, b| Value::Bool(a <= b), pc),
            Inst::GT => self.binop(vm, &|a, b| Value::Bool(a > b), pc),
            Inst::GE => self.binop(vm, &|a, b| Value::Bool(a >= b), pc),
            Inst::SHL => self.binop(vm, &|a, b| a << b, pc),
            Inst::SHR => self.binop(vm, &|a, b| a >> b, pc),
            Inst::BITNOT => {
                let frame = vm.call_frame.as_mut().unwrap();

                if frame.sp < 1 {
                    return Err(SquareError::RuntimeError(
                        format!("bad binary operation, operand stack length is {}", frame.sp),
                        self.clone(),
                        *pc,
                    ));
                }

                frame.stack[frame.sp - 1] = !&frame.stack[frame.sp - 1];
                Ok(())
            }

            Inst::JMP(value) => self.jump(insts, pc, *value),
            Inst::JNE(value) => {
                let frame = vm.call_frame.as_mut().unwrap();

                if frame.sp < 1 {
                    return Err(SquareError::RuntimeError(
                        "bad jne, operand stack empty".to_string(),
                        self.clone(),
                        *pc,
                    ));
                }

                if !frame.top().unwrap().to_bool() {
                    frame.sp -= 1;
                    self.jump(insts, pc, *value)
                } else {
                    frame.sp -= 1;
                    Ok(())
                }
            }

            Inst::STORE(name) => {
                let frame = vm.call_frame.as_mut().unwrap();

                if let Some(val) = frame.top() {
                    frame.define_local(name, val.clone());
                    self.pop(vm, pc)
                } else {
                    frame.define_local(name, Value::Nil);
                    Ok(())
                }
            }
            Inst::LOAD(name) => {
                let frame = vm.call_frame.as_mut().unwrap();
                let mut tmp = Value::Nil;

                if let Some(val) = frame.resolve_local(name) {
                    tmp = val.clone(); // avoid mutable ref and immutable ref of vm at the same time
                }
                self.push(vm, tmp, pc)
            }

            Inst::CALL => {
                let frame = vm.call_frame.as_mut().unwrap();

                if let Some(Value::Closure(closure)) = frame.top() {
                    let mut new_frame = CallFrame::new();

                    new_frame.prev = vm.call_frame.take();
                    // return address is next instruction
                    new_frame.ra = *pc + 1;

                    vm.call_frame = Some(Box::new(new_frame));
                    // jump to function definition
                    *pc = closure.ip;

                    return Ok(());
                }

                Err(SquareError::RuntimeError(
                    "invalid call".to_string(),
                    self.clone(),
                    *pc,
                ))
            }
            Inst::RET => {
                let frame = vm.call_frame.as_mut().unwrap();
                let top = frame.top().unwrap_or(&Value::Nil).clone();

                vm.call_frame = frame.prev.take();
                // jump back
                *pc = frame.ra;
                // always return the top value
                self.push(vm, top, pc)
            }
        }
    }

    fn push<'a>(&self, vm: &mut VM, value: Value, _pc: &mut usize) -> ExecResult<'a> {
        let frame = vm.call_frame.as_mut().unwrap();

        if frame.sp >= frame.stack.len() {
            frame.stack.resize(frame.stack.len() * 2, Value::Nil);
        }

        frame.stack[frame.sp] = value;
        frame.sp += 1;
        Ok(())
    }

    fn pop<'a>(&self, vm: &mut VM, pc: &mut usize) -> ExecResult<'a> {
        let frame = vm.call_frame.as_mut().unwrap();

        if frame.sp < 1 {
            return Err(SquareError::RuntimeError(
                "bad pop, operand stack empty".to_string(),
                self.clone(),
                *pc,
            ));
        }
        frame.sp -= 1;
        Ok(())
    }

    fn binop<'a>(&self, vm: &mut VM, op_fn: &OpFn, pc: &mut usize) -> ExecResult<'a> {
        let frame = vm.call_frame.as_mut().unwrap();

        if frame.sp < 2 {
            return Err(SquareError::RuntimeError(
                format!("bad binary operation, operand stack length is {}", frame.sp),
                self.clone(),
                *pc,
            ));
        }

        frame.stack[frame.sp - 2] = op_fn(&frame.stack[frame.sp - 2], &frame.stack[frame.sp - 1]);
        frame.sp -= 1;
        Ok(())
    }

    fn jump<'a>(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) -> ExecResult<'a> {
        let new_pc = *pc as i32 + offset;
        if new_pc < 0 || new_pc >= insts.len() as i32 {
            return Err(SquareError::RuntimeError(
                "bad pc".to_string(),
                self.clone(),
                *pc,
            ));
        }

        *pc = new_pc as usize;
        Ok(())
    }
}

pub struct CallFrame {
    locals: HashMap<String, Value>,

    // operand stack
    stack: Vec<Value>,
    // fake length, avoid frequent operand stak push/pop
    sp: usize,

    ra: usize, // return address
    prev: Option<Box<CallFrame>>,
}

impl fmt::Display for CallFrame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Return Address: {}", self.ra)?;
        writeln!(f, "Locals:")?;
        for (key, value) in &self.locals {
            writeln!(f, "{:>8}: {}", key, value)?;
        }
        writeln!(f, "Operand Stack:")?;
        for (index, value) in self.stack.iter().enumerate() {
            if index >= self.sp {
                break;
            }

            writeln!(f, "{:8}: {}", index, value)?;
        }

        Ok(())
    }
}

impl CallFrame {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            stack: vec![Value::Nil; 16], // item count, not byte length
            sp: 0,
            ra: 0,
            prev: None,
        }
    }

    fn find_frame_by_varname<'a>(&mut self, name: &'a str) -> Option<&mut CallFrame> {
        if self.locals.contains_key(name) {
            return Some(self);
        }

        let mut frame = &mut self.prev;
        while let Some(f) = frame {
            if f.locals.contains_key(name) {
                return Some(f.as_mut());
            }
            frame = &mut f.prev;
        }
        return None;
    }

    pub fn resolve_local<'a>(&mut self, name: &'a str) -> Option<&Value> {
        let frame = self.find_frame_by_varname(name)?;

        return frame.locals.get(name);
    }

    pub fn define_local<'a>(&mut self, name: &'a str, value: Value) {
        if let Some(frame) = self.find_frame_by_varname(name) {
            frame.locals.insert(name.to_string(), value);
        } else {
            self.locals.insert(name.to_string(), value);
        }
    }

    pub fn top(&self) -> Option<&Value> {
        if self.sp <= 0 {
            return None;
        }

        Some(&self.stack[self.sp - 1])
    }
}

#[test]
fn test_resolve_local() {
    let mut top = CallFrame::new();

    top.locals.insert("a".to_string(), Value::Int(42));

    let mut frame = CallFrame::new();

    frame.prev = Some(Box::new(top));
    frame.locals.insert("b".to_string(), Value::Int(24));

    assert_eq!(frame.resolve_local("a"), Some(&Value::Int(42)));
    assert_eq!(frame.resolve_local("b"), Some(&Value::Int(24)));
    assert_eq!(frame.resolve_local("c"), None);
}

#[test]
fn test_define_local() {
    let mut top = CallFrame::new();

    top.define_local("a", Value::Int(42));

    let mut frame = CallFrame::new();

    frame.prev = Some(Box::new(top));
    frame.define_local("b", Value::Int(24));

    assert_eq!(frame.resolve_local("a"), Some(&Value::Int(42)));
    assert_eq!(frame.resolve_local("b"), Some(&Value::Int(24)));
    assert_eq!(frame.resolve_local("c"), None);
}

pub struct VM {
    pub call_frame: Option<Box<CallFrame>>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            call_frame: Some(Box::new(CallFrame::new())),
        }
    }

    pub fn reset(&mut self) {
        self.call_frame = Some(Box::new(CallFrame::new()));
    }

    pub fn step<'a>(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult<'a> {
        let inst = &insts[*pc];
        inst.exec(self, insts, pc)?;
        *pc += 1;

        Ok(())
    }

    pub fn run<'a>(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult<'a> {
        while *pc < insts.len() {
            self.step(insts, pc)?;
        }

        Ok(())
    }
}

#[test]
fn test_many_operands() {
    let mut vm = VM::new();
    let mut insts = vec![];

    for i in 0..100 {
        insts.push(Inst::PUSH(Value::Int(i)))
    }

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.call_frame.as_mut().unwrap();

    assert_eq!(callframe.stack.len() >= 100, true);
    assert_eq!(callframe.sp, 100);
    assert_eq!(callframe.top(), Some(&Value::Int(99)));
}

#[test]
fn test_load_undefined() {
    let mut vm = VM::new();
    let insts = vec![
        Inst::LOAD("a".to_string()),
        Inst::LOAD("a".to_string()),
        Inst::EQ,
    ];

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.call_frame.as_mut().unwrap();
    assert_eq!(callframe.top(), Some(&Value::Bool(true)));
}

#[test]
fn test_store_undefined() {
    let mut vm = VM::new();
    let insts = vec![Inst::STORE("a".to_string())];

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.call_frame.as_mut().unwrap();
    assert_eq!(callframe.resolve_local("a"), Some(&Value::Nil));
}

#[test]
fn test_call_ret() {
    let mut vm = VM::new();
    let insts = vec![
        Inst::CALL,
        Inst::CALL,
        Inst::CALL,
        Inst::PUSH(Value::Int(42)),
        Inst::RET,
        Inst::RET,
        Inst::RET,
        Inst::BITNOT,
    ];

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.call_frame.as_mut().unwrap();
    assert_eq!(callframe.top(), Some(&Value::Int(!42)));
}

#[test]
fn test_jump_if() {
    let mut vm = VM::new();
    let mut insts = vec![
        Inst::PUSH(Value::Bool(true)), // condition
        Inst::JNE(2),
        Inst::PUSH(Value::Int(42)), // true branch
        Inst::JMP(1),
        Inst::PUSH(Value::Int(24)), // false branch
    ];

    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.call_frame.as_mut().unwrap();
    assert_eq!(callframe.top(), Some(&Value::Int(42)));

    insts[0] = Inst::PUSH(Value::Bool(false));

    vm.reset();
    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.call_frame.as_mut().unwrap();
    assert_eq!(callframe.top(), Some(&Value::Int(24)));
}

#[test]
fn test_jump_while() {
    let mut vm = VM::new();
    let insts = vec![
        // i = 0
        Inst::PUSH(Value::Int(0)),
        Inst::STORE("i".to_string()),
        // while i < 10
        Inst::LOAD("i".to_string()),
        Inst::PUSH(Value::Int(10)),
        Inst::LT,
        // i = i + 1
        Inst::JNE(5),
        Inst::LOAD("i".to_string()),
        Inst::PUSH(Value::Int(1)),
        Inst::ADD,
        Inst::STORE("i".to_string()),
        Inst::JMP(-9), // pc will automaticlly increase 1, which is different from jump forward
    ];

    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.call_frame.as_mut().unwrap();
    assert_eq!(callframe.resolve_local("i"), Some(&Value::Int(10)));
}
