use alloc::{boxed::Box, format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use core::{cell::RefCell, fmt};

use hashbrown::HashMap;

#[cfg(not(test))]
use crate::println;

use crate::{
    errors::SquareError,
    vm_insts::Inst,
    vm_value::{CalcResult, Closure, Value},
};

type OpFn = dyn Fn(&Value, &Value) -> CalcResult;
pub type ExecResult = Result<(), SquareError>;

impl Inst {
    fn exec(&self, vm: &mut VM, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
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
                    return Err(SquareError::RuntimeError(
                        format!("bad binary operation, operand stack length is {}", frame.sp),
                        self.clone(),
                        *pc,
                    ));
                }

                let result = !&frame.stack[frame.sp - 1];

                if let Err(SquareError::TypeError(msg)) = result {
                    return Err(SquareError::RuntimeError(msg, self.clone(), *pc));
                }

                frame.stack[frame.sp - 1] = result?;
                Ok(())
            }

            Inst::JMP(value) => self.jump(insts, pc, *value),
            Inst::JNE(value) => {
                let frame = vm.current_frame();

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
                let frame = vm.current_frame();

                if let Some(val) = frame.top() {
                    frame.define_local(name, val.clone());
                    self.pop(vm, pc)
                } else {
                    frame.define_local(name, Value::Nil);
                    Ok(())
                }
            }
            Inst::LOAD(name) => {
                let frame = vm.current_frame();
                let result = if let Some(val) = frame.resolve_local(name) {
                    val.clone()
                } else {
                    Value::Nil
                };
                self.push(vm, result, pc)
            }

            Inst::CALL => {
                let frame = vm.current_frame();

                if frame.sp < 2 {
                    return Err(SquareError::RuntimeError(
                        "bad call, closure and params required".to_string(),
                        self.clone(),
                        *pc,
                    ));
                }

                if let Value::Closure(ref closure) = &frame.stack[frame.sp - 2] {
                    let new_pc = closure.ip;
                    let params = frame.stack[frame.sp - 1].clone();
                    frame.sp -= 2; // pop params and closure

                    let mut new_frame = CallFrame::new();
                    closure
                        .captures
                        .iter()
                        .for_each(|(key, value)| new_frame.define_local(key, value.clone()));
                    new_frame.prev = vm.call_frame.take();
                    new_frame.ra = *pc;

                    // jump to function definition
                    *pc = new_pc as usize;

                    vm.call_frame = Some(Box::new(new_frame));

                    // always push top value of last call_frame as params, the value should be a pack
                    return self.push(vm, params, pc);
                }

                Err(SquareError::RuntimeError(
                    format!("bad call, cannot call with {}", frame.stack[frame.sp - 2]),
                    self.clone(),
                    *pc,
                ))
            }
            Inst::RET => {
                let frame = vm.current_frame();
                let top = frame.top().unwrap_or(&Value::Nil).clone();

                // jump back
                *pc = frame.ra;
                vm.call_frame = frame.prev.take();
                // always return the top value
                self.push(vm, top, pc)
            }
            Inst::PUSH_CLOSURE(meta) => {
                let mut closure = meta.clone();
                closure.ip = (*pc as i32) + closure.ip;

                if closure.ip < 0 {
                    return Err(SquareError::RuntimeError(
                        format!("invalid function address: {}", closure.ip),
                        self.clone(),
                        *pc,
                    ));
                }

                let frame = vm.current_frame();

                // upgrade value to capture
                let keys: Vec<_> = closure.captures.keys().cloned().collect();
                for name in keys {
                    let value =
                        Value::UpValue(Rc::new(frame.resolve_local(&name).unwrap().clone()));
                    frame.define_local(&name, value.clone());
                    closure.captures.insert(name, value.clone());
                }

                self.push(vm, Value::Closure(Rc::new(closure)), pc)
            }

            Inst::PACK(len) => {
                let frame = vm.current_frame();

                if frame.sp < *len {
                    return Err(SquareError::RuntimeError(
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
                self.push(vm, Value::Vec(Rc::new(RefCell::new(result))), pc)
            }
            Inst::PEEK(offset, i) => {
                let top = vm.current_frame().top();
                let result = if let Some(Value::Vec(val)) = top {
                    let pack = val.borrow();

                    if *offset >= pack.len() {
                        return Err(SquareError::RuntimeError(
                            format!("bad peek, offset {} out of range", offset),
                            self.clone(),
                            *pc,
                        ));
                    }

                    let len = (pack.len() - offset) as i32;
                    let index = if *i > 0 { *i } else { (i + len) % len } as usize + offset;

                    if index < pack.len() {
                        Ok(pack.get(index).unwrap().clone())
                    } else {
                        Err(SquareError::RuntimeError(
                            format!("bad peek, index {} out of range", index),
                            self.clone(),
                            *pc,
                        ))
                    }
                } else {
                    Err(SquareError::RuntimeError(
                        "bad peek, top value is not a pack".to_string(),
                        self.clone(),
                        *pc,
                    ))
                };

                self.push(vm, result?, pc)
            }
        }
    }

    fn push(&self, vm: &mut VM, value: Value, _pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();

        if frame.sp >= frame.stack.len() {
            frame.stack.resize(frame.stack.len() * 2, Value::Nil);
        }

        frame.stack[frame.sp] = value;
        frame.sp += 1;
        Ok(())
    }

    fn pop(&self, vm: &mut VM, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();

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

    fn binop(&self, vm: &mut VM, op_fn: &OpFn, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();

        if frame.sp < 2 {
            return Err(SquareError::RuntimeError(
                format!("bad binary operation, operand stack length is {}", frame.sp),
                self.clone(),
                *pc,
            ));
        }

        let result = op_fn(&frame.stack[frame.sp - 2], &frame.stack[frame.sp - 1]);

        if let Err(SquareError::TypeError(msg)) = result {
            return Err(SquareError::RuntimeError(msg, self.clone(), *pc));
        }

        frame.stack[frame.sp - 2] = result?;
        frame.sp -= 1;
        Ok(())
    }

    fn jump(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) -> ExecResult {
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
        writeln!(f, "--- Call Frame ---")?;
        writeln!(f, "Return Address: {}", self.ra)?;
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
            stack: vec![Value::Nil; 16],
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

    top.locals.insert("a".to_string(), Value::Num(42.0));

    let mut frame = CallFrame::new();

    frame.prev = Some(Box::new(top));
    frame.locals.insert("b".to_string(), Value::Num(24.0));

    assert_eq!(frame.resolve_local("a"), Some(&Value::Num(42.0)));
    assert_eq!(frame.resolve_local("b"), Some(&Value::Num(24.0)));
    assert_eq!(frame.resolve_local("c"), None);
}

#[test]
fn test_define_local() {
    let mut top = CallFrame::new();

    top.define_local("a", Value::Num(42.0));

    let mut frame = CallFrame::new();

    frame.prev = Some(Box::new(top));
    frame.define_local("b", Value::Num(24.0));

    assert_eq!(frame.resolve_local("a"), Some(&Value::Num(42.0)));
    assert_eq!(frame.resolve_local("b"), Some(&Value::Num(24.0)));
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

    pub fn current_frame(&mut self) -> &mut CallFrame {
        self.call_frame.as_mut().unwrap()
    }

    pub fn reset(&mut self) {
        self.call_frame = Some(Box::new(CallFrame::new()));
    }

    pub fn step(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        let inst = &insts[*pc];
        inst.exec(self, insts, pc)?;
        *pc += 1;

        println!("{}:\n{}\n", inst, self.current_frame());

        Ok(())
    }

    pub fn run(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
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
        insts.push(Inst::PUSH(Value::Num(i as f64)))
    }

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.stack.len() >= 100, true);
    assert_eq!(callframe.sp, 100);
    assert_eq!(callframe.top(), Some(&Value::Num(99.0)));
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

    let callframe = vm.current_frame();
    assert_eq!(callframe.top(), Some(&Value::Bool(true)));
}

#[test]
fn test_store_undefined() {
    let mut vm = VM::new();
    let insts = vec![Inst::STORE("a".to_string())];

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("a"), Some(&Value::Nil));
}

#[test]
fn test_jump_if() {
    let mut vm = VM::new();
    let mut insts = vec![
        Inst::PUSH(Value::Bool(true)), // condition
        Inst::JNE(2),
        Inst::PUSH(Value::Num(42.0)), // true branch
        Inst::JMP(1),
        Inst::PUSH(Value::Num(24.0)), // false branch
    ];

    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.current_frame();
    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));

    insts[0] = Inst::PUSH(Value::Bool(false));

    vm.reset();
    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.current_frame();
    assert_eq!(callframe.top(), Some(&Value::Num(24.0)));
}

#[test]
fn test_jump_while() {
    let mut vm = VM::new();
    let insts = vec![
        // i = 0
        Inst::PUSH(Value::Num(0.0)),
        Inst::STORE("i".to_string()),
        // while i < 10
        Inst::LOAD("i".to_string()),
        Inst::PUSH(Value::Num(10.0)),
        Inst::LT,
        // i = i + 1
        Inst::JNE(5),
        Inst::LOAD("i".to_string()),
        Inst::PUSH(Value::Num(1.0)),
        Inst::ADD,
        Inst::STORE("i".to_string()),
        Inst::JMP(-9), // pc will automaticlly increase 1, which is different from jump forward
    ];

    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("i"), Some(&Value::Num(10.0)));
}

#[test]
fn test_call_ret() {
    let mut vm = VM::new();

    let insts = vec![
        Inst::PUSH(Value::Num(1.0)),
        Inst::STORE("a".to_string()),
        // test call before define
        Inst::PUSH_CLOSURE(Closure::new(3)),
        Inst::PACK(0),
        Inst::CALL,
        // skip fn def
        Inst::JMP(5),
        Inst::POP,
        Inst::PUSH(Value::Num(42.0)),
        Inst::LOAD("a".to_string()),
        Inst::ADD,
        Inst::RET,
        Inst::PUSH_CLOSURE(Closure::new(-6)),
        Inst::PACK(0),
        Inst::CALL,
        Inst::POP,
    ];

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top(), Some(&Value::Num(43.0)));
}

#[test]
fn test_vec() {
    let mut vm = VM::new();

    let insts = vec![
        Inst::PUSH(Value::Num(0.0)),
        Inst::PUSH(Value::Num(1.0)),
        Inst::PUSH(Value::Num(2.0)),
        Inst::PUSH(Value::Num(3.0)),
        Inst::PACK(3),
        Inst::PEEK(0, -2),
        Inst::STORE("a".to_string()),
        Inst::PEEK(0, 0),
        Inst::LOAD("a".to_string()),
        Inst::ADD,
        Inst::STORE("b".to_string()),
        Inst::POP,
    ];

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top(), Some(&Value::Num(0.0)));
    assert_eq!(callframe.resolve_local("b"), Some(&Value::Num(3.0)));
}
