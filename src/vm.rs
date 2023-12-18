use alloc::{boxed::Box, string::String, string::ToString, vec, vec::Vec};

use hashbrown::HashMap;

use crate::{emit::Inst, vm_value::Value};

type OpFn = dyn Fn(&Value, &Value) -> Value;

impl Inst {
    fn exec(&self, vm: &mut VM, insts: &Vec<Inst>, pc: &mut usize) {
        match self {
            Inst::PUSH(value) => {
                self.push(vm, value.clone());
            }
            Inst::POP => {
                self.pop(vm);
            }
            Inst::NOT => {
                if vm.call_frame.sp < 1 {
                    todo!();
                }

                vm.call_frame.stack[vm.call_frame.sp - 1] =
                    !&vm.call_frame.stack[vm.call_frame.sp - 1];
            }

            Inst::ADD => self.binop(vm, &|a, b| a + b),
            Inst::SUB => self.binop(vm, &|a, b| a - b),
            Inst::MUL => self.binop(vm, &|a, b| a * b),
            Inst::DIV => self.binop(vm, &|a, b| a / b),
            Inst::REM => self.binop(vm, &|a, b| a % b),
            Inst::BITAND => self.binop(vm, &|a, b| a & b),
            Inst::BITOR => self.binop(vm, &|a, b| a | b),
            Inst::BITXOR => self.binop(vm, &|a, b| a ^ b),
            Inst::EQ => self.binop(vm, &|a, b| Value::Bool(a == b)),
            Inst::NE => self.binop(vm, &|a, b| Value::Bool(a != b)),
            Inst::LT => self.binop(vm, &|a, b| Value::Bool(a < b)),
            Inst::LE => self.binop(vm, &|a, b| Value::Bool(a <= b)),
            Inst::GT => self.binop(vm, &|a, b| Value::Bool(a > b)),
            Inst::GE => self.binop(vm, &|a, b| Value::Bool(a >= b)),

            Inst::JMP(value) => {
                self.jump(insts, pc, *value);
            }
            Inst::JEQ(value) => {
                if vm.call_frame.sp < 1 {
                    todo!();
                }

                let top = &vm.call_frame.stack[vm.call_frame.sp - 1];
                vm.call_frame.sp = vm.call_frame.sp - 1;
                if top != &Value::Bool(true) {
                    return;
                }

                self.jump(insts, pc, *value);
            }
            Inst::STORE(name) => {
                if let Some(val) = vm.call_frame.top() {
                    vm.call_frame.define_local(name, val.clone());
                } else {
                    vm.call_frame.define_local(name, Value::Undefined);
                }

                self.pop(vm);
            }
            Inst::LOAD(name) => {
                let mut tmp = Value::Undefined;
                if let Some(val) = vm.call_frame.resolve_local(name) {
                    tmp = val.clone(); // avoid mutable ref and immutable ref of vm at the same time
                }
                self.push(vm, tmp);
            }
            _ => todo!(),
        }
    }

    fn push(&self, vm: &mut VM, value: Value) {
        if vm.call_frame.sp >= vm.call_frame.stack.len() {
            todo!();
        }

        vm.call_frame.stack[vm.call_frame.sp] = value;
        vm.call_frame.sp = vm.call_frame.sp + 1;
    }

    fn pop(&self, vm: &mut VM) {
        if vm.call_frame.sp < 1 {
            todo!();
        }
        vm.call_frame.sp = vm.call_frame.sp - 1;
    }

    fn binop(&self, vm: &mut VM, op_fn: &OpFn) {
        if vm.call_frame.sp < 2 {
            todo!();
        }

        vm.call_frame.stack[vm.call_frame.sp - 2] = op_fn(
            &vm.call_frame.stack[vm.call_frame.sp - 2],
            &vm.call_frame.stack[vm.call_frame.sp - 1],
        );
        vm.call_frame.sp = vm.call_frame.sp - 1;
    }

    fn jump(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) {
        let new_pc = *pc as i32 + offset;
        if new_pc < 0 || new_pc > insts.len() as i32 {
            todo!();
        }

        *pc = new_pc as usize;
    }
}

pub struct CallFrame {
    locals: HashMap<String, Value>,
    stack: Vec<Value>,
    // fake length, avoid frequent push/pop
    sp: usize,
    prev: Option<Box<CallFrame>>,
}

impl CallFrame {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            stack: vec![Value::Undefined; 1024], // item count, not byte length
            sp: 0,
            prev: None,
        }
    }

    fn find_frame_by_varname<'a>(&self, name: &'a str) -> Option<&CallFrame> {
        if self.locals.contains_key(name) {
            return Some(self);
        }

        let mut frame = &self.prev;
        while let Some(f) = frame {
            if f.locals.contains_key(name) {
                return Some(f.as_ref());
            }
            frame = &f.prev;
        }
        return None;
    }

    pub fn resolve_local<'a>(&self, name: &'a str) -> Option<&Value> {
        let frame = self.find_frame_by_varname(name)?;

        return frame.locals.get(name);
    }

    fn find_frame_by_varname_mut<'a>(&mut self, name: &'a str) -> Option<&mut CallFrame> {
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

    pub fn define_local<'a>(&mut self, name: &'a str, value: Value) {
        if let Some(frame) = self.find_frame_by_varname_mut(name) {
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
    pub call_frame: Box<CallFrame>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            call_frame: Box::new(CallFrame::new()),
        }
    }

    pub fn step(&mut self, insts: &Vec<Inst>, pc: &mut usize) {
        let inst = &insts[*pc];
        inst.exec(self, insts, pc);
        *pc += 1;
    }

    pub fn run(&mut self, insts: &Vec<Inst>, pc: &mut usize) {
        while *pc < insts.len() {
            self.step(insts, pc);
        }
    }
}

#[test]
fn test_step() {
    let mut vm = VM::new();
    let insts = vec![
        Inst::PUSH(Value::Int(42)),
        Inst::PUSH(Value::Float(24.0)),
        Inst::POP,
        Inst::POP,
    ];
    let mut pc = 0;

    vm.step(&insts, &mut pc);
    vm.step(&insts, &mut pc);

    assert_eq!(vm.call_frame.sp, 2);

    vm.step(&insts, &mut pc);
    vm.step(&insts, &mut pc);

    assert_eq!(vm.call_frame.sp, 0);
}

#[test]
fn test_run() {
    let mut vm = VM::new();
    let insts = vec![
        Inst::PUSH(Value::Int(42)),
        Inst::PUSH(Value::Int(24)),
        Inst::ADD,
        Inst::PUSH(Value::Int(2)),
        Inst::MUL,
    ];
    let mut pc = 0;

    vm.run(&insts, &mut pc);

    assert_eq!(vm.call_frame.top(), Some(&Value::Int(132)));
}
