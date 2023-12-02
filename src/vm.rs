#[cfg(not(test))]
use alloc::vec::Vec;

use crate::vm_value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    PUSH(Value),
    POP,
    ADD, // +
    SUB, // -
    MUL, // *
    DIV, // /
    REM, // %
    AND, // &
    OR,  // |
    XOR, // ^
    NOT, // !
    EQ,  // ==
    NE,  // !=
    LT,  // <
    LE,  // <=
    GT,  // >
    GE,  // >=
    RET,
}

macro_rules! handle_binop {
    ($vm:ident, $op:tt) => {
        {
            if $vm.sp < 2 {
                unimplemented!();
            }

            $vm.stack[$vm.sp - 2] = &$vm.stack[$vm.sp - 2] $op &$vm.stack[$vm.sp - 1];
            $vm.sp = $vm.sp - 1;
        }
    };
}

macro_rules! handle_ordop {
    ($vm:ident, $op:tt) => {
        {
            if $vm.sp < 2 {
                unimplemented!();
            }

            $vm.stack[$vm.sp - 2] = Value::Bool(&$vm.stack[$vm.sp - 2] $op &$vm.stack[$vm.sp - 1]);
            $vm.sp = $vm.sp - 1;
        }
    };
}

impl Inst {
    fn exec(self, vm: &mut VM) {
        match self {
            Inst::PUSH(value) => {
                vm.stack.push(value);
                vm.sp = vm.sp + 1;
            }
            Inst::POP => {
                vm.stack.pop();
                vm.sp = vm.sp - 1;
            }
            Inst::NOT => {
                if vm.sp < 1 {
                    unimplemented!();
                }

                vm.stack[vm.sp - 1] = !&vm.stack[vm.sp - 1];
            }
            Inst::ADD => handle_binop!(vm, +),
            Inst::SUB => handle_binop!(vm, -),
            Inst::MUL => handle_binop!(vm, *),
            Inst::DIV => handle_binop!(vm, /),
            Inst::REM => handle_binop!(vm, %),
            Inst::AND => handle_binop!(vm, &),
            Inst::OR => handle_binop!(vm, |),
            Inst::XOR => handle_binop!(vm, ^),
            Inst::EQ => handle_ordop!(vm, ==),
            Inst::NE => handle_ordop!(vm, !=),
            Inst::LT => handle_ordop!(vm, <),
            Inst::LE => handle_ordop!(vm, <=),
            Inst::GT => handle_ordop!(vm, >),
            Inst::GE => handle_ordop!(vm, >=),
            _ => todo!(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct VM {
    stack: Vec<Value>,
    // fake length, avoid frequent push/pop
    sp: usize,
}

impl Default for VM {
    fn default() -> Self {
        Self::new(1024)
    }
}

impl VM {
    pub fn new(stack_size: usize) -> Self {
        Self {
            stack: Vec::with_capacity(stack_size), // item count, not byte length
            sp: 0,
        }
    }

    pub fn exec(&mut self, inst: Inst) {
        inst.exec(self);
    }

    pub fn top(&self) -> Option<&Value> {
        return self.stack.get(self.sp - 1);
    }
}

#[test]
fn test_push_pop() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Float(24.0)));

    assert_eq!(vm.sp, 2);

    vm.exec(Inst::POP);
    vm.exec(Inst::POP);

    assert_eq!(vm.sp, 0);
}

#[test]
fn test_add() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Float(42.5)));
    vm.exec(Inst::ADD);

    assert_eq!(vm.sp, 1);
    assert_eq!(vm.stack.len(), 2);
    assert_eq!(vm.top(), Some(&Value::Float(84.5)));
}

#[test]
fn test_sub() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Float(24.5)));
    vm.exec(Inst::SUB);

    assert_eq!(vm.top(), Some(&Value::Float(42.0 - 24.5)));
}

#[test]
fn test_mul() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int64(2)));
    vm.exec(Inst::PUSH(Value::Double(24.5)));
    vm.exec(Inst::MUL);

    assert_eq!(vm.top(), Some(&Value::Double(49.0)));
}

#[test]
fn test_div() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int64(48)));
    vm.exec(Inst::PUSH(Value::Float(1.5)));
    vm.exec(Inst::DIV);

    assert_eq!(vm.top(), Some(&Value::Float(48.0 / 1.5)));
}

#[test]
fn test_rem() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Double(11.0)));
    vm.exec(Inst::PUSH(Value::Int(3)));
    vm.exec(Inst::REM);

    assert_eq!(vm.top(), Some(&Value::Double(2.0)));
}

#[test]
fn test_and() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(0b1001)));
    vm.exec(Inst::PUSH(Value::Int(0b1010)));
    vm.exec(Inst::AND);

    assert_eq!(vm.top(), Some(&Value::Int(0b1000)));
}

#[test]
fn test_or() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(0b1001)));
    vm.exec(Inst::PUSH(Value::Int(0b1010)));
    vm.exec(Inst::OR);

    assert_eq!(vm.top(), Some(&Value::Int(0b1011)));
}

#[test]
fn test_xor() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(0b1001)));
    vm.exec(Inst::PUSH(Value::Int(0b1010)));
    vm.exec(Inst::XOR);

    assert_eq!(vm.top(), Some(&Value::Int(0b11)));
}

#[test]
fn test_not() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(0b1001)));
    vm.exec(Inst::NOT);

    assert_eq!(vm.top(), Some(&Value::Bool(false)));

    vm.exec(Inst::PUSH(Value::Float(0.0)));
    vm.exec(Inst::NOT);

    assert_eq!(vm.top(), Some(&Value::Bool(true)));
}

#[test]
fn test_eq() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::EQ);

    assert_eq!(vm.top(), Some(&Value::Bool(true)));
}

#[test]
fn test_lt() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Int(84)));
    vm.exec(Inst::LT);

    assert_eq!(vm.top(), Some(&Value::Bool(true)));
}
