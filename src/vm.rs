use core::fmt;
use core::ops::{Add, Div, Mul, Rem, Sub};

#[cfg(not(test))]
use alloc::boxed::Box;
#[cfg(not(test))]
use alloc::vec::Vec;

#[derive(Debug, Clone, Copy, PartialEq)]
struct StrValue {
    length: usize,
    ptr: *const u8,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum HeapValue {
    Str(StrValue),
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Bool(bool),
    Float(f32),
    Double(f64),
    Int(i32),
    Int64(i64),
    HeapPtr(Box<HeapValue>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(val) => {
                write!(f, "Bool({})", val)
            }
            Value::Float(val) => {
                write!(f, "Float({})", val)
            }
            Value::Double(val) => {
                write!(f, "Double({})", val)
            }
            Value::Int(val) => {
                write!(f, "Int({})", val)
            }
            Value::Int64(val) => {
                write!(f, "Int({})", val)
            }
            Value::HeapPtr(val) => {
                write!(f, "HeapPtr({:?})", val)
            }
        }
    }
}

macro_rules! impl_op {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for Value {
            type Output = Self;

            fn $method(self, another: Self) -> Self::Output {
                match self {
                    Value::Int(lhs)  => {
                        match another {
                            Value::Int(rhs)  => {
                                Value::Int(lhs $op rhs)
                            }
                            Value::Int64(rhs) => {
                                Value::Int64(lhs as i64 $op rhs)
                            }
                            Value::Float(rhs) => {
                                Value::Float(lhs as f32 $op rhs)
                            }
                            Value::Double(rhs) => {
                                Value::Double(lhs as f64 $op rhs)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Value::Int64(lhs) => {
                        match another {
                            Value::Int(rhs)  => {
                                Value::Int64(lhs $op rhs as i64)
                            }
                            Value::Int64(rhs) => {
                                Value::Int64(lhs $op rhs)
                            }
                            Value::Float(rhs) => {
                                Value::Float(lhs as f32 $op rhs)
                            }
                            Value::Double(rhs) => {
                                Value::Double(lhs as f64 $op rhs)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Value::Float(lhs) => {
                        match another {
                            Value::Int(rhs)  => {
                                Value::Float(lhs $op rhs as f32)
                            }
                            Value::Int64(rhs) => {
                                Value::Float(lhs $op rhs as f32)
                            }
                            Value::Float(rhs) => {
                                Value::Float(lhs $op rhs)
                            }
                            Value::Double(rhs) => {
                                Value::Double(lhs as f64 $op rhs)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    Value::Double(lhs) => {
                        match another {
                            Value::Int(rhs)  => {
                                Value::Double(lhs $op rhs as f64)
                            }
                            Value::Int64(rhs) => {
                                Value::Double(lhs $op rhs as f64)
                            }
                            Value::Float(rhs) => {
                                Value::Double(lhs $op rhs as f64)
                            }
                            Value::Double(rhs) => {
                                Value::Double(lhs $op rhs)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }
}

impl_op!(Add, add, +);
impl_op!(Sub, sub, -);
impl_op!(Mul, mul, *);
impl_op!(Div, div, /);
impl_op!(Rem, rem, %);

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
    NOT, // !
    EQ,  // ==
    NEQ, // !=
    LT,  // <
    LTE, // <=
    GT,  // >
    GTE, // >=
    RET,
}

macro_rules! handle_op {
    ($vm:ident, $op:tt) => {
        {
            if let Some(rhs) = $vm.stack.pop() {
                if let Some(lhs) = $vm.stack.pop() {
                    let result = lhs $op rhs;

                    Inst::PUSH(result).exec($vm);
                }
            }
        }
    };
}

impl Inst {
    fn exec(self, vm: &mut VM) {
        match self {
            Inst::PUSH(value) => {
                vm.stack.push(value);
            }
            Inst::POP => {
                vm.stack.pop();
            }
            Inst::ADD => handle_op!(vm, +),
            Inst::SUB => handle_op!(vm, -),
            Inst::MUL => handle_op!(vm, *),
            Inst::DIV => handle_op!(vm, /),
            Inst::REM => handle_op!(vm, %),
            _ => todo!(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct VM {
    stack: Vec<Value>,
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
        }
    }

    pub fn exec(&mut self, inst: Inst) {
        inst.exec(self);
    }

    pub fn sp(&self) -> usize {
        return self.stack.len();
    }
}

#[test]
fn test_push_pop() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Float(24.0)));

    assert_eq!(vm.sp(), 2);

    vm.exec(Inst::POP);
    vm.exec(Inst::POP);

    assert_eq!(vm.sp(), 0);
}

#[test]
fn test_add() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Float(42.5)));
    vm.exec(Inst::ADD);

    assert_eq!(vm.stack.pop(), Some(Value::Float(84.5)));
}

#[test]
fn test_sub() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int(42)));
    vm.exec(Inst::PUSH(Value::Float(24.5)));
    vm.exec(Inst::SUB);

    assert_eq!(vm.stack.pop(), Some(Value::Float(42.0 - 24.5)));
}

#[test]
fn test_mul() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int64(2)));
    vm.exec(Inst::PUSH(Value::Double(24.5)));
    vm.exec(Inst::MUL);

    assert_eq!(vm.stack.pop(), Some(Value::Double(49.0)));
}

#[test]
fn test_div() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Int64(48)));
    vm.exec(Inst::PUSH(Value::Float(1.5)));
    vm.exec(Inst::DIV);

    assert_eq!(vm.stack.pop(), Some(Value::Float(48.0 / 1.5)));
}

#[test]
fn test_rem() {
    let mut vm = VM::default();

    vm.exec(Inst::PUSH(Value::Double(11.0)));
    vm.exec(Inst::PUSH(Value::Int(3)));
    vm.exec(Inst::REM);

    assert_eq!(vm.stack.pop(), Some(Value::Double(2.0)));
}
