use core::cmp::PartialEq;
use core::fmt;
use core::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Sub};

#[cfg(not(test))]
use alloc::boxed::Box;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord)]
pub struct SqString {
    length: usize,
    ptr: *const u8,
}

impl PartialOrd for SqString {
    fn partial_cmp(&self, _other: &Self) -> Option<core::cmp::Ordering> {
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord)]
pub enum HeapValue {
    Str(SqString),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
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

macro_rules! impl_binop {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for &Value {
            type Output = Value;

            fn $method(self, another: Self) -> Self::Output {
                match (self, another) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs $op rhs),
                    (Value::Int(lhs), Value::Int64(rhs)) => Value::Int64(*lhs as i64 $op rhs),
                    (Value::Int(lhs), Value::Float(rhs)) => Value::Float(*lhs as f32 $op rhs),
                    (Value::Int(lhs), Value::Double(rhs)) => Value::Double(*lhs as f64 $op rhs),
                    (Value::Int64(lhs), Value::Int(rhs)) => Value::Int64(lhs $op *rhs as i64),
                    (Value::Int64(lhs), Value::Int64(rhs)) => Value::Int64(lhs $op rhs),
                    (Value::Int64(lhs), Value::Float(rhs)) => Value::Float(*lhs as f32 $op rhs),
                    (Value::Int64(lhs), Value::Double(rhs)) => Value::Double(*lhs as f64 $op rhs),
                    (Value::Float(lhs), Value::Int(rhs)) => Value::Float(lhs $op *rhs as f32),
                    (Value::Float(lhs), Value::Int64(rhs)) => Value::Float(lhs $op *rhs as f32),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs $op rhs),
                    (Value::Float(lhs), Value::Double(rhs)) => Value::Double(*lhs as f64 $op rhs),
                    (Value::Double(lhs), Value::Int(rhs)) => Value::Double(lhs $op *rhs as f64),
                    (Value::Double(lhs), Value::Int64(rhs)) => Value::Double(lhs $op *rhs as f64),
                    (Value::Double(lhs), Value::Float(rhs)) => Value::Double(lhs $op *rhs as f64),
                    (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs $op rhs),
                    _ => unimplemented!(),
                }
            }
        }
    }
}

macro_rules! impl_bitop {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for &Value {
            type Output = Value;

            fn $method(self, another: Self) -> Self::Output {
                match (self, another) {
                    (Value::Int(lhs), Value::Int(rhs)) => Value::Int(lhs $op rhs),
                    (Value::Int(lhs), Value::Int64(rhs)) => Value::Int64(*lhs as i64 $op rhs),
                    (Value::Int64(lhs), Value::Int(rhs)) => Value::Int64(lhs $op *rhs as i64),
                    (Value::Int64(lhs), Value::Int64(rhs)) => Value::Int64(lhs $op rhs),
                    _ => unimplemented!(),
                }
            }
        }
    }
}

impl_binop!(Add, add, +);
impl_binop!(Sub, sub, -);
impl_binop!(Mul, mul, *);
impl_binop!(Div, div, /);
impl_binop!(Rem, rem, %);
impl_bitop!(BitAnd, bitand, &);
impl_bitop!(BitOr, bitor, |);
impl_bitop!(BitXor, bitxor, ^);

impl Not for &Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(val) => Value::Bool(!*val),
            Value::Int(val) => Value::Bool(*val == 0),
            Value::Int64(val) => Value::Bool(*val == 0),
            Value::Float(val) => Value::Bool(*val == 0.0),
            Value::Double(val) => Value::Bool(*val == 0.0),
            Value::HeapPtr(_) => Value::Bool(false), // TODO
        }
    }
}
