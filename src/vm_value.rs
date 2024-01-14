use core::cmp::PartialEq;
use core::fmt;
use core::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub};

use alloc::string::String;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Float(f32),
    Double(f64),
    Int(i32),
    Int64(i64),
    Str(String),
    Nil,
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
            Value::Str(val) => {
                write!(f, "Str({})", val)
            }
            Value::Nil => {
                write!(f, "Nil")
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs.partial_cmp(rhs),
            (Value::Int(lhs), Value::Int(rhs)) => lhs.partial_cmp(rhs),
            (Value::Int(lhs), Value::Int64(rhs)) => (*lhs as i64).partial_cmp(rhs),
            (Value::Int(lhs), Value::Float(rhs)) => (*lhs as f32).partial_cmp(rhs),
            (Value::Int(lhs), Value::Double(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Int64(lhs), Value::Int64(rhs)) => lhs.partial_cmp(rhs),
            (Value::Int64(lhs), Value::Int(rhs)) => lhs.partial_cmp(&(*rhs as i64)),
            (Value::Int64(lhs), Value::Float(rhs)) => (*lhs as f32).partial_cmp(rhs),
            (Value::Int64(lhs), Value::Double(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Float(lhs), Value::Float(rhs)) => lhs.partial_cmp(rhs),
            (Value::Float(lhs), Value::Int(rhs)) => lhs.partial_cmp(&(*rhs as f32)),
            (Value::Float(lhs), Value::Int64(rhs)) => lhs.partial_cmp(&(*rhs as f32)),
            (Value::Float(lhs), Value::Double(rhs)) => (*lhs as f64).partial_cmp(rhs),
            (Value::Double(lhs), Value::Double(rhs)) => lhs.partial_cmp(rhs),
            (Value::Double(lhs), Value::Int(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            (Value::Double(lhs), Value::Int64(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            (Value::Double(lhs), Value::Float(rhs)) => lhs.partial_cmp(&(*rhs as f64)),
            (Value::Str(lhs), Value::Str(rhs)) => lhs.partial_cmp(rhs),
            (Value::Nil, Value::Nil) => Some(core::cmp::Ordering::Equal),
            _ => None,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Int64(rhs)) => *lhs as i64 == *rhs,
            (Value::Int(lhs), Value::Float(rhs)) => *lhs as f32 == *rhs,
            (Value::Int(lhs), Value::Double(rhs)) => *lhs as f64 == *rhs,
            (Value::Int64(lhs), Value::Int64(rhs)) => lhs == rhs,
            (Value::Int64(lhs), Value::Int(rhs)) => *lhs == *rhs as i64,
            (Value::Int64(lhs), Value::Float(rhs)) => *lhs as f32 == *rhs,
            (Value::Int64(lhs), Value::Double(rhs)) => *lhs as f64 == *rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Int(rhs)) => *lhs == *rhs as f32,
            (Value::Float(lhs), Value::Int64(rhs)) => *lhs == *rhs as f32,
            (Value::Float(lhs), Value::Double(rhs)) => *lhs as f64 == *rhs,
            (Value::Double(lhs), Value::Double(rhs)) => lhs == rhs,
            (Value::Double(lhs), Value::Int(rhs)) => *lhs == *rhs as f64,
            (Value::Double(lhs), Value::Int64(rhs)) => *lhs == *rhs as f64,
            (Value::Double(lhs), Value::Float(rhs)) => *lhs == *rhs as f64,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            _ => false,
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
                    (Value::Int(lhs), Value::Int64(rhs)) => Value::Int64((*lhs as i64) $op rhs),
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
impl_bitop!(Shl, shl, <<);
impl_bitop!(Shr, shr, >>);

// bitwise not ~
impl Not for &Value {
    type Output = Value;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(val) => Value::Bool(!*val),
            Value::Int(val) => Value::Int(!*val),
            Value::Int64(val) => Value::Int64(!*val),
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Bool(val) => *val,
            Value::Int(val) => *val != 0,
            Value::Int64(val) => *val != 0,
            Value::Float(val) => *val != 0.0,
            Value::Double(val) => *val != 0.0,
            Value::Str(val) => !val.is_empty(),
            Value::Nil => false,
        }
    }
}

#[test]
fn test_partial_cmp() {
    assert_eq!(
        Value::Double(core::f64::INFINITY) > Value::Int64(core::i64::MAX),
        true
    );
    assert_eq!(Value::Double(0.0) == Value::Int(0), true);
    assert_eq!(Value::Nil == Value::Bool(false), false);
}

#[test]
fn test_binop_overflow() {
    let lhs = Value::Int((2i64.pow(31) - 1) as i32);
    let rhs = Value::Float(1.0);

    assert_eq!(&lhs + &rhs, Value::Float((2i64.pow(31) - 1) as f32));
}

#[test]
fn test_binop_precison() {
    let mut lhs = Value::Double(0.2);
    lhs = &lhs + &Value::Double(0.1);
    lhs = &lhs - &Value::Double(0.3);

    let mut i = Value::Int(0);
    while &lhs < &Value::Int(1) {
        lhs = &lhs + &lhs;
        i = &i + &Value::Int(1);
        println!("{}", lhs);
    }

    assert_eq!(i, Value::Int(54));
    assert_eq!(lhs, Value::Double(1.0));
}

#[test]
fn test_bitop_xor() {
    let mut lhs = Value::Int(0x1ff);
    let mut rhs = Value::Int64(0xfec);

    lhs = &rhs ^ &lhs;
    rhs = &lhs ^ &rhs;
    lhs = &lhs ^ &rhs;

    assert_eq!(lhs, Value::Int64(0xfec));
    assert_eq!(rhs, Value::Int64(0x1ff));
}

#[test]
fn test_bitop_not() {
    assert_eq!(!&Value::Int(0x1ff), Value::Int(!0x1ff));
    assert_eq!(!&Value::Bool(false), Value::Bool(true));
}
