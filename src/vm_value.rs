use alloc::rc::Rc;
use alloc::string::String;
use core::cmp::PartialEq;
use core::fmt;
use core::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub};
use hashbrown::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub ip: i32, // function location
    pub captures: HashMap<String, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Closure(Rc<Closure>),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(val) => {
                write!(f, "Bool({})", val)
            }
            Value::Num(val) => {
                write!(f, "Num({})", val)
            }
            Value::Str(val) => {
                write!(f, "Str({})", val)
            }
            Value::Closure(closure) => {
                write!(f, "Closure({})", closure.ip)
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
            (Value::Num(lhs), Value::Num(rhs)) => lhs.partial_cmp(rhs),
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
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs == rhs,
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
                    (Value::Num(lhs), Value::Num(rhs)) => Value::Num(lhs $op rhs),
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
                    (Value::Num(lhs), Value::Num(rhs)) => Value::Num(((*lhs as i64) $op (*rhs as i64)) as f64),
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
            Value::Num(val) => Value::Num(!(*val as i64) as f64),
            _ => unimplemented!(),
        }
    }
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Bool(val) => *val,
            Value::Num(val) => *val != 0.0,
            Value::Str(val) => !val.is_empty(),
            _ => false,
        }
    }
}

#[test]
fn test_partial_cmp() {
    assert_eq!(
        Value::Num(core::f64::INFINITY) > Value::Num(core::i64::MAX as f64),
        true
    );
    assert_eq!(Value::Num(0.0) == Value::Num(0 as f64), true);
    assert_eq!(Value::Nil == Value::Bool(false), false);
}

#[test]
fn test_binop_overflow() {
    let lhs = Value::Num(2i64.pow(53) as f64);
    let rhs = Value::Num(1.0);

    assert_eq!(&lhs + &rhs, Value::Num(2i64.pow(53) as f64));
}

#[test]
fn test_binop_precison() {
    let mut lhs = Value::Num(0.2);
    lhs = &lhs + &Value::Num(0.1);
    lhs = &lhs - &Value::Num(0.3);

    let mut i = Value::Num(0.0);
    while &lhs < &Value::Num(1.0) {
        lhs = &lhs + &lhs;
        i = &i + &Value::Num(1.0);
        println!("{}", lhs);
    }

    assert_eq!(i, Value::Num(54.0));
    assert_eq!(lhs, Value::Num(1.0));
}

#[test]
fn test_bitop_xor() {
    let mut lhs = Value::Num(0x1ff as f64);
    let mut rhs = Value::Num(0xfec as f64);

    lhs = &rhs ^ &lhs;
    rhs = &lhs ^ &rhs;
    lhs = &lhs ^ &rhs;

    assert_eq!(lhs, Value::Num(0xfec as f64));
    assert_eq!(rhs, Value::Num(0x1ff as f64));
}

#[test]
fn test_bitop_not() {
    assert_eq!(!&Value::Num(0x1ff as f64), Value::Num(!0x1ff as f64));
    assert_eq!(!&Value::Bool(false), Value::Bool(true));
}
