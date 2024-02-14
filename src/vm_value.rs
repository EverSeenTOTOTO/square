use alloc::{format, rc::Rc, string::String, vec::Vec};
use core::{
    cell::RefCell,
    cmp::PartialEq,
    fmt,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub},
};
use hashbrown::HashMap;

use crate::errors::SquareError;

// meta at compile time, instance at runtime
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub ip: i32, // offset at compile time, function address at runtime
    pub captures: HashMap<String, Value>, // (name: nil) at compile time, (name: upvalue) at runtime
}

impl Closure {
    pub fn new(ip: i32) -> Self {
        Self {
            ip,
            captures: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Num(f64),
    Str(String),
    UpValue(Rc<Value>),
    Closure(Rc<Closure>),
    Vec(Rc<RefCell<Vec<Value>>>),
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
            Value::Vec(val) => {
                write!(f, "Vec({:?})", val.borrow())
            }
            Value::Closure(closure) => {
                write!(f, "Closure({})", closure.ip)
            }
            Value::UpValue(val) => {
                write!(f, "UpValue({})", val)
            }
            Value::Nil => {
                write!(f, "Nil")
            }
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        let lhs = match self {
            Value::UpValue(value) => &**value,
            _ => self,
        };
        let rhs = match other {
            Value::UpValue(value) => &**value,
            _ => other,
        };
        match (lhs, rhs) {
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
        let lhs = match self {
            Value::UpValue(value) => &**value,
            _ => self,
        };
        let rhs = match other {
            Value::UpValue(value) => &**value,
            _ => other,
        };
        match (lhs, rhs) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Closure(lhs), Value::Closure(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

pub type CalcResult = Result<Value, SquareError>;

macro_rules! impl_binop {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for &Value {
            type Output = CalcResult;

            fn $method(self, other: Self) -> Self::Output {
                let lhs = match self {
                    Value::UpValue(value) => &**value,
                    _ => self,
                };
                let rhs = match other {
                    Value::UpValue(value) => &**value,
                    _ => other,
                };
                match (lhs, rhs) {
                    (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(lhs $op rhs)),
                    _ => Err(SquareError::RuntimeError(format!("cannot perform operation on {} and {}", self, other)))
                }
            }
        }
    }
}

macro_rules! impl_bitop {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for &Value {
            type Output = CalcResult;

            fn $method(self, other: Self) -> Self::Output {
                let lhs = match self {
                    Value::UpValue(value) => &**value,
                    _ => self,
                };
                let rhs = match other {
                    Value::UpValue(value) => &**value,
                    _ => other,
                };
                match (lhs, rhs) {
                    (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(((*lhs as i64) $op (*rhs as i64)) as f64)),
                    _ => Err(SquareError::RuntimeError(format!("cannot perform operation on {} and {}", self, other)))
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
    type Output = CalcResult;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(val) => Ok(Value::Bool(!*val)),
            Value::Num(val) => Ok(Value::Num(!(*val as i64) as f64)),
            Value::UpValue(val) => (&**val).not(),
            _ => Err(SquareError::RuntimeError(format!(
                "cannot perform operation on {}",
                self
            ))),
        }
    }
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Bool(val) => *val,
            Value::Num(val) => *val != 0.0,
            Value::Str(val) => !val.is_empty(),
            Value::UpValue(val) => (&**val).to_bool(),
            _ => true,
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

    assert_eq!((&lhs + &rhs).unwrap(), Value::Num(2i64.pow(53) as f64));
}

#[test]
fn test_binop_precison() {
    let mut lhs = Value::Num(0.2);
    lhs = (&lhs + &Value::Num(0.1)).unwrap();
    lhs = (&lhs - &Value::Num(0.3)).unwrap();

    let mut i = Value::Num(0.0);
    while &lhs < &Value::Num(1.0) {
        lhs = (&lhs + &lhs).unwrap();
        i = (&i + &Value::Num(1.0)).unwrap();
        println!("{}", lhs);
    }

    assert_eq!(i, Value::Num(54.0));
    assert_eq!(lhs, Value::Num(1.0));
}

#[test]
fn test_bitop_xor() {
    let mut lhs = Value::Num(0x1ff as f64);
    let mut rhs = Value::Num(0xfec as f64);

    lhs = (&rhs ^ &lhs).unwrap();
    rhs = (&lhs ^ &rhs).unwrap();
    lhs = (&lhs ^ &rhs).unwrap();

    assert_eq!(lhs, Value::Num(0xfec as f64));
    assert_eq!(rhs, Value::Num(0x1ff as f64));
}

#[test]
fn test_bitop_not() {
    assert_eq!(!&Value::Num(0x1ff as f64), Ok(Value::Num(!0x1ff as f64)));
    assert_eq!(!&Value::Bool(false), Ok(Value::Bool(true)));
}

#[test]
fn test_up_value() {
    let val = Rc::new(Value::Num(1.0));
    let upval = Value::UpValue(val.clone());
    assert_eq!(upval == Value::UpValue(val), true);
    assert_eq!(&upval + &upval, Ok(Value::Num(2.0)));
}
