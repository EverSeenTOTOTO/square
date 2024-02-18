use alloc::{
    format,
    rc::Rc,
    string::{String, ToString},
    vec::Vec,
};

use core::{
    cell::RefCell,
    cmp::PartialEq,
    fmt,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Not, Rem, Shl, Shr, Sub},
};
use hashbrown::{HashMap, HashSet};

use crate::errors::SquareError;

// use at both runtime and compile time
// at compile time, ip is the offset from instruction to fn definition, upvalues is empty and captures contains variable names
// at runtime, ip is the function address or syscall index(when < 0), upvalues contains captured variables
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub ip: i32,
    pub upvalues: HashMap<String, Value>,
    pub captures: HashSet<String>,
}

impl Closure {
    pub fn new(ip: i32) -> Self {
        Self {
            ip,
            upvalues: HashMap::new(),
            captures: HashSet::new(),
        }
    }

    pub fn capture(&mut self, name: &str, upvalue: &Value) -> bool {
        if self.captures.contains(name) {
            self.upvalues.insert(name.to_string(), upvalue.clone());
            return true;
        }
        false
    }
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.captures.is_empty() {
            write!(f, "Closure({})", self.ip)
        } else {
            write!(
                f,
                "Closure({}, {})",
                self.ip,
                self.captures
                    .iter()
                    .map(|k| {
                        if self.upvalues.contains_key(k) {
                            format!("{}✓", k)
                        } else {
                            format!("{}?", k)
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(",")
            )
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Vec(Rc<RefCell<Vec<Value>>>),
    Closure(Rc<RefCell<Closure>>),
    UpValue(Rc<RefCell<Value>>),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(val) => {
                write!(f, "{}", val)
            }
            Value::Num(val) => {
                write!(f, "{}", val)
            }
            Value::Str(val) => {
                write!(f, "{}", val)
            }
            Value::Vec(val) => {
                write!(
                    f,
                    "[{}]",
                    val.borrow()
                        .iter()
                        .map(|v| {
                            match v {
                                Value::Vec(_) => "[...]".to_string(),
                                _ => format!("{}", v),
                            }
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Value::Closure(closure) => closure.borrow().fmt(f),
            Value::UpValue(val) => match *val.borrow() {
                // FIXME: print vec may still cause circular
                Value::UpValue(_) => unreachable!(),
                _ => write!(f, "{}", val.borrow()),
            },
            Value::Nil => {
                write!(f, "nil")
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

            (Value::UpValue(lhs), Value::UpValue(rhs)) => lhs.borrow().partial_cmp(&rhs.borrow()),
            (Value::UpValue(lhs), rhs) => lhs.borrow().partial_cmp(rhs),
            (lhs, Value::UpValue(rhs)) => lhs.partial_cmp(&rhs.borrow()),
            _ => None,
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Vec(lhs), Value::Vec(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Closure(lhs), Value::Closure(rhs)) => Rc::ptr_eq(lhs, rhs),

            (Value::UpValue(lhs), Value::UpValue(rhs)) => lhs.borrow().eq(&rhs.borrow()),
            (Value::UpValue(lhs), rhs) => lhs.borrow().eq(rhs),
            (lhs, Value::UpValue(rhs)) => lhs.eq(&rhs.borrow()),

            _ => false,
        }
    }
}

#[test]
fn test_partial_eq() {
    let closure = Rc::new(RefCell::new(Closure::new(0)));
    let lhs = Value::Closure(closure.clone());
    let rhs = Value::Closure(closure.clone());

    assert_eq!(lhs, rhs);

    closure.borrow_mut().captures.insert("lhs".to_string());
    closure.borrow_mut().capture("lhs", &lhs);

    assert_eq!(lhs, rhs);

    let lhs_up = lhs.upgrade();
    let rhs_up = rhs.upgrade();

    assert_eq!(lhs_up, rhs_up);
}

pub type CalcResult = Result<Value, SquareError>;

macro_rules! impl_binop {
    ($trait:ty, $method:ident, $op:tt) => {
        impl $trait for &Value {
            type Output = CalcResult;

            fn $method(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(lhs $op rhs)),

                    (Value::UpValue(lhs), Value::UpValue(rhs)) => &*lhs.borrow() $op &*rhs.borrow(),
                    (Value::UpValue(lhs), rhs) => &*lhs.borrow() $op rhs,
                    (lhs, Value::UpValue(rhs)) => lhs $op &*rhs.borrow(),

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
                match (self, other) {
                    (Value::Num(lhs), Value::Num(rhs)) => Ok(Value::Num(((*lhs as i64) $op (*rhs as i64)) as f64)),

                    (Value::UpValue(lhs), Value::UpValue(rhs)) => &*lhs.borrow() $op &*rhs.borrow(),
                    (Value::UpValue(lhs), rhs) => &*lhs.borrow() $op rhs,
                    (lhs, Value::UpValue(rhs)) => lhs $op &*rhs.borrow(),

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
            Value::UpValue(val) => val.borrow().not(),
            _ => Err(SquareError::RuntimeError(format!(
                "cannot perform operation on {}",
                self
            ))),
        }
    }
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
fn test_reference() {
    let val = Rc::new(RefCell::new(Value::Num(1.0)));
    let upval = Value::UpValue(val.clone());
    assert_eq!(upval == Value::Num(1.0), true);
    assert_eq!(&Value::Num(1.0) + &upval, Ok(Value::Num(2.0)));
    assert_eq!(&upval + &upval, Ok(Value::Num(2.0)));
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Bool(val) => *val,
            Value::Num(val) => *val != 0.0,
            Value::Str(val) => !val.is_empty(),
            Value::UpValue(val) => val.borrow().to_bool(),
            _ => true,
        }
    }

    pub fn upgrade(&self) -> Value {
        match self {
            Value::UpValue(_) => self.clone(), // Rc::clone
            _ => Value::UpValue(Rc::new(RefCell::new(self.clone()))),
        }
    }
}

#[test]
fn test_upgrade() {
    let closure = Value::Closure(Rc::new(RefCell::new(Closure::new(0))));
    let upval = Value::UpValue(Rc::new(RefCell::new(closure)));

    assert_eq!(upval, upval.upgrade());
    assert_eq!(upval.upgrade().upgrade(), upval.upgrade());
    assert_eq!(upval.clone(), upval.upgrade());
}
