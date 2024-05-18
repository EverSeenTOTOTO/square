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

use crate::{builtin::Builtin, errors::SquareError, vm::CallFrame};

pub type Object = HashMap<String, Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Function {
    ClosureMeta(i32, HashSet<String>), // compile time, (offset, captures)
    Closure(usize, HashMap<String, Value>), // runtime, (ip, upvalues)
    Syscall(&'static str),             // (name)
    Contiuation(usize, Vec<Rc<RefCell<CallFrame>>>), // (ra, context)
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::ClosureMeta(offset, captures) => {
                if captures.is_empty() {
                    write!(f, "{}", offset)
                } else {
                    write!(
                        f,
                        "{}, {}",
                        offset,
                        captures
                            .iter()
                            .map(|k| { format!("{}?", k) })
                            .collect::<Vec<String>>()
                            .join(",")
                    )
                }
            }
            Function::Closure(ip, upvalues) => {
                if upvalues.is_empty() {
                    write!(f, "Closure({})", ip)
                } else {
                    write!(
                        f,
                        "Closure({}, {})",
                        ip,
                        upvalues
                            .iter()
                            .map(|(k, _)| format!("{}✓", k))
                            .collect::<Vec<String>>()
                            .join(",")
                    )
                }
            }
            Function::Syscall(name) => {
                write!(f, "Syscall({})", name)
            }
            Function::Contiuation(ra, context) => {
                write!(f, "Continuation({}, {})", ra, context.len() - 1)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Num(f64),
    Str(String),
    Vec(Rc<RefCell<Vec<Value>>>),
    Obj(Rc<RefCell<Object>>),
    Function(Rc<RefCell<Function>>),
    UpValue(Rc<RefCell<Value>>),
    Nil,
}

fn stringify_nested(val: &Value) -> String {
    match val {
        Value::Vec(_) => "[...]".to_string(),
        Value::Obj(_) => "{...}".to_string(),
        Value::UpValue(val) => match *val.borrow() {
            Value::UpValue(_) => panic!("nested upvalue"), // this should be unreachable, see Value::upgrade && CallFrame::assign_local
            _ => format!("&{}", stringify_nested(&val.borrow())),
        },
        _ => format!("{}", val),
    }
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
                        .map(stringify_nested)
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Value::Obj(obj) => {
                if let Some(v) = Builtin::get_internal_vec(self) {
                    return write!(
                        f,
                        "[{}]",
                        v.borrow()
                            .iter()
                            .map(stringify_nested)
                            .collect::<Vec<String>>()
                            .join(", ")
                    );
                }

                write!(
                    f,
                    "{{{}}}",
                    obj.borrow()
                        .iter()
                        .map(|(k, v)| format!("{}: {}", k, stringify_nested(v)))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Value::Function(func) => func.borrow().fmt(f),
            Value::UpValue(val) => match *val.borrow() {
                Value::UpValue(_) => panic!("nested upvalue"),
                _ => write!(f, "&{}", &val.borrow()),
            },
            Value::Nil => {
                write!(f, "nil")
            }
        }
    }
}

#[test]
fn test_print_circular() {
    let obj = Rc::new(RefCell::new(HashMap::new()));
    let vec = Rc::new(RefCell::new(vec![Value::Obj(obj.clone())]));

    obj.borrow_mut()
        .insert("vec".to_string(), Value::Vec(vec.clone()));
    obj.borrow_mut()
        .insert("obj".to_string(), Value::Obj(obj.clone()).upgrade());

    assert_eq!(format!("{}", Value::Obj(obj)), "{obj: &{...}, vec: [...]}");
    assert_eq!(format!("{}", Value::Vec(vec)), "[{...}]");
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
    assert!(
        Value::Num(core::f64::INFINITY) > Value::Num(core::i64::MAX as f64)
    );
    assert!(Value::Num(0.0) == Value::Num(0 as f64));
    assert!(Value::Nil != Value::Bool(false));
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Vec(lhs), Value::Vec(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Obj(lhs), Value::Obj(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Function(lhs), Value::Function(rhs)) => Rc::ptr_eq(lhs, rhs),

            (Value::UpValue(lhs), Value::UpValue(rhs)) => lhs.borrow().eq(&rhs.borrow()),
            (Value::UpValue(lhs), rhs) => lhs.borrow().eq(rhs),
            (lhs, Value::UpValue(rhs)) => lhs.eq(&rhs.borrow()),

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
    while lhs < Value::Num(1.0) {
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
    assert!(upval == Value::Num(1.0));
    assert_eq!(&Value::Num(1.0) + &upval, Ok(Value::Num(2.0)));
    assert_eq!(&upval + &upval, Ok(Value::Num(2.0)));
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(val) => *val,
            Value::Num(val) => *val != 0.0,
            Value::UpValue(val) => val.borrow().as_bool(),
            _ => true,
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        match self {
            Value::Num(val) => Some(*val),
            Value::UpValue(val) => val.borrow().as_num(),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<String> {
        match self {
            Value::Str(val) => Some(val.clone()),
            Value::UpValue(val) => val.borrow().as_str(),
            _ => None,
        }
    }

    pub fn as_vec(&self) -> Option<Rc<RefCell<Vec<Value>>>> {
        match self {
            Value::Vec(val) => Some(val.clone()),
            Value::UpValue(val) => val.borrow().as_vec(),
            _ => None,
        }
    }

    pub fn as_obj(&self) -> Option<Rc<RefCell<Object>>> {
        match self {
            Value::Obj(val) => Some(val.clone()),
            Value::UpValue(val) => val.borrow().as_obj(),
            _ => None,
        }
    }

    pub fn as_fn(&self) -> Option<Rc<RefCell<Function>>> {
        match self {
            Value::Function(val) => Some(val.clone()),
            Value::UpValue(val) => val.borrow().as_fn(),
            _ => None,
        }
    }

    pub fn typename(&self) -> &'static str {
        match self {
            Value::Bool(_) => "bool",
            Value::Num(_) => "num",
            Value::Str(_) => "str",
            Value::Vec(_) => "vec",
            Value::Obj(_) => {
                if Builtin::get_internal_vec(self).is_some() {
                    "vec"
                } else {
                    "obj"
                }
            }
            Value::Function(f) => match *f.borrow() {
                Function::Contiuation(..) => "cc",
                _ => "fn",
            },
            Value::Nil => "nil",
            Value::UpValue(val) => val.borrow().typename(),
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
    let closure = Value::Function(Rc::new(RefCell::new(Function::Syscall("print"))));
    let upval = Value::UpValue(Rc::new(RefCell::new(closure)));

    assert_eq!(upval, upval.upgrade());
    assert_eq!(upval.upgrade().upgrade(), upval.upgrade());
    assert_eq!(upval.clone(), upval.upgrade());
}
