use core::cell::RefCell;

use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec;
use hashbrown::HashMap;

use crate::errors::SquareError;
use crate::{
    vm::{ExecResult, VM},
    vm_value::{Closure, Value},
};

type Syscall = Rc<dyn Fn(&mut VM, Value, &mut usize) -> ExecResult>;

pub struct Builtin {
    pub values: HashMap<String, Value>,
    pub syscalls: HashMap<i32, Syscall>,
}

impl Builtin {
    pub fn new() -> Self {
        let mut values = HashMap::new();

        values.insert("true".to_string(), Value::Bool(true));
        values.insert("false".to_string(), Value::Bool(false));
        values.insert("nil".to_string(), Value::Nil);
        values.insert(
            "print".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-1)))),
        );
        values.insert(
            "println".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-2)))),
        );
        values.insert(
            "vec".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-3)))),
        );
        values.insert(
            "typeof".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-4)))),
        );
        values.insert(
            "concat".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-5)))),
        );

        #[cfg(not(test))]
        use crate::print;

        let mut syscalls = HashMap::new();

        syscalls.insert(
            -1,
            Rc::new(
                |_vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                    if let Value::Vec(top) = params {
                        top.borrow().iter().for_each(|val| print!("{}", val));
                    }
                    Ok(())
                },
            ) as Syscall,
        );
        syscalls.insert(
            -2,
            Rc::new(
                |_vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                    if let Value::Vec(top) = params {
                        top.borrow().iter().for_each(|val| print!("{}", val));
                    }
                    print!("\n");
                    Ok(())
                },
            ) as Syscall,
        );
        syscalls.insert(
            -3,
            Rc::new(
                |vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                    // params have already be packed
                    Ok(vm.current_frame().push(params))
                },
            ) as Syscall,
        );
        syscalls.insert(
            -4,
            Rc::new(
                |vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                    if let Value::Vec(top) = params {
                        if let Some(val) = top.borrow().get(0) {
                            return Ok(vm
                                .current_frame()
                                .push(Value::Str(val.typename().to_string())));
                        }
                    }

                    Err(SquareError::RuntimeError(
                        "typeof only accept one parameter".to_string(),
                    ))
                },
            ) as Syscall,
        );
        syscalls.insert(
            -5,
            Rc::new(
                |vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                    if let Value::Vec(top) = params {
                        if top.borrow().len() == 2 {
                            match (top.borrow().get(0).unwrap(), top.borrow().get(1).unwrap()) {
                                (Value::Str(s1), Value::Str(s2)) => {
                                    return Ok(vm
                                        .current_frame()
                                        .push(Value::Str(s1.to_string() + s2)))
                                }
                                (Value::Vec(v1), Value::Vec(v2)) => {
                                    let mut v = vec![];
                                    v.extend(v1.borrow().clone());
                                    v.extend(v2.borrow().clone());
                                    return Ok(vm
                                        .current_frame()
                                        .push(Value::Vec(Rc::new(RefCell::new(v)))));
                                }
                                (Value::Num(f1), Value::Num(f2)) => {
                                    let n1 = *f1 as i64;
                                    let n2 = *f2 as i64;
                                    return Ok(vm.current_frame().push(Value::Vec(Rc::new(
                                        RefCell::new(
                                            (n1..n2).map(|x| Value::Num(x as f64)).collect(),
                                        ),
                                    ))));
                                }
                                _ => {
                                    return Err(SquareError::RuntimeError(
                                        "concat type mismatch".to_string(),
                                    ));
                                }
                            }
                        } else {
                            return Err(SquareError::RuntimeError(
                                "concat only accept two parameters".to_string(),
                            ));
                        }
                    }

                    Err(SquareError::RuntimeError(
                        "typeof only accept one parameter".to_string(),
                    ))
                },
            ) as Syscall,
        );

        Self { values, syscalls }
    }
}
