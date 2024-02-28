use core::cell::RefCell;

use alloc::format;
use alloc::rc::Rc;
use alloc::string::{String, ToString};
use alloc::vec;
use hashbrown::HashMap;

use crate::errors::SquareError;
use crate::vm::CallFrame;
use crate::{
    vm::{ExecResult, VM},
    vm_value::{Closure, Value},
};

pub type Syscall = Rc<dyn Fn(&mut VM, Rc<RefCell<Closure>>, Value, &mut usize) -> ExecResult>;

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
            "__concat".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-4)))),
        );
        values.insert(
            "obj".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-6)))),
        );
        values.insert(
            "callcc".to_string(),
            Value::Closure(Rc::new(RefCell::new(Closure::new(-7)))),
        );
        // -8 is used for call cc

        #[cfg(not(test))]
        use crate::print;

        let mut syscalls = HashMap::new();

        // print
        syscalls.insert(
            -1,
            Rc::new(
                |_vm: &mut VM,
                 _fn: Rc<RefCell<Closure>>,
                 params: Value,
                 _pc: &mut usize|
                 -> ExecResult {
                    if let Value::Vec(top) = params {
                        top.borrow().iter().for_each(|val| print!("{}", val));
                    }
                    Ok(())
                },
            ) as Syscall,
        );
        // println
        syscalls.insert(
            -2,
            Rc::new(
                |_vm: &mut VM,
                 _fn: Rc<RefCell<Closure>>,
                 params: Value,
                 _pc: &mut usize|
                 -> ExecResult {
                    if let Value::Vec(top) = params {
                        top.borrow().iter().for_each(|val| print!("{}", val));
                    }
                    print!("\n");
                    Ok(())
                },
            ) as Syscall,
        );
        // vec
        syscalls.insert(
            -3,
            Rc::new(
                |vm: &mut VM,
                 _fn: Rc<RefCell<Closure>>,
                 params: Value,
                 _pc: &mut usize|
                 -> ExecResult {
                    // params have already be packed
                    Ok(vm.current_frame().push(params))
                },
            ) as Syscall,
        );
        // typeof
        syscalls.insert(
            -4,
            Rc::new(
                |vm: &mut VM,
                 _fn: Rc<RefCell<Closure>>,
                 params: Value,
                 _pc: &mut usize|
                 -> ExecResult {
                    if let Value::Vec(ref top) = params {
                        if let Some(val) = top.borrow().get(0) {
                            return Ok(vm
                                .current_frame()
                                .push(Value::Str(val.typename().to_string())));
                        } else {
                            return Err(SquareError::RuntimeError(format!(
                                "typeof only accept one parameter, got {}",
                                params
                            )));
                        }
                    }

                    unreachable!()
                },
            ) as Syscall,
        );
        // concat
        syscalls.insert(
            -5,
            Rc::new(
                |vm: &mut VM,
                 _fn: Rc<RefCell<Closure>>,
                 params: Value,
                 _pc: &mut usize|
                 -> ExecResult {
                    if let Value::Vec(ref top) = params {
                        if top.borrow().len() != 2 {
                            return Err(SquareError::RuntimeError(format!(
                                "concat only accept two parameters, got {}",
                                params
                            )));
                        }

                        match (&top.borrow()[0], &top.borrow()[1]) {
                            (Value::Str(s1), Value::Str(s2)) => {
                                return Ok(vm.current_frame().push(Value::Str(s1.to_string() + s2)))
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
                                    RefCell::new((n1..n2).map(|x| Value::Num(x as f64)).collect()),
                                ))));
                            }
                            _ => {
                                return Err(SquareError::RuntimeError(format!(
                                    "concat type mismatch, expect str, vec or integer, got {}",
                                    params
                                )));
                            }
                        }
                    }

                    unreachable!()
                },
            ) as Syscall,
        );
        // obj
        syscalls.insert(
            -6,
            Rc::new(
                |vm: &mut VM,
                 _fn: Rc<RefCell<Closure>>,
                 params: Value,
                 _pc: &mut usize|
                 -> ExecResult {
                    if let Value::Vec(ref top) = params {
                        let mut obj = HashMap::new();
                        for i in (0..top.borrow().len()).step_by(2) {
                            if let (Some(Value::Str(key)), Some(val)) =
                                (top.borrow().get(i), top.borrow().get(i + 1))
                            {
                                obj.insert(key.to_string(), val.clone());
                            } else {
                                return Err(SquareError::RuntimeError(format!(
                                    "obj only accept key-value pairs, got {}",
                                    params
                                )));
                            }
                        }

                        return Ok(vm
                            .current_frame()
                            .push(Value::Obj(Rc::new(RefCell::new(obj)))));
                    }

                    unreachable!()
                },
            ) as Syscall,
        );
        // cc
        syscalls.insert(
            -7,
            Rc::new(
                |vm: &mut VM,
                 _fn: Rc<RefCell<Closure>>,
                 params: Value,
                 pc: &mut usize|
                 -> ExecResult {
                    if let Value::Vec(ref top) = params {
                        if let Value::Closure(ref iife) = top.borrow()[0] {
                            let mut cc = Closure::new(-8);
                            cc.capture_ctx(*pc, vm.save_context());

                            vm.current_frame().push(Value::Closure(iife.clone()));
                            vm.current_frame()
                                .push(Value::Closure(Rc::new(RefCell::new(cc))));

                            Ok(())
                        } else {
                            Err(SquareError::RuntimeError(format!(
                                "callcc only accept one function parameter, got {}",
                                params
                            )))
                        }
                    } else {
                        unreachable!()
                    }
                },
            ) as Syscall,
        );
        syscalls.insert(
            -8,
            Rc::new(
                |vm: &mut VM,
                 closure: Rc<RefCell<Closure>>,
                 params: Value,
                 pc: &mut usize|
                 -> ExecResult {
                    if let Value::Vec(ref top) = params {
                        let cc = closure.borrow().context.clone().unwrap();
                        *pc = cc.0;
                        vm.restore_context(cc.1);
                        Ok(vm.current_frame().push(top.borrow()[0].clone()))
                    } else {
                        Err(SquareError::RuntimeError(format!(
                            "cc only accept one parameter, got {}",
                            params
                        )))
                    }
                },
            ) as Syscall,
        );

        Self { values, syscalls }
    }
}
