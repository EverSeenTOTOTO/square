use core::cell::RefCell;

use alloc::format;
use alloc::rc::Rc;
use alloc::string::ToString;
use hashbrown::HashMap;

use crate::errors::SquareError;
use crate::{
    vm::{ExecResult, VM},
    vm_value::{Function, Value},
};

pub type Syscall = Rc<dyn Fn(&mut VM, Value, &mut usize) -> ExecResult>;

pub struct Builtin {
    values: HashMap<&'static str, (Value, Option<Syscall>)>,
}

impl Builtin {
    pub fn new() -> Self {
        let mut values = HashMap::new();

        #[cfg(target_family = "wasm")]
        use crate::print;

        values.insert("true", (Value::Bool(true), None));
        values.insert("false", (Value::Bool(false), None));
        values.insert("nil", (Value::Nil, None));
        values.insert(
            "print",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("print")))),
                Some(Rc::new(
                    |_vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                        if let Value::Vec(top) = params {
                            top.borrow().iter().for_each(|val| print!("{}", val));
                        }
                        Ok(())
                    },
                ) as Syscall),
            ),
        );
        values.insert(
            "println",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("println")))),
                Some(Rc::new(
                    |_vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                        if let Value::Vec(top) = params {
                            top.borrow().iter().for_each(|val| print!("{}", val));
                        }
                        print!("\n");
                        Ok(())
                    },
                ) as Syscall),
            ),
        );
        values.insert(
            "vec",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("vec")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                        // params have already be packed
                        Ok(vm.current_frame().borrow_mut().push(params))
                    },
                ) as Syscall),
            ),
        );
        values.insert(
            "typeof",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("typeof")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                        if let Value::Vec(ref top) = params {
                            if let Some(val) = top.borrow().get(0) {
                                return Ok(vm
                                    .current_frame()
                                    .borrow_mut()
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
                ) as Syscall),
            ),
        );
        values.insert(
            "obj",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("obj")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Value, _pc: &mut usize| -> ExecResult {
                        if let Value::Vec(ref top) = params {
                            let obj = Rc::new(RefCell::new(HashMap::new()));

                            for i in (0..top.borrow().len()).step_by(2) {
                                if let (Value::Str(key), val) =
                                    (&top.borrow()[i], &top.borrow()[i + 1])
                                {
                                    if let Some(member_fn) = val.as_fn() {
                                        match *member_fn.borrow_mut() {
                                            Function::Closure(_, ref mut captures) => {
                                                captures.insert(
                                                    "this".to_string(),
                                                    Value::Obj(obj.clone()),
                                                );
                                            }
                                            _ => {}
                                        }
                                    }
                                    obj.borrow_mut().insert(key.to_string(), val.clone());
                                } else {
                                    return Err(SquareError::RuntimeError(format!(
                                        "obj only accept key-value pairs, got {}",
                                        params
                                    )));
                                }
                            }

                            return Ok(vm.current_frame().borrow_mut().push(Value::Obj(obj)));
                        }

                        unreachable!()
                    },
                ) as Syscall),
            ),
        );
        values.insert(
            "callcc",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("callcc")))),
                Some(
                    Rc::new(|vm: &mut VM, params: Value, pc: &mut usize| -> ExecResult {
                        if let Value::Vec(ref top) = params {
                            if let Some(ref iife) = top.borrow()[0].as_fn() {
                                let cc = Function::Contiuation(*pc, vm.save_context());

                                *pc = *pc - 2; // callcc is actually another kind of CALL instruction, we reuse the PACK and CALL that call callcc itself to call the parameter lambda provided to callcc

                                let binding = vm.current_frame();
                                let mut frame = binding.borrow_mut();

                                frame.push(Value::Function(iife.clone()));
                                frame.push(Value::Function(Rc::new(RefCell::new(cc))));

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
                    }) as Syscall,
                ),
            ),
        );

        Self { values }
    }

    pub fn is_builtin(&self, name: &str) -> bool {
        return self.values.contains_key(name);
    }

    pub fn resolve_builtin(&self, name: &str) -> Option<Value> {
        if let Some((value, _)) = self.values.get(name) {
            Some(value.clone())
        } else {
            None
        }
    }

    pub fn get_syscall(&self, name: &str) -> Option<Syscall> {
        if let Some((_, syscall)) = self.values.get(name) {
            syscall.clone()
        } else {
            None
        }
    }
}
