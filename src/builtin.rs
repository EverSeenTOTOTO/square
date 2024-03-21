use core::cell::RefCell;

use alloc::format;
use alloc::rc::Rc;
use alloc::string::String;
use alloc::string::ToString;
use alloc::vec;
use alloc::vec::Vec;
use hashbrown::HashMap;

use crate::errors::SquareError;
use crate::{
    vm::{ExecResult, VM},
    vm_value::{Function, Value},
};

pub type Syscall = Rc<dyn Fn(&mut VM, Value, Rc<RefCell<Vec<Value>>>, &mut usize) -> ExecResult>;

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
                Value::Function(Rc::new(RefCell::new(Function::Syscall(
                    "print",
                    Value::Nil,
                )))),
                Some(Rc::new(
                    |_vm: &mut VM,
                     _this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     _pc: &mut usize|
                     -> ExecResult {
                        params.borrow().iter().for_each(|val| print!("{}", val));
                        Ok(())
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "println",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall(
                    "println",
                    Value::Nil,
                )))),
                Some(Rc::new(
                    |_vm: &mut VM,
                     _this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     _pc: &mut usize|
                     -> ExecResult {
                        params.borrow().iter().for_each(|val| print!("{}", val));
                        print!("\n");
                        Ok(())
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "vec",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("vec", Value::Nil)))),
                Some(Rc::new(
                    |vm: &mut VM,
                     _this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     _pc: &mut usize|
                     -> ExecResult {
                        // params have already be packed
                        Ok(vm
                            .current_frame()
                            .borrow_mut()
                            .push(Self::wrap_internal_vec(params)))
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "at",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("at", Value::Nil)))),
                Some(Rc::new(
                    |vm: &mut VM,
                     this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     _pc: &mut usize|
                     -> ExecResult {
                        if let Some(internal) = Self::get_internal_vec(&this) {
                            if let Some(Value::Num(index)) = params.borrow().get(0) {
                                Ok(vm.current_frame().borrow_mut().push(
                                    internal
                                        .borrow()
                                        .get(*index as usize)
                                        .unwrap_or(&Value::Nil)
                                        .clone(),
                                ))
                            } else {
                                Err(SquareError::RuntimeError(
                                    "at() expect a index parameter".to_string(),
                                ))
                            }
                        } else {
                            Err(SquareError::RuntimeError(
                                "at() method cannot be called directly".to_string(),
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "splice",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("at", Value::Nil)))),
                Some(Rc::new(
                    |vm: &mut VM,
                     this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     _pc: &mut usize|
                     -> ExecResult {
                        if let Some(internal) = Self::get_internal_vec(&this) {
                            if let (
                                Some(Value::Num(index)),
                                Some(Value::Num(del_count)),
                                Some(insert_obj),
                            ) = (
                                params.borrow().get(0),
                                params.borrow().get(1),
                                params.borrow().get(2),
                            ) {
                                let insert = Self::get_internal_vec(insert_obj)
                                    .unwrap_or(Rc::new(RefCell::new(vec![])));
                                let start = *index as usize;
                                let end_ = (index + del_count) as usize;
                                let end = if end_ > internal.borrow().len() {
                                    internal.borrow().len()
                                } else {
                                    end_
                                };

                                let deleted = internal
                                    .borrow_mut()
                                    .splice(start..end, insert.borrow().clone())
                                    .collect();

                                Ok(vm
                                    .current_frame()
                                    .borrow_mut()
                                    .push(Self::wrap_internal_vec(Rc::new(RefCell::new(deleted)))))
                            } else {
                                Err(SquareError::RuntimeError(
                                    "splice() expect (index, deleteCount, toInsert) parameter"
                                        .to_string(),
                                ))
                            }
                        } else {
                            Err(SquareError::RuntimeError(
                                "splice() cannot be called directly".to_string(),
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "typeof",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall(
                    "typeof",
                    Value::Nil,
                )))),
                Some(Rc::new(
                    |vm: &mut VM,
                     _this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     _pc: &mut usize|
                     -> ExecResult {
                        if let Some(val) = params.borrow().get(0) {
                            return Ok(vm
                                .current_frame()
                                .borrow_mut()
                                .push(Value::Str(val.typename().to_string())));
                        } else {
                            return Err(SquareError::RuntimeError(
                                "typeof() expect a parameter".to_string(),
                            ));
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "obj",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("obj", Value::Nil)))),
                Some(Rc::new(
                    |vm: &mut VM,
                     _this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     _pc: &mut usize|
                     -> ExecResult {
                        let obj = Rc::new(RefCell::new(HashMap::new()));

                        for i in (0..params.borrow().len()).step_by(2) {
                            if let (Some(key), Some(val)) =
                                (&params.borrow()[i].as_str(), params.borrow().get(i + 1))
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
                                    "failed to create object, index out of range {}",
                                    i
                                )));
                            }
                        }

                        return Ok(vm.current_frame().borrow_mut().push(Value::Obj(obj)));
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "callcc",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall(
                    "callcc",
                    Value::Nil,
                )))),
                Some(Rc::new(
                    |vm: &mut VM,
                     _this: Value,
                     params: Rc<RefCell<Vec<Value>>>,
                     pc: &mut usize|
                     -> ExecResult {
                        if let Some(ref iife) = params.borrow()[0].as_fn() {
                            let cc = Function::Contiuation(*pc, vm.save_context());

                            *pc = *pc - 2; // callcc is actually another kind of CALL instruction, we reuse the PACK and CALL that call callcc itself to call the parameter lambda provided to callcc

                            let binding = vm.current_frame();
                            let mut frame = binding.borrow_mut();

                            frame.push(Value::Function(iife.clone()));
                            frame.push(Value::Function(Rc::new(RefCell::new(cc))));

                            Ok(())
                        } else {
                            Err(SquareError::RuntimeError(
                                "callcc() expect a function parameter".to_string(),
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        Self { values }
    }

    pub fn wrap_internal_vec(internal: Rc<RefCell<Vec<Value>>>) -> Value {
        let obj = Rc::new(RefCell::new(HashMap::new()));

        obj.borrow_mut()
            .insert("this".to_string(), Value::Obj(obj.clone()));

        // FIXME
        obj.borrow_mut()
            .insert("__internal__".to_string(), Value::Vec(internal.clone()));

        obj.borrow_mut().insert(
            "at".to_string(),
            Value::Function(Rc::new(RefCell::new(Function::Syscall(
                "at",
                Value::Obj(obj.clone()),
            )))),
        );
        obj.borrow_mut().insert(
            "splice".to_string(),
            Value::Function(Rc::new(RefCell::new(Function::Syscall(
                "splice",
                Value::Obj(obj.clone()),
            )))),
        );

        Value::Obj(obj)
    }

    pub fn get_internal_vec(val: &Value) -> Option<Rc<RefCell<Vec<Value>>>> {
        match val {
            Value::Obj(obj) => {
                return obj
                    .borrow()
                    .get("__internal__")
                    .unwrap_or(&Value::Nil)
                    .as_vec();
            }
            Value::Vec(v) => Some(v.clone()),
            _ => None,
        }
    }

    #[inline]
    pub fn is_builtin(&self, name: &str) -> bool {
        return self.values.contains_key(name);
    }

    #[inline]
    pub fn resolve_builtin(&self, name: &str) -> Option<Value> {
        if let Some((value, _)) = self.values.get(name) {
            Some(value.clone())
        } else {
            None
        }
    }

    #[inline]
    pub fn get_syscall(&self, name: &str) -> Option<Syscall> {
        if let Some((_, syscall)) = self.values.get(name) {
            syscall.clone()
        } else {
            None
        }
    }
}
