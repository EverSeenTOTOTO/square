use core::cell::RefCell;

use alloc::format;
use alloc::rc::Rc;
use alloc::string::ToString;
use alloc::vec;
use alloc::vec::Vec;
use hashbrown::HashMap;

use crate::errors::SquareError;
use crate::vm_insts::Inst;
use crate::vm_value::Object;
use crate::{
    vm::{ExecResult, VM},
    vm_value::{Function, Value},
};

// Fn(vm, params, inst, pc)
pub type Syscall = Rc<dyn Fn(&mut VM, Rc<RefCell<Vec<Value>>>, &Inst, &mut usize) -> ExecResult>;

pub static INTERNAL_KEY: &'static str = "__internal__";
pub static GETTER_KEY: &'static str = "__get__";
pub static SETTER_KEY: &'static str = "__set__";

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
                    |_vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     _inst: &Inst,
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
                Value::Function(Rc::new(RefCell::new(Function::Syscall("println")))),
                Some(Rc::new(
                    |_vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     _inst: &Inst,
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
                Value::Function(Rc::new(RefCell::new(Function::Syscall("vec")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     _inst: &Inst,
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
                Value::Function(Rc::new(RefCell::new(Function::Syscall("at")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        if let Some(internal) =
                            Self::get_internal_vec(params.borrow().get(0).unwrap_or(&Value::Nil))
                        {
                            if let Some(Value::Num(index)) = params.borrow().get(1) {
                                Ok(vm.current_frame().borrow_mut().push(
                                    internal
                                        .borrow()
                                        .get(*index as usize)
                                        .unwrap_or(&Value::Nil)
                                        .clone(),
                                ))
                            } else {
                                Err(SquareError::InstructionError(
                                    "at() expect (vector, index) parameter".to_string(),
                                    inst.clone(),
                                    *pc,
                                ))
                            }
                        } else {
                            Err(SquareError::InstructionError(
                                "at() expect (vector, index) parameter".to_string(),
                                inst.clone(),
                                *pc,
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "len",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("len")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        if let Some(internal) =
                            Self::get_internal_vec(params.borrow().get(0).unwrap_or(&Value::Nil))
                        {
                            Ok(vm
                                .current_frame()
                                .borrow_mut()
                                .push(Value::Num(internal.borrow().len() as f64)))
                        } else {
                            Err(SquareError::InstructionError(
                                "at() expect (vector, index) parameter".to_string(),
                                inst.clone(),
                                *pc,
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "splice",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("splice")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        if let Some(internal) =
                            Self::get_internal_vec(params.borrow().get(0).unwrap_or(&Value::Nil))
                        {
                            if let (
                                Some(Value::Num(index)),
                                Some(Value::Num(del_count)),
                            ) = (
                                params.borrow().get(1),
                                params.borrow().get(2),
                            ) {
                                let insert = Self::get_internal_vec(params.borrow().get(3).unwrap_or(&Value::Nil))
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
                                Err(SquareError::InstructionError(
                                    "splice() expect (vector, index, deleteCount, toInsert) parameter"
                                        .to_string(),
                                    inst.clone(),
                                    *pc,
                                ))
                            }
                        } else {
                            Err(SquareError::InstructionError(
                                "splice() expect (vector, index, deleteCount, toInsert) parameter"
                                    .to_string(),
                                inst.clone(),
                                *pc,
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "typeof",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("typeof")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        if let Some(val) = params.borrow().get(0) {
                            return Ok(vm
                                .current_frame()
                                .borrow_mut()
                                .push(Value::Str(val.typename().to_string())));
                        } else {
                            return Err(SquareError::InstructionError(
                                "typeof() expect a parameter".to_string(),
                                inst.clone(),
                                *pc,
                            ));
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "obj",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("obj")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        let obj = Rc::new(RefCell::new(HashMap::new()));

                        for i in (0..params.borrow().len()).step_by(2) {
                            if let (Some(key), Some(val)) =
                                (&params.borrow()[i].as_str(), params.borrow().get(i + 1))
                            {
                                Self::try_capture_this(val, &obj);
                                obj.borrow_mut().insert(key.to_string(), val.clone());
                            } else {
                                return Err(SquareError::InstructionError(
                                    format!("failed to create object, index out of range {}", i),
                                    inst.clone(),
                                    *pc,
                                ));
                            }
                        }

                        return Ok(vm.current_frame().borrow_mut().push(Value::Obj(obj)));
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "set",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("set")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        let target = params.borrow().get(0).unwrap_or(&Value::Nil).as_obj();
                        let key = params.borrow().get(1).unwrap_or(&Value::Nil).as_str();
                        let value = params.borrow().get(2).unwrap_or(&Value::Nil).clone();

                        if let (Some(o), Some(k)) = (target, key) {
                            Self::try_capture_this(&value, &o);
                            o.borrow_mut().insert(k, value);
                            return Ok(vm.current_frame().borrow_mut().push(Value::Obj(o)));
                        }

                        Err(SquareError::InstructionError(
                            "bad arguments provided to set()".to_string(),
                            inst.clone(),
                            *pc,
                        ))
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "get",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("get")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        let target = params.borrow().get(0).unwrap_or(&Value::Nil).as_obj();
                        let key = params.borrow().get(1).unwrap_or(&Value::Nil).as_str();

                        if let (Some(o), Some(k)) = (target, key) {
                            let cloned = o.borrow_mut().get(&k).cloned().unwrap_or(Value::Nil);
                            return Ok(vm.current_frame().borrow_mut().push(cloned));
                        }

                        Err(SquareError::InstructionError(
                            "bad arguments provided to get()".to_string(),
                            inst.clone(),
                            *pc,
                        ))
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "callcc",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("callcc")))),
                Some(Rc::new(
                    |vm: &mut VM,
                     params: Rc<RefCell<Vec<Value>>>,
                     inst: &Inst,
                     pc: &mut usize|
                     -> ExecResult {
                        if let Some(ref iife) = params.borrow()[0].as_fn() {
                            let cc = Function::Contiuation(*pc, vm.save_context());

                            return inst.call(
                                vm,
                                iife.clone(),
                                Rc::new(RefCell::new(vec![Value::Function(Rc::new(
                                    RefCell::new(cc),
                                ))])),
                                false,
                                pc,
                            );
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

    fn wrap_internal_vec(internal: Rc<RefCell<Vec<Value>>>) -> Value {
        let obj = Rc::new(RefCell::new(HashMap::new()));

        obj.borrow_mut()
            .insert("this".to_string(), Value::Obj(obj.clone()));
        // FIXME
        obj.borrow_mut()
            .insert(INTERNAL_KEY.to_string(), Value::Vec(internal.clone()));

        Value::Obj(obj)
    }

    pub fn get_internal_vec(val: &Value) -> Option<Rc<RefCell<Vec<Value>>>> {
        if let Some(obj) = val.as_obj() {
            return obj
                .borrow()
                .get(INTERNAL_KEY)
                .unwrap_or(&Value::Nil)
                .as_vec();
        } else if let Some(v) = val.as_vec() {
            return Some(v);
        }

        None
    }

    fn try_capture_this(val: &Value, obj: &Rc<RefCell<Object>>) {
        if let Some(member_fn) = val.as_fn() {
            match *member_fn.borrow_mut() {
                Function::Closure(_, ref mut captures) => {
                    captures.insert("this".to_string(), Value::Obj(obj.clone()));
                }
                _ => {}
            }
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
    pub fn get_syscall(&self, name: &str) -> Syscall {
        let syscall = self.values.get(name).unwrap().1.clone();
        syscall.unwrap()
    }
}
