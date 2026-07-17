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
    vm::{ExecResult, RtCx, VM},
    vm_value::{Function, Value},
};

#[cfg(target_family = "wasm")]
use crate::vm::{CallFrame, UnwindFrame};
#[cfg(target_family = "wasm")]
use alloc::string::String;

// Fn(vm, params, cx, inst)
pub type Syscall =
    Rc<dyn Fn(&mut VM, Rc<RefCell<Vec<Value>>>, &mut RtCx, &Inst) -> ExecResult>;

pub static INTERNAL_KEY: &str = "__internal__";
pub static GETTER_KEY: &str = "__get__";
pub static SETTER_KEY: &str = "__set__";

/// 把闭包（`ip` + `upvalues`）包装成 `UnwindFrame`，供 `defer`/`spawn` 使用。
///
/// `ra = ip + 1`：`PUSH_CLOSURE` 的 `ip` 指向闭包体入口前的 `JMP`，正常 `CALL` 靠 `step` 的
/// `pc += 1` 跳过它；这里 `tick` 直接 `vm.pc = ra` 进 `run`，没有那步 +1，故手动对齐到 `ip + 1`。
/// `stack[0]` 预置空参数 vec、`sp = 1` 对应入口的 `POP`（参数解包）。
#[cfg(target_family = "wasm")]
fn new_closure_unwind(ip: usize, upvalues: &HashMap<String, Value>) -> UnwindFrame {
    let mut frame = CallFrame::new();
    frame.stack[0] = Value::Vec(Rc::new(RefCell::new(vec![])));
    frame.sp = 1;
    frame.extend_locals(upvalues.clone());
    let sentinel = CallFrame::new();

    UnwindFrame {
        ra: ip + 1,
        context: vec![Rc::new(RefCell::new(sentinel)), Rc::new(RefCell::new(frame))],
    }
}

pub struct Builtin {
    values: HashMap<&'static str, (Value, Option<Syscall>)>,
}

impl Builtin {
    pub fn new() -> Self {
        let mut values = HashMap::new();

        #[cfg(target_family = "wasm")]
        use crate::print;
        #[cfg(target_family = "wasm")]
        use crate::println;

        values.insert("true", (Value::Bool(true), None));
        values.insert("false", (Value::Bool(false), None));
        values.insert("nil", (Value::Nil, None));

        values.insert(
            "print",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("print")))),
                Some(Rc::new(
                    |_vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, _inst: &Inst| -> ExecResult {
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
                    |_vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, _inst: &Inst| -> ExecResult {
                        params.borrow().iter().for_each(|val| print!("{}", val));
                        println!();
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
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, _inst: &Inst| -> ExecResult {
                        // params have already be packed
                        vm.current_frame()
                            .borrow_mut()
                            .push(Self::wrap_internal_vec(params));
                        Ok(())
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "at",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("at")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        if let Some(internal) =
                            Self::get_internal_vec(params.borrow().first().unwrap_or(&Value::Nil))
                        {
                            if let Some(Value::Num(index)) = params.borrow().get(1) {
                                vm.current_frame().borrow_mut().push(
                                    internal
                                        .borrow()
                                        .get(*index as usize)
                                        .unwrap_or(&Value::Nil)
                                        .clone(),
                                );
                                Ok(())
                            } else {
                                Err(SquareError::InstructionError(
                                    "at() expect (vector, index) parameter".to_string(),
                                    inst.clone(),
                                    vm.pc,
                                ))
                            }
                        } else {
                            Err(SquareError::InstructionError(
                                "at() expect (vector, index) parameter".to_string(),
                                inst.clone(),
                                vm.pc,
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
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        if let Some(internal) =
                            Self::get_internal_vec(params.borrow().first().unwrap_or(&Value::Nil))
                        {
                            vm.current_frame()
                                .borrow_mut()
                                .push(Value::Num(internal.borrow().len() as f64));
                            Ok(())
                        } else {
                            Err(SquareError::InstructionError(
                                "at() expect (vector, index) parameter".to_string(),
                                inst.clone(),
                                vm.pc,
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
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        if let Some(internal) =
                            Self::get_internal_vec(params.borrow().first().unwrap_or(&Value::Nil))
                        {
                            if let (Some(Value::Num(index)), Some(Value::Num(del_count))) =
                                (params.borrow().get(1), params.borrow().get(2))
                            {
                                let insert = Self::get_internal_vec(
                                    params.borrow().get(3).unwrap_or(&Value::Nil),
                                )
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

                                return {
                                    vm.current_frame()
                                        .borrow_mut()
                                        .push(Self::wrap_internal_vec(Rc::new(RefCell::new(
                                            deleted,
                                        ))));
                                    Ok(())
                                };
                            }
                        }

                        Err(SquareError::InstructionError(
                            "splice() expect (vector, index, deleteCount, toInsert) parameter"
                                .to_string(),
                            inst.clone(),
                            vm.pc,
                        ))
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "slice",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("slice")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        if let Some(internal) =
                            Self::get_internal_vec(params.borrow().first().unwrap_or(&Value::Nil))
                        {
                            if let (Some(Value::Num(start_index)), Some(Value::Num(end_index))) =
                                (params.borrow().get(1), params.borrow().get(2))
                            {
                                let start = *start_index as usize;
                                let end_ = *end_index as usize;
                                let end = if end_ > internal.borrow().len() {
                                    internal.borrow().len()
                                } else {
                                    end_
                                };

                                let slice = internal.borrow_mut()[start..end].to_vec();

                                return {
                                    vm.current_frame()
                                        .borrow_mut()
                                        .push(Self::wrap_internal_vec(Rc::new(RefCell::new(
                                            slice,
                                        ))));
                                    Ok(())
                                };
                            }
                        }

                        Err(SquareError::InstructionError(
                            "slice() expect (vector, start, end) parameter".to_string(),
                            inst.clone(),
                            vm.pc,
                        ))
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "typeof",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("typeof")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        if let Some(val) = params.borrow().first() {
                            return {
                                vm.current_frame()
                                    .borrow_mut()
                                    .push(Value::Str(val.typename().to_string()));
                                Ok(())
                            };
                        } else {
                            Err(SquareError::InstructionError(
                                "typeof() expect a parameter".to_string(),
                                inst.clone(),
                                vm.pc,
                            ))
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
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
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
                                    vm.pc,
                                ));
                            }
                        }

                        return {
                            vm.current_frame().borrow_mut().push(Value::Obj(obj));
                            Ok(())
                        };
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "set",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("set")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        let target = params.borrow().first().unwrap_or(&Value::Nil).as_obj();
                        let key = params.borrow().get(1).unwrap_or(&Value::Nil).as_str();
                        let value = params.borrow().get(2).unwrap_or(&Value::Nil).clone();

                        if let (Some(o), Some(k)) = (target, key) {
                            Self::try_capture_this(&value, &o);
                            o.borrow_mut().insert(k, value);
                            return {
                                vm.current_frame().borrow_mut().push(Value::Obj(o));
                                Ok(())
                            };
                        }

                        Err(SquareError::InstructionError(
                            "bad arguments provided to set()".to_string(),
                            inst.clone(),
                            vm.pc,
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
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, _cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        let target = params.borrow().first().unwrap_or(&Value::Nil).as_obj();
                        let key = params.borrow().get(1).unwrap_or(&Value::Nil).as_str();

                        if let (Some(o), Some(k)) = (target, key) {
                            let cloned = o.borrow_mut().get(&k).cloned().unwrap_or(Value::Nil);
                            return {
                                vm.current_frame().borrow_mut().push(cloned);
                                Ok(())
                            };
                        }

                        Err(SquareError::InstructionError(
                            "bad arguments provided to get()".to_string(),
                            inst.clone(),
                            vm.pc,
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
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, cx: &mut RtCx, inst: &Inst| -> ExecResult {
                        if let Some(iife) = params.borrow().first().and_then(Value::as_fn) {
                            let cc = Function::Continuation(vm.pc, vm.save_context());

                            inst.call(
                                vm,
                                cx,
                                iife.clone(),
                                Rc::new(RefCell::new(vec![Value::Function(Rc::new(
                                    RefCell::new(cc),
                                ))])),
                                false,
                            )
                        } else {
                            Err(SquareError::RuntimeError(
                                "callcc() expect a function parameter".to_string(),
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "sleep",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("sleep")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, cx: &mut RtCx, _inst: &Inst| -> ExecResult {
                        if let Some(cost) = params.borrow().first().and_then(Value::as_num) {
                            #[cfg(target_family = "wasm")]
                            {
                                let frame = UnwindFrame {
                                    ra: vm.pc + 1,
                                    context: vm.save_context(),
                                };
                                cx.park_sleep(frame, cost as u32);
                            }
                            Ok(())
                        } else {
                            Err(SquareError::RuntimeError(
                                "sleep expect a number parameter".to_string(),
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "defer",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("defer")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, cx: &mut RtCx, _inst: &Inst| -> ExecResult {
                        if let Some(func) = params.borrow().first().and_then(Value::as_fn) {
                            #[cfg(target_family = "wasm")]
                            {
                                if let Function::Closure(ip, ref upvalues) = *func.borrow() {
                                    cx.defer(new_closure_unwind(ip, upvalues));
                                }
                            }
                            let _ = vm;
                            Ok(())
                        } else {
                            Err(SquareError::RuntimeError(
                                "defer expect a function parameter".to_string(),
                            ))
                        }
                    },
                ) as Syscall),
            ),
        );

        values.insert(
            "spawn",
            (
                Value::Function(Rc::new(RefCell::new(Function::Syscall("spawn")))),
                Some(Rc::new(
                    |vm: &mut VM, params: Rc<RefCell<Vec<Value>>>, cx: &mut RtCx, _inst: &Inst| -> ExecResult {
                        if let Some(func) = params.borrow().first().and_then(Value::as_fn) {
                            #[cfg(target_family = "wasm")]
                            {
                                if let Function::Closure(ip, ref upvalues) = *func.borrow() {
                                    cx.spawn(new_closure_unwind(ip, upvalues));
                                }
                            }
                            let _ = vm;
                            Ok(())
                        } else {
                            Err(SquareError::RuntimeError(
                                "spawn expect a function parameter".to_string(),
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
            if let Function::Closure(_, ref mut captures) = *member_fn.borrow_mut() {
                captures.insert("this".to_string(), Value::Obj(obj.clone()));
            }
        }
    }

    #[inline]
    pub fn is_builtin(&self, name: &str) -> bool {
        self.values.contains_key(name)
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
