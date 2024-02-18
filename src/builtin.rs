use core::cell::RefCell;

use alloc::rc::Rc;
use alloc::string::{String, ToString};
use hashbrown::HashMap;

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
                    vm.current_frame().push(params)
                },
            ) as Syscall,
        );

        Self { values, syscalls }
    }
}
