use alloc::{boxed::Box, format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use core::{cell::RefCell, fmt};

use hashbrown::{HashMap, HashSet};

use crate::{
    code_frame::Position,
    emit::{emit, EmitContext},
    errors::SquareError,
    parse::parse,
    vm_insts::Inst,
    vm_value::{CalcResult, Closure, Value},
};

pub type ExecResult = Result<(), SquareError>;

type OpFn = dyn Fn(&Value, &Value) -> CalcResult;

impl Inst {
    fn exec(&self, vm: &mut VM, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        match self {
            Inst::COMMENT(_) => Ok(()),
            Inst::PUSH(value) => self.push(vm, value.clone(), pc),
            Inst::POP => self.pop(vm, pc),

            Inst::ADD => self.binop(vm, &|a, b| a + b, pc),
            Inst::SUB => self.binop(vm, &|a, b| a - b, pc),
            Inst::MUL => self.binop(vm, &|a, b| a * b, pc),
            Inst::DIV => self.binop(vm, &|a, b| a / b, pc),
            Inst::REM => self.binop(vm, &|a, b| a % b, pc),
            Inst::BITAND => self.binop(vm, &|a, b| a & b, pc),
            Inst::BITOR => self.binop(vm, &|a, b| a | b, pc),
            Inst::BITXOR => self.binop(vm, &|a, b| a ^ b, pc),
            Inst::EQ => self.binop(vm, &|a, b| Ok(Value::Bool(a == b)), pc),
            Inst::NE => self.binop(vm, &|a, b| Ok(Value::Bool(a != b)), pc),
            Inst::LT => self.binop(vm, &|a, b| Ok(Value::Bool(a < b)), pc),
            Inst::LE => self.binop(vm, &|a, b| Ok(Value::Bool(a <= b)), pc),
            Inst::GT => self.binop(vm, &|a, b| Ok(Value::Bool(a > b)), pc),
            Inst::GE => self.binop(vm, &|a, b| Ok(Value::Bool(a >= b)), pc),
            Inst::SHL => self.binop(vm, &|a, b| a << b, pc),
            Inst::SHR => self.binop(vm, &|a, b| a >> b, pc),
            Inst::BITNOT => {
                let frame = vm.current_frame();

                if frame.sp < 1 {
                    return Err(SquareError::InstructionError(
                        format!("bad binary operation, operand stack length is {}", frame.sp),
                        self.clone(),
                        *pc,
                    ));
                }

                let result = !&frame.stack[frame.sp - 1];

                if let Err(SquareError::RuntimeError(msg)) = result {
                    return Err(SquareError::InstructionError(msg, self.clone(), *pc));
                }

                frame.stack[frame.sp - 1] = result?;
                Ok(())
            }

            Inst::JMP(value) => self.jump(insts, pc, *value),
            Inst::JNE(value) => {
                let frame = vm.current_frame();

                if frame.sp < 1 {
                    return Err(SquareError::InstructionError(
                        "bad jne, operand stack empty".to_string(),
                        self.clone(),
                        *pc,
                    ));
                }

                if !frame.top().unwrap().to_bool() {
                    frame.sp -= 1;
                    self.jump(insts, pc, *value)
                } else {
                    frame.sp -= 1;
                    Ok(())
                }
            }

            Inst::STORE(name) => {
                let frame = vm.current_frame();

                if let Some(val) = frame.top() {
                    self.scope_lift(frame, name, val.clone());
                    self.pop(vm, pc)
                } else {
                    frame.define_local(name, Value::Nil);
                    Ok(())
                }
            }
            Inst::LOAD(name) => {
                let frame = vm.current_frame();
                let result = if let Some(val) = frame.resolve(name) {
                    Ok(val.clone())
                } else {
                    Err(SquareError::InstructionError(
                        format!("undefined variable: {}", name),
                        self.clone(),
                        *pc,
                    ))
                };
                self.push(vm, result?, pc)
            }

            Inst::CALL => {
                // call closure
                let frame = vm.current_frame();

                if frame.sp < 2 {
                    return Err(SquareError::InstructionError(
                        "bad call, closure and params required".to_string(),
                        self.clone(),
                        *pc,
                    ));
                }

                // operand stack top should be [closure, params]<-
                if let Value::Closure(ref closure) = &frame.stack[frame.sp - 2].refer() {
                    let new_pc = closure.borrow().ip;
                    let params = frame.stack[frame.sp - 1].clone();
                    frame.sp -= 2; // pop params and closure

                    let mut new_frame = CallFrame::new();

                    // fill up captures
                    if let Some(unresolved) = closure.borrow().first_unresolved() {
                        return Err(SquareError::InstructionError(
                            format!("unresolved upvalue: {}", unresolved),
                            self.clone(),
                            *pc,
                        ));
                    }
                    closure
                        .borrow()
                        .upvalues
                        .iter()
                        .for_each(|(key, value)| new_frame.define_local(key, value.clone()));

                    new_frame.prev = vm.call_frame.take();
                    new_frame.ra = *pc;

                    // jump to function
                    *pc = new_pc as usize;

                    vm.call_frame = Some(Box::new(new_frame));

                    // always push param pack
                    return self.push(vm, params, pc);
                }

                Err(SquareError::InstructionError(
                    format!("bad call, cannot call with {}", frame.stack[frame.sp - 2]),
                    self.clone(),
                    *pc,
                ))
            }
            Inst::RET => {
                let frame = vm.current_frame();
                let top = frame.top().unwrap_or(&Value::Nil).clone();

                // jump back
                *pc = frame.ra;
                vm.call_frame = frame.prev.take();
                // always return the top value
                self.push(vm, top, pc)
            }
            Inst::PUSH_CLOSURE(meta) => {
                // create closure
                let mut closure = meta.clone();
                closure.ip = (*pc as i32) + closure.ip;

                if closure.ip < 0 {
                    return Err(SquareError::InstructionError(
                        format!("invalid function address: {}", closure.ip),
                        self.clone(),
                        *pc,
                    ));
                }

                let frame = vm.current_frame();

                // upgrade value to captured
                let varnames: Vec<_> = closure.captures.iter().cloned().collect();
                for name in varnames {
                    if let Some(target_frame) = frame.find_frame_by_varname(&name) {
                        let upvalue = target_frame.resolve(&name).unwrap().upgrade();

                        closure.capture(&name, &upvalue);
                        target_frame.define_local(&name, upvalue.clone());
                    }
                    // else undefined yet, if later be defined in same scope,
                    // the value will still be captured (see scope lifting in implementation of Inst::STORE)
                }

                self.push(vm, Value::Closure(Rc::new(RefCell::new(closure))), pc)
            }

            Inst::PACK(len) => {
                let frame = vm.current_frame();

                if frame.sp < *len {
                    return Err(SquareError::InstructionError(
                        format!("bad pack, {} requied, {} provided", len, frame.sp),
                        self.clone(),
                        *pc,
                    ));
                }

                let mut result = vec![];
                for index in frame.sp - len..frame.sp {
                    result.push(frame.stack[index].clone());
                }
                frame.sp -= *len;
                self.push(vm, Value::Vec(Rc::new(RefCell::new(result))), pc)
            }
            Inst::PEEK(offset, i) => {
                let top = vm.current_frame().top();
                let result = if let Some(Value::Vec(val)) = top {
                    let pack = val.borrow();

                    if *offset >= pack.len() {
                        return Err(SquareError::InstructionError(
                            format!(
                                "bad peek, offset {} out of range, pack length is {}",
                                offset,
                                pack.len()
                            ),
                            self.clone(),
                            *pc,
                        ));
                    }

                    let len = (pack.len() - offset) as i32;
                    let index = if *i > 0 { *i } else { (i + len) % len } as usize + offset;

                    if index < pack.len() {
                        Ok(pack.get(index).unwrap().clone())
                    } else {
                        Err(SquareError::InstructionError(
                            format!(
                                "bad peek, index {} out of range, pack length is {}",
                                index,
                                pack.len()
                            ),
                            self.clone(),
                            *pc,
                        ))
                    }
                } else {
                    Err(SquareError::InstructionError(
                        "bad peek, top value is not a pack".to_string(),
                        self.clone(),
                        *pc,
                    ))
                };

                self.push(vm, result?, pc)
            }
        }
    }

    fn push(&self, vm: &mut VM, value: Value, _pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();

        if frame.sp >= frame.stack.len() {
            frame.stack.resize(frame.stack.len() * 2, Value::Nil);
        }

        frame.stack[frame.sp] = value;
        frame.sp += 1;
        Ok(())
    }

    fn pop(&self, vm: &mut VM, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();

        if frame.sp < 1 {
            return Err(SquareError::InstructionError(
                "bad pop, operand stack empty".to_string(),
                self.clone(),
                *pc,
            ));
        }
        frame.sp -= 1;
        Ok(())
    }

    fn binop(&self, vm: &mut VM, op_fn: &OpFn, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();

        if frame.sp < 2 {
            return Err(SquareError::InstructionError(
                format!("bad binary operation, operand stack length is {}", frame.sp),
                self.clone(),
                *pc,
            ));
        }

        let result = op_fn(&frame.stack[frame.sp - 2], &frame.stack[frame.sp - 1]);

        if let Err(SquareError::RuntimeError(msg)) = result {
            return Err(SquareError::InstructionError(msg, self.clone(), *pc));
        }

        frame.stack[frame.sp - 2] = result?;
        frame.sp -= 1;
        Ok(())
    }

    fn jump(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) -> ExecResult {
        let new_pc = *pc as i32 + offset;
        if new_pc < 0 || new_pc >= insts.len() as i32 {
            return Err(SquareError::InstructionError(
                "bad pc".to_string(),
                self.clone(),
                *pc,
            ));
        }

        *pc = new_pc as usize;
        Ok(())
    }

    fn scope_lift(&self, frame: &mut CallFrame, name: &String, value: Value) {
        let upvalue = value.upgrade();
        let mut captured = upvalue.scope_lift(name, &upvalue); // handle self recursion

        // scope lifting
        frame.locals.iter_mut().for_each(|(_, val)| {
            captured = val.scope_lift(name, &upvalue);
        });

        if captured {
            frame.define_local(name, upvalue);
        } else {
            frame.define_local(name, value);
        }
    }
}

pub struct CallFrame {
    locals: HashMap<String, Value>,

    // operand stack
    stack: Vec<Value>,
    // fake stack pointer, avoid frequent operand stak push/pop
    sp: usize,

    ra: usize, // return address
    prev: Option<Box<CallFrame>>,
}

impl fmt::Display for CallFrame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "------- RA: {} -------", self.ra)?;
        writeln!(f, "Locals:")?;
        for (key, value) in &self.locals {
            writeln!(f, "{:>8}: {}", key, value)?;
        }

        writeln!(f, "Operand Stack:")?;
        for index in 0..self.sp {
            let value = &self.stack[index];
            writeln!(f, "{:>8}: {}", index, value)?;
        }

        Ok(())
    }
}

impl CallFrame {
    fn new() -> Self {
        Self {
            locals: HashMap::new(),
            stack: vec![Value::Nil; 8],
            sp: 0,
            ra: 0,
            prev: None,
        }
    }

    pub fn find_frame_by_varname<'a>(&mut self, name: &'a str) -> Option<&mut CallFrame> {
        if self.locals.contains_key(name) {
            return Some(self);
        }

        let mut frame = &mut self.prev;
        while let Some(f) = frame {
            if f.locals.contains_key(name) {
                return Some(f.as_mut());
            }
            frame = &mut f.prev;
        }
        return None;
    }

    pub fn resolve(&mut self, name: &str) -> Option<&Value> {
        let frame = self.find_frame_by_varname(name)?;

        return frame.locals.get(name);
    }

    pub fn define(&mut self, name: &str, value: &Value) {
        if let Some(frame) = self.find_frame_by_varname(name) {
            frame.define_local(name, value.clone());
        } else {
            self.define_local(name, value.clone());
        }
    }

    pub fn define_local(&mut self, name: &str, value: Value) {
        self.locals.insert(name.to_string(), value);
    }

    pub fn top(&self) -> Option<&Value> {
        if self.sp <= 0 {
            return None;
        }

        Some(&self.stack[self.sp - 1])
    }
}

#[test]
fn test_resolve_local() {
    let mut top = CallFrame::new();

    top.locals.insert("a".to_string(), Value::Num(42.0));

    let mut frame = CallFrame::new();

    frame.prev = Some(Box::new(top));
    frame.locals.insert("b".to_string(), Value::Num(24.0));

    assert_eq!(frame.resolve("a"), Some(&Value::Num(42.0)));
    assert_eq!(frame.resolve("b"), Some(&Value::Num(24.0)));
    assert_eq!(frame.resolve("c"), None);
}

#[test]
fn test_define_local() {
    let mut top = CallFrame::new();

    top.define_local("a", Value::Num(42.0));

    let mut frame = CallFrame::new();

    frame.prev = Some(Box::new(top));
    frame.define_local("b", Value::Num(24.0));

    assert_eq!(frame.resolve("a"), Some(&Value::Num(42.0)));
    assert_eq!(frame.resolve("b"), Some(&Value::Num(24.0)));
    assert_eq!(frame.resolve("c"), None);
}

pub struct VM {
    pub call_frame: Option<Box<CallFrame>>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            call_frame: Some(Box::new(CallFrame::new())),
        }
    }

    pub fn current_frame(&mut self) -> &mut CallFrame {
        self.call_frame.as_mut().unwrap()
    }

    pub fn step(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        let inst = &insts[*pc];
        let old_pc = *pc;
        inst.exec(self, insts, pc)?;
        *pc += 1;

        #[cfg(not(test))]
        use crate::println;

        match inst {
            Inst::COMMENT(_) => {}
            _ => println!("{}: {}\n{}\n", old_pc, inst, self.current_frame()),
        }

        Ok(())
    }

    pub fn run(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        while *pc < insts.len() {
            self.step(insts, pc)?;
        }

        Ok(())
    }
}

#[test]
fn test_grow_operand_stack() {
    let mut vm = VM::new();
    let mut insts = vec![];

    for i in 0..100 {
        insts.push(Inst::PUSH(Value::Num(i as f64)))
    }

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.stack.len() >= 100, true);
    assert_eq!(callframe.sp, 100);
    assert_eq!(callframe.top(), Some(&Value::Num(99.0)));
}

#[test]
fn test_exec_token() {
    let code = "42";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load() {
    let code = "x";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().define_local("x", Value::Num(42.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load_undefined() {
    let code = "x";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD("x".to_string()),
            0,
        ))
    );
}

#[test]
fn test_exec_assign() {
    let code = "[= x 42]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand() {
    let code = "[= [x] [vec 42]]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand_dot() {
    let code = "[= [. x] [vec 1 42 3]]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand_dot_error() {
    let code = "[= [. x] [vec 42]]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "bad peek, index 1 out of range, pack length is 1".to_string(),
            Inst::PEEK(0, 1),
            4,
        ))
    );
}

#[test]
fn test_exec_assign_expand_greed() {
    let code = "[= [... x] [vec 1 2 42]]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand_greed_error() {
    let code = "[= [... x] [vec]]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "bad peek, offset 0 out of range, pack length is 0".to_string(),
            Inst::PEEK(0, -1),
            1,
        ))
    );
}

#[test]
fn test_exec_assign_expand_nested() {
    let code = "[= [. [x]] [vec 1 [vec 42] 3]]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_op() {
    let code = "[- 1 2]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(-1.0))
}

#[test]
fn test_exec_op_assign() {
    let code = "[+= x 2]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().define_local("x", Value::Num(1.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0));
    assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(3.0))
}

#[test]
fn test_exec_begin() {
    let code = "[begin 1 2 3]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0))
}

#[test]
fn test_exec_scope() {
    let code = "
        [= x 1]
        [= y [begin [= x 2] x]]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(1.0));
    assert_eq!(callframe.resolve("y").unwrap(), &Value::Num(2.0));
}

#[test]
fn test_exec_if_true() {
    let code = "[if true 42]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_if_true_nil() {
    let code = "[if false 42]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Nil)
}

#[test]
fn test_exec_if_true_false() {
    let code = "[if false 42 24]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(24.0))
}

// #[test]
// fn test_exec_while() {
//     let code = "[while [< x 4] [begin [+= x 1]]]";
//     let ast = parse(code, &mut Position::default()).unwrap();
//     let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
//     let mut vm = VM::new();
// 
//     vm.current_frame().define_local("x", Value::Num(0.0));
//     insts.iter().for_each(|inst| println!("{}", inst));
//     vm.run(&insts, &mut 0).unwrap();
// 
//     let callframe = vm.current_frame();
//     assert_eq!(callframe.resolve("x").unwrap(), &Value::Num(4.0))
// }

#[test]
fn test_exec_fn_def() {
    let code = "/[] 42";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(
        callframe.top().unwrap(),
        &Value::Closure(Rc::new(RefCell::new(Closure::new(1))))
    );
}

#[test]
fn test_exec_fn_def_capture() {
    let code = "/[] x";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().define_local("x", Value::Num(42.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    let mut upvalues = HashMap::new();
    // should capture x
    upvalues.insert("x".to_string(), Value::UpValue(Rc::new(Value::Num(42.0))));
    let mut captures = HashSet::new();
    captures.insert("x".to_string());

    assert_eq!(
        callframe.top().unwrap(),
        &Value::Closure(Rc::new(RefCell::new(Closure {
            ip: 1,
            upvalues,
            captures
        })))
    );
}

#[test]
fn test_exec_fn_capture_lit() {
    let code = "
    [= fn /[] [begin 
        [= x 1]
        /[] [+= x 1]]]
    [= f [fn]]
    [= g [fn]]
    [f]
    [g]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top().unwrap(), &Value::Num(2.0));
    assert_eq!(callframe.stack[callframe.sp - 2], Value::Num(2.0));
}

#[test]
fn test_exec_fn_capture_nested() {
    let code = "
[= foo /[] [begin 
               [= x 1]
               /[] [begin
                       [= y 1]
                       /[] [+ x y]]]]
[[[foo]]]
";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top().unwrap(), &Value::Num(2.0));
}

#[test]
fn test_exec_fn_call() {
    let code = "[/[] 42]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_call_with_params() {
    let code = "[/[x] x 42]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_call_mixed() {
    let code = "[= fn /[x] [begin
        [= y 1]
        [+ x y]]]
        [fn 1]
        [fn 1]";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(2.0));
    assert_eq!(callframe.stack[callframe.sp - 2], Value::Num(2.0));
}

#[test]
fn test_exec_fn_scope_lift() {
    let code = "
[= fn /[] x]
[= x 42]
[fn]
";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_scope_lift_error() {
    let code = "
[= fn /[] x]
[/[] [begin 
    [= x 42]
    [fn]]]
";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "unresolved upvalue: x".to_string(),
            Inst::CALL,
            17,
        ))
    )
}

#[test]
fn test_exec_fn_recursive() {
    let code = "
[= foo /[x] [if [> x 0] 42 [foo 1]]]
[foo 0]
";
    let ast = parse(code, &mut Position::default()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}
