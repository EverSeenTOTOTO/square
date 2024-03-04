use alloc::{format, rc::Rc, string::String, string::ToString, vec, vec::Vec};
use core::{cell::RefCell, fmt};

use hashbrown::HashMap;

use crate::{
    builtin::Builtin,
    errors::SquareError,
    vm_insts::Inst,
    vm_value::{CalcResult, Function, Value},
};

#[cfg(test)]
use crate::code_frame::Position;
#[cfg(test)]
use crate::emit::{emit, EmitContext};
#[cfg(test)]
use crate::parse::parse;
#[cfg(test)]
use std::time::Instant;

pub type ExecResult = Result<(), SquareError>;

type OpFn = dyn Fn(&Value, &Value) -> CalcResult;

impl Inst {
    fn exec(&self, vm: &mut VM, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        match self {
            Inst::PUSH(value) => Ok(vm.current_frame().push(value.clone())),
            Inst::POP => Ok(vm.current_frame().pop()),

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
                let result = (!frame.top().unwrap()).map_err(|e| match e {
                    SquareError::RuntimeError(msg) => {
                        SquareError::InstructionError(msg, self.clone(), *pc)
                    }
                    _ => e,
                });

                frame.stack[frame.sp - 1] = result?;
                Ok(())
            }

            Inst::JMP(value) => self.jump(insts, pc, *value),
            Inst::JNE(value) => {
                let frame = vm.current_frame();

                if !frame.top().unwrap().as_bool() {
                    frame.pop();
                    self.jump(insts, pc, *value)
                } else {
                    Ok(frame.pop())
                }
            }

            Inst::STORE(name) => {
                let frame = vm.current_frame();

                if let Some(val) = frame.top() {
                    let cloned = val.clone();
                    frame.pop();
                    Ok(frame.assign_local(name, cloned))
                } else {
                    Err(SquareError::InstructionError(
                        "bad store, operand stack empty".to_string(),
                        self.clone(),
                        *pc,
                    ))
                }
            }
            Inst::LOAD(name) => {
                if let Some(value) = vm.buildin.resolve_builtin(name) {
                    let cloned = value.clone();
                    return Ok(vm.current_frame().push(cloned));
                }

                let frame = vm.current_frame();
                if let Some(value) = frame.resolve_local(name) {
                    let cloned = value.clone();
                    Ok(frame.push(cloned))
                } else {
                    Err(SquareError::InstructionError(
                        format!("undefined variable: {}", name),
                        self.clone(),
                        *pc,
                    ))
                }
            }

            Inst::CALL => {
                // call closure
                let frame = vm.current_frame();

                let params = frame.stack[frame.sp - 1].clone();
                let func = frame.stack[frame.sp - 2].as_fn();

                frame.sp -= 2;

                if let Some(val) = func {
                    let is_tail_call = *pc + 1 < insts.len() && insts[*pc + 1] == Inst::RET;
                    self.call(vm, val, params, is_tail_call, pc)
                } else {
                    Err(SquareError::InstructionError(
                        format!(
                            "bad call, cannot call with {}",
                            frame.top().unwrap_or(&Value::Nil).clone()
                        ),
                        self.clone(),
                        *pc,
                    ))
                }
            }
            Inst::RET => {
                let frame = vm.current_frame();

                // jump back
                *pc = frame.ra;

                let top = frame.top().unwrap_or(&Value::Nil).clone();

                vm.pop_frame();

                // always return the top value
                Ok(vm.current_frame().push(top))
            }
            Inst::PUSH_CLOSURE(meta) => {
                if let Function::ClosureMeta(offset, captures) = meta {
                    // create closure
                    let mut upvalues = HashMap::new();
                    let ip = (*pc as i32) + offset;
                    let frame = vm.current_frame();

                    let varnames = captures.iter().cloned().collect::<Vec<_>>();
                    // upgrade value to captured
                    for name in varnames {
                        if let Some(value) = frame.resolve_local(&name) {
                            let upvalue = value.upgrade();

                            upvalues.insert(name.clone(), upvalue.clone());
                            frame.insert_local(&name, upvalue);
                        } else {
                            // else undefined yet, if later be defined in same scope,
                            // the value will be assigned
                            let upvalue = Value::UpValue(Rc::new(RefCell::new(Value::Nil)));

                            upvalues.insert(name.clone(), upvalue.clone());
                            frame.insert_local(&name, upvalue);
                        }
                    }

                    return Ok(frame.push(Value::Function(Rc::new(RefCell::new(
                        Function::Closure(ip as usize, upvalues),
                    )))));
                }

                unreachable!()
            }

            Inst::PACK(len) => {
                let frame = vm.current_frame();
                let result = frame.stack[frame.sp - len..frame.sp].to_vec().clone();

                frame.sp -= *len;

                Ok(frame.push(Value::Vec(Rc::new(RefCell::new(result)))))
            }
            Inst::PEEK(pair) => match pair {
                (Value::Num(offset), Value::Num(index)) => {
                    self.peek_vec(vm, *offset as usize, *index as i32, pc)
                }
                (Value::Str(key), _) => self.peek_obj(vm, key, pc),
                _ => Err(SquareError::InstructionError(
                    "bad peek, offset and index must be number, key must be string".to_string(),
                    self.clone(),
                    *pc,
                )),
            },
            Inst::PATCH(key) => match key {
                Value::Num(index) => self.patch_vec(vm, *index as usize, pc),
                Value::Str(key) => self.patch_obj(vm, key, pc),
                _ => Err(SquareError::InstructionError(
                    format!("bad patch, key {} must be string or number", key),
                    self.clone(),
                    *pc,
                )),
            },
        }
    }

    fn binop(&self, vm: &mut VM, op_fn: &OpFn, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();
        let result =
            op_fn(&frame.stack[frame.sp - 2], &frame.stack[frame.sp - 1]).map_err(|e| match e {
                SquareError::RuntimeError(msg) => {
                    SquareError::InstructionError(msg, self.clone(), *pc)
                }
                _ => e,
            });

        frame.stack[frame.sp - 2] = result?;
        frame.sp -= 1;
        Ok(())
    }

    fn jump(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) -> ExecResult {
        let new_pc = *pc as i32 + offset;
        if new_pc < 0 || new_pc >= insts.len() as i32 {
            return Err(SquareError::InstructionError(
                "bad jump".to_string(),
                self.clone(),
                *pc,
            ));
        }

        *pc = new_pc as usize;
        Ok(())
    }

    fn call(
        &self,
        vm: &mut VM,
        closure: Rc<RefCell<Function>>,
        params: Value,
        is_tail_call: bool,
        pc: &mut usize,
    ) -> ExecResult {
        match *closure.borrow() {
            Function::ClosureMeta(..) => unreachable!(),
            Function::Closure(ip, ref upvalues) => {
                if is_tail_call {
                    let frame = vm.current_frame();
                    // always push params as first operand
                    frame.stack[0] = params;
                    frame.sp = 1;
                    frame.clear_locals();
                    // fill up captures
                    frame.extend_locals(upvalues.clone());
                } else {
                    let mut frame = CallFrame::new();
                    frame.ra = *pc;
                    // always push params as first operand
                    frame.stack[0] = params;
                    frame.sp = 1;
                    // fill up captures
                    frame.extend_locals(upvalues.clone());

                    vm.push_frame(frame);
                }

                // jump to function
                *pc = ip;
                Ok(())
            }
            Function::Syscall(index) => {
                let syscall = vm.buildin.get_syscall(index);
                syscall(vm, params, pc)
            }
            Function::Contiuation(ra, ref context) => {
                if let Value::Vec(ref top) = params {
                    *pc = ra;
                    vm.restore_context(context.clone());
                    Ok(vm
                        .current_frame()
                        .push(top.borrow().get(0).unwrap_or(&Value::Nil).clone()))
                } else {
                    Err(SquareError::RuntimeError(format!(
                        "cc only accept one parameter, got {}",
                        params
                    )))
                }
            }
        }
    }

    fn peek_vec(&self, vm: &mut VM, offset: usize, i: i32, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();
        let top = frame.stack[frame.sp - 1].as_vec();

        if let Some(val) = top {
            let pack = val.borrow().clone();

            if offset >= pack.len() {
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
            let index = if i > 0 { i } else { (i + len) % len } as usize + offset;

            if index < pack.len() {
                // frame.pop();
                return Ok(frame.push(pack[index].clone()));
            } else {
                return Err(SquareError::InstructionError(
                    format!(
                        "bad peek, index {} out of range, pack length is {}",
                        index,
                        pack.len()
                    ),
                    self.clone(),
                    *pc,
                ));
            }
        }

        Err(SquareError::InstructionError(
            format!(
                "bad peek, top value is not an vector, got {}",
                frame.top().unwrap_or(&Value::Nil).clone()
            ),
            self.clone(),
            *pc,
        ))
    }

    fn peek_obj(&self, vm: &mut VM, key: &str, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();
        let top = frame.stack[frame.sp - 1].as_obj();

        if let Some(obj) = top {
            let cloned = obj.borrow().get(key).cloned().unwrap_or(Value::Nil);
            frame.pop();
            return Ok(frame.push(cloned));
        }

        Err(SquareError::InstructionError(
            format!(
                "bad peek, top value is not an object, got {}",
                frame.top().unwrap_or(&Value::Nil).clone()
            ),
            self.clone(),
            *pc,
        ))
    }

    fn patch_vec(&self, vm: &mut VM, index: usize, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();
        let value = frame.stack[frame.sp - 1].clone();
        let target = frame.stack[frame.sp - 2].as_vec();

        if let Some(val) = target {
            let mut pack = val.borrow_mut();
            if index < pack.len() {
                pack[index] = value;
                frame.pop();
                return Ok(());
            } else {
                return Err(SquareError::InstructionError(
                    format!(
                        "bad patch, index {} out of range, pack length is {}",
                        index,
                        pack.len()
                    ),
                    self.clone(),
                    *pc,
                ));
            }
        }

        Err(SquareError::InstructionError(
            format!("bad patch, top value is not a vector, got {}", value),
            self.clone(),
            *pc,
        ))
    }

    fn patch_obj(&self, vm: &mut VM, key: &String, pc: &mut usize) -> ExecResult {
        let frame = vm.current_frame();
        let value = frame.stack[frame.sp - 1].clone();
        let target = frame.stack[frame.sp - 2].as_obj();

        if let Some(obj) = target {
            obj.borrow_mut().insert(key.clone(), value);
            frame.pop();
            return Ok(());
        }

        Err(SquareError::InstructionError(
            format!(
                "bad patch, top value is not an object, got {}",
                frame.stack[frame.sp - 2].clone()
            ),
            self.clone(),
            *pc,
        ))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFrame {
    pub locals: HashMap<String, Value>,

    // operand stack
    pub stack: Vec<Value>,
    // fake stack pointer, avoid frequent operand stack push/pop
    pub sp: usize,

    pub ra: usize, // return address
}

impl fmt::Display for CallFrame {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Return Addr: {}", self.ra)?;
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
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            stack: vec![Value::Nil; 8],
            sp: 0,
            ra: 0,
        }
    }

    pub fn push(&mut self, value: Value) {
        if self.sp >= self.stack.len() {
            self.stack.resize(self.stack.len() * 2, Value::Nil);
        }

        self.stack[self.sp] = value;
        self.sp += 1;
    }

    #[inline]
    pub fn pop(&mut self) {
        self.sp -= 1;
    }

    #[inline]
    pub fn top(&self) -> Option<&Value> {
        self.stack.get(self.sp - 1)
    }

    #[inline]
    pub fn insert_local(&mut self, name: &str, value: Value) {
        self.locals.insert(name.to_string(), value);
    }

    #[inline]
    pub fn extend_locals(&mut self, upvalues: HashMap<String, Value>) {
        self.locals.extend(upvalues);
    }

    #[inline]
    pub fn resolve_local(&mut self, name: &str) -> Option<&Value> {
        self.locals.get(name)
    }

    #[inline]
    pub fn clear_locals(&mut self) {
        self.locals.clear();
    }

    pub fn assign_local(&mut self, name: &str, value: Value) {
        let new = if let Value::UpValue(upval) = value {
            upval.borrow().clone() // if ref appeared as right value, it should be cloned
        } else {
            value
        };

        if let Some(Value::UpValue(old)) = self.locals.get(name) {
            *old.borrow_mut() = new;
        } else {
            self.insert_local(name, new);
        }
    }
}

pub struct VM {
    pub call_frames: Vec<CallFrame>,
    buildin: Builtin,

    #[cfg(test)]
    inst_times: HashMap<&'static str, (u128, usize)>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            call_frames: vec![CallFrame::new()],
            buildin: Builtin::new(),

            #[cfg(test)]
            inst_times: HashMap::new(),
        }
    }

    #[inline]
    pub fn current_frame(&mut self) -> &mut CallFrame {
        self.call_frames.last_mut().unwrap()
    }

    #[inline]
    pub fn push_frame(&mut self, frame: CallFrame) {
        self.call_frames.push(frame)
    }

    #[inline]
    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        self.call_frames.pop()
    }

    #[inline]
    pub fn save_context(&self) -> Vec<CallFrame> {
        self.call_frames.clone()
    }

    #[inline]
    pub fn restore_context(&mut self, context: Vec<CallFrame>) {
        self.call_frames = context;
    }

    pub fn step(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        let inst = &insts[*pc];

        // #[cfg(test)]
        // println!("{}: {}", *pc, inst);

        #[cfg(test)]
        let start = Instant::now();

        inst.exec(self, insts, pc)?;

        #[cfg(test)]
        {
            let duration = start.elapsed().as_nanos();
            let entry = self.inst_times.entry(inst.name()).or_insert((0, 0));
            *entry = (entry.0 + duration, entry.1 + 1);
        }

        // #[cfg(test)]
        // println!(
        //     "-------- CallFrame {} --------\n{}",
        //     self.call_frames.len() - 1,
        //     self.current_frame()
        // );

        *pc += 1;
        Ok(())
    }

    pub fn run(&mut self, insts: &Vec<Inst>, pc: &mut usize) -> ExecResult {
        self.current_frame().ra = insts.len();

        #[cfg(test)]
        self.inst_times.clear();

        while *pc < insts.len() {
            self.step(insts, pc)?;
        }

        Ok(())
    }

    #[cfg(test)]
    pub fn print_times(&self) {
        let mut data: Vec<_> = self
            .inst_times
            .iter()
            .map(|(name, (total_time, count))| {
                (
                    name,
                    *total_time,
                    (*total_time as f64) / (*count as f64),
                    *count,
                )
            })
            .collect();

        println!("");
        // Sort and print by total time
        data.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        for (name, total_time, ..) in &data {
            println!("Total time for {}: {} ns", name, total_time);
        }

        println!("");
        // Sort and print by average time
        data.sort_by(|a, b| b.2.partial_cmp(&a.2).unwrap());
        for (name, _, average_time, _) in &data {
            println!("Average time for {}: {:.2} ns", name, average_time);
        }

        println!("");
        // Sort and print by count
        data.sort_by(|a, b| b.3.partial_cmp(&a.3).unwrap());
        for (name, .., count) in &data {
            println!("Count for {}: {} times", name, count);
        }
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
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load() {
    let code = "x";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().assign_local("x", Value::Num(42.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top(), Some(&Value::Num(42.0)));
}

#[test]
fn test_exec_load_undefined() {
    let code = "x";
    let ast = parse(code, &mut Position::new()).unwrap();
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
fn test_exec_define() {
    let code = "[let x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign() {
    let code = "[let x nil] [= x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_capture() {
    let code = "[let x nil] [begin [= x 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand() {
    let code = "[let [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand() {
    let code = "[let x nil] [= [x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_expand_capture() {
    let code = "[let x nil] [begin [= [x] [vec 42]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot() {
    let code = "[let [. x] [vec 1 42 3]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_dot_error() {
    let code = "[let [. x] [vec 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "bad peek, index 1 out of range, pack length is 1".to_string(),
            Inst::PEEK((Value::Num(0.0), Value::Num(1.0))),
            6,
        ))
    );
}

#[test]
fn test_exec_define_expand_greed() {
    let code = "[let [... x] [vec 1 2 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_define_expand_greed_error() {
    let code = "[let [... x] [vec]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "bad peek, offset 0 out of range, pack length is 0".to_string(),
            Inst::PEEK((Value::Num(0.0), Value::Num(-1.0))),
            3,
        ))
    );
}

#[test]
fn test_exec_define_expand_nested() {
    let code = "[let [. [x]] [vec 1 [vec 42] 3]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_op() {
    let code = "[- 1 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(-1.0))
}

#[test]
fn test_exec_op_assign() {
    let code = "[+= x 2]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().assign_local("x", Value::Num(1.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0));
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(3.0))
}

#[test]
fn test_exec_op_assign_dot() {
    let code = "[let x [obj 'y' [obj 'z' 0]]]
        [+= x.y.z 42]
        x.y.z";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_dot() {
    let code = "o.x.y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    let mut o = HashMap::new();
    let mut x = HashMap::new();
    let y = Value::Num(42.0);
    x.insert("y".to_string(), y);
    o.insert("x".to_string(), Value::Obj(Rc::new(RefCell::new(x))));
    vm.current_frame()
        .assign_local("o", Value::Obj(Rc::new(RefCell::new(o))));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_assign_dot() {
    let code = "[= o.x.y 42] o.x.y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    let mut o = HashMap::new();
    let mut x = HashMap::new();
    let y = Value::Num(24.0);
    x.insert("y".to_string(), y);
    o.insert("x".to_string(), Value::Obj(Rc::new(RefCell::new(x))));
    vm.current_frame()
        .assign_local("o", Value::Obj(Rc::new(RefCell::new(o))));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_obj() {
    let code = "
[let o [obj 
        'x' 42
        'inc' /[] [+= o.x 1]]]

[println o]

[o.inc]
[= o.o o]
[o.o.o.o.o.o.inc]

o.x
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    insts.iter().for_each(|inst| println!("{}", inst));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(44.0))
}

#[test]
fn test_exec_begin() {
    let code = "[begin 1 2 3]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(3.0))
}

#[test]
fn test_exec_scope() {
    let code = "
        [let x 1]
        [let y [begin [let x 2] x]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(1.0));
    assert_eq!(callframe.resolve_local("y").unwrap(), &Value::Num(2.0));
}

#[test]
fn test_exec_tail_call() {
    let code = "[begin [begin [begin 42]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();
    let mut pc = 0;
    let mut max_depth = vm.call_frames.len();

    vm.current_frame().ra = insts.len();

    while pc < insts.len() {
        vm.step(&insts, &mut pc).unwrap();
        if vm.call_frames.len() > max_depth {
            max_depth = vm.call_frames.len();
        }
    }

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
    assert_eq!(max_depth, 2);
}

#[test]
fn test_exec_if_true() {
    let code = "[if true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_if_false_nil() {
    let code = "[if false 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Nil)
}

#[test]
fn test_exec_if_false() {
    let code = "[if false 42 24]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(24.0))
}

#[test]
fn test_exec_while() {
    let code = "[while [< x 4] [begin [+= x 1]]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.current_frame().assign_local("x", Value::Num(0.0));
    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(4.0))
}

#[test]
fn test_exec_match() {
    let code = "[cond
        [false 24]
        [true 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_fn_call() {
    let code = "[/[] 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_call_with_params() {
    let code = "[/[x] x 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_overwrite_params() {
    let code = "
    [let x 24]
    [/[x] [begin
            [= x 42]
            x] x]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(24.0))
}

#[test]
fn test_exec_fn_capture_assign() {
    let code = "
    [let fn /[] [begin 
        [let x 1]
        /[] [+= x 1]]]
    [let f [fn]]
    [let g [fn]]
    [f]
    [g]";
    let ast = parse(code, &mut Position::new()).unwrap();
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
[let foo /[] [begin
               [let x 1]
               /[] [begin ; should capture x
                       [let y 1]
                       /[] [+ x y]]]]
[[[foo]]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();

    assert_eq!(callframe.top().unwrap(), &Value::Num(2.0));
}

#[test]
fn test_exec_fn_capture_scope_lift() {
    let code = "
[let fn /[] x]
[let x 42]
[fn]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_capture_error() {
    let code = "
[let fn /[] x]
[begin 
    [let x 42]
    [fn]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap(); // FIXME: should report undefined variable: x
    assert_eq!(vm.current_frame().top(), Some(&Value::Nil))
}

#[test]
fn test_exec_fn_capture_shadow() {
    let code = "
[let fn /[] [begin x [let x 24] x]]
[let x 42]
[fn]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    assert_eq!(
        vm.run(&insts, &mut 0),
        Err(SquareError::InstructionError(
            "undefined variable: x".to_string(),
            Inst::LOAD("x".to_string()),
            3,
        )),
    );
}

#[test]
fn test_exec_fn_capture_shadow2() {
    let code = "
    [let f [begin [let x 42] /[] x]]

    [let x [f]]
    [+= x 2]
    [let y [f]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("y").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_exec_fn_capture_lazy() {
    let code = "
[let foo /[f] /[] [f]]
[let bar [foo /[] x]]
[let x 42]
[bar]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();
    let callframe = vm.current_frame();

    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_fn_capture_self() {
    let code = "
[let foo /[x] [if [> x 0] 42 [foo 1]]]
[foo 0]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.top().unwrap(), &Value::Num(42.0));
}

#[test]
fn test_exec_builtin() {
    let code = "
[let p println]
[p [.. [.. 1 4] [vec 4 5 6]]]
[let t [typeof p]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(
        callframe.resolve_local("t").unwrap(),
        &Value::Str("fn".to_string())
    )
}

#[test]
fn test_callcc_flow() {
    let code = "
[let x [callcc /[cc] 42]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();

    let callframe = vm.current_frame();
    assert_eq!(callframe.resolve_local("x").unwrap(), &Value::Num(42.0))
}

#[test]
fn test_callcc_break() {
    let code = "
        [let x [.. 1 4]]
        [begin 
            [let y x]
            [= y 42]
            [println x]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();
}

#[test]
fn test_profile() {
    let code = "
[let fib /[n] 
  [if [<= n 2] 
    1
    [+ [fib [- n 1]] [fib [- n 2]]]]]

[println [fib 20]]
";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();
    let mut vm = VM::new();

    vm.run(&insts, &mut 0).unwrap();
    vm.print_times();
}
