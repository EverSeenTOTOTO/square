#[cfg(not(test))]
use alloc::{vec, vec::Vec};

use core::fmt;

use crate::vm_value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    PUSH(Value),
    POP,
    ADD, // +
    SUB, // -
    MUL, // *
    DIV, // /
    REM, // %
    AND, // &
    OR,  // |
    XOR, // ^
    NOT, // !
    EQ,  // ==
    NE,  // !=
    LT,  // <
    LE,  // <=
    GT,  // >
    GE,  // >=
    JMP(i32),
    JEQ(i32), // jump if eq
    CALL,
    RET,
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Inst::PUSH(value) => write!(f, "PUSH {}", value),
            Inst::POP => write!(f, "POP"),
            Inst::ADD => write!(f, "ADD"),
            Inst::SUB => write!(f, "SUB"),
            Inst::MUL => write!(f, "MUL"),
            Inst::DIV => write!(f, "DIV"),
            Inst::REM => write!(f, "REM"),
            Inst::AND => write!(f, "AND"),
            Inst::OR => write!(f, "OR"),
            Inst::XOR => write!(f, "XOR"),
            Inst::NOT => write!(f, "NOT"),
            Inst::EQ => write!(f, "EQ"),
            Inst::NE => write!(f, "NE"),
            Inst::LT => write!(f, "LT"),
            Inst::LE => write!(f, "LE"),
            Inst::GT => write!(f, "GT"),
            Inst::GE => write!(f, "GE"),
            Inst::JMP(value) => write!(f, "JMP {}", value),
            Inst::JEQ(value) => write!(f, "JEQ {}", value),
            Inst::CALL => write!(f, "CALL"),
            Inst::RET => write!(f, "RET"),
        }
    }
}

type OpFn = dyn Fn(&Value, &Value) -> Value;

impl Inst {
    fn exec(&self, vm: &mut VM, insts: &Vec<Inst>, pc: &mut usize) {
        match self {
            Inst::PUSH(value) => {
                if vm.sp >= vm.stack.len() {
                    todo!();
                }

                vm.stack[vm.sp] = value.clone();
                vm.sp = vm.sp + 1;
            }
            Inst::POP => {
                if vm.sp < 1 {
                    todo!();
                }

                vm.sp = vm.sp - 1;
            }
            Inst::NOT => {
                if vm.sp < 1 {
                    todo!();
                }

                vm.stack[vm.sp - 1] = !&vm.stack[vm.sp - 1];
            }

            Inst::ADD => self.binop(vm, &|a, b| a + b),
            Inst::SUB => self.binop(vm, &|a, b| a - b),
            Inst::MUL => self.binop(vm, &|a, b| a * b),
            Inst::DIV => self.binop(vm, &|a, b| a / b),
            Inst::REM => self.binop(vm, &|a, b| a % b),
            Inst::AND => self.binop(vm, &|a, b| a & b),
            Inst::OR => self.binop(vm, &|a, b| a | b),
            Inst::XOR => self.binop(vm, &|a, b| a ^ b),
            Inst::EQ => self.binop(vm, &|a, b| Value::Bool(a == b)),
            Inst::NE => self.binop(vm, &|a, b| Value::Bool(a != b)),
            Inst::LT => self.binop(vm, &|a, b| Value::Bool(a < b)),
            Inst::LE => self.binop(vm, &|a, b| Value::Bool(a <= b)),
            Inst::GT => self.binop(vm, &|a, b| Value::Bool(a > b)),
            Inst::GE => self.binop(vm, &|a, b| Value::Bool(a >= b)),

            Inst::JMP(value) => {
                self.jump(insts, pc, *value);
            }
            Inst::JEQ(value) => {
                if vm.sp < 1 {
                    todo!();
                }

                let top = &vm.stack[vm.sp - 1];
                vm.sp = vm.sp - 1;
                if top != &Value::Bool(true) {
                    return;
                }

                self.jump(insts, pc, *value);
            }
            _ => todo!(),
        }
    }

    fn binop(&self, vm: &mut VM, op_fn: &OpFn) {
        if vm.sp < 2 {
            todo!();
        }

        vm.stack[vm.sp - 2] = op_fn(&vm.stack[vm.sp - 2], &vm.stack[vm.sp - 1]);
        vm.sp = vm.sp - 1;
    }

    fn jump(&self, insts: &Vec<Inst>, pc: &mut usize, offset: i32) {
        let new_pc = *pc as i32 + offset;
        if new_pc < 0 || new_pc > insts.len() as i32 {
            todo!();
        }

        *pc = new_pc as usize;
    }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct VM {
    stack: Vec<Value>,
    // fake length, avoid frequent push/pop
    sp: usize,
}

impl Default for VM {
    fn default() -> Self {
        Self::new(1024) // default 4KB stack
    }
}

impl VM {
    pub fn new(stack_size: usize) -> Self {
        Self {
            stack: vec![Value::Int(0); stack_size], // item count, not byte length
            sp: 0,
        }
    }

    pub fn step(&mut self, insts: &Vec<Inst>, pc: &mut usize) {
        let inst = &insts[*pc];

        inst.exec(self, insts, pc);
        *pc += 1;
    }

    pub fn run(&mut self, insts: &Vec<Inst>, pc: &mut usize) {
        while *pc < insts.len() {
            self.step(insts, pc);
        }
    }

    pub fn top(&self) -> Option<&Value> {
        return self.stack.get(self.sp - 1);
    }
}

#[test]
fn test_step() {
    let mut vm = VM::default();
    let insts = vec![
        Inst::PUSH(Value::Int(42)),
        Inst::PUSH(Value::Float(24.0)),
        Inst::POP,
        Inst::POP,
    ];
    let mut pc = 0;

    vm.step(&insts, &mut pc);
    vm.step(&insts, &mut pc);

    assert_eq!(vm.sp, 2);

    vm.step(&insts, &mut pc);
    vm.step(&insts, &mut pc);

    assert_eq!(vm.sp, 0);
}

#[test]
fn test_run() {
    let mut vm = VM::default();
    let insts = vec![
        Inst::PUSH(Value::Int(42)),
        Inst::PUSH(Value::Int(24)),
        Inst::ADD,
        Inst::PUSH(Value::Int(2)),
        Inst::MUL,
    ];
    let mut pc = 0;

    vm.run(&insts, &mut pc);

    assert_eq!(vm.top(), Some(&Value::Int(132)));
}

#[test]
fn test_jmp() {
    let mut vm = VM::default();
    let insts = vec![
        Inst::PUSH(Value::Int(42)),
        Inst::PUSH(Value::Int(24)),
        Inst::ADD,
        Inst::JMP(2),
        Inst::PUSH(Value::Int(2)),
        Inst::MUL,
        Inst::PUSH(Value::Int(-2)),
        Inst::DIV,
    ];
    let mut pc = 0;

    vm.run(&insts, &mut pc);

    assert_eq!(vm.top(), Some(&Value::Int(-33)));
}

#[test]
fn test_jeq() {
    let mut vm = VM::default();
    let insts = vec![
        Inst::PUSH(Value::Int(42)),
        Inst::PUSH(Value::Int(24)),
        Inst::ADD,
        Inst::PUSH(Value::Bool(false)),
        Inst::JEQ(6),
        Inst::PUSH(Value::Bool(true)),
        Inst::JEQ(2),
        Inst::PUSH(Value::Int(2)),
        Inst::MUL,
        Inst::PUSH(Value::Int(-2)),
        Inst::DIV,
    ];
    let mut pc = 0;

    vm.run(&insts, &mut pc);

    assert_eq!(vm.top(), Some(&Value::Int(-33)));
}
