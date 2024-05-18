use crate::{
    code_frame::{code_frame, Position},
    vm_insts::Inst,
};

use alloc::{string::String};
use core::fmt;

#[derive(Debug, PartialEq)]
pub enum SquareError {
    UnexpectedToken(String, String, Position),
    SyntaxError(String, String, Position, Option<Position>),
    InstructionError(String, Inst, usize),
    RuntimeError(String),
}

impl fmt::Display for SquareError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SquareError::UnexpectedToken(source, msg, pos) => {
                let frame = code_frame(source, pos, pos);
                write!(f, "Unexpected token at {:?}, {}:\n{}", pos, msg, frame)
            }
            SquareError::SyntaxError(source, msg, start, end) => {
                let frame = code_frame(source, start, end.as_ref().unwrap_or(start));
                write!(f, "Syntax error at {:?}, {}:\n{}", start, msg, frame)
            }
            SquareError::InstructionError(msg, inst, pc) => {
                write!(f, "Instruction error, {}:\n{:>4}: {}\n", msg, pc, inst)
            }
            SquareError::RuntimeError(msg) => {
                write!(f, "Runtime error: {}", msg)
            }
        }
    }
}
