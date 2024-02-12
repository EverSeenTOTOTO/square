use crate::{
    code_frame::{code_frame, Position},
    vm_insts::Inst,
};

use alloc::{format, string::String};
use core::fmt;

#[derive(Debug, PartialEq)]
pub enum SquareError {
    UnexpectedToken(String, String, Position),
    SyntaxError(String, String, Position, Option<Position>),
    RuntimeError(String, Inst, usize),
    TypeError(String),
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
            SquareError::RuntimeError(msg, inst, pc) => {
                write!(
                    f,
                    "Runtime error, {}:\n{}",
                    msg,
                    format!("{:>4}: {}\n", pc, inst)
                )
            }
            SquareError::TypeError(msg) => {
                write!(f, "{}", msg)
            }
        }
    }
}
