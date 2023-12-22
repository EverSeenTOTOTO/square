use crate::{
    code_frame::{code_frame, Position},
    emit::Inst,
};

use alloc::{format, string::String};
use core::fmt;

#[derive(Debug, PartialEq)]
pub enum SquareError<'a> {
    UnexpectedToken(&'a str, String, Position),
    SyntaxError(&'a str, String, Position, Option<Position>),
    RuntimeError(String, Inst, usize),
}

impl<'a> fmt::Display for SquareError<'a> {
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
        }
    }
}
