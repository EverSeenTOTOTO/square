use crate::{
    code_frame::{code_frame, Position},
    parse::Node,
};

use alloc::boxed::Box;
use alloc::string::String;
use core::fmt;

#[derive(Debug, PartialEq)]
pub enum SquareError<'a> {
    UnexpectedToken(&'a str, String, Position),
    SyntaxError(&'a str, String, Position, Option<Position>),
    EmitError(&'a str, String, Box<Node>),
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
            SquareError::EmitError(source, msg, node) => {
                let range = node.range();
                let frame = code_frame(source, &range.0, &range.1);
                write!(f, "Emit error at {:?}, {}:\n{}", range.0, msg, frame)
            }
        }
    }
}
