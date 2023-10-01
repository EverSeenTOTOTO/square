use crate::code_frame::{code_frame, Position};
#[cfg(not(test))]
use alloc::string::String;
use core::fmt;
#[cfg(test)]
use std::string::String;

#[derive(Debug, PartialEq)]
pub enum SquareError<'a> {
    UnexpectedToken(&'a str, String, Position),
    SyntaxError(&'a str, String, Position, Option<Position>),
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
                write!(f, "Syntax error at {:?}, {}: \n {}", start, msg, frame)
            }
        }
    }
}
