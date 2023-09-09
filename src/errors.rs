use crate::code_frame::{code_frame, Position};
#[cfg(not(test))]
use alloc::string::String;
use core::fmt;
#[cfg(test)]
use std::string::String;

#[derive(Debug, PartialEq)]
pub enum ParseError<'a> {
    UnexpectedToken(&'a str, String, Position),
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken(source, msg, pos) => {
                let code_frame_str = code_frame(source, pos, pos);
                write!(
                    f,
                    "Unexpected token at {:?}, {}:\n{}",
                    pos, msg, code_frame_str
                )
            }
        }
    }
}
