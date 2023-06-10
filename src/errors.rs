use std::fmt;

use crate::code_frame::code_frame;
use crate::scan::Position;

#[derive(Debug)]
pub enum ParseError<'a> {
    InvalidToken(&'a str, Position),
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::InvalidToken(source, pos) => {
                let code_frame_str = code_frame(source, pos.clone(), pos.clone());
                write!(
                    f,
                    "Invalid token at position {:?}:\n{}",
                    pos, code_frame_str
                )
            }
        }
    }
}

impl<'a> std::error::Error for ParseError<'a> {}
