use crate::{
    code_frame::{code_frame, Position, SourceMap},
    vm_insts::Inst,
};

use alloc::{format, string::String};
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

impl SquareError {
    /// 像 `Display` 一样格式化，但若提供了 source map，把 `InstructionError` 的 pc
    /// 反查为源码位置（`line:column`）。无映射时退化为 `pc N`。
    pub fn enrich(&self, sm: Option<&SourceMap>) -> String {
        match self {
            SquareError::InstructionError(msg, _inst, pc) => match sm.and_then(|m| m.lookup(*pc)) {
                Some(pos) => {
                    format!("Instruction error at line {}:{}: {}", pos.line, pos.column, msg)
                }
                None => format!("Instruction error at pc {}: {}", pc, msg),
            },
            other => format!("{}", other),
        }
    }
}
