use crate::vm_value::{Closure, Value};

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    PUSH(Value),
    POP,

    ADD,    // +
    SUB,    // -
    MUL,    // *
    DIV,    // /
    REM,    // %
    BITAND, // &
    BITOR,  // |
    BITXOR, // ^
    BITNOT, // ~
    EQ,     // ==
    NE,     // !=
    LT,     // <
    LE,     // <=
    GT,     // >
    GE,     // >=
    SHL,    // <<
    SHR,    // >>

    JMP(i32),
    JNE(i32), // jump if false

    STORE(String),
    LOAD(String),

    CALL,
    RET,
    PUSH_CLOSURE(Closure), // closure meta at compile time, which contains function address and capture names

    PACK(usize), // pack n elements on top of the operand stack

    PEEK(usize, i32), // (offset, index), peek an element within the top pack of the operand stack.
    // 'offset' is either 0 or the number of elements consumed once greedy placehoder already appeared.
    COMMENT(String),
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
            Inst::BITAND => write!(f, "AND"),
            Inst::BITOR => write!(f, "OR"),
            Inst::BITXOR => write!(f, "XOR"),
            Inst::BITNOT => write!(f, "NOT"),
            Inst::EQ => write!(f, "EQ"),
            Inst::NE => write!(f, "NE"),
            Inst::LT => write!(f, "LT"),
            Inst::LE => write!(f, "LE"),
            Inst::GT => write!(f, "GT"),
            Inst::GE => write!(f, "GE"),
            Inst::SHL => write!(f, "SHL"),
            Inst::SHR => write!(f, "SHR"),
            Inst::JMP(value) => write!(f, "JMP {}", value),
            Inst::JNE(value) => write!(f, "JNE {}", value),
            Inst::STORE(name) => write!(f, "STORE {}", name),
            Inst::LOAD(name) => write!(f, "LOAD {}", name),
            Inst::CALL => write!(f, "CALL"),
            Inst::RET => write!(f, "RET"),
            Inst::PUSH_CLOSURE(closure) => write!(
                // closure meta
                f,
                "PUSH_CLOSURE {}",
                closure,
            ),
            Inst::PACK(len) => write!(f, "PACK {}", len),
            Inst::PEEK(offset, index) => write!(f, "PEEK {} {}", offset, index),
            Inst::COMMENT(comment) => write!(f, "\n# {}\n", comment),
        }
    }
}
