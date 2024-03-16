use crate::vm_value::{Function, Value};

use alloc::string::String;
use core::fmt;

#[allow(non_camel_case_types)]
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
    PUSH_CLOSURE(Function), // create a closure and push on top of the operand stack

    PACK(usize),      // pack n elements on top of the operand stack
    PEEK(usize, i32), // (offset, index), peek an element within the top pack of the operand stack

    GET(String), // get a field from the top object on the operand stack
    SET(String), // set a field

    DELIMITER(usize), // delimiter for top level expressions
}

impl Inst {
    pub fn name(&self) -> &'static str {
        match self {
            Inst::PUSH(_) => "PUSH",
            Inst::POP => "POP",
            Inst::ADD => "ADD",
            Inst::SUB => "SUB",
            Inst::MUL => "MUL",
            Inst::DIV => "DIV",
            Inst::REM => "REM",
            Inst::BITAND => "AND",
            Inst::BITOR => "OR",
            Inst::BITXOR => "XOR",
            Inst::BITNOT => "NOT",
            Inst::EQ => "EQ",
            Inst::NE => "NE",
            Inst::LT => "LT",
            Inst::LE => "LE",
            Inst::GT => "GT",
            Inst::GE => "GE",
            Inst::SHL => "SHL",
            Inst::SHR => "SHR",
            Inst::JMP(_) => "JMP",
            Inst::JNE(_) => "JNE",
            Inst::STORE(_) => "STORE",
            Inst::LOAD(_) => "LOAD",
            Inst::CALL => "CALL",
            Inst::RET => "RET",
            Inst::PUSH_CLOSURE(_) => "PUSH_CLOSURE",
            Inst::PACK(_) => "PACK",
            Inst::PEEK(..) => "PEEK",

            Inst::SET(_) => "SET",
            Inst::GET(_) => "GET",

            Inst::DELIMITER(_) => "DELIMITER",
        }
    }
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
            Inst::PEEK(offset, index) => write!(f, "PEEK {}, {}", offset, index),

            Inst::GET(key) => write!(f, "GET {}", key),
            Inst::SET(key) => write!(f, "SET {}", key),

            Inst::DELIMITER(mindex) => write!(f, "DELIMITER {}", mindex),
        }
    }
}
