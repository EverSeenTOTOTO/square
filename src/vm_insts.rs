use crate::vm_value::{Function, Value};

use alloc::rc::Rc;
use alloc::string::String;
use alloc::vec::Vec;
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

    LOAD_LOCAL(u16), // frame slot i
    LOAD_UP(u16),    // closure upvalue i
    LOAD_GLOBAL(String), // 全局表/builtin（编译期无法静态归属的名字）
    STORE_LOCAL(u16),
    STORE_UP(u16),
    STORE_GLOBAL(String), // `= x v` 定义未声明名字 → 全局表

    CALL(u16), // callee 之下 n 个参数
    RET,
    PUSH_CLOSURE(Function), // create a closure and push on top of the operand stack

    PACK(usize),      // pack n elements on top of the operand stack
    PEEK(usize, i32), // (offset, index), peek an element within the top pack of the operand stack

    GET(String), // get a field from the top object on the operand stack
    SET(String), // set a field, stack: [target, value] -> [target]

    DELIMITER(usize), // delimiter for top level expressions

    NAMES(Rc<Vec<String>>), // 程序开头：登记根帧槽位名表（快照/调试用）
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
            Inst::LOAD_LOCAL(_) => "LOAD_LOCAL",
            Inst::LOAD_UP(_) => "LOAD_UP",
            Inst::LOAD_GLOBAL(_) => "LOAD_GLOBAL",
            Inst::STORE_LOCAL(_) => "STORE_LOCAL",
            Inst::STORE_UP(_) => "STORE_UP",
            Inst::STORE_GLOBAL(_) => "STORE_GLOBAL",
            Inst::CALL(_) => "CALL",
            Inst::RET => "RET",
            Inst::PUSH_CLOSURE(_) => "PUSH_CLOSURE",
            Inst::PACK(_) => "PACK",
            Inst::PEEK(..) => "PEEK",
            Inst::GET(_) => "GET",
            Inst::SET(_) => "SET",

            Inst::DELIMITER(_) => "DELIMITER",
            Inst::NAMES(_) => "NAMES",
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

            Inst::LOAD_LOCAL(i) => write!(f, "LOAD_LOCAL {}", i),
            Inst::LOAD_UP(i) => write!(f, "LOAD_UP {}", i),
            Inst::LOAD_GLOBAL(name) => write!(f, "LOAD_GLOBAL {}", name),
            Inst::STORE_LOCAL(i) => write!(f, "STORE_LOCAL {}", i),
            Inst::STORE_UP(i) => write!(f, "STORE_UP {}", i),
            Inst::STORE_GLOBAL(name) => write!(f, "STORE_GLOBAL {}", name),

            Inst::CALL(n) => write!(f, "CALL {}", n),
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
            Inst::NAMES(names) => write!(f, "NAMES {}", names.len()),
        }
    }
}
