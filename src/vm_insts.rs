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

    // 超指令（peephole 融合，见 emit::fuse_superinsts）
    CMP_JNE(u8, i32), // 比较 + 条件跳转：op 用 Inst::id 的 EQ..GE，为假跳转，免中间 Bool
    LOAD2_LOCAL(u16, u16), // 两次槽位读取合一次派发/借用
    LOADP_LOCAL(u16, Value), // 槽位读取 + 立即数入栈合一次派发/借用
    BINOP_IMM(u8, Value), // 立即数为右操作数的二元运算（op 用 Inst::id 的 ADD..REM）
    LOADC_JNE(u16, u8, Value, i32), // 槽位与立即数比较、为假跳转（整条循环条件一条指令）
    LOADC_JNZ(u16, u8, Value, i32), // 同上、为真跳转（回边穿线专用，极性相反免取反比较）
    LOAD_ARITH_IMM(u16, u8, Value), // 槽位读 + 立即数运算入栈
    LOAD2_ARITH(u16, u16, u8), // 两槽位读 + 运算入栈
    LOADGET_LOCAL(u16, String), // 槽位读 + 属性访问（o.y 一条指令）
    LOADP_UP(u16, Value), // upvalue 读 + 立即数入栈
    LOADU_ARITH(u16, u8, Value), // upvalue 读 + 立即数运算入栈
}

/// 指令名表（与 [`Inst::id`] 对齐），剖析输出用
pub const NAMES: [&str; 47] = [
    "PUSH", "POP", "ADD", "SUB", "MUL", "DIV", "REM", "BITAND", "BITOR", "BITXOR", "BITNOT",
    "EQ", "NE", "LT", "LE", "GT", "GE", "SHL", "SHR", "JMP", "JNE", "LOAD_LOCAL", "LOAD_UP",
    "LOAD_GLOBAL", "STORE_LOCAL", "STORE_UP", "STORE_GLOBAL", "CALL", "RET", "PUSH_CLOSURE",
    "PACK", "PEEK", "GET", "SET", "DELIMITER", "NAMES", "CMP_JNE", "LOAD2_LOCAL", "LOADP_LOCAL", "BINOP_IMM", "LOADC_JNE", "LOAD_ARITH_IMM", "LOAD2_ARITH", "LOADGET_LOCAL", "LOADP_UP", "LOADU_ARITH", "LOADC_JNZ",
];

impl Inst {
    #[inline]
    pub fn name_of(id: usize) -> &'static str {
        NAMES[id]
    }

    /// 稳定序号（供剖析表数组索引），新增指令时顺延
    pub fn id(&self) -> usize {
        match self {
            Inst::PUSH(_) => 0,
            Inst::POP => 1,
            Inst::ADD => 2,
            Inst::SUB => 3,
            Inst::MUL => 4,
            Inst::DIV => 5,
            Inst::REM => 6,
            Inst::BITAND => 7,
            Inst::BITOR => 8,
            Inst::BITXOR => 9,
            Inst::BITNOT => 10,
            Inst::EQ => 11,
            Inst::NE => 12,
            Inst::LT => 13,
            Inst::LE => 14,
            Inst::GT => 15,
            Inst::GE => 16,
            Inst::SHL => 17,
            Inst::SHR => 18,
            Inst::JMP(_) => 19,
            Inst::JNE(_) => 20,
            Inst::LOAD_LOCAL(_) => 21,
            Inst::LOAD_UP(_) => 22,
            Inst::LOAD_GLOBAL(_) => 23,
            Inst::STORE_LOCAL(_) => 24,
            Inst::STORE_UP(_) => 25,
            Inst::STORE_GLOBAL(_) => 26,
            Inst::CALL(_) => 27,
            Inst::RET => 28,
            Inst::PUSH_CLOSURE(_) => 29,
            Inst::PACK(_) => 30,
            Inst::PEEK(..) => 31,
            Inst::GET(_) => 32,
            Inst::SET(_) => 33,
            Inst::DELIMITER(_) => 34,
            Inst::NAMES(_) => 35,
            Inst::CMP_JNE(..) => 36,
            Inst::LOAD2_LOCAL(..) => 37,
            Inst::LOADP_LOCAL(..) => 38,
            Inst::BINOP_IMM(..) => 39,
            Inst::LOADC_JNE(..) => 40,
            Inst::LOADC_JNZ(..) => 46,
            Inst::LOAD_ARITH_IMM(..) => 41,
            Inst::LOAD2_ARITH(..) => 42,
            Inst::LOADGET_LOCAL(..) => 43,
            Inst::LOADP_UP(..) => 44,
            Inst::LOADU_ARITH(..) => 45,
        }
    }

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
            Inst::CMP_JNE(..) => "CMP_JNE",
            Inst::LOAD2_LOCAL(..) => "LOAD2_LOCAL",
            Inst::LOADP_LOCAL(..) => "LOADP_LOCAL",
            Inst::BINOP_IMM(..) => "BINOP_IMM",
            Inst::LOADC_JNE(..) => "LOADC_JNE",
            Inst::LOADC_JNZ(..) => "LOADC_JNZ",
            Inst::LOAD_ARITH_IMM(..) => "LOAD_ARITH_IMM",
            Inst::LOAD2_ARITH(..) => "LOAD2_ARITH",
            Inst::LOADGET_LOCAL(..) => "LOADGET_LOCAL",
            Inst::LOADP_UP(..) => "LOADP_UP",
            Inst::LOADU_ARITH(..) => "LOADU_ARITH",
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
            Inst::CMP_JNE(op, off) => write!(f, "CMP_JNE {}, {}", NAMES[*op as usize], off),
            Inst::LOAD2_LOCAL(a, b) => write!(f, "LOAD2_LOCAL {}, {}", a, b),
            Inst::LOADP_LOCAL(a, v) => write!(f, "LOADP_LOCAL {}, {}", a, v),
            Inst::BINOP_IMM(op, v) => write!(f, "BINOP_IMM {}, {}", NAMES[*op as usize], v),
            Inst::LOADC_JNE(a, op, v, off) => {
                write!(f, "LOADC_JNE {}, {} {}, {}", a, NAMES[*op as usize], v, off)
            }
            Inst::LOADC_JNZ(a, op, v, off) => {
                write!(f, "LOADC_JNZ {}, {} {}, {}", a, NAMES[*op as usize], v, off)
            }
            Inst::LOAD_ARITH_IMM(a, op, v) => {
                write!(f, "LOAD_ARITH_IMM {}, {} {}", a, NAMES[*op as usize], v)
            }
            Inst::LOAD2_ARITH(a, b, op) => {
                write!(f, "LOAD2_ARITH {}, {} {}", a, b, NAMES[*op as usize])
            }
            Inst::LOADGET_LOCAL(a, key) => write!(f, "LOADGET_LOCAL {}, .{}", a, key),
            Inst::LOADP_UP(u, v) => write!(f, "LOADP_UP {}, {}", u, v),
            Inst::LOADU_ARITH(u, op, v) => {
                write!(f, "LOADU_ARITH {}, {} {}", u, NAMES[*op as usize], v)
            }
        }
    }
}
