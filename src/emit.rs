use crate::{
    errors::SquareError,
    parse::Node,
    scan::{Token, TokenName},
    vm_value::Value,
};

use alloc::{boxed::Box, format, string::String, vec, vec::Vec};
use core::fmt;

type EmitResult<'a> = Result<Box<Vec<Inst>>, SquareError<'a>>;

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
    NOT,    // !
    EQ,     // ==
    NE,     // !=
    LT,     // <
    LE,     // <=
    GT,     // >
    GE,     // >=
    JMP(i32),
    JEQ(i32), // jump if eq
    STORE(String),
    LOAD(String),
    CALL,
    RET,
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
            Inst::NOT => write!(f, "NOT"),
            Inst::EQ => write!(f, "EQ"),
            Inst::NE => write!(f, "NE"),
            Inst::LT => write!(f, "LT"),
            Inst::LE => write!(f, "LE"),
            Inst::GT => write!(f, "GT"),
            Inst::GE => write!(f, "GE"),
            Inst::JMP(value) => write!(f, "JMP {}", value),
            Inst::JEQ(value) => write!(f, "JEQ {}", value),
            Inst::STORE(name) => write!(f, "STORE {}", name),
            Inst::LOAD(name) => write!(f, "LOAD {}", name),
            Inst::CALL => write!(f, "CALL"),
            Inst::RET => write!(f, "RET"),
        }
    }
}

fn emit_token<'a>(input: &'a str, token: Token) -> EmitResult<'a> {
    match token.name {
        TokenName::NUM => {
            let val = Value::Int(
                token.source.parse::<i32>().unwrap(), // TODO: numeric types
            );
            return Ok(Box::new(vec![Inst::PUSH(val)]));
        }
        TokenName::STR => {
            let val = Value::Str(token.source.clone());
            return Ok(Box::new(vec![Inst::PUSH(val)]));
        }
        TokenName::ID => {
            return Ok(Box::new(vec![Inst::LOAD(token.source.clone())]));
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input,
                format!(
                    "failed to emit_token, expect number, string or identifier name, got {}",
                    token.name
                ),
                token.pos,
                None,
            ));
        }
    }
}

fn emit_assign<'a>(
    input: &'a str,
    target: Box<Node>,
    _properties: Vec<Box<Node>>,
    expression: Box<Node>,
) -> EmitResult<'a> {
    let mut insts = Box::new(vec![]);
    insts.extend(*emit_expr(input, expression)?);

    match *target {
        Node::Token(id) => {
            if id.name == TokenName::ID {
                insts.push(Inst::STORE(id.source));
            } else {
                return Err(SquareError::SyntaxError(
                    input,
                    format!("failed to emit_assign, cannot assign to {}, expect identifier", id.name),
                    id.pos,
                    None,
                ));
            }
        }
        _ => todo!(),
    }

    return Ok(insts);
}

fn emit_op<'a>(input: &'a str, op: Token, expressions: Vec<Box<Node>>) -> EmitResult<'a> {
    let emit_op_inst = |inst: Inst| -> EmitResult<'a> {
        let mut result = emit(input, expressions)?;
        result.push(inst);
        return Ok(Box::new(result));
    };
    let emit_op_assign_inst = |inst: Inst| -> EmitResult<'a> {
        todo!();
    };

    if op.name == TokenName::OP {
        return match op.source.as_str() {
            "+" => emit_op_inst(Inst::ADD),
            "-" => emit_op_inst(Inst::SUB),
            "*" => emit_op_inst(Inst::MUL),
            "/" => emit_op_inst(Inst::DIV),
            "%" => emit_op_inst(Inst::REM),
            "&" => emit_op_inst(Inst::BITAND),
            "|" => emit_op_inst(Inst::BITOR),
            "^" => emit_op_inst(Inst::BITXOR),
            "==" => emit_op_inst(Inst::EQ),
            "!=" => emit_op_inst(Inst::NE),
            "<" => emit_op_inst(Inst::LT),
            "<=" => emit_op_inst(Inst::LE),
            ">" => emit_op_inst(Inst::GT),
            ">=" => emit_op_inst(Inst::GE),
            "+=" => emit_op_assign_inst(Inst::ADD),
            "-=" => emit_op_assign_inst(Inst::SUB),
            "*=" => emit_op_assign_inst(Inst::MUL),
            "/=" => emit_op_assign_inst(Inst::DIV),
            "%=" => emit_op_assign_inst(Inst::REM),
            "&=" => emit_op_assign_inst(Inst::BITAND),
            "|=" => emit_op_assign_inst(Inst::BITOR),
            "^=" => emit_op_assign_inst(Inst::BITXOR),
            _ => todo!(),
        };
    }

    return Err(SquareError::SyntaxError(
        input,
        format!("failed to emit_op, expect operator, got {}", op.name),
        op.pos,
        None,
    ));
}

fn emit_call<'a>(input: &'a str, expressions: Vec<Box<Node>>) -> EmitResult<'a> {
    return Ok(Box::new(emit(input, expressions)?));
}

fn emit_expr<'a>(input: &'a str, node: Box<Node>) -> EmitResult<'a> {
    match *node {
        Node::Token(token) => emit_token(input, token),
        Node::Assign(_, target, properties, expression) => {
            emit_assign(input, target, properties, expression)
        }
        Node::Op(op, expressions) => emit_op(input, op, expressions),
        Node::Call(_, _, expressions) => emit_call(input, expressions),
        _ => todo!(),
    }
}

pub fn emit<'a>(input: &'a str, ast: Vec<Box<Node>>) -> Result<Vec<Inst>, SquareError<'a>> {
    let mut insts = vec![];

    for node in ast {
        let result = emit_expr(input, node)?;
        insts.extend(*result);
    }

    return Ok(insts);
}
