use core::cell::RefCell;

use crate::{
    errors::SquareError,
    parse::Node,
    scan::Token,
    vm_insts::Inst,
    vm_value::{Closure, Value},
};

#[cfg(test)]
use crate::code_frame::Position;
#[cfg(test)]
use crate::parse::parse;
#[cfg(test)]
use hashbrown::HashMap;

use alloc::{boxed::Box, format, string::String, string::ToString, vec, vec::Vec};
use hashbrown::HashSet;

pub type EmitResult = Result<Vec<Inst>, SquareError>;

#[derive(Debug)]
pub struct EmitContext {
    scopes: Vec<(HashSet<String>, HashSet<String>)>, // (locals, captures)
}

impl EmitContext {
    pub fn new() -> Self {
        return Self {
            scopes: vec![(HashSet::new(), HashSet::new())],
        };
    }

    pub fn add_local(&mut self, name: String) {
        let (ref mut locals, ref mut captures) = self.scopes.last_mut().unwrap();
        captures.remove(&name); // scope shadow
        locals.insert(name);
    }

    pub fn mark_if_capture(&mut self, name: &String) {
        // once a value is captured, it will be captured in all upper scopes until where it is defined
        for (locals, ref mut captures) in self.scopes.iter_mut().rev() {
            if !locals.contains(name) {
                captures.insert(name.clone());
            } else {
                break;
            }
        }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push((HashSet::new(), HashSet::new()));
    }

    pub fn pop_scope(&mut self) -> HashSet<String> {
        self.scopes.pop().unwrap().1
    }
}

fn emit_token(input: &str, token: &Token, ctx: &RefCell<EmitContext>) -> EmitResult {
    match token {
        Token::Num(_, num) => {
            return Ok(vec![Inst::PUSH(Value::Num(num.parse::<f64>().unwrap()))]);
        }
        Token::Str(_, str) => {
            let val = Value::Str(str.clone());
            return Ok(vec![Inst::PUSH(val)]);
        }
        Token::Id(_, id) => {
            ctx.borrow_mut().mark_if_capture(id);
            return Ok(vec![Inst::LOAD(id.clone())]);
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                format!(
                    "failed to emit_token, expect number, string or identifier name, got {}",
                    token.to_string()
                ),
                token.pos().clone(),
                None,
            ));
        }
    }
}

#[test]
fn test_emit_token_num() {
    let code = "42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(insts, vec![Inst::PUSH(Value::Num(42.0))]);
}

#[test]
fn test_emit_token_str() {
    let code = "'42'";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(insts, vec![Inst::PUSH(Value::Str("42".to_string()))]);
}

#[test]
fn test_emit_token_lit() {
    let code = "nil";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(insts, vec![Inst::LOAD("nil".to_string())]);
}

#[test]
fn test_emit_token_id() {
    let code = "a";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(insts, vec![Inst::LOAD("a".to_string())]);
}

fn emit_assign(
    input: &str,
    eq: &Token,
    target: &Box<Node>,
    _properties: &Vec<Box<Node>>,
    expression: &Box<Node>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let mut insts = emit_node(input, expression, ctx)?;
    let is_define = eq.source() == "let";

    match target.as_ref() {
        Node::Token(id) => {
            if let Token::Id(_, source) = id {
                if is_define {
                    ctx.borrow_mut().add_local(source.clone());
                }

                insts.push(Inst::STORE(source.clone()));
            } else {
                return Err(SquareError::SyntaxError(
                    input.to_string(),
                    format!(
                        "failed to emit_assign, cannot assign to {}, expect identifier",
                        id.to_string()
                    ),
                    id.pos().clone(),
                    None,
                ));
            }
        }
        Node::Expand(.., placehoders) => {
            insts.extend(emit_expand(input, is_define, placehoders, ctx)?);
        }
        _ => unreachable!(),
    }

    return Ok(insts);
}

#[test]
fn test_emit_assign() {
    let code = "[= a 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![Inst::PUSH(Value::Num(42.0)), Inst::STORE("a".to_string())]
    );
}

#[test]
fn test_emit_define() {
    let code = "[let a 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![Inst::PUSH(Value::Num(42.0)), Inst::STORE("a".to_string())]
    );
}

fn emit_op(
    input: &str,
    op: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() != 2 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_op, operands size not match".to_string(),
            op.pos().clone(),
            None,
        ));
    }
    let op_action = |action: Inst| {
        let mut result = emit(input, expressions, ctx)?;
        result.push(action);
        return Ok(result);
    };
    let op_assign_action = |action: Inst| {
        let mut result = vec![];
        let first = expressions.first().unwrap();

        match first.as_ref() {
            Node::Token(token) => {
                if let Token::Id(_, source) = token {
                    result.extend(emit(input, expressions, ctx)?);
                    result.push(action);
                    result.push(Inst::STORE(source.clone()));
                    // always push latest value to operand stack
                    result.push(Inst::LOAD(source.clone()));
                    return Ok(result);
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    };

    if let Token::Op(_, source) = op {
        return match source.as_str() {
            "+" => op_action(Inst::ADD),
            "-" => op_action(Inst::SUB),
            "*" => op_action(Inst::MUL),
            "/" => op_action(Inst::DIV),
            "%" => op_action(Inst::REM),
            "&" => op_action(Inst::BITAND),
            "|" => op_action(Inst::BITOR),
            "^" => op_action(Inst::BITXOR),
            "~" => op_action(Inst::BITNOT),
            "==" => op_action(Inst::EQ),
            "!=" => op_action(Inst::NE),
            "<" => op_action(Inst::LT),
            "<=" => op_action(Inst::LE),
            ">" => op_action(Inst::GT),
            ">=" => op_action(Inst::GE),
            ">>" => op_action(Inst::SHR),
            "<<" => op_action(Inst::SHL),
            ">>=" => op_assign_action(Inst::SHR),
            "<<=" => op_assign_action(Inst::SHL),
            "+=" => op_assign_action(Inst::ADD),
            "-=" => op_assign_action(Inst::SUB),
            "*=" => op_assign_action(Inst::MUL),
            "/=" => op_assign_action(Inst::DIV),
            "%=" => op_assign_action(Inst::REM),
            "&=" => op_assign_action(Inst::BITAND),
            "|=" => op_assign_action(Inst::BITOR),
            "^=" => op_assign_action(Inst::BITXOR),
            ".." => {
                let name = "concat".to_string();
                ctx.borrow_mut().mark_if_capture(&name);
                let mut result = vec![];
                result.push(Inst::LOAD(name));
                result.extend(emit(input, expressions, ctx)?);
                result.push(Inst::PACK(2));
                result.push(Inst::CALL);
                return Ok(result);
            }
            _ => todo!(),
        };
    }

    return Err(SquareError::SyntaxError(
        input.to_string(),
        format!("failed to emit_op, expect operator, got {}", op.to_string()),
        op.pos().clone(),
        None,
    ));
}

#[test]
fn test_emit_op() {
    let code = "[+ a b]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("a".to_string()),
            Inst::LOAD("b".to_string()),
            Inst::ADD,
        ]
    );
}

#[test]
fn test_emit_op_assign() {
    let code = "[+= a b]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("a".to_string()),
            Inst::LOAD("b".to_string()),
            Inst::ADD,
            Inst::STORE("a".to_string()),
            Inst::LOAD("a".to_string()),
        ]
    );
}

fn emit_if(
    input: &str,
    if_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() < 3 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_if, expect at least condition and true_branch".to_string(),
            if_token.pos().clone(),
            None,
        ));
    }

    let mut result = vec![];
    let condition = expressions.get(1).unwrap();
    ctx.borrow_mut().push_scope();
    let condition_result = emit_node(input, condition, ctx)?;
    let condition_len = condition_result.len() as i32;
    result.extend(condition_result);

    let true_branch = expressions.get(2).unwrap();

    ctx.borrow_mut().push_scope();
    let true_branch_result = emit_node(input, true_branch, ctx)?;
    let mut captures = ctx.borrow_mut().pop_scope();
    let true_branch_len = true_branch_result.len() as i32;
    // skip true branch, +1 for one extra JMP instcurtion
    result.push(Inst::JNE(true_branch_len + 1));
    result.extend(true_branch_result);

    if let Some(false_branch) = expressions.get(3) {
        ctx.borrow_mut().push_scope();
        let false_branch_result = emit_node(input, false_branch, ctx)?;
        captures.extend(ctx.borrow_mut().pop_scope());
        captures.extend(ctx.borrow_mut().pop_scope());
        let false_branch_len = false_branch_result.len() as i32;
        // skip false branch
        result.push(Inst::JMP(false_branch_len));
        result.extend(false_branch_result);
        result.push(Inst::RET);
        result.insert(
            0,
            Inst::JMP(condition_len + true_branch_len + false_branch_len + 3),
        );

        let mut meta = Closure::new(-4 - (condition_len + true_branch_len + false_branch_len));
        meta.captures = captures;
        result.push(Inst::PUSH_CLOSURE(meta));
    } else {
        captures.extend(ctx.borrow_mut().pop_scope());

        result.push(Inst::JMP(1));
        result.push(Inst::PUSH(Value::Nil)); // FIXME: else { nil }
        result.push(Inst::RET);
        result.insert(0, Inst::JMP(condition_len + true_branch_len + 4));

        let mut meta = Closure::new(-5 - (condition_len + true_branch_len));
        meta.captures = captures;
        result.push(Inst::PUSH_CLOSURE(meta));
    }

    return Ok(result);
}

#[test]
fn test_emit_if_true() {
    let code = "[if true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(6),
            Inst::LOAD("true".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(1),
            Inst::PUSH(Value::Nil),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -7,
                upvalues: HashMap::new(),
                captures: {
                    let mut unresolved_true = HashSet::new();
                    unresolved_true.insert("true".to_string());
                    unresolved_true
                }
            }),
            Inst::PACK(0),
            Inst::CALL
        ]
    );
}

#[test]
fn test_emit_if_true_false() {
    let code = "[if true 42 24]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(6),
            Inst::LOAD("true".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(1),
            Inst::PUSH(Value::Num(24.0)),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -7,
                upvalues: HashMap::new(),
                captures: {
                    let mut unresolved_true = HashSet::new();
                    unresolved_true.insert("true".to_string());
                    unresolved_true
                }
            }),
            Inst::PACK(0),
            Inst::CALL
        ]
    );
}

fn emit_while(
    input: &str,
    while_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() < 3 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_while, expect condition and body".to_string(),
            while_token.pos().clone(),
            None,
        ));
    }

    let condition = expressions.get(1).unwrap();
    ctx.borrow_mut().push_scope();
    let condition_result = emit_node(input, condition, ctx)?;
    let condition_len = condition_result.len() as i32;
    let body = expressions.get(2).unwrap();

    ctx.borrow_mut().push_scope();
    let body_result = emit_node(input, body, ctx)?;
    let mut captures = ctx.borrow_mut().pop_scope();
    captures.extend(ctx.borrow_mut().pop_scope());
    let body_len = body_result.len() as i32;
    let offset = condition_len + body_len;

    let mut result = vec![Inst::JMP(3 + offset)];
    result.extend(condition_result);
    result.push(Inst::JNE(body_len + 1));
    result.extend(body_result);
    result.push(Inst::JMP(-((condition_len + body_len + 2) as i32)));
    result.push(Inst::RET);

    let mut meta = Closure::new(-4 - offset);
    meta.captures = captures;
    result.push(Inst::PUSH_CLOSURE(meta));
    return Ok(result);
}

#[test]
fn test_emit_while() {
    let code = "[while true 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(5),
            Inst::LOAD("true".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(-4),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -6,
                upvalues: HashMap::new(),
                captures: {
                    let mut unresolved_true = HashSet::new();
                    unresolved_true.insert("true".to_string());
                    unresolved_true
                }
            }),
            Inst::PACK(0),
            Inst::CALL
        ]
    );
}

fn emit_begin(
    input: &str,
    _begin_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    ctx.borrow_mut().push_scope();
    let body = emit(input, &expressions[1..].to_vec(), ctx)?;
    let captures = ctx.borrow_mut().pop_scope();
    let offset = body.len() as i32;
    let mut result = vec![Inst::JMP(1 + offset)];
    result.extend(body);
    result.push(Inst::RET);

    let mut meta = Closure::new(-2 - offset);
    meta.captures = captures;
    result.push(Inst::PUSH_CLOSURE(meta));

    return Ok(result);
}

#[test]
fn test_emit_begin() {
    let code = "[begin 42]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure::new(-3)),
            Inst::PACK(0),
            Inst::CALL
        ]
    );
}

fn emit_cond(
    input: &str,
    match_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() < 1 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_cond, expect patterns".to_string(),
            match_token.pos().clone(),
            None,
        ));
    }

    ctx.borrow_mut().push_scope();
    let mut result = vec![];
    let mut skip = 0;

    for node in expressions[1..].to_vec().iter().rev() {
        if let Node::Call(left_bracket, _, ref exprs) = node.as_ref() {
            if exprs.len() != 2 {
                return Err(SquareError::SyntaxError(
                    input.to_string(),
                    "failed to emit_cond, expect pattern and action".to_string(),
                    left_bracket.pos().clone(),
                    None,
                ));
            }

            let pattern_result = emit_node(input, &exprs[0], ctx)?;
            let pattern_len = pattern_result.len() as i32;
            let action_result = emit_node(input, &exprs[1], ctx)?;
            let action_len = action_result.len() as i32;

            result.insert(0, Inst::JMP(skip));
            result.splice(0..0, action_result);
            result.insert(0, Inst::JNE(action_len + 1));
            result.splice(0..0, pattern_result);
            skip += pattern_len + action_len + 2;
        } else {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                "failed to emit_cond, expect call expression".to_string(),
                match_token.pos().clone(),
                None,
            ));
        }
    }
    let result_len = result.len() as i32;
    result.push(Inst::RET);
    result.insert(0, Inst::PUSH(Value::Nil));
    result.insert(0, Inst::JMP(result_len + 2));

    let captures = ctx.borrow_mut().pop_scope();
    let mut meta = Closure::new(-result_len - 3);
    meta.captures = captures;
    result.push(Inst::PUSH_CLOSURE(meta));

    return Ok(result);
}

#[test]
fn test_cond() {
    let code = "[cond
        [false 24]
        [true 42]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(10),
            Inst::PUSH(Value::Nil),
            Inst::LOAD("false".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(24.0)),
            Inst::JMP(4),
            Inst::LOAD("true".to_string()),
            Inst::JNE(2),
            Inst::PUSH(Value::Num(42.0)),
            Inst::JMP(0),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -11,
                upvalues: HashMap::new(),
                captures: {
                    let mut unresolved = HashSet::new();
                    unresolved.insert("true".to_string());
                    unresolved.insert("false".to_string());
                    unresolved
                }
            }),
            Inst::PACK(0),
            Inst::CALL
        ]
    );
}

fn emit_call(
    input: &str,
    left_bracket: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if expressions.len() == 0 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_call, expect function name".to_string(),
            left_bracket.pos().clone(),
            None,
        ));
    }

    let first = expressions.first().unwrap();
    let mut result = vec![];

    match first.as_ref() {
        Node::Token(id) => match id {
            Token::Id(_, name) => match name.as_str() {
                // build-in function with speciall syntax
                "if" => {
                    result.extend(emit_if(input, id, expressions, ctx)?);
                    result.push(Inst::PACK(0));
                    result.push(Inst::CALL);
                }
                "while" => {
                    result.extend(emit_while(input, id, expressions, ctx)?);
                    result.push(Inst::PACK(0));
                    result.push(Inst::CALL);
                }
                "begin" => {
                    result.extend(emit_begin(input, id, expressions, ctx)?);
                    result.push(Inst::PACK(0));
                    result.push(Inst::CALL);
                }
                "cond" => {
                    result.extend(emit_cond(input, id, expressions, ctx)?);
                    result.push(Inst::PACK(0));
                    result.push(Inst::CALL);
                }
                _ => {
                    // normal function call
                    ctx.borrow_mut().mark_if_capture(name);
                    result.push(Inst::LOAD(name.clone()));
                    result.extend(emit(input, &expressions[1..].to_vec(), ctx)?); // provided params
                    result.push(Inst::PACK(expressions.len() - 1));
                    result.push(Inst::CALL);
                }
            },
            _ => {
                return Err(SquareError::SyntaxError(
                    input.to_string(),
                    format!(
                        "failed to emit_call, expect function name, got {}",
                        id.to_string()
                    ),
                    id.pos().clone(),
                    None,
                ));
            }
        },
        Node::Fn(_, params, body) => {
            result.extend(emit_fn(input, params, body, ctx)?);
            result.extend(emit(input, &expressions[1..].to_vec(), ctx)?);
            result.push(Inst::PACK(expressions.len() - 1));
            result.push(Inst::CALL);
        }
        Node::Op(op, body) => result.extend(emit_op(input, op, body, ctx)?),
        Node::Assign(eq, expansion, dot, body) => {
            result.extend(emit_assign(input, eq, expansion, dot, body, ctx)?)
        }
        Node::Call(left_bracket, _, exprs) => {
            result.extend(emit_call(input, left_bracket, exprs, ctx)?);
            result.extend(emit(input, &expressions[1..].to_vec(), ctx)?);
            result.push(Inst::PACK(expressions.len() - 1));
            result.push(Inst::CALL);
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                format!(
                    "failed to emit_call, expect function name, got {}",
                    first.to_string()
                ),
                left_bracket.pos().clone(),
                None,
            ));
        }
    }

    return Ok(result);
}

#[test]
fn test_emit_call_no_params() {
    let code = "[foo]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![Inst::LOAD("foo".to_string()), Inst::PACK(0), Inst::CALL]
    );
}

#[test]
fn test_emit_call_with_params() {
    let code = "[foo bar]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("foo".to_string()),
            Inst::LOAD("bar".to_string()),
            Inst::PACK(1),
            Inst::CALL
        ]
    );
}

fn emit_fn(
    input: &str,
    params: &Box<Node>,
    body: &Box<Node>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if let Node::Expand(.., placehoders) = params.as_ref() {
        ctx.borrow_mut().push_scope();
        let params_result = emit_expand(input, true, placehoders, ctx)?;
        let body_result = emit_node(input, body, ctx)?;
        let captures = ctx.borrow_mut().pop_scope();
        let offset = (params_result.len() + body_result.len()) as i32;

        let mut result = vec![Inst::JMP(offset + 1)];
        result.extend(params_result);
        result.extend(body_result);
        result.push(Inst::RET);

        let mut meta = Closure::new(-(offset + 2));
        meta.captures = captures;

        result.push(Inst::PUSH_CLOSURE(meta));

        return Ok(result);
    }

    unreachable!()
}

#[test]
fn test_emit_fn() {
    let code = "/[] 42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(3),
            Inst::POP, // pop param pack
            Inst::PUSH(Value::Num(42.0)),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure::new(-4)),
        ]
    );
}

#[test]
fn test_emit_fn_params() {
    let code = "/[x] 42";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(5),
            Inst::PEEK(0, 0), // peek param
            Inst::STORE("x".to_string()),
            Inst::POP, // pop param pack
            Inst::PUSH(Value::Num(42.0)),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure::new(-6)),
        ]
    );
}

#[test]
fn test_emit_fn_capture() {
    let code = "/[] y";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    let mut captures = HashSet::new();
    captures.insert("y".to_string());

    assert_eq!(
        insts,
        vec![
            Inst::JMP(3),
            Inst::POP,
            Inst::LOAD("y".to_string()),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -4,
                upvalues: HashMap::new(),
                captures
            }),
        ]
    );
}

#[test]
fn test_emit_fn_capture_nested() {
    let code = "/[] [begin 
        [let x 1]
        /[] [+ x y]]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::JMP(16),
            Inst::POP,
            Inst::JMP(10),
            Inst::PUSH(Value::Num(1.0)),
            Inst::STORE("x".to_string()),
            Inst::JMP(5),
            Inst::POP,
            Inst::LOAD("x".to_string()),
            Inst::LOAD("y".to_string()),
            Inst::ADD,
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -6,
                upvalues: HashMap::new(),
                captures: {
                    let mut unresolved_xy = HashSet::new();
                    unresolved_xy.insert("x".to_string());
                    unresolved_xy.insert("y".to_string());
                    unresolved_xy
                }
            }),
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -11,
                upvalues: HashMap::new(),
                captures: {
                    let mut unresolved_y = HashSet::new();
                    unresolved_y.insert("y".to_string());
                    unresolved_y
                }
            }),
            Inst::PACK(0),
            Inst::CALL,
            Inst::RET,
            Inst::PUSH_CLOSURE(Closure {
                ip: -17,
                upvalues: HashMap::new(),
                captures: {
                    let mut unresolved_y = HashSet::new();
                    unresolved_y.insert("y".to_string());
                    unresolved_y
                }
            }),
        ]
    );
}

fn emit_expand(
    input: &str,
    is_define: bool,
    placeholders: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let mut result = vec![];
    let mut greedy_pos: Option<usize> = None;

    for (i, placeholder) in placeholders.iter().enumerate() {
        let (offset, index) = if greedy_pos.is_some() {
            (i - 1, -((placeholders.len() - i) as i32))
        } else {
            (0, i as i32)
        };

        match placeholder.as_ref() {
            Node::Expand(.., nested) => {
                result.push(Inst::PEEK(offset, index));
                result.extend(emit_expand(input, is_define, nested, ctx)?);
            }
            Node::Token(token) => match token {
                Token::Id(_, source) => {
                    if is_define {
                        ctx.borrow_mut().add_local(source.clone());
                    }

                    result.push(Inst::PEEK(offset, index));
                    result.push(Inst::STORE(source.clone()));
                }
                Token::Op(pos, op) => {
                    if op == "." {
                        result.push(Inst::PEEK(offset, index));
                        result.push(Inst::POP);
                    } else if op == "..." {
                        if greedy_pos.is_some() {
                            return Err(SquareError::SyntaxError(
                                input.to_string(),
                                format!("failed to emit_expand, multiple greedy placeholder, first occurs at index {}", greedy_pos.unwrap()),
                                pos.clone(),
                                None,
                            ));
                        } else {
                            greedy_pos = Some(i);
                        }
                    } else {
                        return Err(SquareError::SyntaxError(
                            input.to_string(),
                            format!(
                                "failed to emit_expand, expect identifier or placeholder, got {}",
                                token.to_string()
                            ),
                            pos.clone(),
                            None,
                        ));
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    result.push(Inst::POP); // drop the pack on top of operand stack

    return Ok(result);
}

#[test]
fn test_emit_expand() {
    let code = "[= [a b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::STORE("a".to_string()),
            Inst::PEEK(0, 1),
            Inst::STORE("b".to_string()),
            Inst::POP
        ]
    );
}

#[test]
fn test_emit_expand_dot() {
    let code = "[= [. b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::POP,
            Inst::PEEK(0, 1),
            Inst::STORE("b".to_string()),
            Inst::POP
        ]
    );
}

#[test]
fn test_emit_expand_greed() {
    let code = "[= [... b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("c".to_string()),
            Inst::PEEK(0, -1),
            Inst::STORE("b".to_string()),
            Inst::POP
        ]
    );
}

#[test]
fn test_emit_expand_greed_offset() {
    let code = "[= [. ... . b] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::POP,
            Inst::PEEK(1, -2),
            Inst::POP,
            Inst::PEEK(2, -1),
            Inst::STORE("b".to_string()),
            Inst::POP
        ]
    );
}

#[test]
fn test_emit_expand_nested() {
    let code = "[= [[[b]]] c]";
    let ast = parse(code, &mut Position::new()).unwrap();
    let insts = emit(code, &ast, &RefCell::new(EmitContext::new())).unwrap();

    assert_eq!(
        insts,
        vec![
            Inst::LOAD("c".to_string()),
            Inst::PEEK(0, 0),
            Inst::PEEK(0, 0),
            Inst::PEEK(0, 0),
            Inst::STORE("b".to_string()),
            Inst::POP,
            Inst::POP,
            Inst::POP,
        ]
    );
}

fn emit_node(input: &str, node: &Box<Node>, ctx: &RefCell<EmitContext>) -> EmitResult {
    match node.as_ref() {
        Node::Token(token) => emit_token(input, token, ctx),
        Node::Assign(eq, target, properties, expression) => {
            emit_assign(input, eq, target, properties, expression, ctx)
        }
        Node::Fn(_, params, body) => emit_fn(input, params, body, ctx),
        Node::Op(op, expressions) => emit_op(input, op, expressions, ctx),
        Node::Call(left_bracket, _, expressions) => {
            emit_call(input, left_bracket, expressions, ctx)
        }
        _ => todo!(),
    }
}

pub fn emit(
    input: &str,
    ast: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> Result<Vec<Inst>, SquareError> {
    let mut insts = vec![];

    for node in ast {
        let result = emit_node(input, node, ctx)?;
        insts.extend(result);
    }

    return Ok(insts);
}
