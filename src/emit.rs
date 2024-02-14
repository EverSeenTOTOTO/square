use core::cell::RefCell;

use crate::{
    errors::SquareError,
    parse::Node,
    scan::Token,
    vm_insts::Inst,
    vm_value::{Closure, Value},
};

use alloc::{boxed::Box, format, string::String, string::ToString, vec, vec::Vec};
use hashbrown::HashSet;

pub type EmitResult = Result<Vec<Inst>, SquareError>;

#[derive(Debug)]
pub struct EmitContext {
    depth_info: Vec<(HashSet<String>, HashSet<String>)>, // (locals, captures)
}

impl EmitContext {
    pub fn new() -> Self {
        return Self {
            depth_info: vec![(HashSet::new(), HashSet::new())],
        };
    }

    pub fn add_local(&mut self, name: String) {
        self.depth_info.last_mut().unwrap().0.insert(name);
    }

    pub fn mark_if_capture(&mut self, name: &String) {
        let (locals, ref mut captures) = self.depth_info.last_mut().unwrap();

        if !locals.contains(name) {
            captures.insert(name.clone());
        }
    }

    pub fn push_scope(&mut self) {
        self.depth_info.push((HashSet::new(), HashSet::new()));
    }

    pub fn pop_scope(&mut self) -> HashSet<String> {
        self.depth_info.pop().unwrap().1
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
            let inst = match id.as_str() {
                // build-in literals
                "true" => Inst::PUSH(Value::Bool(true)),
                "false" => Inst::PUSH(Value::Bool(false)),
                "nil" => Inst::PUSH(Value::Nil),
                _ => {
                    ctx.borrow_mut().mark_if_capture(id);
                    Inst::LOAD(id.clone())
                }
            };

            return Ok(vec![inst]);
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

fn emit_assign(
    input: &str,
    target: &Box<Node>,
    _properties: &Vec<Box<Node>>,
    expression: &Box<Node>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let mut insts = emit_node(input, expression, ctx)?;

    match target.as_ref() {
        Node::Token(id) => {
            if let Token::Id(_, source) = id {
                // remember scope depth info
                ctx.borrow_mut().add_local(source.clone());

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
            insts.extend(emit_expand(input, placehoders, ctx)?);
        }
        _ => unreachable!(),
    }

    return Ok(insts);
}

fn emit_op(
    input: &str,
    op: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let op_action = |action: Inst| {
        if expressions.len() != 2 {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                "failed to emit_op, operands size not match".to_string(),
                op.pos().clone(),
                None,
            ));
        }

        let mut result = emit(input, expressions, ctx)?;
        result.push(action);
        return Ok(result);
    };
    let op_assign_action = |action: Inst| {
        if expressions.len() != 2 {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                "failed to emit_op, operands size not match".to_string(),
                op.pos().clone(),
                None,
            ));
        }

        let mut result = vec![];
        let first = expressions.first().unwrap();

        match first.as_ref() {
            Node::Token(token) => {
                if let Token::Id(_, source) = token {
                    result.extend(emit(input, expressions, ctx)?);
                    result.push(action);
                    result.push(Inst::STORE(source.clone()));
                    result.push(Inst::LOAD(source.clone()));
                    return Ok(result);
                } else {
                    return Err(SquareError::SyntaxError(
                        input.to_string(),
                        format!(
                            "failed to emit_assign_op, expect identifier, got {}",
                            token.to_string()
                        ),
                        token.pos().clone(),
                        None,
                    ));
                }
            }
            _ => todo!(),
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

    let mut result = vec![Inst::COMMENT(format!(
        "if at line {}, col {} start",
        if_token.pos().line,
        if_token.pos().column
    ))];
    let condition = expressions.get(1).unwrap();
    let condition_result = emit_node(input, condition, ctx)?;
    let condition_len = condition_result.len() as i32;
    result.extend(condition_result);

    let true_branch = expressions.get(2).unwrap();

    let true_branch_result = emit_node(input, true_branch, ctx)?;
    let true_branch_len = true_branch_result.len() as i32;
    // skip true branch, +1 for one extra JMP instcurtion
    result.push(Inst::JNE(true_branch_len + 1));
    result.extend(true_branch_result);

    if let Some(false_branch) = expressions.get(3) {
        let false_branch_result = emit_node(input, false_branch, ctx)?;
        let false_branch_len = false_branch_result.len() as i32;
        // skip false branch
        result.push(Inst::JMP(false_branch_len));
        result.extend(false_branch_result);
        result.push(Inst::RET);
        result.insert(
            0,
            Inst::JMP(condition_len + true_branch_len + false_branch_len + 3),
        );
        result.push(Inst::PUSH_CLOSURE(Closure::new(
            -4 - (condition_len + true_branch_len + false_branch_len),
        )));
    } else {
        result.push(Inst::JMP(1));
        result.push(Inst::PUSH(Value::Nil)); // FIXME: else { nil }
        result.push(Inst::RET);
        result.insert(0, Inst::JMP(condition_len + true_branch_len + 3));
        result.push(Inst::PUSH_CLOSURE(Closure::new(
            -4 - (condition_len + true_branch_len),
        )));
    }

    result.push(Inst::COMMENT(format!(
        "if at line {}, col {} end",
        if_token.pos().line,
        if_token.pos().column
    )));

    return Ok(result);
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
    let condition_result = emit_node(input, condition, ctx)?;
    let condition_len = condition_result.len() as i32;
    let body = expressions.get(2).unwrap();

    let body_result = emit_node(input, body, ctx)?;
    let body_len = body_result.len() as i32;
    let offset = condition_len + body_len;

    let mut result = vec![
        Inst::COMMENT(format!(
            "while at line {}, col {} start",
            while_token.pos().line,
            while_token.pos().column
        )),
        Inst::JMP(3 + offset),
    ];
    result.extend(condition_result);
    result.push(Inst::JNE(body_len + 1));
    result.extend(body_result);
    result.push(Inst::JMP(-((condition_len + body_len + 2) as i32)));
    result.push(Inst::RET);
    result.push(Inst::PUSH_CLOSURE(Closure::new(-4 - offset)));
    result.push(Inst::COMMENT(format!(
        "while at line {}, col {} end",
        while_token.pos().line,
        while_token.pos().column
    )));

    return Ok(result);
}

fn emit_begin(
    input: &str,
    begin_token: &Token,
    expressions: &Vec<Box<Node>>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    let body = emit(input, &expressions[1..].to_vec(), ctx)?;
    let offset = body.len() as i32;
    let mut result = vec![
        Inst::COMMENT(format!(
            "begin at line {}, col {} start",
            begin_token.pos().line,
            begin_token.pos().column
        )),
        Inst::JMP(1 + offset),
    ];
    result.extend(body);
    result.push(Inst::RET);
    result.push(Inst::PUSH_CLOSURE(Closure::new(-2 - offset)));
    result.push(Inst::COMMENT(format!(
        "begin at line {}, col {} end",
        begin_token.pos().line,
        begin_token.pos().column
    )));

    return Ok(result);
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
                // build-in functions
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
                "match" => todo!(),
                "print" => todo!(),
                "typeof" => todo!(),
                "callcc" => todo!(),
                "obj" => todo!(),
                "vec" => {
                    result.extend(emit(input, &expressions[1..].to_vec(), ctx)?);
                    result.push(Inst::PACK(expressions.len() - 1));
                }
                _ => {
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
        Node::Assign(_, expansion, dot, body) => {
            result.extend(emit_assign(input, expansion, dot, body, ctx)?)
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

fn emit_fn(
    input: &str,
    params: &Box<Node>,
    body: &Box<Node>,
    ctx: &RefCell<EmitContext>,
) -> EmitResult {
    if let Node::Expand(left_bracket, _, placehoders) = params.as_ref() {
        ctx.borrow_mut().push_scope();
        let params_result = emit_expand(input, placehoders, ctx)?;
        let body_result = emit_node(input, body, ctx)?;
        let captures = ctx.borrow_mut().pop_scope();
        let offset = (params_result.len() + body_result.len()) as i32;

        let mut result = vec![
            Inst::COMMENT(format!(
                "fn at line {}, col {} start",
                left_bracket.pos().line,
                left_bracket.pos().column
            )),
            Inst::JMP(offset + 1),
        ];
        result.extend(params_result);
        result.extend(body_result);
        result.push(Inst::RET);
        let mut closure_meta = Closure::new(-(offset + 2));
        captures.iter().for_each(|name| {
            closure_meta.captures.insert(name.clone(), Value::Nil);
        });
        result.push(Inst::PUSH_CLOSURE(closure_meta));
        result.push(Inst::COMMENT(format!(
            "fn at line {}, col {} end",
            left_bracket.pos().line,
            left_bracket.pos().column
        )));

        return Ok(result);
    }

    unreachable!()
}

fn emit_expand(
    input: &str,
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
                result.extend(emit_expand(input, nested, ctx)?);
            }
            Node::Token(token) => match token {
                Token::Id(_, source) => {
                    // scope depth
                    ctx.borrow_mut().add_local(source.clone());

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

    result.push(Inst::POP); // drop the vec on top of operand stack

    return Ok(result);
}

fn emit_node(input: &str, node: &Box<Node>, ctx: &RefCell<EmitContext>) -> EmitResult {
    match node.as_ref() {
        Node::Token(token) => emit_token(input, token, ctx),
        Node::Expand(.., placehoders) => emit_expand(input, placehoders, ctx),
        Node::Assign(_, target, properties, expression) => {
            emit_assign(input, target, properties, expression, ctx)
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
