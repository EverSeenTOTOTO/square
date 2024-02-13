use crate::{errors::SquareError, parse::Node, scan::Token, vm_insts::Inst, vm_value::Value};

use alloc::{boxed::Box, format, string::ToString, vec, vec::Vec};

type EmitResult = Result<Vec<Inst>, SquareError>;

fn emit_token<'a, 'b>(input: &'a str, token: &'b Token) -> EmitResult {
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
                _ => Inst::LOAD(id.clone()),
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

fn emit_assign<'a, 'b>(
    input: &'a str,
    target: &'b Box<Node>,
    _properties: &'b Vec<Box<Node>>,
    expression: &'b Box<Node>,
) -> EmitResult {
    let mut insts = emit_node(input, expression)?;

    match target.as_ref() {
        Node::Token(id) => {
            if let Token::Id(_, source) = id {
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
            insts.extend(emit_expand(input, placehoders)?);
        }
        _ => unreachable!(),
    }

    return Ok(insts);
}

fn emit_op<'a, 'b>(input: &'a str, op: &'b Token, expressions: &'b Vec<Box<Node>>) -> EmitResult {
    let op_action = |action: Inst| {
        if expressions.len() != 2 {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                "failed to emit_op, operands size not match".to_string(),
                op.pos().clone(),
                None,
            ));
        }

        let mut result = emit(input, expressions)?;
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
                    result.extend(emit(input, expressions)?);
                    result.push(action);
                    result.push(Inst::STORE(source.clone()));
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

fn emit_if<'a, 'b>(input: &'a str, expressions: &'b Vec<Box<Node>>) -> EmitResult {
    let leading = expressions.first().unwrap();

    if expressions.len() < 3 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_if, expect at least condition and true_branch".to_string(),
            if let Node::Token(if_token) = leading.as_ref() {
                if_token.pos().clone()
            } else {
                unreachable!()
            },
            None,
        ));
    }

    let mut result = vec![];
    let condition = expressions.get(1).unwrap();
    let condition_result = emit_node(input, condition)?;
    let condition_len = condition_result.len() as i32;
    result.extend(condition_result);

    let true_branch = expressions.get(2).unwrap();
    let true_branch_result = emit_node(input, true_branch)?;
    let true_branch_len = true_branch_result.len() as i32;
    // skip true branch, +1 for one extra JMP instcurtion
    result.push(Inst::JNE(true_branch_len + 1));
    result.extend(true_branch_result);

    if let Some(false_branch) = expressions.get(3) {
        let false_branch_result = emit_node(input, false_branch)?;
        let false_branch_len = false_branch_result.len() as i32;
        // skip false branch
        result.push(Inst::JMP(false_branch_len));
        result.extend(false_branch_result);
        result.push(Inst::RET);
        result.insert(
            0,
            Inst::JMP(condition_len + true_branch_len + false_branch_len + 3),
        );
        result.push(Inst::PUSH_CLOSURE(
            -4 - (condition_len + true_branch_len + false_branch_len),
        ));
    } else {
        result.push(Inst::JMP(1));
        result.push(Inst::PUSH(Value::Nil)); // FIXME: else { nil }
        result.push(Inst::RET);
        result.insert(0, Inst::JMP(condition_len + true_branch_len + 3));
        result.push(Inst::PUSH_CLOSURE(-4 - (condition_len + true_branch_len)));
    }

    return Ok(result);
}

fn emit_while<'a, 'b>(input: &'a str, expressions: &'b Vec<Box<Node>>) -> EmitResult {
    let leading = expressions.first().unwrap();

    if expressions.len() < 3 {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to emit_while, expect condition and body".to_string(),
            if let Node::Token(while_token) = leading.as_ref() {
                while_token.pos().clone()
            } else {
                unreachable!()
            },
            None,
        ));
    }

    let condition = expressions.get(1).unwrap();
    let condition_result = emit_node(input, condition)?;
    let condition_len = condition_result.len() as i32;
    let body = expressions.get(2).unwrap();
    let body_result = emit_node(input, body)?;
    let body_len = body_result.len() as i32;
    let offset = condition_len + body_len;

    let mut result = vec![Inst::JMP(3 + offset)];
    result.extend(condition_result);
    result.push(Inst::JNE(body_len + 1));
    result.extend(body_result);
    result.push(Inst::JMP(-((condition_len + body_len + 2) as i32)));
    result.push(Inst::RET);
    result.push(Inst::PUSH_CLOSURE(-4 - offset));

    return Ok(result);
}

fn emit_begin<'a, 'b>(input: &'a str, expressions: &'b Vec<Box<Node>>) -> EmitResult {
    let body = emit(input, &expressions[1..].to_vec())?;
    let offset = body.len() as i32;
    let mut result = vec![Inst::JMP(1 + offset)];
    result.extend(body);
    result.push(Inst::RET);
    result.push(Inst::PUSH_CLOSURE(-2 - offset));
    return Ok(result);
}

fn emit_call<'a, 'b>(
    input: &'a str,
    left_bracket: &'b Token,
    expressions: &'b Vec<Box<Node>>,
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
            Token::Id(_, id) => match id.as_str() {
                // build-in functions
                "if" => {
                    result.extend(emit_if(input, expressions)?);
                    result.push(Inst::CALL);
                }
                "while" => {
                    result.extend(emit_while(input, expressions)?);
                    result.push(Inst::CALL);
                }
                "begin" => {
                    result.extend(emit_begin(input, expressions)?);
                    result.push(Inst::CALL);
                }
                "vec" => {
                    result.extend(emit(input, &expressions[1..].to_vec())?);
                    result.push(Inst::PACK(expressions.len() - 1));
                }
                _ => {
                    result.extend(emit(input, &expressions[1..].to_vec())?); // provided params
                    result.push(Inst::PACK(expressions.len() - 1));
                    result.push(Inst::LOAD(id.clone()));
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
            result.extend(emit(input, &expressions[1..].to_vec())?);
            result.push(Inst::PACK(expressions.len() - 1));
            result.extend(emit_fn(input, params, body)?);
            result.push(Inst::CALL);
        }
        Node::Op(op, body) => result.extend(emit_op(input, op, body)?),
        Node::Assign(_, expansion, dot, body) => {
            result.extend(emit_assign(input, expansion, dot, body)?)
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

fn emit_fn<'a, 'b>(input: &'a str, params: &'b Box<Node>, body: &'b Box<Node>) -> EmitResult {
    if let Node::Expand(.., placehoders) = params.as_ref() {
        let params_result = emit_expand(input, placehoders)?;
        let body_result = emit_node(input, body)?;
        let offset = (params_result.len() + body_result.len()) as i32;
        let mut result = vec![Inst::JMP(offset + 1)];
        result.extend(params_result);
        result.extend(body_result);
        result.push(Inst::RET);
        result.push(Inst::PUSH_CLOSURE(-(offset + 2)));
        return Ok(result);
    }

    unreachable!()
}

fn emit_expand<'a, 'b>(input: &'a str, placeholders: &'b Vec<Box<Node>>) -> EmitResult {
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
                result.extend(emit_expand(input, nested)?);
            }
            Node::Token(token) => match token {
                Token::Id(_, source) => {
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

fn emit_node<'a, 'b>(input: &'a str, node: &'b Box<Node>) -> EmitResult {
    match node.as_ref() {
        Node::Token(token) => emit_token(input, token),
        Node::Expand(.., placehoders) => emit_expand(input, placehoders),
        Node::Assign(_, target, properties, expression) => {
            emit_assign(input, target, properties, expression)
        }
        Node::Fn(_, params, body) => emit_fn(input, params, body),
        Node::Op(op, expressions) => emit_op(input, op, expressions),
        Node::Call(left_bracket, _, expressions) => emit_call(input, left_bracket, expressions),
        _ => todo!(),
    }
}

pub fn emit<'a, 'b>(input: &'a str, ast: &'b Vec<Box<Node>>) -> Result<Vec<Inst>, SquareError> {
    let mut insts = vec![];

    for node in ast {
        let result = emit_node(input, node)?;
        insts.extend(result);
    }

    return Ok(insts);
}
