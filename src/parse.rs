use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use core::fmt;

use crate::code_frame::Position;
use crate::errors::SquareError;
use crate::scan::{expect, lookahead, raise_token, skip_whitespace, RaiseResult, Token};

pub type ParseResult = Result<Box<Node>, SquareError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Token(Token),                                        // literal
    Expand(Token, Token, Vec<Box<Node>>),                // '[', ']', placeholders*
    Fn(Token, Box<Node>, Box<Node>),                     // '/', params, body
    Prop(Token, Token),                                  // '.', prop
    Assign(Token, Box<Node>, Vec<Box<Node>>, Box<Node>), // '=' | 'let', expansion | id, (dot property)*, expression
    Op(Token, Vec<Box<Node>>),                           // operator, expressions+
    Call(Token, Token, Vec<Box<Node>>),                  // '[', ']', expressions+
    Dot(Box<Node>, Vec<Box<Node>>),                      // id | call, (dot property)*
}

fn print_nodevec(nodevec: &Vec<Box<Node>>) -> String {
    return match nodevec.is_empty() {
        true => "Empty".to_string(),
        _ => nodevec
            .iter()
            .map(|node| format!("{}", node))
            .collect::<Vec<String>>()
            .join(", "),
    };
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Token(token) => write!(f, "Token({})", token),
            Node::Expand(.., placeholders) => {
                write!(f, "Expand({})", print_nodevec(placeholders))
            }
            Node::Fn(_, params, body) => {
                write!(f, "Fn({}, {})", params, body)
            }
            Node::Prop(_, prop) => {
                write!(f, "Prop({})", prop)
            }
            Node::Assign(_, expansion, properties, expression) => {
                write!(
                    f,
                    "Assign({}.{} {})",
                    expansion,
                    print_nodevec(properties),
                    expression
                )
            }
            Node::Op(operator, expressions) => {
                write!(f, "Op({}, {})", operator, print_nodevec(expressions))
            }
            Node::Call(.., expressions) => {
                write!(f, "Call({})", print_nodevec(expressions))
            }
            Node::Dot(instance, properties) => {
                write!(f, "Dot({} {})", instance, print_nodevec(properties))
            }
        }
    }
}

// wrap scanner error to parser error
fn create_wrapper(fn_name: &'static str) -> Box<dyn Fn(RaiseResult) -> RaiseResult> {
    Box::new(move |result: RaiseResult| {
        if let Err(SquareError::UnexpectedToken(input, message, position)) = result {
            return Err(SquareError::SyntaxError(
                input,
                format!("failed to {}, {}", fn_name, message),
                position,
                None,
            ));
        }

        result
    })
}

fn expect_whitespace(input: &str, pos: &mut Position) -> Result<Token, SquareError> {
    expect(
        &|token| {
            if let Token::Whitespace(..) = token {
                return (true, "".to_string());
            }
            (false, "expect WHITESPACE".to_string())
        },
        input,
        pos,
    )
}

fn parse_expand(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_expand");

    let left_bracket = wrap(expect(
        &|token| (token.source() == "[", "expect [".to_string()),
        input,
        pos,
    ))?;

    wrap(skip_whitespace(input, pos))?;

    let mut token = wrap(raise_token(input, pos))?;
    let mut nodes = vec![];

    loop {
        match token.source() {
            "." | "..." => {
                nodes.push(Box::new(Node::Token(token)));

                if wrap(lookahead(input, pos))?.source() != "]" {
                    wrap(expect_whitespace(input, pos))?;
                }
            }
            "[" => {
                *pos = token.pos().clone();
                nodes.push(parse_expand(input, pos)?);
            }
            "]" => break,
            _ => {
                if let Token::Id(..) = token {
                    nodes.push(Box::new(Node::Token(token)));

                    if wrap(lookahead(input, pos))?.source() != "]" {
                        wrap(expect_whitespace(input, pos))?;
                    }
                } else {
                    return Err(SquareError::SyntaxError(
                        input.to_string(),
                        format!(
                            "faield to parse_expand, expect identifier or placeholders, got {}",
                            token
                        ),
                        token.pos().clone(),
                        None,
                    ));
                }
            }
        }

        wrap(skip_whitespace(input, pos))?;

        token = wrap(raise_token(input, pos))?;
    }

    Ok(Box::new(Node::Expand(left_bracket, token, nodes)))
}

#[test]
fn test_parse_expand_empty() {
    let input = "[]";
    let mut pos = Position::new();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(.., phs) = node.as_ref() {
        assert_eq!(phs.len(), 0);

        return;
    }

    panic!();
}

#[test]
fn test_parse_expand() {
    let input = "[ x ;comment; y z   \t ]";
    let mut pos = Position::new();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(.., phs) = node.as_ref() {
        assert_eq!(phs.len(), 3);

        return;
    }

    panic!();
}

#[test]
fn test_parse_expand_placehoders() {
    let input = "[. ... . ... .]";
    let mut pos = Position::new();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(.., phs) = node.as_ref() {
        assert_eq!(phs.len(), 5);
        return;
    }

    panic!();
}

#[test]
fn test_parse_expand_nested() {
    let input = "[. [ x [ ;comment; y] [z ... ]] ... s  ]";
    let mut pos = Position::new();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(.., phs) = node.as_ref() {
        assert_eq!(phs.len(), 4);

        if let Node::Expand(.., phs2) = phs[1].as_ref() {
            assert_eq!(phs2.len(), 3);

            if let Node::Expand(.., phs3) = phs2[1].as_ref() {
                if let Node::Token(y) = phs3[0].as_ref() {
                    assert_eq!(y.source(), "y");

                    return;
                }
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_expand_error() {
    let mut input = "[.x]";
    let mut pos = Position::new();
    let mut err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to parse_expand, expect WHITESPACE, got Id(x)".to_string(),
            pos,
            None
        ))
    );

    input = "[....]";
    pos = Position::new();
    err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to parse_expand, expect WHITESPACE, got Op(.)".to_string(),
            pos,
            None
        ))
    );

    input = "[x....]";
    pos = Position::new();
    err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to parse_expand, expect WHITESPACE, got Op(...)".to_string(),
            pos,
            None
        ))
    );
}

fn parse_fn(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_fn");

    let slash = wrap(expect(
        &|token| (token.source() == "/[", "expect  /[".to_string()),
        input,
        pos,
    ))?;
    pos.cursor -= 1;
    pos.column -= 1;
    let expand = parse_expand(input, pos)?;

    wrap(skip_whitespace(input, pos))?;

    let expr = parse_expr(input, pos)?;

    Ok(Box::new(Node::Fn(slash, expand, expr)))
}

#[test]
fn test_parse_fn() {
    let input = "/[] 42";
    let mut pos = Position::new();
    let node = parse_fn(input, &mut pos).unwrap();

    if let Node::Fn(_, params, body) = node.as_ref() {
        if let Node::Expand(.., phs) = params.as_ref() {
            assert_eq!(phs.len(), 0);

            if let Node::Token(value) = body.as_ref() {
                assert_eq!(value.source(), "42");

                return;
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_fn_params() {
    let input = "/[. [ x [ ;comment; y] [z ... ]] ... s  ] 42";
    let mut pos = Position::new();
    let node = parse_fn(input, &mut pos).unwrap();

    if let Node::Fn(_, params, _) = node.as_ref() {
        if let Node::Expand(.., phs) = params.as_ref() {
            assert_eq!(phs.len(), 4);

            if let Node::Expand(.., phs2) = phs[1].as_ref() {
                assert_eq!(phs2.len(), 3);

                if let Node::Expand(.., phs3) = phs2[1].as_ref() {
                    if let Node::Token(y) = phs3[0].as_ref() {
                        assert_eq!(y.source(), "y");

                        return;
                    }
                }
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_fn_high_order() {
    let input = "/[x] /[y] /[z] [+ [- x y] [* y z]]";
    let mut pos = Position::new();
    let node = parse_fn(input, &mut pos).unwrap();

    if let Node::Fn(_, x_param, x_body) = node.as_ref() {
        if let Node::Expand(.., x_phs) = x_param.as_ref() {
            if let Node::Token(x) = x_phs[0].as_ref() {
                assert_eq!(x.source(), "x");

                if let Node::Fn(_, y_param, y_body) = x_body.as_ref() {
                    if let Node::Expand(.., y_phs) = y_param.as_ref() {
                        if let Node::Token(y) = y_phs[0].as_ref() {
                            assert_eq!(y.source(), "y");

                            if let Node::Fn(_, z_param, _) = y_body.as_ref() {
                                if let Node::Expand(.., z_phs) = z_param.as_ref() {
                                    if let Node::Token(z) = z_phs[0].as_ref() {
                                        assert_eq!(z.source(), "z");

                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    panic!();
}

fn parse_prop(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_prop");
    let dot = wrap(expect(
        &|token| (token.source() == ".", "expect DOT".to_string()),
        input,
        pos,
    ))?;

    Ok(Box::new(Node::Prop(
        dot,
        wrap(expect(
            &|token| {
                if let Token::Id(..) = token {
                    return (true, "".to_string());
                }
                (false, "expect ID".to_string())
            },
            input,
            pos,
        ))?,
    )))
}

#[test]
fn test_parse_prop() {
    let input = ".foo";
    let mut pos = Position::new();
    let node = parse_prop(input, &mut pos).unwrap();

    if let Node::Prop(_, id) = node.as_ref() {
        assert_eq!(id.source(), "foo");

        return;
    }

    panic!()
}

fn parse_prop_chain(input: &str, pos: &mut Position) -> Result<Vec<Box<Node>>, SquareError> {
    let wrap = create_wrapper("parse_prop_chain");
    let mut nodes = vec![];

    loop {
        let backup = pos.clone();

        wrap(skip_whitespace(input, pos))?;

        let dot = wrap(lookahead(input, pos))?;

        if dot.source() != "." {
            *pos = backup;
            break;
        }

        nodes.push(parse_prop(input, pos)?);
    }

    Ok(nodes)
}

#[test]
fn test_parse_prop_chain() {
    let input = r#"
.foo
.bar 
.baz"#;
    let mut pos = Position::new();
    let chain = parse_prop_chain(input, &mut pos).unwrap();

    if let Node::Prop(_, baz) = chain[2].as_ref() {
        assert_eq!(baz.source(), "baz");

        return;
    }

    panic!()
}

fn parse_assign(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_assign");
    let eq = wrap(expect(
        &|token| {
            (
                token.source() == "=" || token.source() == "let",
                "expect = or let".to_string(),
            )
        },
        input,
        pos,
    ))?;
    let is_define = eq.source() == "let";

    wrap(expect_whitespace(input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let token = wrap(raise_token(input, pos))?;

    let target = if token.source() == "[" {
        *pos = token.pos().clone();
        parse_expand(input, pos)?
    } else if let Token::Id(..) = token {
        Box::new(Node::Token(token))
    } else {
        return Err(SquareError::SyntaxError(
            input.to_string(),
            format!(
                "faield to parse_assign, expect identifier or expansion, got {}",
                token
            ),
            token.pos().clone(),
            None,
        ));
    };

    let props = if is_define {
        vec![]
    } else {
        parse_prop_chain(input, pos)?
    };

    wrap(expect_whitespace(input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let expr = parse_expr(input, pos)?;

    Ok(Box::new(Node::Assign(eq, target, props, expr)))
}

#[test]
fn test_parse_define() {
    let input = "let a b";
    let mut pos = Position::new();
    let node = parse_assign(input, &mut pos).unwrap();

    if let Node::Assign(_, lhs, _, rhs) = node.as_ref() {
        if let Node::Token(a) = lhs.as_ref() {
            assert_eq!(a.source(), "a");

            if let Node::Token(b) = rhs.as_ref() {
                assert_eq!(b.source(), "b");

                return;
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_assign() {
    let input = "= a b";
    let mut pos = Position::new();
    let node = parse_assign(input, &mut pos).unwrap();

    if let Node::Assign(_, lhs, _, rhs) = node.as_ref() {
        if let Node::Token(a) = lhs.as_ref() {
            assert_eq!(a.source(), "a");

            if let Node::Token(b) = rhs.as_ref() {
                assert_eq!(b.source(), "b");

                return;
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_assign_expand() {
    let input = "= [. [a] [b . []]] b";
    let mut pos = Position::new();
    let node = parse_assign(input, &mut pos).unwrap();

    if let Node::Assign(_, lhs, _, rhs) = node.as_ref() {
        if let Node::Expand(.., exprs) = lhs.as_ref() {
            assert_eq!(exprs.len(), 3);

            if let Node::Token(b) = rhs.as_ref() {
                assert_eq!(b.source(), "b");

                return;
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_assign_prop() {
    let input = "= x.y z";
    let mut pos = Position::new();
    let node = parse_assign(input, &mut pos).unwrap();

    if let Node::Assign(_, lhs, _, rhs) = node.as_ref() {
        if let Node::Token(x) = lhs.as_ref() {
            assert_eq!(x.source(), "x");

            if let Node::Token(z) = rhs.as_ref() {
                assert_eq!(z.source(), "z");

                return;
            }
        }
    }
    panic!();
}

#[test]
fn test_parse_define_prop() {
    let input = "let x.y z";
    let mut pos = Position::new();
    assert_eq!(
        parse_assign(input, &mut pos),
        Err(SquareError::SyntaxError(
            input.to_string(),
            "failed to parse_assign, expect WHITESPACE, got Op(.)".to_string(),
            pos,
            None
        ))
    );
}

fn is_binary_op(op: &str) -> bool {
    matches!(
        op,
        "+" | "-" | "*" | "/" | "^" | "%" | "&" | "|" | "==" | "!=" | ">" | "<" | ">=" | "<="
    )
}

fn is_binary_assign_op(op: &str) -> bool {
    matches!(op, "+=" | "-=" | "*=" | "/=" | "^=" | "%=" | "&=" | "|=")
}

fn is_op(op: &str) -> bool {
    is_binary_op(op) || is_binary_assign_op(op)
}

fn parse_op(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_op");
    let operator = wrap(raise_token(input, pos))?;

    wrap(expect_whitespace(input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let mut nodes = vec![];

    match operator.source() {
        op if is_binary_op(op) => {
            nodes.push(parse_expr(input, pos)?);

            wrap(expect_whitespace(input, pos))?;
            wrap(skip_whitespace(input, pos))?;

            nodes.push(parse_expr(input, pos)?);
        }
        op if is_binary_assign_op(op) => {
            let id = wrap(raise_token(input, pos))?;

            if let Token::Id(..) = id {
                nodes.push(Box::new(Node::Token(id)));
            } else {
                return Err(SquareError::SyntaxError(
                    input.to_string(),
                    format!("faield to parse_op, expect identifier, got {}", id),
                    id.pos().clone(),
                    None,
                ));
            }

            nodes.extend(parse_prop_chain(input, pos)?);

            wrap(expect_whitespace(input, pos))?;
            wrap(skip_whitespace(input, pos))?;

            nodes.push(parse_expr(input, pos)?);
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input.to_string(),
                format!(
                    "faield to parse_op, expect operator, got {}",
                    operator.source()
                ),
                operator.pos().clone(),
                None,
            ));
        }
    }

    Ok(Box::new(Node::Op(operator, nodes)))
}

#[test]
fn test_parse_op_binary() {
    let input = "+ a 4";
    let mut pos = Position::new();
    let node = parse_op(input, &mut pos).unwrap();

    if let Node::Op(op, exprs) = node.as_ref() {
        assert_eq!(exprs.len(), 2);
        assert_eq!(op.source(), "+");

        return;
    }

    panic!();
}

#[test]
fn test_parse_op_binary_assign() {
    let input = "+= 42 a";
    let mut pos = Position::new();

    assert_eq!(
        parse_op(input, &mut pos),
        Err(SquareError::SyntaxError(
            input.to_string(),
            "faield to parse_op, expect identifier, got Num(42)".to_string(),
            Position {
                line: 1,
                column: 4,
                cursor: 3
            },
            None,
        ))
    );
}

fn parse_call(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_call");
    let left_bracket = wrap(expect(
        &|token| (token.source() == "[", "expect [".to_string()),
        input,
        pos,
    ))?;

    wrap(skip_whitespace(input, pos))?;

    let mut leading = wrap(lookahead(input, pos))?;
    let mut nodes = vec![];

    if leading.source() == "=" || leading.source() == "let" {
        nodes.push(parse_assign(input, pos)?);
    } else if is_op(leading.source()) {
        nodes.push(parse_op(input, pos)?);
    } else {
        while leading.source() != "]" {
            nodes.push(parse_expr(input, pos)?);
            leading = wrap(lookahead(input, pos))?;

            if leading.source() != "]" {
                wrap(expect_whitespace(input, pos))?;
                wrap(skip_whitespace(input, pos))?;
                leading = wrap(lookahead(input, pos))?;
            }
        }
    };

    wrap(skip_whitespace(input, pos))?;

    let right_bracket = wrap(expect(
        &|token| (token.source() == "]", "expect ]".to_string()),
        input,
        pos,
    ))?;

    Ok(Box::new(Node::Call(left_bracket, right_bracket, nodes)))
}

#[test]
fn test_parse_call_assign() {
    let input = "[= [a b] [42 24]]";
    let mut pos = Position::new();
    let node = parse_call(input, &mut pos).unwrap();

    if let Node::Call(.., exprs) = node.as_ref() {
        if let Node::Assign(_, expansion, ..) = exprs[0].as_ref() {
            if let Node::Expand(.., phs) = expansion.as_ref() {
                if let Node::Token(b) = phs[1].as_ref() {
                    assert_eq!(b.source(), "b");

                    return;
                }
            }
        }
    }

    panic!();
}

fn parse_dot(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_dot");
    let leading = wrap(lookahead(input, pos))?;

    match leading {
        _ if leading.source() == "[" => {
            let call = parse_call(input, pos)?;
            let nodes = parse_prop_chain(input, pos)?;

            if nodes.is_empty() {
                Ok(call)
            } else {
                Ok(Box::new(Node::Dot(call, nodes)))
            }
        }
        _ => {
            let id = wrap(expect(
                &|token| match token {
                    Token::Id(..) => (true, "".to_string()),
                    Token::Str(..) => (true, "".to_string()),
                    _ => (false, "expect ID or STR".to_string()),
                },
                input,
                pos,
            ))?;
            let nodes = parse_prop_chain(input, pos)?;

            if nodes.is_empty() {
                Ok(Box::new(Node::Token(id)))
            } else {
                Ok(Box::new(Node::Dot(Box::new(Node::Token(id)), nodes)))
            }
        }
    }
}

#[test]
fn test_parse_dot_empty() {
    let input = "[= [a b] [42 24]]";
    let mut pos = Position::new();
    let node = parse_dot(input, &mut pos).unwrap();

    if let Node::Call(.., exprs) = node.as_ref() {
        if let Node::Assign(.., expr) = exprs[0].as_ref() {
            if let Node::Call(.., exprs) = expr.as_ref() {
                if let Node::Token(fourty_two) = exprs[0].as_ref() {
                    assert_eq!(fourty_two.source(), "42");

                    return;
                }
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_dot() {
    let input = "[= [a b] [42 24]].foo.bar";
    let mut pos = Position::new();
    let node = parse_dot(input, &mut pos).unwrap();

    if let Node::Dot(_, props) = node.as_ref() {
        if let Node::Prop(_, foo) = props[0].as_ref() {
            assert_eq!(foo.source(), "foo");

            if let Node::Prop(_, bar) = props[1].as_ref() {
                assert_eq!(bar.source(), "bar");

                return;
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_dot_str() {
    let input = "'ss'.foo";
    let mut pos = Position::new();
    let node = parse_dot(input, &mut pos).unwrap();

    if let Node::Dot(_, props) = node.as_ref() {
        if let Node::Prop(_, foo) = props[0].as_ref() {
            assert_eq!(foo.source(), "foo");
            return;
        }
    }

    panic!();
}

#[test]
fn test_parse_dot_id() {
    let input = "ss.foo";
    let mut pos = Position::new();
    let node = parse_dot(input, &mut pos).unwrap();

    if let Node::Dot(_, props) = node.as_ref() {
        if let Node::Prop(_, foo) = props[0].as_ref() {
            assert_eq!(foo.source(), "foo");
            return;
        }
    }

    panic!();
}

fn parse_expr(input: &str, pos: &mut Position) -> ParseResult {
    let wrap = create_wrapper("parse_expr");
    let leading = wrap(lookahead(input, pos))?;

    match leading {
        _ if leading.source() == "/[" => parse_fn(input, pos),
        _ if leading.source() == "-" => {
            let minus = wrap(raise_token(input, pos))?;
            if let Token::Num(..) = wrap(lookahead(input, pos))? {
                let num = wrap(raise_token(input, pos))?;
                Ok(Box::new(Node::Token(Token::Num(
                    leading.pos().clone(),
                    format!("-{}", num.source()),
                ))))
            } else {
                let target = parse_dot(input, pos)?;
                Ok(Box::new(Node::Op(
                    minus,
                    vec![
                        // convert -x into 0 - 42
                        Box::new(Node::Token(Token::Num(
                            leading.pos().clone(),
                            "0".to_string(),
                        ))),
                        target,
                    ],
                )))
            }
        }
        Token::Num(..) => Ok(Box::new(Node::Token(raise_token(input, pos)?))),
        _ => parse_dot(input, pos),
    }
}

pub fn parse(input: &str, pos: &mut Position) -> Result<Vec<Box<Node>>, SquareError> {
    let wrap = create_wrapper("parse");
    let mut ast = vec![];

    wrap(skip_whitespace(input, pos))?;

    loop {
        if let Token::Eof(_) = wrap(lookahead(input, pos))? {
            break;
        }

        ast.push(parse_expr(input, pos)?);
        wrap(skip_whitespace(input, pos))?;
    }

    Ok(ast)
}
