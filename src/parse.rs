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
use crate::scan::{
    expect_name, expect_source, lookahead, raise_token, skip_whitespace, RaiseResult, Token,
    TokenName,
};

type ParseResult<'a> = Result<Box<Node>, SquareError<'a>>;

#[derive(Debug, PartialEq)]
pub enum Node {
    Empty(),
    Token(Token),                         // literal
    Expand(Token, Token, Vec<Box<Node>>), // '[', ']', placeholders*
    Fn(Token, Box<Node>, Box<Node>),      // '/', params, body
    Assign(Token, Box<Node>, Box<Node>),  // '=', expansion, expression
    Op(Token, Vec<Box<Node>>),            // operator, expressions+
    Call(Token, Token, Vec<Box<Node>>),   // '[', ']', expressions+
    Dot(Box<Node>, Vec<Box<Node>>),       // instance, (dot property)*
}

fn nodevec_to_string(vector: &Vec<Box<Node>>) -> String {
    return match vector.is_empty() {
        true => "Empty".to_string(),
        _ => vector
            .iter()
            .map(|node| format!("{}", node))
            .collect::<Vec<String>>()
            .join(", "),
    };
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Empty() => write!(f, "Empty"),
            Node::Token(token) => write!(f, "Token({})", token),
            Node::Expand(_, _, placeholders) => {
                write!(f, "Expand({})", nodevec_to_string(placeholders))
            }
            Node::Fn(_, params, body) => {
                write!(f, "Fn({}, {})", params, body)
            }
            Node::Assign(_, expansion, expression) => {
                write!(f, "Assign({} {})", expansion, expression)
            }
            Node::Op(operator, expressions) => {
                write!(f, "Op({}, {})", operator, nodevec_to_string(expressions))
            }
            Node::Call(_, _, expressions) => {
                write!(f, "Call({})", nodevec_to_string(expressions))
            }
            Node::Dot(instance, properties) => {
                write!(f, "Dot({} {})", instance, nodevec_to_string(properties))
            }
        }
    }
}

// wrap scanner error to parser error
fn create_wrapper<'a>(fn_name: &'static str) -> Box<dyn Fn(RaiseResult<'a>) -> RaiseResult<'a>> {
    return Box::new(move |result: RaiseResult<'a>| {
        if let Err(SquareError::UnexpectedToken(input, message, position)) = result {
            return Err(SquareError::SyntaxError(
                input,
                format!("failed to {}, {}", fn_name, message),
                position,
                None,
            ));
        }

        result
    });
}

pub fn parse_expand<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_expand");

    let left_bracket = wrap(expect_source("[", input, pos))?;

    wrap(skip_whitespace(input, pos))?;

    let mut token = wrap(raise_token(input, pos))?;
    let mut nodes = vec![];

    loop {
        match token.source.as_str() {
            "." | "..." => {
                nodes.push(Box::new(Node::Token(token)));

                if wrap(lookahead(input, pos))?.source != "]" {
                    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
                }
            }
            "[" => {
                *pos = token.pos;
                nodes.push(parse_expand(input, pos)?);
            }
            "]" => break,
            _ => {
                if token.name == TokenName::ID {
                    nodes.push(Box::new(Node::Token(token)));

                    if wrap(lookahead(input, pos))?.source != "]" {
                        wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
                    }
                } else {
                    return Err(SquareError::SyntaxError(
                        input,
                        format!(
                            "faield to parse_expand, expect identifier or placeholders, got '{}'",
                            token.source
                        ),
                        token.pos,
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
    let mut pos = Position::default();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(_, _, phs) = *node {
        assert_eq!(phs.len(), 0);

        return;
    }

    panic!();
}

#[test]
fn test_parse_expand_base() {
    let input = "[ x ;comment; y z   \t ]";
    let mut pos = Position::default();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(_, _, phs) = *node {
        assert_eq!(phs.len(), 3);

        return;
    }

    panic!();
}

#[test]
fn test_parse_expand_placehoders() {
    let input = "[. ... . ... .]";
    let mut pos = Position::default();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(_, _, phs) = *node {
        assert_eq!(phs.len(), 5);
        return;
    }

    panic!();
}

#[test]
fn test_parse_expand_nested() {
    let input = "[. [ x [ ;comment; y] [z ... ]] ... s  ]";
    let mut pos = Position::default();
    let node = parse_expand(input, &mut pos).unwrap();

    if let Node::Expand(_, _, phs) = *node {
        assert_eq!(phs.len(), 4);

        if let Node::Expand(_, _, ref phs2) = *phs[1] {
            assert_eq!(phs2.len(), 3);

            if let Node::Expand(_, _, ref phs3) = *phs2[1] {
                if let Node::Token(ref y) = *phs3[0] {
                    assert_eq!(y.source, "y");

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
    let mut pos = Position::default();
    let mut err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(SquareError::SyntaxError(
            input,
            "failed to parse_expand, expect WHITESPACE, got ID: 'x'".to_string(),
            pos,
            None
        ))
    );

    input = "[....]";
    pos = Position::default();
    err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(SquareError::SyntaxError(
            input,
            "failed to parse_expand, expect WHITESPACE, got OP: '.'".to_string(),
            pos,
            None
        ))
    );

    input = "[x....]";
    pos = Position::default();
    err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(SquareError::SyntaxError(
            input,
            "failed to parse_expand, expect WHITESPACE, got OP: '...'".to_string(),
            pos,
            None
        ))
    );
}

pub fn parse_fn<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_fn");

    let slash = wrap(expect_source("/", input, pos))?;
    let expand = parse_expand(input, pos)?;

    wrap(skip_whitespace(input, pos))?;

    let expr = parse_expr(input, pos)?;

    Ok(Box::new(Node::Fn(slash, expand, expr)))
}

#[test]
fn test_parse_fn_base() {
    let input = "/[] 42";
    let mut pos = Position::default();
    let node = parse_fn(input, &mut pos).unwrap();

    if let Node::Fn(_, params, body) = *node {
        if let Node::Expand(_, _, phs) = *params {
            assert_eq!(phs.len(), 0);
        }

        if let Node::Token(value) = *body {
            assert_eq!(value.source, "42");

            return;
        }
    }

    panic!();
}

#[test]
fn test_parse_fn_params() {
    let input = "/[. [ x [ ;comment; y] [z ... ]] ... s  ] 42";
    let mut pos = Position::default();
    let node = parse_fn(input, &mut pos).unwrap();

    if let Node::Fn(_, params, body) = *node {
        if let Node::Expand(_, _, phs) = *params {
            assert_eq!(phs.len(), 4);

            if let Node::Expand(_, _, ref phs2) = *phs[1] {
                assert_eq!(phs2.len(), 3);

                if let Node::Expand(_, _, ref phs3) = *phs2[1] {
                    if let Node::Token(ref y) = *phs3[0] {
                        assert_eq!(y.source, "y");
                    }
                }
            }
        }

        if let Node::Token(value) = *body {
            assert_eq!(value.source, "42");

            return;
        }
    }

    panic!();
}

#[test]
fn test_parse_fn_high_order() {
    let input = "/[x] /[y] /[z] [+ [- x y] [* y z]]";
    let mut pos = Position::default();
    let node = parse_fn(input, &mut pos).unwrap();

    if let Node::Fn(_, x_param, x_body) = *node {
        if let Node::Expand(_, _, x_phs) = *x_param {
            if let Node::Token(ref x) = *x_phs[0] {
                assert_eq!(x.source, "x");
            }
        }

        if let Node::Fn(_, y_param, y_body) = *x_body {
            if let Node::Expand(_, _, y_phs) = *y_param {
                if let Node::Token(ref y) = *y_phs[0] {
                    assert_eq!(y.source, "y");
                }
            }

            if let Node::Fn(_, z_param, z_body) = *y_body {
                if let Node::Expand(_, _, z_phs) = *z_param {
                    if let Node::Token(ref z) = *z_phs[0] {
                        assert_eq!(z.source, "z");
                    }
                }

                // println!("{}", z_body);

                if let Node::Call(_, _, expressions) = *z_body {
                    if let Node::Token(ref plus) = *expressions[0] {
                        assert_eq!(plus.source, "+");
                    }

                    if let Node::Call(_, _, ref exprs) = *expressions[2] {
                        if let Node::Token(ref z) = *exprs[2] {
                            assert_eq!(z.source, "z");

                            return;
                        }
                    }
                }
            }
        }
    }

    panic!();
}

pub fn parse_assign<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_assign");
    let eq = wrap(expect_source("=", input, pos))?;

    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let token = wrap(raise_token(input, pos))?;

    let target = if token.source == "[" {
        *pos = token.pos;
        parse_expand(input, pos)?
    } else if token.name == TokenName::ID {
        Box::new(Node::Token(token))
    } else {
        return Err(SquareError::SyntaxError(
            input,
            format!(
                "faield to parse_assign, expect identifier or expansion, got '{}'",
                token.source
            ),
            token.pos,
            None,
        ));
    };

    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let expr = parse_expr(input, pos)?;

    Ok(Box::new(Node::Assign(eq, target, expr)))
}

pub fn parse_op<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_op");
    let operator = wrap(raise_token(input, pos))?;

    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let mut nodes = vec![];

    match operator.source.as_str() {
        // binary
        "+" | "-" | "*" | "/" | "+=" | "-=" | "*=" | "/=" | "^" | "%" | "&" | "|" | "^=" | "%="
        | "&=" | "|=" | "==" | "!=" | ">" | "<" | ">=" | "<=" | ".." => {
            nodes.push(parse_expr(input, pos)?);

            wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
            wrap(skip_whitespace(input, pos))?;

            nodes.push(parse_expr(input, pos)?);
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input,
                format!(
                    "faield to parse_op, expect operator, got '{}'",
                    operator.source
                ),
                operator.pos,
                None,
            ));
        }
    }

    Ok(Box::new(Node::Op(operator, nodes)))
}

pub fn parse_call<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_call");
    let left_bracket = wrap(expect_source("[", input, pos))?;

    wrap(skip_whitespace(input, pos))?;

    let mut leading = wrap(lookahead(input, pos))?;
    let mut nodes = vec![];

    let expr = if leading.source == "=" {
        parse_assign(input, pos)?
    } else if leading.name == TokenName::OP {
        parse_op(input, pos)?
    } else {
        while leading.source != "]" {
            nodes.push(parse_expr(input, pos)?);
            leading = wrap(lookahead(input, pos))?;

            if leading.source != "]" {
                wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
                wrap(skip_whitespace(input, pos))?;
                leading = wrap(lookahead(input, pos))?;
            }
        }
        Box::new(Node::Empty())
    };

    wrap(skip_whitespace(input, pos))?;

    let right_bracket = wrap(expect_source("]", input, pos))?;

    if let Node::Assign(eq, expansion, expression) = *expr {
        nodes.insert(0, Box::new(Node::Token(eq)));
        nodes.push(expansion);
        nodes.push(expression);

        Ok(Box::new(Node::Call(left_bracket, right_bracket, nodes)))
    } else if let Node::Op(op, mut expressions) = *expr {
        expressions.insert(0, Box::new(Node::Token(op)));
        Ok(Box::new(Node::Call(
            left_bracket,
            right_bracket,
            expressions,
        )))
    } else {
        Ok(Box::new(Node::Call(left_bracket, right_bracket, nodes)))
    }
}

pub fn parse_lit<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_lit");
    let leading = wrap(raise_token(input, pos))?;

    match leading.name {
        TokenName::STR | TokenName::NUM => Ok(Box::new(Node::Token(leading))),
        _ => Err(SquareError::SyntaxError(
            input,
            format!(
                "faield to parse_lit, expect literal, got '{}'",
                leading.source
            ),
            leading.pos,
            None,
        )),
    }
}

pub fn parse_dot<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_dot");
    let leading = wrap(lookahead(input, pos))?;

    let node = if leading.source == "[" {
        parse_call(input, pos)?
    } else if leading.name == TokenName::ID {
        wrap(raise_token(input, pos))?;
        Box::new(Node::Token(leading))
    } else {
        parse_lit(input, pos)?
    };

    let mut nodes = vec![];

    loop {
        let backup = pos.clone();
        wrap(skip_whitespace(input, pos))?;
        let dot = wrap(lookahead(input, pos))?;

        if dot.source != "." {
            *pos = backup;
            break;
        }

        wrap(raise_token(input, pos))?;
        nodes.push(Box::new(Node::Token(dot)));

        let prop = wrap(expect_name(TokenName::ID, input, pos))?;

        nodes.push(Box::new(Node::Token(prop)));
    }

    if nodes.len() == 0 {
        Ok(node) // avoid unnecessary layer
    } else {
        Ok(Box::new(Node::Dot(node, nodes)))
    }
}

pub fn parse_expr<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_expr");
    let leading = wrap(lookahead(input, pos))?;

    if leading.source == "/" {
        return parse_fn(input, pos);
    } else {
        return parse_dot(input, pos);
    }
}
