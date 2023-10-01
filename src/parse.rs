use alloc::{boxed::Box, format, vec, vec::Vec};

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
    Token(Token),                                  // literal
    Expand(Token, Token, Vec<Box<Node>>),          // '[', ']', placeholders*
    Fn(Token, Box<Node>, Box<Node>),               // '/', params, body
    Assign(Token, Box<Node>, Box<Node>),           // '=', expansion, expression
    Op(Token, Vec<Box<Node>>),                     // operator, expressions+
    Call(Token, Token, Box<Node>, Vec<Box<Node>>), // '[', ']', caller, callee*
    Dot(Box<Node>, Vec<Box<Node>>),                // instance, (dot property)*
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Empty() => write!(f, "Empty"),
            Node::Token(token) => write!(f, "Token({})", token.name),
            Node::Expand(_, _, placeholders) => {
                write!(f, "Expand({:?})", placeholders)
            }
            Node::Fn(_, params, body) => {
                write!(f, "Fn({}, {})", params, body)
            }
            Node::Assign(_, expansion, expression) => {
                write!(f, "Assign({} {})", expansion, expression)
            }
            Node::Op(_, expressions) => write!(f, "Op({:?})", expressions),
            Node::Call(_, _, _, callee) => {
                write!(f, "Call({:?})", callee)
            }
            Node::Dot(instance, properties) => {
                write!(f, "Dot({} {:?})", instance, properties)
            }
        }
    }
}

// wrap scanner error to parser error
fn create_wrapper<'a>(
    fn_name: &'static str,
) -> Box<dyn Fn(RaiseResult<'a>) -> RaiseResult<'a> + 'a> {
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
        let source = token.source(input);

        match source {
            "." | "..." => {
                nodes.push(Box::new(Node::Token(token)));

                if wrap(lookahead(input, pos))?.source(input) != "]" {
                    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
                }
            }
            "[" => {
                *pos = token.start_pos;
                nodes.push(parse_expand(input, pos)?);
            }
            "]" => break,
            _ => {
                if token.name == TokenName::ID {
                    nodes.push(Box::new(Node::Token(token)));

                    if wrap(lookahead(input, pos))?.source(input) != "]" {
                        wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
                    }
                } else {
                    return Err(SquareError::SyntaxError(
                        input,
                        format!(
                            "faield to parse_expand, expect identifier or placeholders, got '{}'",
                            source
                        ),
                        token.start_pos,
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
                    assert_eq!(y.source(input), "y");

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

        if let Node::Dot(inst, _) = *body {
            if let Node::Token(value) = *inst {
                assert_eq!(value.source(input), "42");

                return;
            }
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
                        assert_eq!(y.source(input), "y");
                    }
                }
            }
        }

        if let Node::Dot(inst, _) = *body {
            if let Node::Token(value) = *inst {
                assert_eq!(value.source(input), "42");

                return;
            }
        }
    }

    panic!();
}

// #[test]
// fn test_parse_fn_high_order() {
//     let input = "/[x] /[y] /[z] [+ [+ x y] z]";
//     let mut pos = Position::default();
//     let node = parse_fn(input, &mut pos).unwrap();
//
//     if let Node::Fn(_, x_param, x_body) = *node {
//         if let Node::Expand(_, _, x_phs) = *x_param {
//             if let Node::Token(ref x) = *x_phs[0] {
//                 assert_eq!(x.source(input), "x");
//             }
//         }
//
//         if let Node::Fn(_, y_param, y_body) = *x_body {
//             if let Node::Expand(_, _, y_phs) = *y_param {
//                 if let Node::Token(ref y) = *y_phs[0] {
//                     assert_eq!(y.source(input), "y");
//                 }
//             }
//
//             if let Node::Fn(_, z_param, z_body) = *y_body {
//                 if let Node::Expand(_, _, z_phs) = *z_param {
//                     if let Node::Token(ref z) = *z_phs[0] {
//                         assert_eq!(z.source(input), "z");
//                     }
//                 }
//
//                 if let Node::Dot(inst, _) = *z_body {
//                     if let Node::Token(value) = *inst {
//                         assert_eq!(value.source(input), "42");
//
//                         return;
//                     }
//                 }
//             }
//         }
//     }
//
//     panic!();
// }

pub fn parse_assign<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_assign");
    let eq = wrap(expect_source("=", input, pos))?;

    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let token = wrap(raise_token(input, pos))?;
    let source = token.source(input);

    let target = if source == "[" {
        *pos = token.start_pos;
        parse_expand(input, pos)?
    } else if token.name == TokenName::ID {
        Box::new(Node::Token(token))
    } else {
        return Err(SquareError::SyntaxError(
            input,
            format!(
                "faield to parse_assign, expect identifier or expansion, got '{}'",
                source
            ),
            token.start_pos,
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

    let source = operator.source(input);
    let mut nodes = vec![];

    match source {
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
                format!("faield to parse_op, expect operator, got '{}'", source),
                operator.start_pos,
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

    let caller = if leading.source(input) == "=" {
        parse_assign(input, pos)?
    } else if leading.name == TokenName::OP {
        parse_op(input, pos)?
    } else {
        while leading.source(input) != "]" {
            nodes.push(parse_expr(input, pos)?);
            leading = wrap(lookahead(input, pos))?;

            if leading.source(input) != "]" {
                wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
                wrap(skip_whitespace(input, pos))?;
                leading = wrap(lookahead(input, pos))?;
            }
        }
        Box::new(Node::Empty())
    };

    wrap(skip_whitespace(input, pos))?;

    let right_bracket = wrap(expect_source("]", input, pos))?;

    Ok(Box::new(Node::Call(
        left_bracket,
        right_bracket,
        caller,
        nodes,
    )))
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
                leading.source(input)
            ),
            leading.start_pos,
            None,
        )),
    }
}

pub fn parse_dot<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_dot");
    let leading = wrap(lookahead(input, pos))?;
    let source = leading.source(input);

    let node = if source == "[" {
        parse_call(input, pos)?
    } else if leading.name == TokenName::ID {
        Box::new(Node::Token(leading))
    } else {
        parse_lit(input, pos)?
    };

    let mut nodes = vec![];

    wrap(skip_whitespace(input, pos))?;

    loop {
        let dot = wrap(lookahead(input, pos))?;

        if dot.source(input) != "." {
            break;
        }

        nodes.push(Box::new(Node::Token(dot)));

        let prop = wrap(expect_name(TokenName::ID, input, pos))?;

        nodes.push(Box::new(Node::Token(prop)));
        wrap(skip_whitespace(input, pos))?;
    }

    Ok(Box::new(Node::Dot(node, nodes)))
}

pub fn parse_expr<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_expr");
    let leading = wrap(lookahead(input, pos))?;
    let source = leading.source(input);

    if source == "/" {
        return parse_fn(input, pos);
    } else {
        return parse_dot(input, pos);
    }
}
