use alloc::{boxed::Box, format, vec, vec::Vec};

use crate::code_frame::Position;
use crate::errors::ParseError;
use crate::scan::{
    expect_name, expect_source, lookahead, raise_token, skip_whitespace, Token, TokenName,
};

type ParseResult<'a> = Result<Box<Node>, ParseError<'a>>;

#[derive(Debug, PartialEq)]
pub enum Node {
    Error(),
    Token(Token),
    Expand(Token, Token, Vec<Box<Node>>),
    Fn(Token, Box<Node>, Box<Node>),
    Assign(Token, Box<Node>, Box<Node>),
    Op(Token, Vec<Box<Node>>),
    Call(Token, Token, Box<Node>, Vec<Box<Node>>),
    Dot(Token, Box<Node>, Vec<Box<Node>>),
    Expr(Box<Node>),
}

pub fn parse_expand<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let left_bracket = expect_source("[", input, pos)?;
    let mut nodes = vec![];

    skip_whitespace(input, pos)?;

    let mut token = raise_token(input, pos)?;

    loop {
        let source = token.source(input);

        match source {
            "." | "..." => {
                nodes.push(Box::new(Node::Token(token)));

                if lookahead(input, pos)?.source(input) != "]" {
                    expect_name(TokenName::WHITESPACE, input, pos)?;
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

                    if lookahead(input, pos)?.source(input) != "]" {
                        expect_name(TokenName::WHITESPACE, input, pos)?;
                    }
                } else {
                    return Err(ParseError::SyntaxError(
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

        skip_whitespace(input, pos)?;

        token = raise_token(input, pos)?;
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
        Err(ParseError::UnexpectedToken(
            input,
            "expect WHITESPACE, got ID: 'x'".to_string(),
            pos
        ))
    );

    input = "[....]";
    pos = Position::default();
    err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(ParseError::UnexpectedToken(
            input,
            "expect WHITESPACE, got OP: '.'".to_string(),
            pos
        ))
    );

    input = "[x....]";
    pos = Position::default();
    err = parse_expand(input, &mut pos);

    assert_eq!(
        err,
        Err(ParseError::UnexpectedToken(
            input,
            "expect WHITESPACE, got OP: '...'".to_string(),
            pos
        ))
    );
}

// pub fn parse_fn<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
//     let slash = expect("/", input, pos)?;
//     let expand = parse_expand(input, pos)?;
//
//     skip_whitespace(input, pos);
//
//     let call = parse_call(input, pos)?;
//
//     Ok(Box::new(Node::Fn(slash, expand, call)))
// }
//
// pub fn parse_assign<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
//     let eq = expect("=", input, pos)?;
//
//     raise_whitespace(input, pos);
//     skip_whitespace(input, pos);
//
//     let mut target = Box::new(Node::Error());
//     let token = raise_token(input, pos)?;
//     let source = token.source(input);
//
//     if source == "[" {
//         *pos = token.start_pos;
//         target = parse_expand(input, pos)?;
//     } else if token.name == TokenName::ID {
//         target = Box::new(Node::Token(token));
//     } else {
//         return Err(ParseError::SyntaxError(
//             input,
//             format!(
//                 "faield to parse_assign, expect identifier or expansion, got '{}'",
//                 source
//             ),
//             token.start_pos,
//             None,
//         ));
//     }
//
//     raise_whitespace(input, pos);
//     skip_whitespace(input, pos);
//
//     let expr = parse_expr(input, pos)?;
//
//     Ok(Box::new(Node::Assign(eq, target, expr)))
// }
//
// pub fn parse_op<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
//     let operator = raise_token(input, pos)?;
//
//     raise_whitespace(input, pos);
//     skip_whitespace(input, pos);
//
//     let source = operator.source(input);
//     let mut nodes = vec![];
//
//     match source {
//         // binary
//         "+" | "-" | "*" | "/" | "+=" | "-=" | "*=" | "/=" | "^" | "%" | "&" | "|" | "^=" | "%="
//         | "&=" | "|=" | "==" | "!=" | ">" | "<" | ">=" | "<=" | ".." => {
//             nodes.push(parse_expr(input, pos)?);
//
//             raise_whitespace(input, pos);
//             skip_whitespace(input, pos);
//
//             nodes.push(parse_expr(input, pos)?);
//         }
//         // unary
//         "!" => {
//             nodes.push(parse_expr(input, pos)?);
//         }
//         _ => {
//             return Err(ParseError::SyntaxError(
//                 input,
//                 format!("faield to parse_op, expect operator, got '{}'", source),
//                 operator.start_pos,
//                 None,
//             ));
//         }
//     }
//
//     Ok(Box::new(Node::Op(operator, nodes)))
// }
//
// pub fn parse_call<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
//     let left_bracket = expect("[", input, pos)?;
//
//     skip_whitespace(input, pos);
//
//     let mut leading = raise_token(input, pos)?;
//     let mut caller = Box::new(Node::Error());
//     let mut nodes = vec![];
//
//     if leading.source(input) == "=" {
//         *pos = leading.start_pos;
//         caller = parse_assign(input, pos)?;
//     } else if leading.name == TokenName::OP {
//         *pos = leading.start_pos;
//         caller = parse_op(input, pos)?;
//     } else {
//         loop {
//             if leading.source(input) == "]" {
//                 *pos = leading.start_pos;
//                 break;
//             }
//
//             nodes.push(parse_expr(input, pos)?);
//
//             raise_whitespace(input, pos);
//             skip_whitespace(input, pos);
//         }
//     }
//
//     skip_whitespace(input, pos);
//
//     let right_bracket = expect("]", input, pos)?;
//
//     Ok(Box::new(Node::Call(
//         left_bracket,
//         right_bracket,
//         caller,
//         nodes,
//     )))
// }
