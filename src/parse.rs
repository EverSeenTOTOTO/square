use alloc::{boxed::Box, format, vec, vec::Vec};

use crate::code_frame::Position;
use crate::errors::ParseError;
use crate::scan::{expect, raise_token, skip_whitespace, Token, TokenName};

type ParseResult<'a> = Result<Box<Node>, ParseError<'a>>;

#[derive(Debug)]
pub enum Node {
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
    let left_bracket = expect("[", input, pos)?;
    let mut nodes = vec![];

    skip_whitespace(input, pos)?;

    let mut token = raise_token(input, pos)?;

    loop {
        let source = token.source(input);

        match source {
            "." | "..." => {
                nodes.push(Box::new(Node::Token(token)));
            }
            "[" => {
                *pos = token.start_pos;
                nodes.push(parse_expand(input, pos)?);
            }
            "]" => {
                break;
            }
            _ => {
                if token.name == TokenName::ID {
                    nodes.push(Box::new(Node::Token(token)));
                } else {
                    return Err(ParseError::SyntaxError(
                        input,
                        format!(
                            "faield to parse_expand, expect identifier or placeholders, got {}",
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
