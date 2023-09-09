use crate::{code_frame::Position, errors::*};

#[cfg(not(test))]
use alloc::{boxed::Box, string::String};
#[cfg(test)]
use std::{boxed::Box, string::String};

#[cfg(not(test))]
use core::{fmt, str::Chars};
use lazy_static::lazy_static;

type RaiseResult<'a> = Result<Token<'a>, ParseError<'a>>;

#[derive(Debug, PartialEq)]
pub enum TokenName {
    COMMENT,
    EOF,
    ID,
    NUM,
    OP,
    STR,
    WHITESPACE,
}

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
    pub name: TokenName,
    pub source: &'a str,
    pub position: Position,
}

impl<'a> Token<'a> {
    pub fn new(name: TokenName, source: &'a str, position: Position) -> Self {
        Self {
            name,
            source,
            position,
        }
    }
}

pub fn make_token<'a>(name: TokenName, source: &'a str, pos: &mut Position) -> Token<'a> {
    let snapshot = pos.clone();

    pos.column += source.len();
    pos.cursor += source.len();

    Token::new(name, source, snapshot)
}

#[test]
fn test_make_token() {
    let mut pos = Position::new(1, 1, 1);
    let token = make_token(TokenName::STR, &"hello world"[1..3], &mut pos);

    let expected_token = Token {
        name: TokenName::STR,
        source: "el",
        position: Position {
            line: 1,
            column: 1,
            cursor: 1,
        },
    };
    let expected_pos = Position::new(1, 3, 3);

    assert_eq!(token, expected_token);
    assert_eq!(pos, expected_pos);
}

pub fn is_whitespace<'a>(input: &'a str, index: usize) -> bool {
    if let Some(c) = input.chars().nth(index) {
        return c.is_whitespace();
    }
    return false;
}

pub fn raise_token<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    if pos.cursor >= input.len() {
        return Ok(make_token(TokenName::EOF, "", pos));
    }

    let pivot = &input[pos.cursor..=pos.cursor];
    let first = pivot
        .chars()
        .next()
        .ok_or_else(|| ParseError::UnexpectedToken(input, pos.clone()))?;

    match first {
        _ if first == '\'' => raise_string(input, pos),
        _ => unimplemented!(),
        // _ if first == ';' => raise_comment(input, pos),
        // _ if first.is_digit(10) => raise_number(input, pos),
        // _ if first.is_alphabetic() || first == '_' => raise_ident(input, pos),
        // _ if first.is_whitespace() => raise_whitespace(input, pos),
        // _ => raise_operator(input, pos),
    }
}

pub fn raise_string<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input.chars();
    let mut string = String::new();

    chars.next(); // skip first \'

    loop {
        match chars.next() {
            Some('\'') => break,
            Some(ch) => string.push(ch),
            None => return Err(ParseError::UnexpectedToken(input, pos.clone())),
        }
    }

    let string_ref: &'a str = Box::leak(string.into_boxed_str());

    Ok(make_token(TokenName::STR, string_ref, pos))
}

#[test]
fn test_raise_string_single_quote() {
    let input = "'hello'";
    let expected_output = Ok(Token {
        name: TokenName::STR,
        source: "hello",
        position: Position::default(),
    });

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

// lazy_static! {
//     static ref NUMBER_PATTERN: Regex = Regex::new("[0-9]+").unwrap();
// }
//
// pub fn raise_number<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
//     let source = NUMBER_PATTERN
//         .find(&input[pos.cursor..])
//         .ok_or_else(|| ParseError::UnexpectedToken(input, pos.clone()))?
//         .as_str();
//     Ok(make_token(TokenName::NUM, source, pos))
// }
//
// lazy_static! {
//     static ref ID_PATTERN: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
// }
//
// pub fn raise_ident<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
//     let source = ID_PATTERN
//         .find(&input[pos.cursor..])
//         .ok_or_else(|| ParseError::UnexpectedToken(input, pos.clone()))?
//         .as_str();
//
//     Ok(make_token(TokenName::ID, source, pos))
// }
//
// lazy_static! {
//     static ref OP_PATTERN: Regex =
//         Regex::new(r#"^(\.\.?\.?)|(/\[)|=|!|-|\+|\*|/|<|>|\^|%|\[|\]"#).unwrap();
// }
//
// pub fn raise_operator<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
//     let source = OP_PATTERN
//         .find(&input[pos.cursor..])
//         .ok_or_else(|| ParseError::UnexpectedToken(input, pos.clone()))?
//         .as_str();
//
//     Ok(make_token(TokenName::OP, source, pos))
// }
//
// lazy_static! {
//     static ref COMMENT_REGEX: Regex = Regex::new(r#";([^;\\\r\n]|\\;)*;?"#).unwrap();
// }
//
// pub fn raise_comment<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
//     let source = COMMENT_REGEX
//         .find(&input[pos.cursor..])
//         .ok_or_else(|| ParseError::UnexpectedToken(input, pos.clone()))?
//         .as_str();
//     Ok(make_token(TokenName::COMMENT, source, pos))
// }
//
// pub fn raise_whitespace<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
//     let mut backup = pos.clone();
//
//     if !is_whitespace(&input, pos.cursor) {
//         return Err(ParseError::UnexpectedToken(input, backup));
//     }
//
//     if &input[pos.cursor..pos.cursor + 1] == "\n" {
//         pos.line += 1;
//         pos.column = 1;
//     } else {
//         pos.column += 1;
//     }
//     pos.cursor += 1;
//
//     Ok(make_token(
//         TokenName::WHITESPACE,
//         &input[backup.cursor..pos.cursor],
//         &mut backup,
//     ))
// }
//
// pub fn skip_whitespace<'a>(input: &'a str, pos: &mut Position) -> Result<(), ParseError<'a>> {
//     while let Some(c) = input.chars().nth(pos.cursor) {
//         if c.is_whitespace() {
//             raise_whitespace(input, pos)?;
//         } else if c == ';' {
//             raise_comment(input, pos)?;
//         } else {
//             break;
//         }
//     }
//
//     Ok(())
// }
//
// pub fn lookahead<'a>(input: &'a str, pos: &mut Position, count: usize) -> RaiseResult<'a> {
//     let backup = pos.clone();
//     let mut token = raise_token(input, pos);
//
//     for _ in 1..count {
//         token = raise_token(input, pos);
//     }
//
//     *pos = backup;
//
//     return token;
// }
//
// pub fn expect<'a>(expected: &'a str, input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
//     let token = raise_token(input, pos)?;
//
//     if token.source != expected {
//         *pos = token.position;
//
//         return Err(ParseError::UnexpectedToken(input, pos.clone()));
//     }
//
//     Ok(token)
// }
