use crate::{code_frame::Position, errors::*};

#[cfg(not(test))]
use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
};

#[cfg(test)]
use std::{boxed::Box, string::String};

use core::{fmt, str::Chars};

type RaiseResult<'a> = Result<Token, ParseError<'a>>;

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
pub struct Token {
    pub name: TokenName,
    pub start_pos: Position,
    pub end_pos: Position,
}

impl Token {
    pub fn new(name: TokenName, start_pos: Position, end_pos: Position) -> Self {
        Self {
            name,
            start_pos,
            end_pos,
        }
    }

    pub fn source<'a>(&self, input: &'a str) -> &'a str {
        return &input[self.start_pos.cursor..self.end_pos.cursor];
    }
}

pub fn raise_token<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    if pos.cursor >= input.len() {
        return Ok(Token::new(TokenName::EOF, pos.clone(), pos.clone()));
    }

    let mut chars = input[pos.cursor..].chars().peekable();

    match chars.peek() {
        Some('\'') => raise_string(input, pos),
        Some(';') => raise_comment(input, pos),
        Some(ch) => match ch {
            _ if ch.is_alphabetic() || *ch == '_' => raise_ident(input, pos),
            _ if ch.is_digit(10) => raise_number(input, pos),
            _ if ch.is_whitespace() => raise_whitespace(input, pos),
            _ => raise_operator(input, pos),
        },
        None => {
            return Err(ParseError::UnexpectedToken(
                input,
                "early eof".to_string(),
                pos.clone(),
            ))
        }
    }
}

pub fn raise_string<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    chars.next(); // skip first \'
    pos.advance();

    while let Some(ch) = chars.next() {
        match ch {
            '\'' => {
                pos.advance();
                break; // terminate
            }
            '\n' => {
                return Err(ParseError::UnexpectedToken(
                    input,
                    "unterminated string, found newline".to_string(),
                    pos.clone(),
                ))
            }
            '\\' => {
                pos.advance();

                match chars.peek() {
                    Some(_) => {
                        chars.next();
                        pos.advance();
                    }
                    None => {
                        return Err(ParseError::UnexpectedToken(
                            input,
                            "bad escape".to_string(),
                            pos.clone(),
                        ))
                    }
                }
            }
            _ => {
                pos.advance();
            }
        }
    }
    Ok(Token::new(TokenName::STR, start_pos, pos.clone()))
}

#[test]
fn test_raise_string() {
    let input = "'hello'";
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "'hello'");
}

#[test]
fn test_raise_string_escaped() {
    let input = "'he\\'llo'";
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "'he\\'llo'");
}

#[test]
fn test_raise_string_escaped2() {
    let input = "'he\\\\'llo'";
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "'he\\\\'");
}

#[test]
fn test_raise_string_unterminated() {
    let input = "'he\nllo";
    let expected_output = Err(ParseError::UnexpectedToken(
        input,
        "unterminated string, found newline".to_string(),
        Position::new(1, 4, 3),
    ));

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

pub fn raise_number<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    while let Some(ch) = chars.peek() {
        if !ch.is_digit(10) {
            break;
        }

        chars.next();
        pos.advance();
    }

    Ok(Token::new(TokenName::NUM, start_pos, pos.clone()))
}

#[test]
fn test_raise_number() {
    let input = "012'34";
    let token = raise_number(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "012");
}

pub fn raise_ident<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    while let Some(ch) = chars.peek() {
        if !(ch.is_alphabetic() || *ch == '_') {
            break;
        }

        chars.next();
        pos.advance();
    }

    Ok(Token::new(TokenName::ID, start_pos, pos.clone()))
}

#[test]
fn test_raise_id() {
    let input = "_demo";
    let token = raise_ident(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "_demo");
}

pub fn raise_operator<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    if let Some(ch) = chars.next() {
        pos.advance();

        match ch {
            '[' | ']' => {}
            '+' | '-' | '*' | '^' | '%' | '&' | '|' | '=' | '!' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    pos.advance();
                }
                _ => {}
            },
            '/' => match chars.peek() {
                Some('[') | Some('=') => {
                    chars.next();
                    pos.advance();
                }
                _ => {}
            },
            '.' => match chars.peek() {
                Some('.') => {
                    chars.next();
                    pos.advance();

                    match chars.peek() {
                        Some('.') => {
                            chars.next();
                            pos.advance();
                        }
                        _ => {}
                    }
                }
                _ => {}
            },
            _ => {
                return Err(ParseError::UnexpectedToken(
                    input,
                    format!("expect operator, got {}", ch),
                    pos.clone(),
                ))
            }
        }
    }

    Ok(Token::new(TokenName::OP, start_pos, pos.clone()))
}

#[test]
fn test_raise_op() {
    let input = "/+";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "/");
}

#[test]
fn test_raise_eq() {
    let input = "-==";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "-=");
}

#[test]
fn test_raise_fn() {
    let input = "/[+";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "/[");
}

#[test]
fn test_raise_dot() {
    let input = ".+";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), ".");
}

#[test]
fn test_raise_dot2() {
    let input = "..+";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "..");
}

#[test]
fn test_raise_dot3() {
    let input = "....";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), "...");
}

pub fn raise_comment<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars();
    let start_pos = pos.clone();

    chars.next(); // skip first ;
    pos.advance();

    while let Some(ch) = chars.next() {
        match ch {
            '\\' => {
                pos.advance();

                match chars.next() {
                    Some(_) => {
                        pos.advance();
                    }
                    None => {
                        return Err(ParseError::UnexpectedToken(
                            input,
                            "bad escape".to_string(),
                            pos.clone(),
                        ))
                    }
                }
            }
            ';' => {
                pos.advance();
                break;
            }
            '\n' => {
                break;
            }
            _ => {
                pos.advance();
            }
        }
    }

    Ok(Token::new(TokenName::COMMENT, start_pos, pos.clone()))
}

#[test]
fn test_raise_comment() {
    let input = ";123;456";
    let token = raise_comment(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), ";123;");
}

#[test]
fn test_raise_comment_escape() {
    let input = ";123\\;456;";
    let token = raise_comment(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), ";123\\;456;");
}

#[test]
fn test_raise_comment_newline() {
    let input = ";123\n456";
    let token = raise_comment(input, &mut Position::default()).unwrap();

    assert_eq!(token.source(input), ";123");
}

pub fn raise_whitespace<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    if let Some(ch) = chars.next() {
        match ch {
            '\n' => {
                pos.advance_newline();
            }
            _ => {
                pos.advance();
            }
        }
    }

    Ok(Token::new(TokenName::WHITESPACE, start_pos, pos.clone()))
}

pub fn skip_whitespace<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut token: RaiseResult<'a> = Err(ParseError::UnexpectedToken(
        input,
        "expect whitespace".to_string(),
        pos.clone(),
    ));

    while let Some(c) = input.chars().nth(pos.cursor) {
        if c.is_whitespace() {
            token = raise_whitespace(input, pos);
        } else if c == ';' {
            token = raise_comment(input, pos);
        } else {
            break;
        }
    }

    token
}

#[test]
fn test_skip_whitespace() {
    let input = " \t\r\n ;inline \\; comment; \n\r\t ";
    let mut pos = Position::default();

    let _ = skip_whitespace(input, &mut pos);

    assert_eq!(pos, Position::new(3, 4, input.len()));
}

pub fn lookahead<'a>(input: &'a str, pos: &mut Position, count: usize) -> RaiseResult<'a> {
    let backup = pos.clone();
    let mut token = raise_token(input, pos);

    for _ in 1..count {
        token = raise_token(input, pos);
    }

    *pos = backup;

    return token;
}

#[test]
fn test_lookahead() {
    let input = "[= fib /[n] [+ n [- n 1]]]";
    let mut pos = Position::default();

    let mut token = lookahead(input, &mut pos, 1).unwrap();
    assert_eq!(token.source(input), "[");

    token = lookahead(input, &mut pos, 2).unwrap();
    assert_eq!(token.source(input), "=");

    token = lookahead(input, &mut pos, 11).unwrap();
    assert_eq!(token.source(input), "+");
}

pub fn expect<'a>(expected: &'a str, input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let token = raise_token(input, pos)?;
    let source = token.source(input);

    if source != expected {
        *pos = token.start_pos;

        return Err(ParseError::UnexpectedToken(
            input,
            format!("expect {}, got {}", expected, source),
            pos.clone(),
        ));
    }

    Ok(token)
}

#[test]
fn test_expect() {
    let input = r#"[= fib /[n] 
        [match n
          [[<= n 1] 1] ; comment
          [true [+ n [- n 1]]]]]"#;

    let mut pos = Position::default();

    assert_eq!(expect("[", input, &mut pos).unwrap().source(input), "[");
    assert_eq!(expect("=", input, &mut pos).unwrap().source(input), "=");

    let _ = skip_whitespace(input, &mut pos);
    assert_eq!(expect("fib", input, &mut pos).unwrap().source(input), "fib");

    let _ = raise_token(input, &mut pos);
    let _ = raise_token(input, &mut pos);
    let _ = raise_token(input, &mut pos);
    let _ = raise_token(input, &mut pos);
    let _ = skip_whitespace(input, &mut pos);

    assert_eq!(expect("[", input, &mut pos).unwrap().source(input), "[");

    let mut test_pos = Position::new(1, 8, 7);

    assert_eq!(
        Err(ParseError::UnexpectedToken(
            input,
            "expect /, got /[".to_string(),
            test_pos.clone()
        )),
        expect("/", input, &mut test_pos)
    );
}
