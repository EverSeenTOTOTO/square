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

pub fn raise_token<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    if pos.cursor >= input.len() {
        return Ok(Token::new(TokenName::EOF, "", pos.clone()));
    }

    let pivot = input[pos.cursor..=pos.cursor]
        .chars()
        .next()
        .ok_or_else(|| ParseError::UnexpectedToken(input, "early eof".to_string(), pos.clone()))?;

    match pivot {
        _ if pivot == '\'' => raise_string(input, pos),
        _ if pivot == ';' => raise_comment(input, pos),
        _ if pivot.is_digit(10) => raise_number(input, pos),
        _ if pivot.is_alphabetic() || pivot == '_' => raise_ident(input, pos),
        _ if pivot.is_whitespace() => raise_whitespace(input, pos),
        _ => raise_operator(input, pos),
    }
}

pub fn raise_string<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let mut string = String::new();

    let start_pos = pos.clone();

    chars.next(); // skip first \'
    pos.advance();

    loop {
        match chars.next() {
            Some('\'') => {
                pos.advance();
                break; // terminate
            }
            Some('\n') => {
                pos.advance_newline();
                break;
            }
            Some('\\') => {
                pos.advance();

                match chars.peek() {
                    Some('\'') => {
                        chars.next();
                        pos.advance();
                        string.push('\'');
                    }
                    Some('\\') => {
                        chars.next();
                        pos.advance();
                        string.push('\\');
                    }
                    Some(_) => {
                        if let Some(ch) = chars.next() {
                            pos.advance();
                            string.push('\\');
                            string.push(ch);
                        } else {
                            unreachable!()
                        }
                    }
                    None => continue,
                }
            }
            Some(ch) => {
                pos.advance();
                string.push(ch)
            }
            None => {
                return Err(ParseError::UnexpectedToken(
                    input,
                    "unterminated string".to_string(),
                    pos.clone(),
                ))
            }
        }
    }

    let string_ref: &'a str = Box::leak(string.into_boxed_str());

    Ok(Token::new(TokenName::STR, string_ref, start_pos))
}

#[test]
fn test_raise_string() {
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

#[test]
fn test_raise_string_escaped() {
    let input = "'he\\'llo'";
    let expected_output = Ok(Token {
        name: TokenName::STR,
        source: "he'llo",
        position: Position::default(),
    });

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_string_escaped2() {
    let input = "'he\\\\'llo'";
    let expected_output = Ok(Token {
        name: TokenName::STR,
        source: "he\\",
        position: Position::default(),
    });

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_string_newline() {
    let input = "'he\nllo'";
    let expected_output = Ok(Token {
        name: TokenName::STR,
        source: "he",
        position: Position::default(),
    });

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_string_unterminated() {
    let input = "'hello";
    let expected_output = Err(ParseError::UnexpectedToken(
        input,
        "unterminated string".to_string(),
        Position::new(1, 7, 6),
    ));

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

pub fn raise_number<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let mut string = String::new();

    let start_pos = pos.clone();

    while let Some(ch) = chars.peek() {
        if !ch.is_ascii_digit() {
            break;
        }

        if let Some(ch) = chars.next() {
            string.push(ch);
            pos.advance();
        }
    }

    let string_ref: &'a str = Box::leak(string.into_boxed_str());

    Ok(Token::new(TokenName::NUM, string_ref, start_pos))
}

#[test]
fn test_raise_number() {
    let input = "012'34";
    let expected_output = Ok(Token {
        name: TokenName::NUM,
        source: "012",
        position: Position::default(),
    });

    assert_eq!(
        raise_number(input, &mut Position::default()),
        expected_output
    );
}

pub fn raise_ident<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let mut string = String::new();

    let start_pos = pos.clone();

    while let Some(ch) = chars.peek() {
        if !(ch.is_alphabetic() || *ch == '_') {
            break;
        }

        if let Some(eat) = chars.next() {
            string.push(eat);
            pos.advance();
        }
    }

    let string_ref: &'a str = Box::leak(string.into_boxed_str());

    Ok(Token::new(TokenName::ID, string_ref, start_pos))
}

#[test]
fn test_raise_id() {
    let input = "_demo";
    let expected_output = Ok(Token {
        name: TokenName::ID,
        source: "_demo",
        position: Position::default(),
    });

    assert_eq!(
        raise_ident(input, &mut Position::default()),
        expected_output
    );
}

pub fn raise_operator<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let mut string = String::new();

    let start_pos = pos.clone();

    if let Some(ch) = chars.next() {
        pos.advance();
        match ch {
            '+' | '-' | '*' | '[' | ']' | '^' | '%' | '&' | '|' | '=' => {
                string.push(ch);
            }
            '/' => match chars.peek() {
                Some('[') => {
                    chars.next();
                    pos.advance();
                    string.push_str("/[")
                }
                _ => {
                    string.push(ch);
                }
            },
            '.' => match chars.peek() {
                Some('.') => {
                    chars.next();
                    pos.advance();

                    match chars.peek() {
                        Some('.') => {
                            chars.next();
                            pos.advance();
                            string.push_str("...");
                        }
                        _ => {
                            string.push_str("..");
                        }
                    }
                }
                _ => {
                    string.push('.');
                }
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

    let string_ref: &'a str = Box::leak(string.into_boxed_str());

    Ok(Token::new(TokenName::OP, string_ref, start_pos))
}

#[test]
fn test_raise_normal() {
    let input = "/+";
    let expected_output = Ok(Token {
        name: TokenName::OP,
        source: "/",
        position: Position::default(),
    });

    assert_eq!(
        raise_operator(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_fn() {
    let input = "/[+";
    let expected_output = Ok(Token {
        name: TokenName::OP,
        source: "/[",
        position: Position::default(),
    });

    assert_eq!(
        raise_operator(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_dot() {
    let input = ".+";
    let expected_output = Ok(Token {
        name: TokenName::OP,
        source: ".",
        position: Position::default(),
    });

    assert_eq!(
        raise_operator(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_dot2() {
    let input = "..+";
    let expected_output = Ok(Token {
        name: TokenName::OP,
        source: "..",
        position: Position::default(),
    });

    assert_eq!(
        raise_operator(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_dot3() {
    let input = "....";
    let expected_output = Ok(Token {
        name: TokenName::OP,
        source: "...",
        position: Position::default(),
    });

    assert_eq!(
        raise_operator(input, &mut Position::default()),
        expected_output
    );
}

pub fn raise_comment<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars();
    let mut string = String::new();

    let start_pos = pos.clone();

    chars.next(); // skip first ;

    while let Some(ch) = chars.next() {
        match ch {
            '\\' => {
                pos.advance();

                match chars.next() {
                    Some(';') => {
                        pos.advance();
                        string.push(';');
                    }
                    Some(next) => {
                        pos.advance();
                        string.push('\\');
                        string.push(next);
                    }
                    None => {
                        return Err(ParseError::UnexpectedToken(
                            input,
                            "unterminated string".to_string(),
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
                pos.advance_newline();
                break;
            }
            _ => {
                pos.advance();
                string.push(ch);
            }
        }
    }

    let string_ref: &'a str = Box::leak(string.into_boxed_str());

    Ok(Token::new(TokenName::COMMENT, string_ref, start_pos))
}

#[test]
fn test_raise_comment() {
    let input = ";123;456";
    let expected_output = Ok(Token {
        name: TokenName::COMMENT,
        source: "123",
        position: Position::default(),
    });

    assert_eq!(
        raise_comment(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_comment_escape() {
    let input = ";123\\;456";
    let expected_output = Ok(Token {
        name: TokenName::COMMENT,
        source: "123;456",
        position: Position::default(),
    });

    assert_eq!(
        raise_comment(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_comment_newline() {
    let input = ";123\n456";
    let expected_output = Ok(Token {
        name: TokenName::COMMENT,
        source: "123",
        position: Position::default(),
    });

    assert_eq!(
        raise_comment(input, &mut Position::default()),
        expected_output
    );
}

pub fn raise_whitespace<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let mut string = String::new();

    let start_pos = pos.clone();

    if let Some(ch) = chars.next() {
        string.push(ch);

        match ch {
            '\n' => {
                pos.advance_newline();
            }
            _ => {
                pos.advance();
            }
        }
    }

    let string_ref: &'a str = Box::leak(string.into_boxed_str());

    Ok(Token::new(TokenName::WHITESPACE, string_ref, start_pos))
}

pub fn skip_whitespace<'a>(input: &'a str, pos: &mut Position) -> Result<(), ParseError<'a>> {
    while let Some(c) = input.chars().nth(pos.cursor) {
        if c.is_whitespace() {
            raise_whitespace(input, pos)?;
        } else if c == ';' {
            raise_comment(input, pos)?;
        } else {
            break;
        }
    }

    Ok(())
}

#[test]
fn test_skip_whitespace() {
    let input = " \t\r\n ;inline \\; comment; \n\r\t ";
    let mut pos = Position::default();

    let _ = skip_whitespace(input, &mut pos);

    assert_eq!(pos, Position::new(4, 4, input.len()));
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

pub fn expect<'a>(expected: &'a str, input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let token = raise_token(input, pos)?;

    if token.source != expected {
        *pos = token.position;

        return Err(ParseError::UnexpectedToken(
            input,
            format!("expect {}, got {}", expected, token.source),
            pos.clone(),
        ));
    }

    Ok(token)
}
