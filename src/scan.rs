use crate::{code_frame::Position, errors::*};

#[cfg(not(test))]
use alloc::{
    format,
    string::{String, ToString},
};

#[cfg(test)]
use std::string::String;

use core::fmt;

pub type RaiseResult<'a> = Result<Token, SquareError<'a>>;

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

impl fmt::Display for TokenName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenName::COMMENT => write!(f, "COMMENT"),
            TokenName::EOF => write!(f, "EOF"),
            TokenName::ID => write!(f, "ID"),
            TokenName::NUM => write!(f, "NUM"),
            TokenName::OP => write!(f, "OP"),
            TokenName::STR => write!(f, "STR"),
            TokenName::WHITESPACE => write!(f, "WHITESPACE"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub name: TokenName,
    pub pos: Position,
    pub source: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, self.source)
    }
}

pub fn raise_token<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    if pos.cursor >= input.len() {
        return Ok(Token {
            name: TokenName::EOF,
            pos: pos.clone(),
            source: "".to_string(),
        });
    }

    let mut chars = input[pos.cursor..].chars().peekable();

    match chars.peek() {
        Some('\'') => raise_string(input, pos),
        Some(';') => raise_comment(input, pos),
        Some(ch) => match ch {
            _ if ch.is_alphabetic() || *ch == '_' => raise_ident(input, pos),
            _ if ch.is_ascii_digit() => raise_integer(input, pos),
            _ if ch.is_whitespace() => raise_whitespace(input, pos),
            _ => raise_operator(input, pos),
        },
        None => {
            return Err(SquareError::UnexpectedToken(
                input,
                "early eof".to_string(),
                pos.clone(),
            ))
        }
    }
}

fn raise_string<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
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
                return Err(SquareError::UnexpectedToken(
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
                        return Err(SquareError::UnexpectedToken(
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

    let source = input[start_pos.cursor..pos.cursor].to_string();

    Ok(Token {
        name: TokenName::STR,
        pos: start_pos,
        source,
    })
}

#[test]
fn test_raise_string() {
    let input = "'hello'";
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "'hello'");
}

#[test]
fn test_raise_string_escaped() {
    let input = "'he\\'llo'";
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "'he\\'llo'");
}

#[test]
fn test_raise_string_escaped2() {
    let input = "'he\\\\'llo'";
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "'he\\\\'");
}

#[test]
fn test_raise_string_unterminated() {
    let input = "'he\nllo";
    let expected_output = Err(SquareError::UnexpectedToken(
        input,
        "unterminated string, found newline".to_string(),
        Position::new(1, 4, 3),
    ));

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

fn raise_integer<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    while let Some(ch) = chars.peek() {
        if !ch.is_ascii_digit() {
            break;
        }

        chars.next();
        pos.advance();
    }

    let source = input[start_pos.cursor..pos.cursor].to_string();

    Ok(Token {
        name: TokenName::NUM,
        pos: start_pos,
        source,
    })
}

#[test]
fn test_raise_integer() {
    let input = "012.34";
    let token = raise_integer(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "012");
}

fn raise_ident<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    while let Some(ch) = chars.peek() {
        if !(ch.is_alphabetic() || *ch == '_') {
            break;
        }

        chars.next();
        pos.advance();
    }

    let source = input[start_pos.cursor..pos.cursor].to_string();

    Ok(Token {
        name: TokenName::ID,
        pos: start_pos,
        source,
    })
}

#[test]
fn test_raise_id() {
    let input = "_demo";
    let token = raise_ident(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "_demo");
}

fn raise_operator<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut chars = input[pos.cursor..].chars().peekable();
    let start_pos = pos.clone();

    if let Some(ch) = chars.next() {
        pos.advance();

        match ch {
            '[' | ']' => {}
            '+' | '*' | '/' | '^' | '%' | '&' | '|' | '=' | '!' | '>' | '<' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    pos.advance();
                }
                _ => {}
            },
            '-' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    pos.advance();
                }
                Some(num) => {
                    if num.is_ascii_digit() {
                        let mut val = raise_integer(input, pos)?;

                        val.source.insert(0, ch);

                        return Ok(val);
                    }
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
                return Err(SquareError::UnexpectedToken(
                    input,
                    format!("expect operator, got '{}'", ch),
                    pos.clone(),
                ))
            }
        }
    }

    let source = input[start_pos.cursor..pos.cursor].to_string();

    Ok(Token {
        name: TokenName::OP,
        pos: start_pos,
        source,
    })
}

#[test]
fn test_raise_op() {
    let input = "/+";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "/");
}

#[test]
fn test_raise_eq() {
    let input = "-==";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "-=");
}

#[test]
fn test_raise_minus() {
    let input = "-42";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.name, TokenName::NUM);
    assert_eq!(token.source, "-42");
}

#[test]
fn test_raise_dot() {
    let input = ".+";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, ".");
}

#[test]
fn test_raise_dot2() {
    let input = "..+";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "..");
}

#[test]
fn test_raise_dot3() {
    let input = "....";
    let token = raise_operator(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "...");
}

fn raise_comment<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
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
                        return Err(SquareError::UnexpectedToken(
                            input,
                            "bad escape".to_string(),
                            pos.clone(),
                        ))
                    }
                }
            }
            ';' => {
                // inline comment
                pos.advance();
                break;
            }
            '\n' => {
                pos.advance_newline();
                break;
            }
            _ => {
                pos.advance();
            }
        }
    }

    let source = input[start_pos.cursor..pos.cursor].to_string();

    Ok(Token {
        name: TokenName::COMMENT,
        pos: start_pos,
        source,
    })
}

#[test]
fn test_raise_comment() {
    let input = ";123;456";
    let token = raise_comment(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, ";123;");
}

#[test]
fn test_raise_comment_escape() {
    let input = ";123\\;456;";
    let token = raise_comment(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, ";123\\;456;");
}

#[test]
fn test_raise_comment_newline() {
    let input = ";123\n456";
    let token = raise_comment(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, ";123\n");
}

fn raise_whitespace<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
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

    let source = input[start_pos.cursor..pos.cursor].to_string();

    Ok(Token {
        name: TokenName::WHITESPACE,
        pos: start_pos,
        source,
    })
}

pub fn skip_whitespace<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut token = raise_token(input, pos)?;

    while token.name == TokenName::WHITESPACE || token.name == TokenName::COMMENT {
        token = raise_token(input, pos)?;
    }

    *pos = token.pos.clone();

    Ok(token)
}

#[test]
fn test_skip_whitespace() {
    let input = " \t\r\n ;inline \\; comment; \n\r\t ";
    let mut pos = Position::default();

    let _ = skip_whitespace(input, &mut pos);

    assert_eq!(pos, Position::new(3, 4, input.len()));
}

#[test]
fn test_skip_whitespace_empty() {
    let input = "[ ;abaaba; ]";
    let mut pos = Position::default();

    let _ = skip_whitespace(input, &mut pos);

    assert_eq!(pos, Position::default());
}

pub fn lookahead<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let backup = pos.clone();
    let token = raise_token(input, pos);

    *pos = backup;

    return token;
}
#[test]
fn test_lookahead() {
    let input = "[= fib /[n] [+ n [- n 1]]]";
    let mut pos = Position::default();
    let mut token = lookahead(input, &mut pos).unwrap();

    assert_eq!(token.source, "[");

    raise_token(input, &mut pos).unwrap();
    token = lookahead(input, &mut pos).unwrap();

    assert_eq!(token.source, "=");
}

type TokenFn<'a, R> = dyn Fn(&Token) -> R + 'a;

fn expect<'a>(
    pred: &TokenFn<bool>,
    input: &'a str,
    pos: &mut Position,
    make_msg: &TokenFn<String>,
) -> RaiseResult<'a> {
    let token = raise_token(input, pos)?;

    if !pred(&token) {
        *pos = token.pos.clone();

        return Err(SquareError::UnexpectedToken(
            input,
            make_msg(&token),
            pos.clone(),
        ));
    }

    Ok(token)
}

pub fn expect_source<'a>(source: &'a str, input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    return expect(&|token| token.source == source, input, pos, &|token| {
        format!("expect {}, got {}({})", source, token.name, token.source)
    });
}

pub fn expect_name<'a>(name: TokenName, input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    return expect(&|token| token.name == name, input, pos, &|token| {
        format!("expect {}, got {}({})", name, token.name, token.source)
    });
}

#[test]
fn test_expect() {
    let input = r#"[= fib /[n] 
        [match n
          [[<= n 1] 1] ; comment
          [true [+ n [- n 1]]]]]"#;

    let mut pos = Position::default();

    assert_eq!(
        expect(&|_| true, input, &mut pos, &|_| "".to_string())
            .unwrap()
            .source,
        "["
    );
    assert_eq!(expect_source("=", input, &mut pos).unwrap().source, "=");

    let _ = skip_whitespace(input, &mut pos);
    assert_eq!(
        expect_name(TokenName::ID, input, &mut pos).unwrap().source,
        "fib"
    );
}
