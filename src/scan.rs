use crate::{code_frame::Position, errors::*};

use alloc::{
    format,
    string::{String, ToString},
    vec::Vec,
};

use core::{fmt, iter::Peekable};

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
    let input_chars: Vec<char> = input.chars().collect();

    if pos.cursor >= input_chars.len() {
        return Ok(Token {
            name: TokenName::EOF,
            pos: pos.clone(),
            source: "".to_string(),
        });
    }

    let mut chars = input_chars[pos.cursor..].iter().peekable();

    match chars.peek() {
        Some('\'') => raise_string(input, pos),
        Some(';') => raise_comment(input, pos),
        Some(ch) => match ch {
            _ if ch.is_alphabetic() || **ch == '_' => raise_ident(input, pos),
            _ if **ch == '-' || ch.is_ascii_digit() => raise_number(input, pos),
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

fn eat_n_hex<'a>(
    chars: &mut Peekable<core::slice::Iter<'_, char>>,
    pos: &mut Position,
    n: usize,
) -> Option<bool> {
    if n == 0 {
        return Some(true);
    }

    return chars
        .next()
        .filter(|ch| ch.is_ascii_hexdigit())
        .and_then(|_| {
            pos.advance();
            eat_n_hex(chars, pos, n - 1)
        });
}

fn raise_string<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
    let start_pos = pos.clone();

    chars.next(); // skip first \'
    pos.advance();

    while let Some(ch) = chars.next() {
        match ch {
            '\'' => {
                pos.advance();
                break; // terminate
            }
            '\r' | '\n' => {
                return Err(SquareError::UnexpectedToken(
                    input,
                    "unterminated string, found newline".to_string(),
                    pos.clone(),
                ))
            }
            '\\' => {
                pos.advance();

                match chars.peek() {
                    Some('x') => {
                        chars.next();
                        pos.advance();
                        if !eat_n_hex(&mut chars, pos, 2).is_some() {
                            return Err(SquareError::UnexpectedToken(
                                input,
                                format!(
                                    "invalid ascii code, expect \\x{{hex}}{{2}}, got {}",
                                    input_chars[start_pos.cursor..pos.cursor]
                                        .iter()
                                        .collect::<String>(),
                                ),
                                pos.clone(),
                            ));
                        }
                    }
                    Some('u') => {
                        chars.next();
                        pos.advance();
                        if !eat_n_hex(&mut chars, pos, 4).is_some() {
                            return Err(SquareError::UnexpectedToken(
                                input,
                                format!(
                                    "invalid unicode, expect \\u{{hex}}{{4}}, got {}",
                                    input_chars[start_pos.cursor..pos.cursor]
                                        .iter()
                                        .collect::<String>(),
                                ),
                                pos.clone(),
                            ));
                        }
                    }
                    Some('\'') | Some('\\') | Some('b') | Some('f') | Some('n') | Some('r')
                    | Some('t') | Some('v') => {
                        chars.next();
                        pos.advance();
                    }
                    _ => {
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

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

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
fn test_raise_string_escape_error() {
    let input = "'he\\allo'";
    let expected_output = Err(SquareError::UnexpectedToken(
        input,
        "bad escape".to_string(),
        Position {
            line: 1,
            column: 5,
            cursor: 4,
        },
    ));

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_string_unterminated() {
    let input = "'he\nllo";
    let expected_output = Err(SquareError::UnexpectedToken(
        input,
        "unterminated string, found newline".to_string(),
        Position {
            line: 1,
            column: 4,
            cursor: 3,
        },
    ));

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_string_ascii() {
    let input = r#"'\x61\x62\x630'"#;
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "'\\x61\\x62\\x630'");
}

#[test]
fn test_raise_string_ascii_error() {
    let input = "'\\x6g'";

    let expected_output = Err(SquareError::UnexpectedToken(
        input,
        "invalid ascii code, expect \\x{hex}{2}, got '\\x6".to_string(),
        Position {
            line: 1,
            column: 5,
            cursor: 4,
        },
    ));

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

#[test]
fn test_raise_string_unicode() {
    let input = r#"'\u4f60\u597d\u3041\ud83e\udd7a'"#;
    let token = raise_string(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "'\\u4f60\\u597d\\u3041\\ud83e\\udd7a'");
}

#[test]
fn test_raise_string_unicode_error() {
    let input = "'\\u123g'";

    let expected_output = Err(SquareError::UnexpectedToken(
        input,
        "invalid unicode, expect \\u{hex}{4}, got '\\u123".to_string(),
        Position {
            line: 1,
            column: 7,
            cursor: 6,
        },
    ));

    assert_eq!(
        raise_string(input, &mut Position::default()),
        expected_output
    );
}

fn eat_digits(chars: &mut Peekable<core::slice::Iter<'_, char>>, pos: &mut Position) {
    while let Some(ch) = chars.peek() {
        if !ch.is_ascii_digit() {
            break;
        }

        chars.next();
        pos.advance();
    }
}

fn raise_number<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
    let start_pos = pos.clone();

    if let Some(leading) = chars.peek() {
        if **leading == '-' || leading.is_ascii_digit() {
            chars.next();
            pos.advance();
        }

        eat_digits(&mut chars, pos);

        if let Some(dot) = chars.peek() {
            if **dot == '.' {
                chars.next();
                pos.advance();

                let backup = pos.clone();

                eat_digits(&mut chars, pos);

                if backup.cursor == pos.cursor {
                    return Err(SquareError::UnexpectedToken(
                        input,
                        "expect at least one digit after dot".to_string(),
                        pos.clone(),
                    ));
                }
            }
        }

        if let Some(exp) = chars.peek() {
            if **exp == 'e' || **exp == 'E' {
                chars.next();
                pos.advance();

                if let Some(minus) = chars.peek() {
                    if **minus == '-' || minus.is_ascii_digit() {
                        chars.next();
                        pos.advance();

                        eat_digits(&mut chars, pos);
                    } else {
                        return Err(SquareError::UnexpectedToken(
                            input,
                            "expect minus or digit after exp".to_string(),
                            pos.clone(),
                        ));
                    }
                } else {
                    return Err(SquareError::UnexpectedToken(
                        input,
                        "expect minus or digit after exp".to_string(),
                        pos.clone(),
                    ));
                }
            }
        }
    }

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

    Ok(Token {
        name: TokenName::NUM,
        pos: start_pos,
        source,
    })
}

#[test]
fn test_raise_number_integer() {
    let input = "012";
    let token = raise_number(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "012");
}

#[test]
fn test_raise_number_float() {
    let input = "012.34.56";
    let token = raise_number(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "012.34");
}

#[test]
fn test_raise_number_float_error() {
    let input = "012.";

    assert_eq!(
        raise_number(input, &mut Position::default()),
        Err(SquareError::UnexpectedToken(
            input,
            "expect at least one digit after dot".to_string(),
            Position {
                line: 1,
                column: 5,
                cursor: 4
            },
        ))
    );
}

#[test]
fn test_raise_number_exp() {
    let input = "01234e5.12";
    let token = raise_number(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "01234e5");
}

#[test]
fn test_raise_number_exp2() {
    let input = "012.34e5.12";
    let token = raise_number(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "012.34e5");
}

#[test]
fn test_raise_number_exp3() {
    let input = "012E5e6";
    let token = raise_number(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "012E5");
}

#[test]
fn test_raise_number_exp_error() {
    let input = "012e.";

    assert_eq!(
        raise_number(input, &mut Position::default()),
        Err(SquareError::UnexpectedToken(
            input,
            "expect minus or digit after exp".to_string(),
            Position {
                line: 1,
                column: 5,
                cursor: 4
            },
        ))
    );
}

#[test]
fn test_raise_number_minus() {
    let input = "-012.34E-56";
    let token = raise_number(input, &mut Position::default()).unwrap();

    assert_eq!(token.source, "-012.34E-56");
}

fn raise_ident<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
    let start_pos = pos.clone();

    while let Some(ch) = chars.peek() {
        if !(ch.is_alphabetic() || **ch == '_') {
            break;
        }

        chars.next();
        pos.advance();
    }

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

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
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
    let start_pos = pos.clone();

    if let Some(ch) = chars.next() {
        pos.advance();

        match ch {
            '[' | ']' => {}
            '+' | '-' | '*' | '/' | '^' | '%' | '&' | '|' | '=' | '!' | '>' | '<' => {
                match chars.peek() {
                    Some('=') => {
                        chars.next();
                        pos.advance();
                    }
                    _ => {}
                }
            }
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

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

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
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
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

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

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
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
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

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

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

    assert_eq!(
        pos,
        Position {
            line: 3,
            column: 4,
            cursor: input.len()
        }
    );
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
