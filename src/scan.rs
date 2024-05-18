use crate::{code_frame::Position, errors::*};

use alloc::{
    format,
    string::{String, ToString},
    vec::Vec,
};

use core::{fmt, iter::Peekable};

pub type RaiseResult = Result<Token, SquareError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Comment(Position, String),
    Eof(Position),
    Id(Position, String),
    Num(Position, String),
    Op(Position, String),
    Str(Position, String),
    Whitespace(Position, String),
}

impl Token {
    pub fn source(&self) -> &str {
        match self {
            Token::Comment(_, source) => source,
            Token::Eof(_) => "",
            Token::Id(_, source) => source,
            Token::Num(_, source) => source,
            Token::Op(_, source) => source,
            Token::Str(_, source) => source,
            Token::Whitespace(_, source) => source,
        }
    }

    pub fn pos(&self) -> &Position {
        match self {
            Token::Comment(pos, _) => pos,
            Token::Eof(pos) => pos,
            Token::Id(pos, _) => pos,
            Token::Num(pos, _) => pos,
            Token::Op(pos, _) => pos,
            Token::Str(pos, _) => pos,
            Token::Whitespace(pos, _) => pos,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Comment(_, source) => write!(f, "Comment({})", source),
            Token::Eof(_) => write!(f, "Eof"),
            Token::Id(_, source) => write!(f, "Id({})", source),
            Token::Num(_, source) => write!(f, "Num({})", source),
            Token::Op(_, source) => write!(f, "Op({})", source),
            Token::Str(_, source) => write!(f, "Str({})", source),
            Token::Whitespace(_, source) => write!(f, "Whitespace({})", source),
        }
    }
}

pub fn raise_token(input: &str, pos: &mut Position) -> RaiseResult {
    let input_chars: Vec<char> = input.chars().collect();

    if pos.cursor >= input_chars.len() {
        return Ok(Token::Eof(pos.clone()));
    }

    let mut chars = input_chars[pos.cursor..].iter().peekable();

    match chars.peek() {
        Some('\'') => raise_string(input, pos),
        Some(';') => raise_comment(input, pos),
        Some(ch) => match ch {
            _ if ch.is_alphabetic() || **ch == '_' => raise_ident(input, pos),
            _ if ch.is_ascii_digit() => raise_number(input, pos),
            _ if ch.is_whitespace() => raise_whitespace(input, pos),
            _ => raise_operator(input, pos),
        },
        None => Err(SquareError::UnexpectedToken(
            input.to_string(),
            "early eof".to_string(),
            pos.clone(),
        )),
    }
}

fn eat_n_hex(
    chars: &mut Peekable<core::slice::Iter<'_, char>>,
    pos: &mut Position,
    n: usize,
) -> Option<bool> {
    if n == 0 {
        return Some(true);
    }

    chars
        .next()
        .filter(|ch| ch.is_ascii_hexdigit())
        .and_then(|_| {
            pos.advance();
            eat_n_hex(chars, pos, n - 1)
        })
}

fn raise_string(input: &str, pos: &mut Position) -> RaiseResult {
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
                    input.to_string(),
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
                        if eat_n_hex(&mut chars, pos, 2).is_none() {
                            return Err(SquareError::UnexpectedToken(
                                input.to_string(),
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
                        if eat_n_hex(&mut chars, pos, 4).is_none() {
                            return Err(SquareError::UnexpectedToken(
                                input.to_string(),
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
                    Some('\'') | Some('\\') | Some('n') | Some('r') | Some('t') | Some('v') => {
                        chars.next();
                        pos.advance();
                    }
                    _ => {
                        return Err(SquareError::UnexpectedToken(
                            input.to_string(),
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

    let source = input_chars[(start_pos.cursor + 1)..(pos.cursor - 1)]
        .iter()
        .collect();

    Ok(Token::Str(start_pos, source))
}

#[test]
fn test_raise_string() {
    let input = "'hello'";
    let token = raise_string(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "hello");
}

#[test]
fn test_raise_string_escaped() {
    let input = "'he\\'llo'";
    let token = raise_string(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "he\\'llo");
}

#[test]
fn test_raise_string_escaped2() {
    let input = "'he\\\\'llo'";
    let token = raise_string(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "he\\\\");
}

#[test]
fn test_raise_string_escape_error() {
    let input = "'he\\allo'";
    let expected_output = Err(SquareError::UnexpectedToken(
        input.to_string(),
        "bad escape".to_string(),
        Position {
            line: 1,
            column: 5,
            cursor: 4,
        },
    ));

    assert_eq!(raise_string(input, &mut Position::new()), expected_output);
}

#[test]
fn test_raise_string_unterminated() {
    let input = "'he\nllo";
    let expected_output = Err(SquareError::UnexpectedToken(
        input.to_string(),
        "unterminated string, found newline".to_string(),
        Position {
            line: 1,
            column: 4,
            cursor: 3,
        },
    ));

    assert_eq!(raise_string(input, &mut Position::new()), expected_output);
}

#[test]
fn test_raise_string_ascii() {
    let input = r#"'\x61\x62\x630'"#;
    let token = raise_string(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "\\x61\\x62\\x630");
}

#[test]
fn test_raise_string_ascii_error() {
    let input = "'\\x6g'";

    let expected_output = Err(SquareError::UnexpectedToken(
        input.to_string(),
        "invalid ascii code, expect \\x{hex}{2}, got '\\x6".to_string(),
        Position {
            line: 1,
            column: 5,
            cursor: 4,
        },
    ));

    assert_eq!(raise_string(input, &mut Position::new()), expected_output);
}

#[test]
fn test_raise_string_unicode() {
    let input = r#"'\u4f60\u597d\u3041\ud83e\udd7a'"#;
    let token = raise_string(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "\\u4f60\\u597d\\u3041\\ud83e\\udd7a");
}

#[test]
fn test_raise_string_unicode_error() {
    let input = "'\\u123g'";

    let expected_output = Err(SquareError::UnexpectedToken(
        input.to_string(),
        "invalid unicode, expect \\u{hex}{4}, got '\\u123".to_string(),
        Position {
            line: 1,
            column: 7,
            cursor: 6,
        },
    ));

    assert_eq!(raise_string(input, &mut Position::new()), expected_output);
}

fn raise_number(input: &str, pos: &mut Position) -> RaiseResult {
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
    let start_pos = pos.clone();
    let eat_digits = |chars: &mut Peekable<core::slice::Iter<'_, char>>, pos: &mut Position| {
        let mut count = 0;
        while let Some(ch) = chars.peek() {
            if !ch.is_ascii_digit() {
                break;
            }

            count += 1;
            chars.next();
            pos.advance();
        }

        count
    };

    if eat_digits(&mut chars, pos) < 1 {
        return Err(SquareError::UnexpectedToken(
            input.to_string(),
            "expect number".to_string(),
            pos.clone(),
        ));
    }

    if let Some(dot) = chars.peek() {
        if **dot == '.' {
            chars.next();
            pos.advance();

            if eat_digits(&mut chars, pos) < 1 {
                return Err(SquareError::UnexpectedToken(
                    input.to_string(),
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
                if **minus == '-' || **minus == '+' {
                    chars.next();
                    pos.advance();
                }

                if eat_digits(&mut chars, pos) < 1 {
                    return Err(SquareError::UnexpectedToken(
                        input.to_string(),
                        "expect at least one digit after exp".to_string(),
                        pos.clone(),
                    ));
                }
            } else {
                return Err(SquareError::UnexpectedToken(
                    input.to_string(),
                    "expect sign or digit after exp".to_string(),
                    pos.clone(),
                ));
            }
        }
    }
    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

    Ok(Token::Num(start_pos, source))
}

#[test]
fn test_raise_number_integer() {
    let input = "012";
    let token = raise_number(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "012");
}

#[test]
fn test_raise_number_float() {
    let input = "012.34.56";
    let token = raise_number(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "012.34");
}

#[test]
fn test_raise_number_float_error() {
    let input = "012.";

    assert_eq!(
        raise_number(input, &mut Position::new()),
        Err(SquareError::UnexpectedToken(
            input.to_string(),
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
    let token = raise_number(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "01234e5");
}

#[test]
fn test_raise_number_exp2() {
    let input = "012.34e5.12";
    let token = raise_number(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "012.34e5");
}

#[test]
fn test_raise_number_exp3() {
    let input = "012E+5e6";
    let token = raise_number(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "012E+5");
}

#[test]
fn test_raise_number_exp_error() {
    let input = "012e.";

    assert_eq!(
        raise_number(input, &mut Position::new()),
        Err(SquareError::UnexpectedToken(
            input.to_string(),
            "expect at least one digit after exp".to_string(),
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
    let input = "012.34E-56";
    let token = raise_number(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "012.34E-56");
}

#[test]
fn test_raise_number_min_max() {
    let max = "1.7976931348623157e+308";
    let min_positive = "2.2250738585072014e-308";

    assert_eq!(
        raise_number(max, &mut Position::new())
            .unwrap()
            .source()
            .parse::<f64>()
            .unwrap(),
        core::f64::MAX
    );

    assert_eq!(
        raise_number(min_positive, &mut Position::new())
            .unwrap()
            .source()
            .parse::<f64>()
            .unwrap(),
        core::f64::MIN_POSITIVE
    );
}

fn raise_ident(input: &str, pos: &mut Position) -> RaiseResult {
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
    let start_pos = pos.clone();

    chars.next(); // leading alpha or _ cannot be digit
    pos.advance();
    while let Some(ch) = chars.peek() {
        if !(ch.is_alphabetic() || ch.is_ascii_digit() || **ch == '_') {
            break;
        }

        chars.next();
        pos.advance();
    }

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

    Ok(Token::Id(start_pos, source))
}

#[test]
fn test_raise_id() {
    let input = "_0";
    let token = raise_ident(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "_0");
}

fn raise_operator(input: &str, pos: &mut Position) -> RaiseResult {
    let input_chars: Vec<char> = input.chars().collect();
    let mut chars = input_chars[pos.cursor..].iter().peekable();
    let start_pos = pos.clone();

    if let Some(ch) = chars.next() {
        pos.advance();

        match ch {
            '~' | '[' | ']' => {}
            '+' | '-' | '*' | '^' | '%' | '&' | '|' | '=' => {
                if let Some('=') = chars.peek() {
                    chars.next();
                    pos.advance();
                }
            }
            '/' => match chars.peek() {
                Some('[') | Some('=') => {
                    chars.next();
                    pos.advance();
                }
                _ => {}
            },
            '>' => match chars.peek() {
                Some('>') => {
                    chars.next();
                    pos.advance();
                    if let Some('=') = chars.peek() {
                        chars.next();
                        pos.advance();
                    }
                }
                Some('=') => {
                    chars.next();
                    pos.advance();
                }
                _ => {}
            },
            '<' => match chars.peek() {
                Some('<') => {
                    chars.next();
                    pos.advance();
                    if let Some('=') = chars.peek() {
                        chars.next();
                        pos.advance();
                    }
                }
                Some('=') => {
                    chars.next();
                    pos.advance();
                }
                _ => {}
            },
            '!' => match chars.peek() {
                Some('=') => {
                    chars.next();
                    pos.advance();
                }
                any => {
                    return Err(SquareError::UnexpectedToken(
                        input.to_string(),
                        format!("expect !=, got '!{:?}'", any),
                        pos.clone(),
                    ))
                }
            },
            '.' => {
                if let Some('.') = chars.peek() {
                    chars.next();
                    pos.advance();

                    match chars.peek() {
                        Some('.') => {
                            chars.next();
                            pos.advance();
                        }
                        _ => {
                            return Err(SquareError::UnexpectedToken(
                                input.to_string(),
                                "expect ..., got ..".to_string(),
                                pos.clone(),
                            ))
                        }
                    }
                }
            }

            _ => {
                return Err(SquareError::UnexpectedToken(
                    input.to_string(),
                    format!("expect operator, got '{}'", ch),
                    pos.clone(),
                ))
            }
        }
    }

    let source = input_chars[start_pos.cursor..pos.cursor].iter().collect();

    Ok(Token::Op(start_pos, source))
}

#[test]
fn test_raise_op() {
    let input = "/+";
    let token = raise_operator(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "/");
}

#[test]
fn test_raise_fn() {
    let input = "/[";
    let token = raise_operator(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "/[");
}

#[test]
fn test_raise_eq() {
    let input = "<<=";
    let token = raise_operator(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "<<=");
}

#[test]
fn test_raise_ne() {
    let input = "!=";
    let token = raise_operator(input, &mut Position::new()).unwrap();
    assert_eq!(token.source(), "!=");

    assert_eq!(
        raise_operator("!", &mut Position::new()),
        Err(SquareError::UnexpectedToken(
            "!".to_string(),
            "expect !=, got '!None'".to_string(),
            Position {
                line: 1,
                column: 2,
                cursor: 1
            },
        ))
    );
}

#[test]
fn test_raise_dot() {
    let input = ".+";
    let token = raise_operator(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), ".");
}

#[test]
fn test_raise_dot3() {
    let input = "....";
    let token = raise_operator(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), "...");
}

fn raise_comment(input: &str, pos: &mut Position) -> RaiseResult {
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
                            input.to_string(),
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

    Ok(Token::Comment(start_pos, source))
}

#[test]
fn test_raise_comment() {
    let input = ";123;456";
    let token = raise_comment(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), ";123;");
}

#[test]
fn test_raise_comment_escape() {
    let input = ";123\\;456;";
    let token = raise_comment(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), ";123\\;456;");
}

#[test]
fn test_raise_comment_newline() {
    let input = ";123\n456";
    let token = raise_comment(input, &mut Position::new()).unwrap();

    assert_eq!(token.source(), ";123\n");
}

fn raise_whitespace(input: &str, pos: &mut Position) -> RaiseResult {
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

    Ok(Token::Whitespace(start_pos, source))
}

pub fn skip_whitespace(input: &str, pos: &mut Position) -> RaiseResult {
    let mut token = raise_token(input, pos)?;

    while let Token::Whitespace(..) | Token::Comment(..) = token {
        token = raise_token(input, pos)?;
    }

    *pos = token.pos().clone();

    Ok(token)
}

#[test]
fn test_skip_whitespace() {
    let input = " \t\r\n ;inline \\; comment; \n\r\t ";
    let mut pos = Position::new();

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
    let mut pos = Position::new();

    let _ = skip_whitespace(input, &mut pos);

    assert_eq!(pos, Position::new());
}

pub fn lookahead(input: &str, pos: &mut Position) -> RaiseResult {
    let backup = pos.clone();
    let token = raise_token(input, pos);

    *pos = backup;

    token
}
#[test]
fn test_lookahead() {
    let input = "[= fib /[n] [+ n [- n 1]]]";
    let mut pos = Position::new();
    let mut token = lookahead(input, &mut pos).unwrap();

    assert_eq!(token.source(), "[");

    raise_token(input, &mut pos).unwrap();
    token = lookahead(input, &mut pos).unwrap();

    assert_eq!(token.source(), "=");
}

type TokenFn<'a, R> = dyn Fn(&Token) -> R + 'a;

pub fn expect(pred: &TokenFn<(bool, String)>, input: &str, pos: &mut Position) -> RaiseResult {
    let token = raise_token(input, pos)?;
    let (result, message) = pred(&token);

    if !result {
        *pos = token.pos().clone();

        return Err(SquareError::UnexpectedToken(
            input.to_string(),
            format!("{}, got {}", message, token),
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

    let mut pos = Position::new();

    assert_eq!(
        expect(
            &|token| (token.source() == "[", "expect [".to_string()),
            input,
            &mut pos
        )
        .unwrap()
        .source(),
        "["
    );
    assert_eq!(
        expect(
            &|token| (token.source() == "=", "expect =".to_string()),
            input,
            &mut pos
        )
        .unwrap()
        .source(),
        "="
    );
}
