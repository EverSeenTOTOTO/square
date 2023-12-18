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

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Token(Token),                                        // literal
    Expand(Token, Token, Vec<Box<Node>>),                // '[', ']', placeholders*
    Fn(Token, Box<Node>, Box<Node>),                     // '/', params, body
    Prop(Token, Token),                                  // '.', prop
    Assign(Token, Box<Node>, Vec<Box<Node>>, Box<Node>), // '=', expansion | id, (dot property)*, expression
    Op(Token, Vec<Box<Node>>),                           // operator, expressions+
    Call(Token, Token, Vec<Box<Node>>),                  // '[', ']', expressions+
    Dot(Box<Node>, Vec<Box<Node>>),                      // id | call, (dot property)*
}

struct BoxNodeVec<'a>(&'a Vec<Box<Node>>);

impl<'a> Into<String> for &BoxNodeVec<'a> {
    fn into(self) -> String {
        return match self.0.is_empty() {
            true => "Empty".to_string(),
            _ => self
                .0
                .iter()
                .map(|node| format!("{}", node))
                .collect::<Vec<String>>()
                .join(", "),
        };
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Token(token) => write!(f, "Token({})", token),
            Node::Expand(_, _, placeholders) => {
                write!(
                    f,
                    "Expand({})",
                    Into::<String>::into(&BoxNodeVec(placeholders))
                )
            }
            Node::Fn(_, params, body) => {
                write!(f, "Fn({}, {})", params, body)
            }
            Node::Prop(_, prop) => {
                write!(f, "Prop({})", prop)
            }
            Node::Assign(_, expansion, properties, expression) => {
                write!(
                    f,
                    "Assign({}.{} {})",
                    expansion,
                    Into::<String>::into(&BoxNodeVec(properties)),
                    expression
                )
            }
            Node::Op(operator, expressions) => {
                write!(
                    f,
                    "Op({}, {})",
                    operator,
                    Into::<String>::into(&BoxNodeVec(expressions))
                )
            }
            Node::Call(_, _, expressions) => {
                write!(
                    f,
                    "Call({})",
                    Into::<String>::into(&BoxNodeVec(expressions))
                )
            }
            Node::Dot(instance, properties) => {
                write!(
                    f,
                    "Dot({} {})",
                    instance,
                    Into::<String>::into(&BoxNodeVec(properties))
                )
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
                            "faield to parse_expand, expect identifier or placeholders, got {}({})",
                            token.name, token.source
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
            "failed to parse_expand, expect WHITESPACE, got ID(x)".to_string(),
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
            "failed to parse_expand, expect WHITESPACE, got OP(.)".to_string(),
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
            "failed to parse_expand, expect WHITESPACE, got OP(...)".to_string(),
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

            if let Node::Token(value) = *body {
                assert_eq!(value.source, "42");

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

    if let Node::Fn(_, params, _) = *node {
        if let Node::Expand(_, _, phs) = *params {
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

                if let Node::Fn(_, y_param, y_body) = *x_body {
                    if let Node::Expand(_, _, y_phs) = *y_param {
                        if let Node::Token(ref y) = *y_phs[0] {
                            assert_eq!(y.source, "y");

                            if let Node::Fn(_, z_param, _) = *y_body {
                                if let Node::Expand(_, _, z_phs) = *z_param {
                                    if let Node::Token(ref z) = *z_phs[0] {
                                        assert_eq!(z.source, "z");

                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    panic!();
}

pub fn parse_prop<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_prop");
    let dot = wrap(expect_source(".", input, pos))?;

    Ok(Box::new(Node::Prop(
        dot,
        wrap(expect_name(TokenName::ID, input, pos))?,
    )))
}

#[test]
fn test_parse_prop() {
    let input = ".foo";
    let mut pos = Position::default();
    let node = parse_prop(input, &mut pos).unwrap();

    if let Node::Prop(_, id) = *node {
        assert_eq!(id.source, "foo");

        return;
    }

    panic!()
}

fn parse_prop_chain<'a>(
    input: &'a str,
    pos: &mut Position,
) -> Result<Vec<Box<Node>>, SquareError<'a>> {
    let wrap = create_wrapper("parse_prop_chain");
    let mut nodes = vec![];

    loop {
        let backup = pos.clone();

        wrap(skip_whitespace(input, pos))?;

        let dot = wrap(lookahead(input, pos))?;

        if dot.source != "." {
            *pos = backup;
            break;
        }

        nodes.push(parse_prop(input, pos)?);
    }

    return Ok(nodes);
}

#[test]
fn test_parse_prop_chain() {
    let input = r#".foo
.bar 
.baz"#;
    let mut pos = Position::default();
    let chain = parse_prop_chain(input, &mut pos).unwrap();

    if let Node::Prop(_, ref baz) = *chain[2] {
        assert_eq!(baz.source, "baz");

        return;
    }

    panic!()
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
                "faield to parse_assign, expect identifier or expansion, got {}({})",
                token.name, token.source
            ),
            token.pos,
            None,
        ));
    };

    let props = parse_prop_chain(input, pos)?;

    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let expr = parse_expr(input, pos)?;

    Ok(Box::new(Node::Assign(eq, target, props, expr)))
}

#[test]
fn test_parse_assign_base() {
    let input = "= a b";
    let mut pos = Position::default();
    let node = parse_assign(input, &mut pos).unwrap();

    if let Node::Assign(_, lhs, _, rhs) = *node {
        if let Node::Token(a) = *lhs {
            assert_eq!(a.source, "a");

            if let Node::Token(b) = *rhs {
                assert_eq!(b.source, "b");

                return;
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_assign_expand() {
    let input = "= [. [a] [b . []]] b";
    let mut pos = Position::default();
    let node = parse_assign(input, &mut pos).unwrap();

    if let Node::Assign(_, lhs, _, rhs) = *node {
        if let Node::Expand(_, _, exprs) = *lhs {
            assert_eq!(exprs.len(), 3);

            if let Node::Token(b) = *rhs {
                assert_eq!(b.source, "b");

                return;
            }
        }
    }

    panic!();
}

fn is_binary_operator<'a>(op: &'a str) -> bool {
    match op {
        "+" | "-" | "*" | "/" | "+=" | "-=" | "*=" | "/=" | "^" | "%" | "&" | "|" | "^=" | "%="
        | "&=" | "|=" | "==" | "!=" | ">" | "<" | ">=" | "<=" | ".." => true,
        _ => false,
    }
}

pub fn parse_op<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_op");
    let operator = wrap(raise_token(input, pos))?;

    wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
    wrap(skip_whitespace(input, pos))?;

    let mut nodes = vec![];

    match operator {
        // binary
        _ if is_binary_operator(operator.source.as_str()) => {
            nodes.push(parse_expr(input, pos)?);

            wrap(expect_name(TokenName::WHITESPACE, input, pos))?;
            wrap(skip_whitespace(input, pos))?;

            nodes.push(parse_expr(input, pos)?);
        }
        _ => {
            return Err(SquareError::SyntaxError(
                input,
                format!(
                    "faield to parse_op, expect operator, got {}({})",
                    operator.name, operator.source
                ),
                operator.pos,
                None,
            ));
        }
    }

    Ok(Box::new(Node::Op(operator, nodes)))
}

#[test]
fn test_parse_op_binary() {
    let input = ".. a 4";
    let mut pos = Position::default();
    let node = parse_op(input, &mut pos).unwrap();

    if let Node::Op(op, exprs) = *node {
        assert_eq!(exprs.len(), 2);
        assert_eq!(op.source, "..");

        return;
    }

    panic!();
}

pub fn parse_call<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_call");
    let left_bracket = wrap(expect_source("[", input, pos))?;

    wrap(skip_whitespace(input, pos))?;

    let mut leading = wrap(lookahead(input, pos))?;
    let mut nodes = vec![];

    if leading.source == "=" {
        nodes.push(parse_assign(input, pos)?);
    } else if is_binary_operator(leading.source.as_str()) {
        nodes.push(parse_op(input, pos)?);
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
    };

    wrap(skip_whitespace(input, pos))?;

    let right_bracket = wrap(expect_source("]", input, pos))?;

    Ok(Box::new(Node::Call(left_bracket, right_bracket, nodes)))
}

#[test]
fn test_parse_call_assign() {
    let input = "[= [a b] [42 24]]";
    let mut pos = Position::default();
    let node = parse_call(input, &mut pos).unwrap();

    if let Node::Call(_, _, ref exprs) = *node {
        if let Node::Assign(_, ref expansion, _, _) = *exprs[0] {
            if let Node::Expand(_, _, ref phs) = **expansion {
                if let Node::Token(ref b) = *phs[1] {
                    assert_eq!(b.source, "b");

                    return;
                }
            }
        }
    }

    panic!();
}

pub fn parse_dot<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_dot");
    let leading = wrap(lookahead(input, pos))?;

    match leading {
        _ if leading.source == "[" => {
            let call = parse_call(input, pos)?;
            let nodes = parse_prop_chain(input, pos)?;

            if nodes.len() == 0 {
                Ok(call)
            } else {
                Ok(Box::new(Node::Dot(call, nodes)))
            }
        }
        _ if leading.name == TokenName::ID => {
            let id = wrap(expect_name(TokenName::ID, input, pos))?;

            let nodes = parse_prop_chain(input, pos)?;

            if nodes.len() == 0 {
                Ok(Box::new(Node::Token(id)))
            } else {
                Ok(Box::new(Node::Dot(Box::new(Node::Token(id)), nodes)))
            }
        }
        _ => Err(SquareError::SyntaxError(
            input,
            format!(
                "faield to parse_dot, expect identifier or call expression, got {}({})",
                leading.name, leading.source
            ),
            leading.pos,
            None,
        )),
    }
}

#[test]
fn test_parse_dot_empty() {
    let input = "[= [a b] [42 24]]";
    let mut pos = Position::default();
    let node = parse_dot(input, &mut pos).unwrap();

    if let Node::Call(_, _, exprs) = *node {
        if let Node::Assign(_, _, _, ref expr) = *exprs[0] {
            if let Node::Call(_, _, ref exprs) = **expr {
                if let Node::Token(ref fourty_two) = *exprs[0] {
                    assert_eq!(fourty_two.source, "42");

                    return;
                }
            }
        }
    }

    panic!();
}

#[test]
fn test_parse_dot() {
    let input = "[= [a b] [42 24]].foo.bar";
    let mut pos = Position::default();
    let node = parse_dot(input, &mut pos).unwrap();

    if let Node::Dot(_, props) = *node {
        if let Node::Prop(_, ref foo) = *props[0] {
            assert_eq!(foo.source, "foo");

            if let Node::Prop(_, ref bar) = *props[1] {
                assert_eq!(bar.source, "bar");

                return;
            }
        }
    }

    panic!();
}

pub fn parse_expr<'a>(input: &'a str, pos: &mut Position) -> ParseResult<'a> {
    let wrap = create_wrapper("parse_expr");
    let leading = wrap(lookahead(input, pos))?;

    match leading {
        _ if leading.source == "/" => parse_fn(input, pos),
        _ if leading.name == TokenName::NUM || leading.name == TokenName::STR => {
            Ok(Box::new(Node::Token(raise_token(input, pos)?)))
        }
        _ => parse_dot(input, pos),
    }
}

pub fn parse<'a>(input: &'a str, pos: &mut Position) -> Result<Vec<Box<Node>>, SquareError<'a>> {
    let wrap = create_wrapper("parse");
    let mut ast = vec![];

    wrap(skip_whitespace(input, pos))?;

    while wrap(lookahead(input, pos))?.name != TokenName::EOF {
        ast.push(parse_expr(input, pos)?);
        wrap(skip_whitespace(input, pos))?;
    }

    Ok(ast)
}

#[test]
fn test_parse_variable() {
    let input = r#"
; basic
[= x 2] ; x = 2
[= x [.. 1 4]] ; x = [1 2 3 4], `..` can concat strings, ranges and vectors

; expansion
[= [x y] [1 2]] ; x = 1, y = 2
[= [. x] [1 2 3]] ; x = 2, `.` is a placehoder that must occupy one position

; convenient placehoders in expansion
[= [... x] [.. 1 10]] ; x = 10, `...` is a placehoder that can occupy zero or as many positions as possible
[= [x ... y] [1]] ; x = 1, y = 1
[= [. [x] ... y] [1 [2] 3 4 5]] ; x = 2, y = 5
"#;
    let mut pos = Position::default();
    let ast = parse(input, &mut pos)
        .map_err(|err| {
            println!("{}", err);
            err
        })
        .unwrap();

    for node in ast {
        println!("{}", node);
    }
}

#[test]
fn test_parse_control_flow() {
    let input = r#"
; match
[match x
  [[> 42 x] foo]
  [[[regex '[a-z]+' 'gi'].test x] bar]]

; branch
[if true true]
[if true true false]

; block
[begin 
  [= i 0]
  [while [< i 10]
    [print i]
    [+= i 1]]]
"#;
    let mut pos = Position::default();
    let ast = parse(input, &mut pos)
        .map_err(|err| {
            println!("{}", err);
            err
        })
        .unwrap();

    for node in ast {
        println!("{}", node);
    }
}

#[test]
fn test_parse_function() {
    let input = r#"
; function starts with /[ as it looks like λ
[= foo /[] 2]

[foo]

; expansion in parameter
[= foo /[. z] [print [.. z 4]]]

[foo 'ignored' 0]

[= fib /[n] [begin
  [print n]
  [match n
    [[< n 2] 1]
    [+
      [fib [- n 1]] 
      [fib [- n 2]]]]]]

[print [[.. 1 10].map /[x] [fib x]]]
"#;
    let mut pos = Position::default();
    let ast = parse(input, &mut pos)
        .map_err(|err| {
            println!("{}", err);
            err
        })
        .unwrap();

    for node in ast {
        println!("{}", node);
    }
}

#[test]
fn test_parse_conroutine() {
    let input = r#"
; similar to Lua coroutine
[= genFib /[n]
  [co.wrap /[]
    [= [a b] [1 1]]
    [while [<= a n]
      [co.yield a]
      [= [a b] [b [+ a b]]]]]]

[[genFib 100].forEach print]
"#;
    let mut pos = Position::default();
    let ast = parse(input, &mut pos)
        .map_err(|err| {
            println!("{}", err);
            err
        })
        .unwrap();

    for node in ast {
        println!("{}", node);
    }
}

#[test]
fn test_parse_structure() {
    let input = r#"
; use built-in function `obj` to create an object
[= stack /[vec] [begin 
  [= this [obj]] ; `this` is just a variable name

  [= this.vec vec]

  [= this.clear /[] [= this.vec []]]

  [= this.push /[x] [begin 
    [= this.vec [.. this.vec [x]]]]]

  [= this.pop /[] [begin
    [= [... x] this.vec]
    [= this.vec [this.vec.slice 0 -1]]
    x]]

  this]]

[= v [1 2 3]]
[= s [stack v]]
[= x [s.pop]] ; x = 3
[s.clear]
[s.push 42]
[= y [s.pop]] ; y = 42
"#;
    let mut pos = Position::default();
    let ast = parse(input, &mut pos)
        .map_err(|err| {
            println!("{}", err);
            err
        })
        .unwrap();

    for node in ast {
        println!("{}", node);
    }
}
