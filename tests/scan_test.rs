use square::scan::*;

#[test]
fn test_make_token() {
    let mut pos = Position::new(0, 1, 1);
    let token = make_token(TokenName::STR, &"hello world"[1..3], &mut pos);

    assert_eq!(token.name, TokenName::STR);
    assert_eq!(token.source, "el");
    assert_eq!(token.position.line, 0);
    assert_eq!(token.position.column, 1);
    assert_eq!(token.position.cursor, 1);
    assert_eq!(pos.line, 0);
    assert_eq!(pos.column, 3);
    assert_eq!(pos.cursor, 3);
}

#[test]
fn test_raise_string_empty() {
    let mut pos = Position::new(0, 0, 0);
    let token = raise_string("''", &mut pos);

    assert_eq!(token.name, TokenName::STR);
    assert_eq!(token.source, "''");
}
