// use square::errors::*;
// use square::scan::*;
// 
// #[test]
// fn test_make_token() {
//     let mut pos = Position::new(1, 1, 1);
//     let token = make_token(TokenName::STR, &"hello world"[1..3], &mut pos);
// 
//     let expected_token = Token {
//         name: TokenName::STR,
//         source: "el",
//         position: Position {
//             line: 1,
//             column: 1,
//             cursor: 1,
//         },
//     };
//     let expected_pos = Position::new(1, 3, 3);
// 
//     assert_eq!(token, expected_token);
//     assert_eq!(pos, expected_pos);
// }
// 
// #[test]
// fn test_raise_string_single_quote() {
//     let input = "'hello'";
//     let expected_output = Ok(Token {
//         name: TokenName::STR,
//         source: "hello",
//         position: Position::default(),
//     });
// 
//     assert_eq!(
//         raise_string(input, &mut Position::default()),
//         expected_output
//     );
// }
// 
// #[test]
// fn test_raise_string_escape_sequence() {
//     let input = r#"'hello\nworld'"#;
//     let expected_output = Ok(Token {
//         name: TokenName::STR,
//         source: "hello\\nworld",
//         position: Position::default(),
//     });
//     assert_eq!(
//         raise_string(input, &mut Position::default()),
//         expected_output
//     );
// }
// 
// #[test]
// fn test_raise_string_unterminated() {
//     let input = "'hello";
//     let expected_output = Err(ParseError::UnexpectedToken(input, Position::default()));
//     assert_eq!(
//         raise_string(input, &mut Position::default()),
//         expected_output
//     );
// }
// 
// #[test]
// fn test_raise_number_invalid() {
//     let input = "abc";
//     let mut pos = Position::default();
//     let expected_output = Err(ParseError::UnexpectedToken(input, pos.clone()));
//     assert_eq!(raise_number(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_raise_number_valid() {
//     let input = "abc 123 def";
//     let mut pos = Position {
//         line: 1,
//         column: 5,
//         cursor: 4,
//     };
//     let expected_output = Ok(Token {
//         name: TokenName::NUM,
//         source: "123",
//         position: pos.clone(),
//     });
//     assert_eq!(raise_number(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_raise_ident_invalid() {
//     let input = "123abc";
//     let mut pos = Position::default();
//     let expected_output = Err(ParseError::UnexpectedToken(input, pos.clone()));
//     assert_eq!(raise_ident(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_raise_ident_valid_id() {
//     let input = "abc def";
//     let mut pos = Position {
//         line: 1,
//         column: 5,
//         cursor: 4,
//     };
//     let expected_output = Ok(Token {
//         name: TokenName::ID,
//         source: "def",
//         position: pos.clone(),
//     });
//     assert_eq!(raise_ident(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_raise_operator_square() {
//     let input = "[";
//     let expected_output = Ok(Token {
//         name: TokenName::OP,
//         source: "[",
//         position: Position::default(),
//     });
//     assert_eq!(
//         raise_operator(input, &mut Position::default()),
//         expected_output
//     );
// }
// 
// #[test]
// fn test_raise_operator_triangle() {
//     let input = "^";
//     let expected_output = Ok(Token {
//         name: TokenName::OP,
//         source: "^",
//         position: Position::default(),
//     });
//     assert_eq!(
//         raise_operator(input, &mut Position::default()),
//         expected_output
//     );
// }
// 
// #[test]
// fn test_raise_operator_fn() {
//     let input = "/[";
//     let expected_output = Ok(Token {
//         name: TokenName::OP,
//         source: "/[",
//         position: Position::default(),
//     });
//     assert_eq!(
//         raise_operator(input, &mut Position::default()),
//         expected_output
//     );
// }
// 
// #[test]
// fn test_raise_operator_dots() {
//     let input = "...";
//     let expected_output = Ok(Token {
//         name: TokenName::OP,
//         source: "...",
//         position: Position::default(),
//     });
//     assert_eq!(
//         raise_operator(input, &mut Position::default()),
//         expected_output
//     );
// }
// 
// #[test]
// fn test_raise_line_comment() {
//     let input = "; This is a comment\n aba;";
//     let mut pos = Position::default();
//     let expected_output = Ok(Token {
//         name: TokenName::COMMENT,
//         source: "; This is a comment",
//         position: Position::default(),
//     });
//     assert_eq!(raise_comment(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_raise_inline_comment() {
//     let input = "; This is an \\; inline comment; aba";
//     let mut pos = Position::default();
//     let expected_output = Ok(Token {
//         name: TokenName::COMMENT,
//         source: "; This is an \\; inline comment;",
//         position: Position::default(),
//     });
//     assert_eq!(raise_comment(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_raise_whitespace_valid() {
//     let input = "\n hello;";
//     let mut pos = Position::default();
//     let expected_output = Ok(Token {
//         name: TokenName::WHITESPACE,
//         source: "\n",
//         position: Position::default(),
//     });
//     assert_eq!(raise_whitespace(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_raise_whitespace_invalid() {
//     let input = "abc";
//     let mut pos = Position::default();
//     let expected_output = Err(ParseError::UnexpectedToken(input, pos.clone()));
//     assert_eq!(raise_whitespace(input, &mut pos), expected_output);
// }
// 
// #[test]
// fn test_skip_whitespace() {
//     let input = "    \n\t hello;";
//     let mut pos = Position::default();
// 
//     skip_whitespace(input, &mut pos).unwrap();
// 
//     assert_eq!(
//         pos,
//         Position {
//             line: 2,
//             column: 3,
//             cursor: 7
//         }
//     )
// }
// 
// #[test]
// fn test_lookahead() {
//     let input = "1 + 2 * 3";
//     let mut pos = Position::default();
// 
//     assert_eq!(
//         lookahead(input, &mut pos, 1),
//         Ok(Token {
//             name: TokenName::NUM,
//             source: "1",
//             position: pos.clone()
//         })
//     );
// 
//     assert_eq!(
//         lookahead(input, &mut pos, 2),
//         Ok(Token {
//             name: TokenName::WHITESPACE,
//             source: " ",
//             position: Position {
//                 line: 1,
//                 column: 2,
//                 cursor: 1
//             }
//         })
//     );
// 
//     assert_eq!(
//         lookahead(input, &mut pos, 3),
//         Ok(Token {
//             name: TokenName::OP,
//             source: "+",
//             position: Position {
//                 line: 1,
//                 column: 3,
//                 cursor: 2
//             }
//         })
//     );
// }
// 
// #[test]
// fn test_expect() {
//     let input = "1 + 2 * 3";
//     let mut pos = Position::default();
// 
//     let backup = pos.clone();
//     assert_eq!(
//         expect("1", input, &mut pos),
//         Ok(Token {
//             name: TokenName::NUM,
//             source: "1",
//             position: backup
//         })
//     );
// 
//     let backup = pos.clone();
//     assert_eq!(
//         expect(" ", input, &mut pos),
//         Ok(Token {
//             name: TokenName::WHITESPACE,
//             source: " ",
//             position: backup
//         })
//     );
// 
//     let backup = pos.clone();
//     assert_eq!(
//         expect("+", input, &mut pos),
//         Ok(Token {
//             name: TokenName::OP,
//             source: "+",
//             position: backup
//         })
//     );
// }
// 
// #[test]
// fn test_integration() {
//     let input = r#"[= foo /[. x]
//   [console.log x]]
// "#;
//     let mut tokens: Vec<Token> = vec![];
//     let mut pos = Position::default();
// 
//     loop {
//         let result = raise_token(input, &mut pos);
//         if let Ok(token) = result {
//             if token.name == TokenName::EOF {
//                 break;
//             } else {
//                 tokens.push(token);
//             }
//         } else if let Err(err) = result {
//             println!("{}", err);
//             break;
//         }
//     }
// 
//     assert_eq!(tokens.len(), 22);
//     assert_eq!(tokens[0].source, "[");
//     assert_eq!(tokens[3].source, "foo");
//     assert_eq!(tokens[5].source, "/[");
//     assert_eq!(tokens.get(tokens.len() - 2).unwrap().source, "]");
// }
