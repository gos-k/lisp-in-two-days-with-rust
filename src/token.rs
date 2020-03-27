#[derive(Debug)]
enum TokeniseState {
    Start,
    LParen,
    RParen,
    Number,
    Symbol,
    WhiteSpace,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    LeftBracket,
    RightBracket,
    Number(i64),
    Symbol(String),
}

pub fn tokenise(source: &str) -> Vec<TokenKind> {
    use TokeniseState::*;

    let mut result = Vec::new();
    let mut start = 0;
    loop {
        let mut state = Start;
        let mut end = start;
        for c in source[start..].chars() {
            let next = match state {
                Start => match c {
                    '(' => Some(LParen),
                    ')' => Some(RParen),
                    '0'..='9' => Some(Number),
                    'a'..='z' | '+' | '*' => Some(TokeniseState::Symbol),
                    c if c.is_whitespace() => Some(WhiteSpace),
                    _ => None,
                },
                LParen | RParen => None,
                Number => match c {
                    '0'..='9' => Some(Number),
                    _ => None,
                },
                TokeniseState::Symbol => match c {
                    'a'..='z' | '+' | '*' | '0'..='9' => Some(TokeniseState::Symbol),
                    _ => None,
                },
                WhiteSpace => {
                    if c.is_whitespace() {
                        Some(WhiteSpace)
                    } else {
                        None
                    }
                }
            };

            if let Some(next_state) = next {
                state = next_state;
                end += c.len_utf8();
            } else {
                break;
            }
        }

        let token_str = &source[start..end];
        start = end;

        let kind = match state {
            Start => break,
            LParen => TokenKind::LeftBracket,
            RParen => TokenKind::RightBracket,
            Number => TokenKind::Number(token_str.parse().unwrap()),
            Symbol => TokenKind::Symbol(token_str.into()),
            WhiteSpace => continue,
        };

        result.push(kind);
    }

    return result;
}

#[test]
fn test_tokenise() {
    use TokenKind::*;

    assert_eq!(tokenise("0"), [Number(0)]);
    assert_eq!(tokenise("test"), [Symbol("test".to_string())]);
    assert_eq!(
        tokenise("(test 0)"),
        [
            LeftBracket,
            Symbol("test".to_string()),
            Number(0),
            RightBracket,
        ]
    );
    assert_eq!(
        tokenise("(cons 0 (cons (cons 1 nil) (cons 2 nil)))"),
        [
            LeftBracket,
            Symbol("cons".to_string()),
            Number(0),
            LeftBracket,
            Symbol("cons".to_string()),
            LeftBracket,
            Symbol("cons".to_string()),
            Number(1),
            Symbol("nil".to_string()),
            RightBracket,
            LeftBracket,
            Symbol("cons".to_string()),
            Number(2),
            Symbol("nil".to_string()),
            RightBracket,
            RightBracket,
            RightBracket,
        ]
    );
}
