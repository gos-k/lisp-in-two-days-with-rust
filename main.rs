#[derive(Debug)]
enum TokeniseState {
    Start,
    LParen,
    RParen,
    Number,
    Symbol,
    WhiteSpace,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    LeftBracket,
    RightBracket,
    Number(i64),
    Symbol(String),
}

fn tokenise(source: &str) -> Vec<TokenKind> {
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
                    'a'..='z' => Some(TokeniseState::Symbol),
                    c if c.is_whitespace() => Some(WhiteSpace),
                    _ => None,
                },
                LParen | RParen => None,
                Number => match c {
                    '0'..='9' => Some(Number),
                    _ => None,
                },
                TokeniseState::Symbol => match c {
                    'a'..='z' | '0'..='9' => Some(TokeniseState::Symbol),
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
            Number => TokenKind::Symbol(token_str.parse().unwrap()),
            Symbol => TokenKind::Symbol(token_str.into()),
            WhiteSpace => continue,
        };

        result.push(kind);
    }

    return result;
}

fn main() {
    println!("{:?}", tokenise("(alfa bravo)"));
}
