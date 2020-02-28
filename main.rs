#[derive(Debug)]
enum TokeniseState {
    Start,
    LParen,
    RParen,
    Number,
    Symbol,
    WhiteSpace,
}

fn tokenise(source: &str) {
    use TokeniseState::*;

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
                    'a'..='z' => Some(Symbol),
                    c if c.is_whitespace() => Some(WhiteSpace),
                    _ => None,
                },
                LParen | RParen => None,
                Number => match c {
                    '0'..='9' => Some(Number),
                    _ => None,
                },
                Symbol => match c {
                    'A'..='Z' | '0'..='9' => Some(Symbol),
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
                println!("{:?}", next_state);
                state = next_state;
                end += c.len_utf8();
            } else {
                break;
            }
        }
    }
}

fn main() {
    tokenise("alfa");
}
