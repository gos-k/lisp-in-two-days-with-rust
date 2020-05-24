extern crate nom;

use nom::{
    branch::alt,
    character::complete::{alpha1, char, digit1, multispace0},
    combinator::map,
    multi::many0,
    sequence::delimited,
    IResult,
};

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

fn token_paren(input: &str) -> IResult<&str, Vec<TokenKind>> {
    use TokenKind::*;
    map(
        delimited(
            multispace0,
            delimited(char('('), tokens, char(')')),
            multispace0,
        ),
        |mut tk| {
            let mut result = vec![LeftBracket];
            result.append(&mut tk);
            result.push(RightBracket);
            result
        },
    )(input)
}

fn token_name(input: &str) -> IResult<&str, Vec<TokenKind>> {
    use TokenKind::*;
    alt((
        map(delimited(multispace0, digit1, multispace0), |n: &str| {
            vec![Number(n.parse().unwrap())]
        }),
        map(delimited(multispace0, alpha1, multispace0), |s: &str| {
            vec![Symbol(s.to_string())]
        }),
        token_paren,
    ))(input)
}

fn tokens(input: &str) -> IResult<&str, Vec<TokenKind>> {
    let (s, output) = many0(token_name)(input)?;
    Ok((s, output.into_iter().flatten().collect::<Vec<TokenKind>>()))
}

pub fn tokenise(input: &str) -> Vec<TokenKind> {
    let (_, output) = token_name(input).unwrap();
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_token() {
        use TokenKind::*;
        assert_eq!(token_name("123"), Ok(("", vec![Number(123)])));
        assert_eq!(token_name("abc"), Ok(("", vec![Symbol("abc".to_string())])));
        assert_eq!(
            token_name("(123)"),
            Ok(("", vec![LeftBracket, Number(123), RightBracket]))
        );
        assert_eq!(
            token_name("(abc)"),
            Ok((
                "",
                vec![LeftBracket, Symbol("abc".to_string()), RightBracket]
            ))
        );
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
}
