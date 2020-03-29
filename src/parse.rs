use super::token::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Symbol(TokenKind, String),
    Number(TokenKind, i64),
    If(
        TokenKind,
        TokenKind,
        Box<Expr>,
        Box<Expr>,
        Box<Expr>,
        TokenKind,
    ),
    Define(TokenKind, TokenKind, TokenKind, Box<Expr>, TokenKind),
    Call(TokenKind, TokenKind, Vec<Expr>, TokenKind),
    Quote(TokenKind, TokenKind, Box<Expr>, TokenKind),
    Lambda(TokenKind, TokenKind, Box<Expr>, Box<Expr>, TokenKind),
}

struct ParseState<I: Iterator<Item = TokenKind>>(std::iter::Peekable<I>);

impl<I> ParseState<I>
where
    I: Iterator<Item = TokenKind>,
{
    fn parse_expr(&mut self) -> Expr {
        if let Some(token_kind) = self.0.next() {
            use TokenKind::*;
            match token_kind {
                LeftBracket => self.parse_form(token_kind),
                RightBracket => panic!("unexpected token!"),
                Number(n) => Expr::Number(token_kind, n),
                Symbol(ref s) => {
                    let sym = s.clone();
                    Expr::Symbol(token_kind, sym)
                }
            }
        } else {
            panic!("invalid expression")
        }
    }

    fn parse_form(&mut self, open: TokenKind) -> Expr {
        use TokenKind::*;
        match self.0.peek() {
            Some(Symbol(ref sym)) => match &sym[..] {
                "if" => {
                    let if_tok = self.0.next().unwrap();
                    let cond = self.parse_expr();
                    let if_true = self.parse_expr();
                    let if_false = self.parse_expr();
                    let close = self.0.next().unwrap();
                    Expr::If(
                        open,
                        if_tok,
                        Box::new(cond),
                        Box::new(if_true),
                        Box::new(if_false),
                        close,
                    )
                }
                "define" => {
                    let def_tok = self.0.next().unwrap();
                    let sym_tok = self.0.next().unwrap();
                    let value = self.parse_expr();
                    let close = self.0.next().unwrap();
                    Expr::Define(open, def_tok, sym_tok, Box::new(value), close)
                }
                "quote" => {
                    let sym_tok = self.0.next().unwrap();
                    let object = self.parse_expr();
                    let close = self.0.next().unwrap();
                    Expr::Quote(open, sym_tok, Box::new(object), close)
                }
                "lambda" => {
                    let lam_tok = self.0.next().unwrap();
                    let arg = self.parse_expr();
                    let body = self.parse_expr();
                    let close = self.0.next().unwrap();
                    Expr::Lambda(open, lam_tok, Box::new(arg), Box::new(body), close)
                }
                _ => {
                    let sym_tok = self.0.next().unwrap();
                    let mut args = Vec::new();
                    while let Some(token_kind) = self.0.peek() {
                        if token_kind == &RightBracket {
                            break;
                        }
                        args.push(self.parse_expr());
                    }
                    let close = self.0.next().unwrap();
                    Expr::Call(open, sym_tok, args, close)
                }
            },
            _ => panic!("invalid expression"),
        }
    }
}

pub fn parse(tokens: Vec<TokenKind>) -> Expr {
    ParseState(tokens.into_iter().peekable()).parse_expr()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        use TokenKind::*;

        assert_eq!(
            parse(vec![Symbol("test".to_string())]),
            Expr::Symbol(Symbol("test".to_string()), "test".to_string())
        );
        assert_eq!(parse(vec![Number(0)]), Expr::Number(Number(0), 0));
        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("if".to_string()),
                Number(0),
                Number(1),
                Number(2),
                RightBracket,
            ]),
            Expr::If(
                LeftBracket,
                Symbol("if".to_string()),
                Box::new(Expr::Number(Number(0), 0)),
                Box::new(Expr::Number(Number(1), 1)),
                Box::new(Expr::Number(Number(2), 2)),
                RightBracket,
            )
        );
        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("define".to_string()),
                Symbol("test".to_string()),
                Number(0),
                RightBracket,
            ]),
            Expr::Define(
                LeftBracket,
                Symbol("define".to_string()),
                Symbol("test".to_string()),
                Box::new(Expr::Number(Number(0), 0)),
                RightBracket
            )
        );
        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("test".to_string()),
                Number(0),
                RightBracket,
            ]),
            Expr::Call(
                LeftBracket,
                Symbol("test".to_string()),
                vec![Expr::Number(Number(0), 0)],
                RightBracket
            )
        );
        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("quote".to_string()),
                Number(0),
                RightBracket,
            ]),
            Expr::Quote(
                LeftBracket,
                Symbol("quote".to_string()),
                Box::new(Expr::Number(Number(0), 0)),
                RightBracket
            )
        );
        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("lambda".to_string()),
                Symbol("arg".to_string()),
                Number(0),
                RightBracket,
            ]),
            Expr::Lambda(
                LeftBracket,
                Symbol("lambda".to_string()),
                Box::new(Expr::Symbol(Symbol("arg".to_string()), "arg".to_string())),
                Box::new(Expr::Number(Number(0), 0)),
                RightBracket
            )
        );
    }
}
