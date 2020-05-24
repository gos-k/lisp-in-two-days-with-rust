use super::token::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Symbol(String),
    Number(i64),
    If(TokenKind, Box<Expr>, Box<Expr>, Box<Expr>),
    Define(TokenKind, TokenKind, Box<Expr>),
    Call(TokenKind, Vec<Expr>),
    Quote(TokenKind, Box<Expr>),
    Lambda(TokenKind, Vec<Expr>, Vec<Expr>),
    Macro(TokenKind, TokenKind, Vec<Expr>, Vec<Expr>),
}

struct ParseState<I: Iterator<Item = TokenKind>>(std::iter::Peekable<I>);

impl<I> ParseState<I>
where
    I: Iterator<Item = TokenKind>,
{
    fn parse_expr(&mut self) -> Expr {
        let token_kind = self
            .0
            .next()
            .ok_or("invalid expression".to_owned())
            .unwrap();
        use TokenKind::*;
        match token_kind {
            LeftBracket => self.parse_form(token_kind),
            RightBracket => panic!("unexpected token!"),
            Number(n) => Expr::Number(n),
            Symbol(ref s) => {
                let sym = s.clone();
                Expr::Symbol(sym)
            }
        }
    }

    fn parse_exprs(&mut self) -> Vec<Expr> {
        use TokenKind::*;
        let mut exprs = Vec::new();
        while let Some(token_kind) = self.0.peek() {
            if token_kind == &RightBracket {
                break;
            }
            exprs.push(self.parse_expr());
        }
        exprs
    }

    fn parse_symbol(&mut self) -> Expr {
        let token_kind = self
            .0
            .next()
            .ok_or("invalid expression".to_owned())
            .unwrap();
        match token_kind {
            TokenKind::Symbol(ref s) => {
                let sym = s.clone();
                Expr::Symbol(sym)
            }
            _ => panic!("invalid expression"),
        }
    }

    fn parse_symbols(&mut self) -> Vec<Expr> {
        use TokenKind::*;
        let mut args = Vec::new();
        while let Some(token_kind) = self.0.peek() {
            if token_kind == &RightBracket {
                break;
            }
            args.push(self.parse_symbol());
        }
        args
    }

    fn parse_form(&mut self, _open: TokenKind) -> Expr {
        use TokenKind::*;
        match self.0.peek() {
            Some(Symbol(ref sym)) => match &sym[..] {
                "if" => {
                    let if_tok = self.0.next().unwrap();
                    let cond = self.parse_expr();
                    let if_true = self.parse_expr();
                    let if_false = self.parse_expr();
                    let _close = self.0.next().unwrap();
                    Expr::If(
                        if_tok,
                        Box::new(cond),
                        Box::new(if_true),
                        Box::new(if_false),
                    )
                }
                "define" => {
                    let def_tok = self.0.next().unwrap();
                    let sym_tok = self.0.next().unwrap();
                    let value = self.parse_expr();
                    let _close = self.0.next().unwrap();
                    Expr::Define(def_tok, sym_tok, Box::new(value))
                }
                "quote" => {
                    let sym_tok = self.0.next().unwrap();
                    let object = self.parse_expr();
                    let _close = self.0.next().unwrap();
                    Expr::Quote(sym_tok, Box::new(object))
                }
                "lambda" => {
                    let lam_tok = self.0.next().unwrap();
                    let _args_open = self.0.next().unwrap();
                    let args = self.parse_symbols();
                    let _args_close = self.0.next().unwrap();
                    let body = self.parse_exprs();
                    let _close = self.0.next().unwrap();
                    Expr::Lambda(lam_tok, args, body)
                }
                "macro" => {
                    let mac_tok = self.0.next().unwrap();
                    let name_tok = self.0.next().unwrap();
                    let _args_open = self.0.next().unwrap();
                    let args = self.parse_symbols();
                    let _args_close = self.0.next().unwrap();
                    let body = self.parse_exprs();
                    let _close = self.0.next().unwrap();
                    Expr::Macro(mac_tok, name_tok, args, body)
                }
                _ => {
                    let sym_tok = self.0.next().unwrap();
                    let args = self.parse_exprs();
                    let _close = self.0.next().unwrap();
                    Expr::Call(sym_tok, args)
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
            Expr::Symbol("test".to_string())
        );
        assert_eq!(parse(vec![Number(0)]), Expr::Number(0));
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
                Symbol("if".to_string()),
                Box::new(Expr::Number(0)),
                Box::new(Expr::Number(1)),
                Box::new(Expr::Number(2)),
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
                Symbol("define".to_string()),
                Symbol("test".to_string()),
                Box::new(Expr::Number(0)),
            )
        );
        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("test".to_string()),
                Number(0),
                RightBracket,
            ]),
            Expr::Call(Symbol("test".to_string()), vec![Expr::Number(0)],)
        );
        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("quote".to_string()),
                Number(0),
                RightBracket,
            ]),
            Expr::Quote(Symbol("quote".to_string()), Box::new(Expr::Number(0)),)
        );
    }

    #[test]
    fn test_parse_lambda() {
        use TokenKind::*;

        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("lambda".to_string()),
                LeftBracket,
                RightBracket,
                Number(0),
                RightBracket,
            ]),
            Expr::Lambda(Symbol("lambda".to_string()), vec![], vec![Expr::Number(0)],)
        );

        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("lambda".to_string()),
                LeftBracket,
                Symbol("arg".to_string()),
                RightBracket,
                Number(0),
                RightBracket,
            ]),
            Expr::Lambda(
                Symbol("lambda".to_string()),
                vec![Expr::Symbol("arg".to_string())],
                vec![Expr::Number(0)],
            )
        );

        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("lambda".to_string()),
                LeftBracket,
                Symbol("alfa".to_string()),
                Symbol("bravo".to_string()),
                RightBracket,
                Number(0),
                Number(1),
                RightBracket,
            ]),
            Expr::Lambda(
                Symbol("lambda".to_string()),
                vec![
                    Expr::Symbol("alfa".to_string()),
                    Expr::Symbol("bravo".to_string()),
                ],
                vec![Expr::Number(0), Expr::Number(1),],
            )
        );
    }

    #[test]
    fn test_parse_macro() {
        use TokenKind::*;

        assert_eq!(
            parse(vec![
                LeftBracket,
                Symbol("macro".to_string()),
                Symbol("test".to_string()),
                LeftBracket,
                RightBracket,
                Number(0),
                RightBracket,
            ]),
            Expr::Macro(
                Symbol("macro".to_string()),
                Symbol("test".to_string()),
                vec![],
                vec![Expr::Number(0)],
            )
        );
    }
}
