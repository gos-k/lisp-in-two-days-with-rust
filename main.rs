use std::collections::HashMap;

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

#[derive(Debug, PartialEq)]
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

pub fn parse(source: &str) -> Expr {
    let tokens = tokenise(source);
    ParseState(tokens.into_iter().peekable()).parse_expr()
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Value {
    Number(i64),
    Callable(Callable),
    Nil,
}

impl Value {
    fn is_truthy(&self) -> bool {
        match *self {
            Value::Number(n) => n != 0,
            _ => true,
        }
    }
}

type Callable = fn(Vec<Value>) -> EvalResult;

pub struct EvalError(String);

pub type EvalResult = Result<Value, EvalError>;

fn last_or_nil(values: Vec<Value>) -> Value {
    values.last().cloned().unwrap_or(Value::Nil)
}

pub fn make_global_env() -> HashMap<String, Value> {
    let mut env = HashMap::new();
    env.insert(
        "begin".into(),
        Value::Callable(|values| Ok(last_or_nil(values))),
    );
    env
}

pub fn eval_with_env(expr: Expr, env: &mut HashMap<String, Value>) -> EvalResult {
    match expr {
        Expr::Symbol(_, s) => env
            .get(&s)
            .cloned()
            .ok_or_else(|| EvalError(format!("eval undefind symbol"))),
        Expr::Number(_, n) => Ok(Value::Number(n)),
        Expr::If(_, _, cond, then, elz, _) => Ok(if eval_with_env(*cond, env)?.is_truthy() {
            eval_with_env(*then, env)?
        } else {
            eval_with_env(*elz, env)?
        }),
        _ => Err(EvalError(format!("eval not impl"))),
    }
}

fn main() {
    let tokens = tokenise("(if (alfa bravo) charlie (delta echo))");
    println!("{:?}", tokens);
    let exprs = ParseState(tokens.into_iter().peekable()).parse_expr();
    println!("{:?}", exprs);
}
