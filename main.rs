use std::collections::HashMap;
use std::io::Write;

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
                    'a'..='z' | '+' => Some(TokeniseState::Symbol),
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
            Number => TokenKind::Number(token_str.parse().unwrap()),
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

#[derive(Debug, Clone)]
pub struct Parent {
    lhs: Box<Child>,
    rhs: Box<Child>,
}

#[derive(Debug, Clone)]
pub enum Child {
    Value(Box<Value>),
    Parent(Box<Parent>),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(i64),
    Symbol(String),
    Callable(Callable),
    Parent(Box<Parent>),
    T,
    Nil,
}

impl Value {
    fn is_truthy(&self) -> bool {
        match *self {
            Value::Nil => false,
            _ => true,
        }
    }

    fn into_num(self) -> i64 {
        match self {
            Value::Number(n) => n,
            other => panic!("NaN {:?}", other),
        }
    }
}

type Callable = fn(Vec<Value>) -> EvalResult;

#[derive(Debug)]
pub struct EvalError(String);

pub type EvalResult = Result<Value, EvalError>;

fn last_or_nil(values: Vec<Value>) -> Value {
    values.last().cloned().unwrap_or(Value::Nil)
}

fn child_to_value(child: Child) -> Value {
    match child {
        Child::Value(box_value) => *box_value,
        Child::Parent(box_parent) => Value::Parent(box_parent),
        Child::Nil => Value::Nil,
    }
}

pub fn make_global_env() -> HashMap<String, Value> {
    let mut env = HashMap::new();
    env.insert("t".into(), Value::T);
    env.insert("nil".into(), Value::Nil);
    env.insert(
        "begin".into(),
        Value::Callable(|values| Ok(last_or_nil(values))),
    );
    env.insert(
        "+".into(),
        Value::Callable(|values| {
            Ok(Value::Number(
                values.iter().map(|i| i.clone().into_num()).sum(),
            ))
        }),
    );
    env.insert(
        "cons".into(),
        Value::Callable(|values| {
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            Ok(Value::Parent(Box::new(Parent {
                lhs: Box::new(Child::Value(Box::new(lhs))),
                rhs: Box::new(Child::Value(Box::new(rhs))),
            })))
        }),
    );
    env.insert(
        "car".into(),
        Value::Callable(|values| {
            let list = values[0].clone();
            match list {
                Value::Parent(parent) => {
                    let child = *parent.lhs;
                    Ok(child_to_value(child))
                }
                other => Err(EvalError(format!("car {:?}", other))),
            }
        }),
    );
    env.insert(
        "cdr".into(),
        Value::Callable(|values| {
            let list = values[0].clone();
            match list {
                Value::Parent(parent) => {
                    let child = *parent.rhs;
                    Ok(child_to_value(child))
                }
                other => Err(EvalError(format!("car {:?}", other))),
            }
        }),
    );
    env
}

fn to_sym(token: TokenKind) -> Result<String, EvalError> {
    match token {
        TokenKind::Symbol(s) => Ok(s),
        other => Err(EvalError(format!("not symbol {:?}", other))),
    }
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
        Expr::Call(_, sym, args, _) => {
            let sym = to_sym(sym)?;
            match env.get(&sym) {
                Some(Value::Callable(c)) => c(args
                    .into_iter()
                    .map(|a| eval_with_env(a, env))
                    .collect::<Result<Vec<_>, _>>()?),
                _ => Err(EvalError(format!("invalid function '{}'", sym))),
            }
        }
        _ => Err(EvalError(format!("eval not impl"))),
    }
}

pub fn eval(expr: Expr) -> EvalResult {
    eval_with_env(expr, &mut make_global_env())
}

pub fn print(result: EvalResult) {
    match result {
        Ok(value) => println!(" ~> {:?}", value),
        Err(error) => println!(" !! {:?}", error),
    }
}

pub fn read() -> Expr {
    let mut buffer = String::new();
    print!(" > ");
    std::io::stdout().flush().unwrap();
    std::io::stdin().read_line(&mut buffer).unwrap();
    parse(&buffer)
}

fn main() {
    //let tokens = tokenise("(+ 2 (if 0 0 1))");
    //let tokens = tokenise("(cons 2 (if 0 0 1))");
    //let tokens = tokenise("(cons 0 (cons 2 (cons (if 0 0 1) 0)))");
    let tokens = tokenise("(cdr (cons 0 (cons 2 (cons (if 0 0 1) 0))))");
    //let tokens = tokenise("(alfa 0 (bravo 0 (if 0 0 1))");
    println!("{:?}", tokens);
    let exprs = ParseState(tokens.into_iter().peekable()).parse_expr();
    println!("{:?}", exprs);
    let result = eval(exprs);
    println!("{:?}", result);
    //let rr = read();
    //println!("{:?}", rr);

    loop {
        print(eval(read()));
    }
}
