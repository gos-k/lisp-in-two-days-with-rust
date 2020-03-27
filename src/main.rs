use std::collections::HashMap;
use std::io::Write;

mod token;
use token::*;

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

#[derive(Debug, PartialEq, Clone)]
pub struct Parent {
    lhs: Box<Child>,
    rhs: Box<Child>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Child {
    Value(Value),
    Parent(Parent),
    Nil,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(i64),
    Symbol(String),
    Callable(Callable),
    Parent(Parent),
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

fn cons(lhs: Value, rhs: Value) -> Value {
    Value::Parent(Parent {
        lhs: Box::new(Child::Value(lhs)),
        rhs: Box::new(Child::Value(rhs)),
    })
}

fn child_to_value(child: Child) -> Value {
    match child {
        Child::Value(value) => value,
        Child::Parent(parent) => Value::Parent(parent),
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
        "*".into(),
        Value::Callable(|values| {
            Ok(Value::Number(
                values.iter().fold(1, |mul, i| mul * i.clone().into_num()),
            ))
        }),
    );
    env.insert(
        "atom".into(),
        Value::Callable(|values| match values[0].clone() {
            Value::Number(_) | Value::Symbol(_) => Ok(Value::T),
            _ => Ok(Value::Nil),
        }),
    );
    env.insert(
        "eq".into(),
        Value::Callable(|values| match values[0].clone() {
            Value::Number(lhs) => {
                if let Value::Number(rhs) = values[1] {
                    Ok(if lhs == rhs { Value::T } else { Value::Nil })
                } else {
                    Ok(Value::Nil)
                }
            }
            Value::Symbol(lhs) => {
                if let Value::Symbol(rhs) = values[1].clone() {
                    Ok(if lhs == rhs { Value::T } else { Value::Nil })
                } else {
                    Ok(Value::Nil)
                }
            }
            Value::T => match values[1] {
                Value::T => Ok(Value::T),
                _ => Ok(Value::Nil),
            },
            _ => Ok(Value::Nil),
        }),
    );
    env.insert(
        "cons".into(),
        Value::Callable(|values| {
            let lhs = values[0].clone();
            let rhs = values[1].clone();
            Ok(cons(lhs, rhs))
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

fn eval_with_list(mut values: Vec<Value>) -> Value {
    let lhs = match values.pop() {
        Some(value) => value,
        None => panic!("no value"),
    };
    let rhs = if values.len() == 0 {
        Value::Nil
    } else {
        eval_with_list(values)
    };
    cons(lhs, rhs)
}

fn eval_with_quote(expr: Expr) -> EvalResult {
    match expr {
        Expr::Symbol(_, s) => Ok(Value::Symbol(s)),
        Expr::Number(_, n) => Ok(Value::Number(n)),
        Expr::Define(_, def, sym, value, _) => {
            let def = to_sym(def)?;
            let sym = to_sym(sym)?;
            let value = eval_with_quote(*value)?;
            Ok(cons(
                Value::Symbol(def.to_string()),
                cons(Value::Symbol(sym.to_string()), value),
            ))
        }
        Expr::Call(_, sym, args, _) => {
            let sym = to_sym(sym)?;
            let mut args = args
                .into_iter()
                .map(|a| eval_with_quote(a.clone()))
                .collect::<Result<Vec<_>, _>>()?;
            args.reverse();
            Ok(cons(Value::Symbol(sym.to_string()), eval_with_list(args)))
        }
        Expr::Quote(_, quote, value, _) => {
            let quote = to_sym(quote)?;
            let value = eval_with_quote(*value)?;
            Ok(cons(Value::Symbol(quote.to_string()), value))
        }
        _ => Err(EvalError(format!("eval not impl"))),
    }
}

pub fn eval_with_env(expr: Expr, env: &mut HashMap<String, Value>) -> EvalResult {
    match expr {
        Expr::Symbol(_, s) => env
            .get(&s)
            .cloned()
            .ok_or_else(|| EvalError(format!("eval undefind symbol {}", s))),
        Expr::Number(_, n) => Ok(Value::Number(n)),
        Expr::If(_, _, cond, then, elz, _) => {
            let result = eval_with_env(*cond, env)?.is_truthy();
            Ok(eval_with_env(if result { *then } else { *elz }, env)?)
        }
        Expr::Define(_, _, sym, value, _) => {
            let value = eval_with_env(*value, env)?;
            let sym = to_sym(sym)?;
            env.insert(sym, value.clone());
            Ok(value)
        }
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
        Expr::Quote(_, _, value, _) => eval_with_quote(*value),
        Expr::Lambda(_, _, _, _, _) => panic!("lambda not impl"),
    }
}

pub fn eval(expr: Expr) -> EvalResult {
    eval_with_env(expr, &mut make_global_env())
}

#[test]
fn test_eval() {
    use TokenKind::*;

    assert_eq!(eval(Expr::Number(Number(0), 0)).unwrap(), Value::Number(0));
    assert_eq!(
        eval(Expr::Symbol(Symbol("t".to_string()), "t".to_string())).unwrap(),
        Value::T
    );
    assert_eq!(
        eval(Expr::Symbol(Symbol("nil".to_string()), "nil".to_string())).unwrap(),
        Value::Nil
    );
    assert_eq!(
        eval(Expr::If(
            LeftBracket,
            Symbol("if".to_string()),
            Box::new(Expr::Symbol(Symbol("t".to_string()), "t".to_string())),
            Box::new(Expr::Number(Number(1), 1)),
            Box::new(Expr::Number(Number(2), 2)),
            RightBracket,
        ))
        .unwrap(),
        Value::Number(1)
    );
    assert_eq!(
        eval(Expr::Quote(
            LeftBracket,
            Symbol("quote".to_string()),
            Box::new(Expr::Symbol(Symbol("test".to_string()), "test".to_string())),
            RightBracket,
        ))
        .unwrap(),
        Value::Symbol("test".to_string())
    );
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
    let tokens = tokenise(&buffer);
    parse(tokens)
}

fn main() {
    let mut env = make_global_env();
    loop {
        print(eval_with_env(read(), &mut env));
    }
}
