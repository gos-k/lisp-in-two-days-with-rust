use std::collections::HashMap;

use super::parse::*;
use super::token::*;

#[derive(Debug, PartialEq, Clone)]
pub struct Parent {
    pub lhs: Box<Value>,
    pub rhs: Box<Value>,
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
    pub fn is_truthy(&self) -> bool {
        match *self {
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn into_num(self) -> i64 {
        match self {
            Value::Number(n) => n,
            other => panic!("NaN {:?}", other),
        }
    }
}

type Callable = fn(Vec<Value>) -> EvalResult;

#[derive(Debug)]
pub struct EvalError(pub String);

pub type EvalResult = Result<Value, EvalError>;

pub fn cons(lhs: Value, rhs: Value) -> Value {
    Value::Parent(Parent {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    })
}

fn to_sym(token: TokenKind) -> Result<String, EvalError> {
    match token {
        TokenKind::Symbol(s) => Ok(s),
        other => Err(EvalError(format!("not symbol {:?}", other))),
    }
}

fn eval_with_list(mut values: Vec<Value>) -> Value {
    let lhs = values.pop().unwrap();
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

fn eval(expr: Expr) -> EvalResult {
    eval_with_env(expr, &mut HashMap::new())
}

#[test]
fn test_eval() {
    use TokenKind::*;

    assert_eq!(eval(Expr::Number(Number(0), 0)).unwrap(), Value::Number(0));
    assert_eq!(
        eval(Expr::If(
            LeftBracket,
            Symbol("if".to_string()),
            Box::new(Expr::Number(Number(0), 0)),
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
