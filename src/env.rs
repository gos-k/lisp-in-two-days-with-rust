use std::collections::HashMap;

use super::eval::*;

fn last_or_nil(values: Vec<Value>) -> Value {
    values.last().cloned().unwrap_or(Value::Nil)
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
                if let Value::Symbol(rhs) = &values[1] {
                    Ok(if lhs == *rhs { Value::T } else { Value::Nil })
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
        Value::Callable(|values| match &values[0] {
            Value::Parent(parent) => Ok(*parent.lhs.clone()),
            other => Err(EvalError(format!("car {:?}", other))),
        }),
    );
    env.insert(
        "cdr".into(),
        Value::Callable(|values| match &values[0] {
            Value::Parent(parent) => Ok(*parent.rhs.clone()),
            other => Err(EvalError(format!("car {:?}", other))),
        }),
    );
    env
}

#[test]
fn test_env() {
    use super::parse::*;
    use super::token::TokenKind::*;

    fn env_eval(expr: Expr) -> EvalResult {
        eval_with_env(expr, &mut make_global_env())
    }

    assert_eq!(
        env_eval(Expr::Symbol(Symbol("t".to_string()), "t".to_string())).unwrap(),
        Value::T
    );
    assert_eq!(
        env_eval(Expr::Symbol(Symbol("nil".to_string()), "nil".to_string())).unwrap(),
        Value::Nil
    );
}
