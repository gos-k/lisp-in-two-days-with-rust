use std::collections::HashMap;

use super::eval::*;

fn last_or_nil(values: Vec<Value>) -> Value {
    values.last().cloned().unwrap_or(Value::Nil)
}

pub fn make_global_env() -> HashMap<String, Value> {
    use Value::*;

    let mut env = HashMap::new();
    env.insert("t".into(), T);
    env.insert("nil".into(), Nil);
    env.insert("begin".into(), Callable(|values| Ok(last_or_nil(values))));
    env.insert(
        "+".into(),
        Callable(|values| Ok(Number(values.iter().map(|i| i.clone().into_num()).sum()))),
    );
    env.insert(
        "*".into(),
        Callable(|values| {
            Ok(Number(
                values.iter().fold(1, |mul, i| mul * i.clone().into_num()),
            ))
        }),
    );
    env.insert(
        "atom".into(),
        Callable(|values| {
            Ok(match values[0].clone() {
                Number(_) | Symbol(_) => T,
                _ => Nil,
            })
        }),
    );
    env.insert(
        "eq".into(),
        Callable(|values| match values[0].clone() {
            Number(_) | Symbol(_) => Ok(if values[0] == values[1] { T } else { Nil }),
            T => Ok(match values[1] {
                T => T,
                _ => Nil,
            }),
            _ => Ok(Nil),
        }),
    );
    env.insert(
        "cons".into(),
        Callable(|values| Ok(cons(values[0].clone(), values[1].clone()))),
    );
    env.insert(
        "car".into(),
        Callable(|values| match &values[0] {
            Pair(x, _) => Ok(*x.clone()),
            other => Err(EvalError(format!("car {:?}", other))),
        }),
    );
    env.insert(
        "cdr".into(),
        Callable(|values| match &values[0] {
            Pair(_, x) => Ok(*x.clone()),
            other => Err(EvalError(format!("car {:?}", other))),
        }),
    );
    env.insert(
        "list".into(),
        Callable(|mut values| {
            values.reverse();
            let result = values
                .iter()
                .fold(Nil, |list, value| cons(value.clone(), list));
            Ok(result)
        }),
    );
    env
}

#[cfg(test)]
mod tests {
    use super::super::parse::*;
    use super::super::token::*;
    use super::*;

    fn eval(expr: Expr) -> EvalResult {
        eval_with_env(expr, &mut make_global_env(), &mut make_global_env())
    }

    #[test]
    fn test_env() {
        let cons_expr = Expr::Call(
            TokenKind::Symbol("cons".to_string()),
            vec![Expr::Symbol("t".to_string()), Expr::Number(0)],
        );

        assert_eq!(eval(Expr::Symbol("t".to_string())).unwrap(), Value::T);
        assert_eq!(eval(Expr::Symbol("nil".to_string())).unwrap(), Value::Nil);

        assert_eq!(
            eval(cons_expr.clone()).unwrap(),
            Value::Pair(Box::new(Value::T), Box::new(Value::Number(0))),
        );

        assert_eq!(
            eval(Expr::Call(
                TokenKind::Symbol("car".to_string()),
                vec![cons_expr.clone()],
            ))
            .unwrap(),
            Value::T,
        );

        assert_eq!(
            eval(Expr::Call(
                TokenKind::Symbol("cdr".to_string()),
                vec![cons_expr.clone()],
            ))
            .unwrap(),
            Value::Number(0),
        );

        assert_eq!(
            eval(Expr::Call(
                TokenKind::Symbol("list".to_string()),
                vec![Expr::Symbol("t".to_string()), Expr::Number(0),],
            ))
            .unwrap(),
            Value::Pair(
                Box::new(Value::T),
                Box::new(Value::Pair(
                    Box::new(Value::Number(0)),
                    Box::new(Value::Nil)
                ))
            )
        );
    }
}
