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
    Lambda(Vec<String>, Vec<Expr>),
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
        Expr::Define(def, sym, value) => {
            let def = to_sym(def)?;
            let sym = to_sym(sym)?;
            let value = eval_with_quote(*value)?;
            Ok(cons(
                Value::Symbol(def.to_string()),
                cons(Value::Symbol(sym.to_string()), value),
            ))
        }
        Expr::Call(sym, args) => {
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

pub fn eval_with_env(
    expr: Expr,
    env: &mut HashMap<String, Value>,
    macro_table: &mut HashMap<String, Value>,
) -> EvalResult {
    match expr {
        Expr::Symbol(_, s) => env
            .get(&s)
            .cloned()
            .ok_or_else(|| EvalError(format!("eval undefind symbol {}", s))),
        Expr::Number(_, n) => Ok(Value::Number(n)),
        Expr::If(_, cond, then, elz) => {
            let result = eval_with_env(*cond, env, macro_table)?.is_truthy();
            Ok(eval_with_env(
                if result { *then } else { *elz },
                env,
                macro_table,
            )?)
        }
        Expr::Define(_, sym, value) => {
            let value = eval_with_env(*value, env, macro_table)?;
            let sym = to_sym(sym)?;
            env.insert(sym, value.clone());
            Ok(value)
        }
        Expr::Call(sym, params) => {
            let sym = to_sym(sym)?;
            match env.get(&sym) {
                Some(Value::Callable(c)) => c(params
                    .into_iter()
                    .map(|a| eval_with_env(a, env, macro_table))
                    .collect::<Result<Vec<_>, _>>()?),
                Some(Value::Lambda(args, exprs)) => {
                    let mut new_env = env.clone();
                    args.into_iter().zip(params.into_iter()).for_each(|(a, p)| {
                        new_env.insert(
                            a.to_string(),
                            eval_with_env(p.clone(), &mut env.clone(), macro_table).unwrap(),
                        );
                        ()
                    });
                    let results = exprs
                        .into_iter()
                        .map(|expr| eval_with_env(expr.clone(), &mut new_env, macro_table))
                        .collect::<Result<Vec<_>, _>>()?;
                    if let Some((last, _)) = results.split_last() {
                        Ok(last.clone())
                    } else {
                        Err(EvalError(format!("exec lambda '{:?}'", exprs)))
                    }
                }
                _ => Err(EvalError(format!("invalid function '{}'", sym))),
            }
        }
        Expr::Quote(_, _, value, _) => eval_with_quote(*value),
        Expr::Lambda(_, _, args, exprs, _) => {
            let args = args
                .into_iter()
                .map(|a| {
                    if let Expr::Symbol(_, sym) = a {
                        sym
                    } else {
                        panic!("lambda eval");
                    }
                })
                .collect::<Vec<String>>();
            Ok(Value::Lambda(args, exprs.to_vec()))
        }
        Expr::Macro(_, _, name, args, exprs, _) => {
            let name = to_sym(name)?;
            let args = args
                .into_iter()
                .map(|a| {
                    if let Expr::Symbol(_, sym) = a {
                        sym
                    } else {
                        panic!("lambda eval");
                    }
                })
                .collect::<Vec<String>>();
            macro_table.insert(name.clone(), Value::Lambda(args, exprs.to_vec()));
            Ok(Value::Symbol(name.to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn eval(expr: Expr) -> EvalResult {
        eval_with_env(expr, &mut HashMap::new(), &mut HashMap::new())
    }

    #[test]
    fn test_eval() {
        use TokenKind::*;

        assert_eq!(eval(Expr::Number(Number(0), 0)).unwrap(), Value::Number(0));
        assert_eq!(
            eval(Expr::If(
                Symbol("if".to_string()),
                Box::new(Expr::Number(Number(0), 0)),
                Box::new(Expr::Number(Number(1), 1)),
                Box::new(Expr::Number(Number(2), 2)),
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

    #[test]
    fn test_lambda() {
        use TokenKind::*;

        assert_eq!(
            eval(Expr::Lambda(
                LeftBracket,
                Symbol("lambda".to_string()),
                vec![Expr::Symbol(Symbol("test".to_string()), "test".to_string())],
                vec![Expr::Number(Number(0), 0)],
                RightBracket,
            ))
            .unwrap(),
            Value::Lambda(vec!["test".to_string()], vec![Expr::Number(Number(0), 0)])
        );

        assert_eq!(
            eval(Expr::Lambda(
                LeftBracket,
                Symbol("lambda".to_string()),
                vec![Expr::Symbol(Symbol("test".to_string()), "test".to_string())],
                vec![
                    Expr::Number(Number(0), 0),
                    Expr::Call(
                        Symbol("*".to_string()),
                        vec![
                            Expr::Symbol(Symbol("test".to_string()), "test".to_string()),
                            Expr::Symbol(Symbol("test".to_string()), "test".to_string()),
                        ],
                    )
                ],
                RightBracket,
            ))
            .unwrap(),
            Value::Lambda(
                vec!["test".to_string()],
                vec![
                    Expr::Number(Number(0), 0),
                    Expr::Call(
                        Symbol("*".to_string()),
                        vec![
                            Expr::Symbol(Symbol("test".to_string()), "test".to_string()),
                            Expr::Symbol(Symbol("test".to_string()), "test".to_string()),
                        ],
                    )
                ]
            )
        );
    }

    #[test]
    fn test_eval_macro() {
        use TokenKind::*;

        let mut env: HashMap<String, Value> = HashMap::new();
        let mut macro_table: HashMap<String, Value> = HashMap::new();

        assert_eq!(
            eval_with_env(
                Expr::Macro(
                    LeftBracket,
                    Symbol("macro".to_string()),
                    Symbol("test".to_string()),
                    vec![Expr::Symbol(Symbol("arg".to_string()), "arg".to_string())],
                    vec![Expr::Number(Number(0), 0)],
                    RightBracket,
                ),
                &mut env,
                &mut macro_table
            )
            .unwrap(),
            Value::Symbol("test".to_string())
        );

        assert_eq!(
            macro_table.get("test").unwrap().clone(),
            Value::Lambda(vec!["arg".to_string()], vec![Expr::Number(Number(0), 0)])
        );
    }
}
