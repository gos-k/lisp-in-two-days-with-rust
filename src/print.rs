use super::eval::*;

fn symbol_expression(result: Value, paren: bool) -> String {
    use Value::*;

    match result {
        Number(number) => number.to_string(),
        Symbol(symbol) => symbol.to_string(),
        Pair(lhs, rhs) => {
            let lhs = symbol_expression(*lhs, true);
            match *rhs {
                Nil => {
                    if paren {
                        format!("({})", lhs)
                    } else {
                        format!("{}", lhs)
                    }
                }
                Pair(_, _) => {
                    let rhs = symbol_expression(*rhs, false);
                    if paren {
                        format!("({} {})", lhs, rhs)
                    } else {
                        format!("{} {}", lhs, rhs)
                    }
                }
                other => format!("({} . {})", lhs, symbol_expression(other, paren)),
            }
        }
        T => "t".to_string(),
        Nil => "nil".to_string(),
        other => panic!("not impl {:?}", other),
    }
}

pub fn print(result: EvalResult) {
    match result {
        Ok(value) => println!(" ~> {}", symbol_expression(value, true)),
        Err(error) => println!(" !! {:?}", error),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_expression() {
        use Value::*;

        assert_eq!(symbol_expression(Number(0), true), "0".to_string());
        assert_eq!(
            symbol_expression(Symbol("test".to_string()), true),
            "test".to_string()
        );
        assert_eq!(symbol_expression(cons(T, Nil), true), "(t)".to_string());
        assert_eq!(
            symbol_expression(cons(Number(0), Symbol("test".to_string())), true),
            "(0 . test)".to_string()
        );
        assert_eq!(
            symbol_expression(
                cons(Number(0), cons(Symbol("test".to_string()), cons(T, Nil))),
                true
            ),
            "(0 test t)".to_string()
        );
        assert_eq!(symbol_expression(T, true), "t".to_string());
        assert_eq!(symbol_expression(Nil, true), "nil".to_string());
    }
}
