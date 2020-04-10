use super::eval::*;

fn symbol_expression(result: Value) -> String {
    use Value::*;

    match result {
        Number(number) => number.to_string(),
        Symbol(symbol) => symbol.to_string(),
        T => "t".to_string(),
        other => panic!("not impl {:?}", other),
    }
}

pub fn print(result: EvalResult) {
    match result {
        Ok(value) => println!(" ~> {}", symbol_expression(value)),
        Err(error) => println!(" !! {:?}", error),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_expression() {
        use Value::*;

        assert_eq!(symbol_expression(Number(0)), "0".to_string());
        assert_eq!(
            symbol_expression(Symbol("test".to_string())),
            "test".to_string()
        );
        assert_eq!(symbol_expression(T), "t".to_string());
    }
}
