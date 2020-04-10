use super::eval::*;

pub fn print(result: EvalResult) {
    match result {
        Ok(value) => println!(" ~> {:?}", value),
        Err(error) => println!(" !! {:?}", error),
    }
}
