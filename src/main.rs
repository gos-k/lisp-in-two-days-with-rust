use std::io::Write;

mod env;
mod eval;
mod parse;
mod token;

use env::*;
use eval::*;
use parse::*;
use token::*;

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
