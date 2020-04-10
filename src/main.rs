use std::io::Write;

mod env;
mod eval;
mod parse;
mod print;
mod token;

use env::*;
use eval::*;
use parse::*;
use print::*;
use token::*;

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
    let mut macro_table = make_global_env();
    loop {
        print(eval_with_env(read(), &mut env, &mut macro_table));
    }
}
