use std::io;
use crate::repl::start;

pub mod token;
pub mod lexer;
pub mod repl;
pub mod ast;
pub mod parser;

fn main() {
    println!("请输入代码");
    start(io::stdin(), io::stdout());
}
