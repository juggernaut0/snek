use std::process::exit;
use crate::debug::AstPrinter;

mod scanner;
mod parser;
mod ast;

#[cfg(debug_assertions)]
mod debug;

fn main() {
    let src = "\
let (Some (Pair _ [() i])) = [1 (a b) (a + b)]\
";
    match parser::parse(src) {
        Ok(ast) => {
            // TODO execution
            AstPrinter::new().print_ast(&ast)
        }
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    }
}
