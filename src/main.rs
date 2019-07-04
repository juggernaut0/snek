use std::process::exit;

mod ast;
mod codegen;
mod interpreter;
mod opcode;
mod parser;
mod resolver;
mod scanner;
mod value;

#[cfg(debug_assertions)]
mod debug;

fn main() {
    let src = "\
#let f = { x -> (x + 1) }
#let x = 6
#let math = (1 + 2 * 3 * 4 - 5 * (f x))
#(println math)
(1*2 + 2*3)
";
    match parser::parse(src) {
        Ok(ast) => {

            debug::AstPrinter::new().print_ast(&ast);
            match codegen::compile(&ast) {
                Ok(code) => {
                    interpreter::execute(&code);
                },
                Err(errs) => {
                    //errs.iter().for_each(|e| eprintln!("[{}] [ERROR] {}", e.line(), e.col(), e.message()));
                    exit(1)
                }
            }
        }
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    }
}
