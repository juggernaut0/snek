use std::process::exit;
use std::mem::size_of;

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
    println!("sizeof(Value) = {}", size_of::<value::Value>());

    let src = "\
#let f = { x -> (x + 1) }
#let x = 6
#let math = (1 + 2 * 3 * 4 - 5 * (f x))
#(println math)
#let _ = 'hello'
#let a = 3
#let result = (true && 1*2 + a*b*5 - 0 == 12)
#(println result)
(println (1 + 2))
";
    let ast = match parser::parse(src) {
        Ok(ast) => ast,
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    };
    debug::AstPrinter::new().print_ast(&ast);
    let code = match codegen::compile(&ast) {
        Ok(code) => code,
        Err(errs) => {
            //errs.iter().for_each(|e| eprintln!("[{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    };
    debug::print_code(&code);
    interpreter::execute(&code);
}
