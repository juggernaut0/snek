use std::process::exit;
use std::mem::size_of;
use crate::value::Value;

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
    println!("sizeof(Value) = {}", size_of::<Value>());

    let src = "\
#let f = { x -> (x + 1) }
#let x = 6
#let math = (1 + 2 * 3 * 4 - 5 * (f x))
#(println math)
let _ = 'hello'
let a = 3
let result = (true && 1*2 + a*b*5 - 0 == 12)
(println result)
";
    match parser::parse(src) {
        Ok(ast) => {
            debug::AstPrinter::new().print_ast(&ast);
            match codegen::compile(&ast) {
                Ok(code) => {
                    debug::print_code(&code);
                    //interpreter::execute(&code);
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
