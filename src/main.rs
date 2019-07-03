use std::process::exit;

mod ast;
mod opcode;
mod parser;
mod resolver;
mod scanner;

#[cfg(debug_assertions)]
mod debug;

fn main() {
    let src = "\
let f = { x -> (x + 1) }
let x = 6
let math = (1 + 2 * 3 * 4 - 5 * (f x))\
(println math)
";
    match parser::parse(src) {
        Ok(ast) => {
            // TODO execution
            debug::AstPrinter::new().print_ast(&ast);
            let res = resolver::Resolver::new(&ast);
        }
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    }
}
