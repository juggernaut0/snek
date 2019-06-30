use std::process::exit;

mod ast;
mod id_box;
mod parser;
mod resolver;
mod scanner;

#[cfg(debug_assertions)]
mod debug;

fn main() {
    id_box::test();
    let src = "\
let (Some (Pair _ [() i])) = [1 (a b) (a + b)]\
";
    match parser::parse(src) {
        Ok(ast) => {
            // TODO execution
            debug::AstPrinter::new().print_ast(&ast)
        }
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    }
}
