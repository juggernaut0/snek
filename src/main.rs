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
(1 + 2 * 3 * 4 - 5 * (f x))\
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
