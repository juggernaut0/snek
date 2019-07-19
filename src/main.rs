use std::env;
use std::fs;
use std::process::exit;
use std::rc::Rc;

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
    let args = env::args();
    let path = args.skip(1).next(); // TODO grab script dir for imports
    if let Some(p) = path {
        run_from_file(&p);
    } else {
        unimplemented!("repl") // TODO
    }
}

fn run_from_file(path: &str) {
    let src = fs::read_to_string(&path).expect("could not read file");
    let ast = match parser::parse(&src) {
        Ok(ast) => ast,
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    };
    drop(src); // no need to keep in memory after this point
    #[cfg(debug_assertions)] {
        debug::AstPrinter::new().print_ast(&ast);
    }
    let code = match codegen::compile(&ast) {
        Ok(code) => code,
        Err(errs) => {
            //errs.iter().for_each(|e| eprintln!("[{}] [ERROR] {}", e.line(), e.col(), e.message()));
            exit(1)
        }
    };
    #[cfg(debug_assertions)] {
        debug::print_code(&code);
    }
    interpreter::execute(Rc::new(code));
}
