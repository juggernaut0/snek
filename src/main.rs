use std::env;
use std::process::exit;
use std::io::{stdin, stdout, Write};
use crate::ast::Ast;
use crate::parser::ParseError;
use std::path::Path;
use crate::importer::BaseError;
use crate::resolver::Resolver;
use std::rc::Rc;

mod ast;
//mod codegen;
//mod interpreter;
mod importer;
//mod gc;
//mod opcode;
mod parser;
mod resolver;
mod scanner;
//mod value;

#[cfg(debug_assertions)]
mod debug;

fn main() {
    let args = env::args_os();
    let path = args.skip(1).next();
    if let Some(p) = path {
        run_from_file(Path::new(&p));
    } else {
        repl();
    }
}

fn run_from_file(path: &Path) {
    let asts = match importer::import(path) {
        Ok(asts) => asts,
        Err(errs) => {
            errs.iter().for_each(|e| {
                match e.base_error() {
                    BaseError::ParseError(pe) => eprintln!("{} [{}:{}] [ERROR] {}", e.file(), pe.line(), pe.col(), pe.message()),
                    BaseError::IoError(ioe) => eprintln!("{} [ERROR] {:?}", e.file(), ioe),
                }
            });
            exit(1)
        }
    };
    #[cfg(debug_assertions)] {
        let mut queue = vec![asts.root()];
        while let Some(name) = queue.pop() {
            let (ast, deps) = asts.get_ast(name);
            println!("{} - deps: {:?}", name, deps);
            debug::AstPrinter::new().print_ast(ast);
            for dep in deps {
                queue.push(dep)
            }
        }
    }
    let root_name = asts.root();
    let (root, _) = asts.get_ast(&root_name);
    Resolver::new(Rc::clone(root_name), root, &[]); // TODO deps
    /*let code = match codegen::compile(&ast) {
        Ok(code) => code,
        Err(errs) => {
            errs.iter().for_each(|e| eprintln!("[{}] [ERROR] {}", e.line(), e.message()));
            exit(1)
        }
    };
    #[cfg(debug_assertions)] {
        debug::print_code(&code);
    }
    interpreter::execute(Rc::new(code));*/
}

fn repl() {
    let mut src = String::new();
    loop {
        print!(">>> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut src).unwrap();
        match extract(parser::parse(&src)) {
            Ok(ast) => {
                #[cfg(debug_assertions)] {
                    debug::AstPrinter::new().print_ast(&ast);
                }
            },
            Err(errs) => {
                errs.iter().for_each(|e| eprintln!("[{}:{}] [ERROR] {}", e.line(), e.col(), e.message()));
            }
        };
        src.clear();
    }
}

fn extract((ast, errs): (Ast, Vec<ParseError>)) -> Result<Ast, Vec<ParseError>> {
    if errs.is_empty() { Ok(ast) } else { Err(errs) }
}
