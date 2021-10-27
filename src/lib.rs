use std::path::Path;
use std::process::exit;
use std::rc::Rc;

use crate::ast::Ast;
use crate::importer::{BaseError, ModuleGraph};
use crate::resolver::irt::IrTree;
use crate::resolver::ModuleDecls;

pub mod ast;
mod importer;
mod parser;
pub mod resolver;
mod scanner;
pub mod util;

pub fn parse(path: &Path) -> ModuleGraph {
    match importer::import(path) {
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
    }
}

pub fn resolve(name: Rc<String>, deps: &[ModuleDecls], ast: &Ast) -> (ModuleDecls, IrTree) {
    match resolver::resolve(Rc::clone(&name), deps, ast) {
        Ok(stuff) => stuff,
        Err(errors) => {
            errors.iter().for_each(|e| {
                eprintln!("{} [{}:{}] [ERROR] {}", name, e.line, e.col, e.message)
            });
            exit(1)
        }
    }
}
