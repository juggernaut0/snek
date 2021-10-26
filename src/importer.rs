use std::path::{Path, PathBuf};
use crate::ast::Ast;
use crate::parser;
use std::rc::Rc;
use std::collections::{VecDeque, HashMap};
use crate::parser::ParseError;

pub fn import(filepath: &Path) -> Result<ModuleGraph, Vec<Error>> {
    Importer::new().parse_all(filepath)
}

pub struct ModuleGraph {
    asts: HashMap<Rc<String>, (Ast, Vec<Rc<String>>)>,
    root: Rc<String>,
}

impl ModuleGraph {
    pub fn root(&self) -> &Rc<String> {
        &self.root
    }

    pub fn get_ast(&self, #[allow(clippy::ptr_arg)] name: &String) -> (&Ast, &[Rc<String>]) {
        let (ast, deps) = self.asts.get(name).expect("ast not found");
        (ast, deps.as_slice())
    }

    pub fn sort(mut self) -> Result<Vec<(Rc<String>, Ast, Vec<Rc<String>>)>, String> {
        let mut result = Vec::new();
        let mut stack = vec![self.root];
        while let Some(name) = stack.pop() {
            let (ast, deps) = if let Some(v) = self.asts.remove(&name) {
                v
            } else {
                return Err(format!("Circular dependency detected with {}", name));
            };
            for dep in &deps {
                stack.push(Rc::clone(dep));
            }
            result.push((name, ast, deps));
        }
        result.reverse();
        Ok(result)
    }
}

struct Importer {
    asts: HashMap<Rc<String>, (Ast, Vec<Rc<String>>)>,
    errors: Vec<Error>,
    queue: VecDeque<(PathBuf, Rc<String>)>,
}

impl Importer {
    fn new() -> Importer {
        Importer {
            asts: HashMap::new(),
            errors: Vec::new(),
            queue: VecDeque::new(),
        }
    }

    fn parse(&mut self, filepath: &Path, name: Rc<String>) {
        if self.asts.contains_key(&name) {
            return
        }

        let src = match std::fs::read_to_string(filepath) {
            Ok(src) => src,
            Err(e) => {
                self.errors.push(Error::from(BaseError::IoError(e), name));
                return;
            },
        };
        let (mut ast, errs) = parser::parse(&src);

        let file_errs = errs.into_iter()
            .map(|e| {
                Error::from(BaseError::ParseError(e), Rc::clone(&name))
            });
        self.errors.extend(file_errs);

        let mut deps = Vec::new();
        for import in &mut ast.imports {
            let new_path = filepath.parent().unwrap().join(&import.filename);
            let name = path_name(&new_path);
            self.queue.push_back((new_path, Rc::clone(&name)));
            deps.push(name)
        }

        self.asts.insert(name, (ast, deps));
    }

    fn parse_all(mut self, root_filepath: &Path) -> Result<ModuleGraph, Vec<Error>> {
        let root_name = path_name(root_filepath);
        self.parse(root_filepath, Rc::clone(&root_name));
        while let Some((path, name)) = self.queue.pop_front() {
            self.parse(&path, name);
        }
        if self.errors.is_empty() {
            Ok(ModuleGraph { asts: self.asts, root: root_name })
        } else {
            Err(self.errors)
        }
    }
}

fn path_name(filepath: &Path) -> Rc<String> {
    Rc::new(filepath.display().to_string())
}

pub struct Error {
    file: Rc<String>,
    base_err: BaseError,
}

pub enum BaseError{
    ParseError(ParseError),
    IoError(std::io::Error),
}

impl Error {
    fn from(base_err: BaseError, file: Rc<String>) -> Error {
        Error {
            base_err,
            file,
        }
    }

    pub fn file(&self) -> &String { &self.file }

    pub fn base_error(&self) -> &BaseError {
        &self.base_err
    }
}
