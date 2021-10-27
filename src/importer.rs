use std::path::{Path, PathBuf};
use crate::ast::Ast;
use crate::parser;
use std::rc::Rc;
use std::collections::{VecDeque, HashMap, HashSet};
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

    pub fn sort(mut self) -> Result<Vec<Module>, String> {
        let mut result = Vec::new();
        let mut stack = vec![(self.root, 0)];
        let mut seen = HashSet::new();
        while let Some((name, i)) = stack.pop() {
            seen.insert(Rc::clone(&name));
            let deps = if let Some((_, deps)) = self.asts.get(&name) {
                deps
            } else {
                continue
            };
            if deps.len() == i {
                let (ast, dependencies) = self.asts.remove(&name).expect("missing ast");
                seen.remove(&name);
                result.push(Module {name, dependencies, ast });
            } else {
                let dep_name = Rc::clone(&deps[i]);
                if seen.contains(&dep_name) {
                    return Err(format!("Dependency cycle found in {} via {}", name, dep_name));
                }
                stack.push((name, i + 1));
                stack.push((dep_name, 0));
            }
        }
        Ok(result)
    }
}

pub struct Module {
    pub name: Rc<String>,
    pub dependencies: Vec<Rc<String>>,
    pub ast: Ast,
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

    fn parse(&mut self, root_dir: &Path, filepath: &Path, name: Rc<String>) {
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
            let name = path_name(&new_path.strip_prefix(root_dir).expect("Make relative path"));
            self.queue.push_back((new_path, Rc::clone(&name)));
            deps.push(name)
        }

        self.asts.insert(name, (ast, deps));
    }

    fn parse_all(mut self, root_filepath: &Path) -> Result<ModuleGraph, Vec<Error>> {
        let root_dir = root_filepath.parent().unwrap();
        let root_name = path_name(root_filepath.strip_prefix(root_dir).expect("Make relative path"));
        self.parse(root_dir, root_filepath, Rc::clone(&root_name));
        while let Some((path, name)) = self.queue.pop_front() {
            self.parse(root_dir, &path, name);
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

#[cfg(test)]
mod test {
    use crate::ast::{Namespace, QName};
    use super::*;

    #[test]
    fn module_graph_sort() {
        let a = Rc::new("a".to_string());
        let b = Rc::new("b".to_string());
        let c = Rc::new("c".to_string());
        let d = Rc::new("d".to_string());
        let e = Rc::new("e".to_string());

        let mut asts = HashMap::new();
        asts.insert(Rc::clone(&a), (make_ast(), vec![Rc::clone(&b), Rc::clone(&e)]));
        asts.insert(Rc::clone(&b), (make_ast(), vec![Rc::clone(&c), Rc::clone(&d)]));
        asts.insert(Rc::clone(&c), (make_ast(), vec![Rc::clone(&d)]));
        asts.insert(Rc::clone(&d), (make_ast(), vec![]));
        asts.insert(Rc::clone(&e), (make_ast(), vec![]));

        let graph = ModuleGraph {
            asts,
            root: a,
        };

        let mut order = graph.sort().expect("no loop").into_iter().map(|module| module.name);

        assert_eq!(Some("d"), order.next().as_ref().map(|it| it.as_str()));
        assert_eq!(Some("c"), order.next().as_ref().map(|it| it.as_str()));
        assert_eq!(Some("b"), order.next().as_ref().map(|it| it.as_str()));
        assert_eq!(Some("e"), order.next().as_ref().map(|it| it.as_str()));
        assert_eq!(Some("a"), order.next().as_ref().map(|it| it.as_str()));
        assert_eq!(None, order.next());
    }

    fn make_ast() -> Ast {
        Ast {
            imports: vec![],
            root_namespace: Namespace {
                name: QName { parts: vec![] },
                public: false,
                decls: vec![]
            }
        }
    }
}