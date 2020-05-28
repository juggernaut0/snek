use std::path::{Path, PathBuf};
use crate::ast::Ast;
use crate::parser;
use std::rc::Rc;
use std::collections::{VecDeque, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::mem::take;

pub fn import(filepath: &Path) -> Result<Vec<Ast>, Vec<ParseError>> {
    Importer::new().parse_all(filepath)
}

struct Importer {
    asts: Vec<Ast>,
    errors: Vec<ParseError>,
    seen: HashSet<u64>,
    queue: VecDeque<PathBuf>,
}

impl Importer {
    fn new() -> Importer {
        Importer {
            asts: Vec::new(),
            errors: Vec::new(),
            seen: HashSet::new(),
            queue: VecDeque::new(),
        }
    }

    fn parse(&mut self, filepath: &Path) {
        let name = Rc::new(filepath.to_string_lossy().to_string());
        let name_hash = hash(&name);
        if self.seen.contains(&name_hash) {
            return
        } else {
            self.seen.insert(name_hash);
        }

        let src = std::fs::read_to_string(filepath).unwrap();
        let (mut ast, errs) = parser::parse(&src);

        let file_errs = errs.into_iter().map(|e| ParseError::from(e, Rc::clone(&name)));
        self.errors.extend(file_errs);

        for import in &mut ast.imports {
            let filename = take(&mut import.filename);
            let new_path = filepath.join(&filename);
            self.queue.push_back(new_path);
        }

        self.asts.push(ast);
    }

    fn parse_all(mut self, root_filepath: &Path) -> Result<Vec<Ast>, Vec<ParseError>> {
        self.parse(root_filepath);
        while let Some(path) = self.queue.pop_front() {
            self.parse(&path);
        }
        if self.errors.is_empty() {
            Ok(self.asts)
        } else {
            Err(self.errors)
        }
    }
}

pub struct ParseError {
    file: Rc<String>,
    base_err: parser::ParseError,
}

impl ParseError {
    fn from(base_err: parser::ParseError, file: Rc<String>) -> ParseError {
        ParseError {
            base_err,
            file,
        }
    }

    pub fn message(&self) -> &String {
        self.base_err.message()
    }

    pub fn line(&self) -> u32 {
        self.base_err.line()
    }

    pub fn col(&self) -> u32 {
        self.base_err.col()
    }

    pub fn file(&self) -> &String { &self.file }
}

fn hash(s: &String) -> u64 {
    let mut h = DefaultHasher::new();
    s.hash(&mut h);
    h.finish()
}
