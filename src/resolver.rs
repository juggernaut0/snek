use std::collections::HashMap;
use crate::ast::*;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Declaration {
    name: Rc<NamePattern>,
    id: u32
}

impl Declaration {
    pub fn name(&self) -> &String {
        &self.name.name
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct QNameExpr {
    line: u32,
    col: u32
}

impl QNameExpr {
    pub fn from(expr: &Expr) -> Option<QNameExpr> {
        if let ExprType::QName(_) = expr.expr_type {
            Some(QNameExpr {
                line: expr.line,
                col: expr.col
            })
        } else {
            None
        }
    }
}

pub struct Resolver {
    declarations: HashMap<Rc<NamePattern>, u32>,
    usages: HashMap<QNameExpr, u32>,
    declaration_id_seq: u32
}

impl Resolver {
    pub fn new(ast: &Ast) -> Resolver {
        let mut r = Resolver {
            declarations: HashMap::new(),
            usages: HashMap::new(),
            declaration_id_seq: 0
        };
        r.resolve(ast);
        r
    }

    fn resolve(&mut self, ast: &Ast) {
        let declarations: Vec<_> = ast.root_namespace.decls.iter().flat_map(|d| self.find_declarations(d)).collect();
        let root_scope = Scope::new(&declarations, None);
        self.add_declarations(&declarations);
        for decl in &ast.root_namespace.decls {
            self.resolve_usages_decl(decl, &root_scope);
        }
        if let Some(expr) = &ast.expr {
            self.resolve_usages_expr(expr, &root_scope);
        }
    }

    fn resolve_usages_decl(&mut self, decl: &Decl, scope: &Scope) {
        unimplemented!("resolve_usages_decl")
    }

    fn resolve_usages_expr(&mut self, expr: &Expr, scope: &Scope) {
        unimplemented!("resolve_usages_expr")
    }

    fn find_declarations(&mut self, decl: &Decl) -> Vec<Declaration> {
        match decl {
            Decl::Binding(b) => self.extract_names(&b.pattern),
            _ => Vec::new()
        }
    }

    fn extract_names(&mut self, pattern: &Pattern) -> Vec<Declaration> {
        match pattern {
            Pattern::Name(np) => vec!(self.make_declaration(Rc::clone(np))),
            Pattern::Type(_, inners) => inners.iter().flat_map(|p| self.extract_names(p)).collect(),
            Pattern::List(inners) => inners.iter().flat_map(|p| self.extract_names(p)).collect(),
            _ => Vec::new()
        }
    }

    fn make_declaration(&mut self, name: Rc<NamePattern>) -> Declaration {
        let id = self.declaration_id_seq;
        self.declaration_id_seq += 1;
        Declaration {
            name,
            id
        }
    }

    fn add_declarations(&mut self, declarations: &Vec<Declaration>) {
        for d in declarations {
            self.declarations.insert(Rc::clone(&d.name), d.id);
        }
    }
}

struct Scope {
    declarations: Vec<Declaration>,
    parent: Option<Box<Scope>>
}

impl Scope {
    fn new(declarations: &Vec<Declaration>, parent: Option<Scope>) -> Scope {
        Scope {
            declarations: declarations.clone(),
            parent: parent.map(|s| Box::new(s))
        }
    }
}
