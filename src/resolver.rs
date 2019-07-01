use std::collections::HashMap;
use crate::ast::*;
use std::rc::Rc;

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct Declaration(Rc<NamePattern>);

impl Declaration {
    pub fn name(&self) -> &String {
        &self.0.name
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
    declarations: HashMap<Rc<NamePattern>, Declaration>,
    usages: HashMap<QNameExpr, Declaration>,
}

impl Resolver {
    pub fn new(ast: &Ast) -> Resolver {
        let mut r = Resolver {
            declarations: HashMap::new(),
            usages: HashMap::new(),
        };
        r.resolve(ast);
        r
    }

    fn resolve(&mut self, ast: &Ast) {
        let declarations: Vec<_> = ast.root_namespace.decls.iter().flat_map(|d| find_declarations(d)).collect();
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

    fn add_declarations(&mut self, declarations: &Vec<Declaration>) {
        for d in declarations {
            self.declarations.insert(Rc::clone(&d.0), d.clone());
        }
    }
}

fn find_declarations(decl: &Decl) -> Vec<Declaration> {
    match decl {
        Decl::Binding(b) => extract_names(&b.pattern),
        _ => Vec::new()
    }
}

fn extract_names(pattern: &Pattern) -> Vec<Declaration> {
    match pattern {
        Pattern::Name(np) => vec!(Declaration(Rc::clone(np))),
        Pattern::Type(_, inners) => inners.iter().flat_map(|p| extract_names(p)).collect(),
        Pattern::List(inners) => inners.iter().flat_map(|p| extract_names(p)).collect(),
        _ => Vec::new()
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
