use std::collections::HashMap;
use crate::ast::*;
use std::rc::Rc;

pub struct ModuleDecls {
    name: Rc<String>,
    root_ns: NamespaceDeclaration,
}

impl ModuleDecls {
    fn get_type(&self, name: &QName) -> Option<&TypeDeclaration> {
        let mut current = &self.root_ns;
        let last_index = name.parts.len() - 1;
        for part in &name.parts[0..last_index] {
            current = current.namespaces.iter().find(|it| &it.name == part)?;
        }
        let last = &name.parts[last_index];
        current.types.iter().find(|it| &it.name == last)
    }
}

pub struct NamespaceDeclaration {
    name: String,
    types: Vec<TypeDeclaration>,
    bindings: Vec<ValueDeclaration>,
    namespaces: Vec<NamespaceDeclaration>,
}

pub struct TypeDeclaration {
    name: String,
    id: TypeId,
}
pub struct ValueDeclaration {
    name: String,
    type_id: TypeId,
    id: ValueId,
}

#[derive(Copy, Clone)]
pub struct ValueId(u16);
#[derive(Copy, Clone)]
pub struct TypeId(u16);

#[derive(Eq, PartialEq, Hash)]
struct QNameExpr {
    line: u32,
    col: u32
}

impl QNameExpr {
    fn from(expr: &Expr) -> Option<QNameExpr> {
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

pub struct Resolver<'deps> {
    declarations: HashMap<Rc<NamePattern>, ValueId>,
    usages: HashMap<QNameExpr, (ValueId, String)>,
    dependencies: HashMap<&'deps String, &'deps ModuleDecls>,
    value_id_seq: u16,
    type_id_seq: u16,
}

impl Resolver<'_> {
    pub fn new<'d>(ast: &Ast, deps: &'d [ModuleDecls]) -> Resolver<'d> {
        let mut dependencies = HashMap::new();
        for dep in deps {
            dependencies.insert(dep.name.as_ref(), dep);
        }
        let mut r = Resolver {
            declarations: HashMap::new(),
            usages: HashMap::new(),
            dependencies,
            value_id_seq: 0,
            type_id_seq: 0,
        };
        r.resolve(ast);
        r
    }

    pub fn get_declaration(&self, name: &Rc<NamePattern>) -> ValueId {
        *self.declarations.get(name).expect("unknown declaration")
    }

    pub fn get_usage(&self, expr: &Expr) -> Option<&(ValueId, String)> {
        let k = QNameExpr::from(expr)?;
        self.usages.get(&k)
    }

    fn resolve(&mut self, ast: &Ast) {
        let mut type_decls = Vec::new();
        let mut value_decls = Vec::new();
        for import in &ast.imports {
            let dep = self.dependencies.get(&import.filename).expect("Missing dependency");
            for name in import.names {

            }
        }
        let declarations = ast.root_namespace.decls.iter().flat_map(|d| self.find_declarations(d)).collect();
        let root_scope = Scope::new(&declarations, None);
        self.add_declarations(&declarations);
        /*for decl in &ast.root_namespace.decls {
            self.resolve_usages_decl(decl, &root_scope);
        }
        if let Some(expr) = &ast.expr {
            self.resolve_usages_expr(expr, &root_scope);
        }*/
    }

    /*fn resolve_usages_decl(&mut self, decl: &Decl, scope: &Scope) {
        match decl {
            Decl::Binding(b) => self.resolve_usages_expr(&b.expr, scope),
            Decl::Namespace(ns) => self.resolve_usages_namespace(ns, scope),
            Decl::Type(t) => {
                if let Some(ns) = &t.namespace {
                    self.resolve_usages_namespace(ns, scope)
                }
            }
        }
    }

    fn resolve_usages_namespace(&mut self, ns: &Namespace, parent: &Scope) {
        let declarations = ns.decls.iter().flat_map(|d| self.find_declarations(d)).collect();
        self.add_declarations(&declarations);
        let scope = Scope::new(&declarations, Some(parent));
        for decl in &ns.decls {
            self.resolve_usages_decl(decl, &scope);
        }
    }

    fn resolve_usages_expr(&mut self, expr: &Expr, scope: &Scope) {
        match &expr.expr_type {
            ExprType::QName(qn) => {
                if qn.parts.len() == 1 {
                    if let Some(d) = scope.get(&qn.parts[0]) {
                        self.add_usage(expr, d);
                    }
                }
            },
            ExprType::Unary(_, e) => self.resolve_usages_expr(e, scope),
            ExprType::Binary(_, e1, e2) => {
                self.resolve_usages_expr(e1, scope);
                self.resolve_usages_expr(e2, scope);
            },
            ExprType::Call(ce) => {
                self.resolve_usages_expr(&ce.callee, scope);
                ce.args.iter().for_each(|e| self.resolve_usages_expr(e, scope));
            },
            ExprType::List(es) => es.iter().for_each(|e| self.resolve_usages_expr(e, scope)),
            ExprType::Lambda(le) => {
                let mut declarations: Vec<_> = le.params.iter()
                    .flat_map(|p| self.extract_names(p))
                    .collect();
                declarations.extend(le.bindings.iter().flat_map(|b| self.extract_names(&b.pattern)));
                self.add_declarations(&declarations);
                let lambda_scope = Scope::new(&declarations, Some(scope));
                for b in &le.bindings {
                    self.resolve_usages_expr(&b.expr, &lambda_scope);
                }
                self.resolve_usages_expr(&le.expr, &lambda_scope);
            }
            ExprType::Constant(_) | ExprType::Dot => {} // No action
        }
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
    }*/

    fn add_declarations(&mut self, declarations: &Vec<Declaration>) {
        for d in declarations {
            self.declarations.insert(Rc::clone(&d.name), d.id);
        }
    }

    fn add_usage(&mut self, qname: &Expr, declaration: &Declaration) {
       let qne = QNameExpr::from(qname).unwrap(); // only called with a QName Expr
        self.usages.insert(qne, (declaration.id, declaration.name().clone()));
    }
}

struct Scope<'a> {
    declarations: Vec<Declaration>,
    parent: Option<&'a Scope<'a>>
}

impl<'a> Scope<'a> {
    fn new(declarations: &Vec<Declaration>, parent: Option<&'a Scope<'a>>) -> Scope<'a> {
        Scope {
            declarations: declarations.clone(),
            parent
        }
    }

    fn get(&'a self, name: &str) -> Option<&'a Declaration> {
        self.declarations.iter().find(|d| &d.name.name == name).or_else(|| self.parent.and_then(|p| p.get(name)))
    }
}
