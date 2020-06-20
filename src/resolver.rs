use std::collections::HashMap;
use crate::ast::*;
use std::rc::Rc;

pub struct ModuleDecls {
    name: Rc<String>,
    root_ns: NamespaceDeclaration,
}

impl ModuleDecls {
    fn goto_ns(&self, name: &QName) -> Option<&NamespaceDeclaration> {
        let mut current = &self.root_ns;
        let last_index = name.parts.len() - 1;
        for part in &name.parts[0..last_index] {
            current = current.namespaces.iter().find(|it| &it.name == part)?;
        }
        Some(current)
    }

    fn get_type(&self, name: &QName) -> Option<&TypeDeclaration> {
        let ns = self.goto_ns(name)?;
        let last = name.parts.last().unwrap();
        ns.types.iter().find(|it| &it.name == last)
    }

    fn get_value(&self, name: &QName) -> Option<&ValueDeclaration> {
        let ns = self.goto_ns(name)?;
        let last = name.parts.last().unwrap();
        ns.bindings.iter().find(|it| &it.name == last)
    }
}

#[derive(Default)]
pub struct NamespaceDeclaration {
    name: String,
    types: Vec<TypeDeclaration>,
    bindings: Vec<ValueDeclaration>,
    namespaces: Vec<NamespaceDeclaration>,
}

pub struct TypeDeclaration {
    name: String,
    id: TypeId,
    definition: TypeDefinition,
    visibility: QName,
}
#[derive(Clone)]
pub enum TypeDefinition {
    Undefined,
    Record(Vec<(String, TypeId)>),
    Union(Vec<TypeId>),
}
pub struct ValueDeclaration {
    name: String,
    type_id: TypeId,
    id: ValueId,
}

#[derive(Copy, Clone)]
pub struct ValueId(u16);
#[derive(Clone, Eq, PartialEq)]
pub struct TypeId(Rc<(Rc<String>, Vec<String>)>);

impl TypeId {
    fn new(mod_name: Rc<String>, fqn: QName) -> TypeId {
        TypeId(Rc::new((mod_name, fqn.parts)))
    }
}

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
    type_declarations: HashMap<String, TypeId>,
    usages: HashMap<QNameExpr, (ValueId, String)>,
    dependencies: HashMap<&'deps String, &'deps ModuleDecls>,
    exports: ModuleDecls,
    value_id_seq: u16,
    errors: Vec<Error>
}

impl Resolver<'_> {
    pub fn new<'d>(name: Rc<String>, ast: &Ast, deps: &'d [ModuleDecls]) -> Resolver<'d> {
        let mut dependencies = HashMap::new();
        for dep in deps {
            dependencies.insert(dep.name.as_ref(), dep);
        }
        let mut exports = ModuleDecls {
            name,
            root_ns: NamespaceDeclaration::default(),
        };
        let mut r = Resolver {
            declarations: HashMap::new(),
            type_declarations: HashMap::new(),
            usages: HashMap::new(),
            dependencies,
            exports,
            value_id_seq: 0,
            errors: Vec::new(),
        };
        r.resolve(ast);
        r
    }

    /*
    pub fn get_declaration(&self, name: &Rc<NamePattern>) -> ValueId {
        *self.declarations.get(name).expect("unknown declaration")
    }

    pub fn get_usage(&self, expr: &Expr) -> Option<&(ValueId, String)> {
        let k = QNameExpr::from(expr)?;
        self.usages.get(&k)
    }
    */

    fn resolve(&mut self, ast: &Ast) {
        let (mut type_decls, mut val_decls) = self.import_names(&ast.imports);
        for decl in &ast.root_namespace.decls {
            self.add_type_decl(decl, QNameList::Empty, QNameList::Empty, &mut type_decls)
        }
        let type_lookup = TypeDeclLookup {
            decls: type_decls.iter().map(|d| d.id.clone()).collect()
        };
        //let declarations = ast.root_namespace.decls.iter().flat_map(|d| self.find_declarations(d)).collect();
        //let root_scope = Scope::new(val_decls, type_decls, None);
        //self.add_declarations(&declarations);
        /*for decl in &ast.root_namespace.decls {
            self.resolve_usages_decl(decl, &root_scope);
        }
        if let Some(expr) = &ast.expr {
            self.resolve_usages_expr(expr, &root_scope);
        }*/
    }

    fn import_names(&mut self, imports: &[Import]) -> (Vec<TypeDeclaration>, Vec<ValueDeclaration>) {
        let mut type_decls = Vec::new();
        let mut value_decls = Vec::new();
        for import in imports {
            let dep = *self.dependencies.get(&import.filename).expect("Missing dependency");
            for name in &import.names {
                let mut found = false;
                if let Some(td) = dep.get_type(&name.name) {
                    type_decls.push(self.import_type(td));
                    found = true;
                }
                if let Some(vd) = dep.get_value(&name.name) {
                    value_decls.push(self.import_value(vd));
                    found = true;
                }
                if !found {
                    self.errors.push(Error::new(format!("{} not found in import {}", name.name, import.filename), name.line, name.col));
                }
            }
        }
        (type_decls, value_decls)
    }

    fn import_type(&mut self, td: &TypeDeclaration) -> TypeDeclaration {
        TypeDeclaration {
            name: td.name.clone(),
            id: td.id.clone(),
            definition: td.definition.clone(),
            visibility: QName {
                parts: td.visibility.parts.clone()
            },
        }
    }

    fn import_value(&mut self, vd: &ValueDeclaration) -> ValueDeclaration {
        let id = ValueId(self.value_id_seq);
        self.value_id_seq += 1;
        ValueDeclaration {
            name: vd.name.clone(),
            type_id: vd.type_id.clone(),
            id
        }
    }

    fn add_type_decl(&mut self, decl: &Decl, namespace: QNameList, visibility: QNameList, type_decls: &mut Vec<TypeDeclaration>) {
        match decl {
            Decl::Namespace(ns) => {
                for decl in &ns.decls {
                    let vis = if decl.is_public() { visibility } else { namespace };
                    self.add_type_decl(decl, namespace.append(&ns.name), vis, type_decls)
                }
            },
            Decl::Type(t) => {
                let fqn = {
                    let mut ns = namespace.to_qname();
                    ns.parts.push(t.name.name.clone());
                    ns
                };
                type_decls.push(TypeDeclaration {
                    name: t.name.name.clone(),
                    id: TypeId::new(Rc::clone(&self.exports.name), fqn),
                    definition: TypeDefinition::Undefined,
                    visibility: visibility.to_qname(),
                });
            }
            _ => ()
        }
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

    /*fn add_declarations(&mut self, declarations: &Vec<Declaration>) {
        for d in declarations {
            self.declarations.insert(Rc::clone(&d.name), d.id);
        }
    }

    fn add_usage(&mut self, qname: &Expr, declaration: &Declaration) {
       let qne = QNameExpr::from(qname).unwrap(); // only called with a QName Expr
        self.usages.insert(qne, (declaration.id, declaration.name().clone()));
    }*/
}

/*struct Scope<'a> {
    val_decls: Vec<ValueDeclaration>,
    type_decls: Vec<TypeDeclaration>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    fn new(val_decls: Vec<ValueDeclaration>, type_decls: Vec<TypeDeclaration>, parent: Option<&'a Scope<'a>>) -> Scope<'a> {
        Scope {
            val_decls,
            type_decls,
            parent,
        }
    }

    fn get(&'a self, name: &str) -> Option<&'a Declaration> {
        self.declarations.iter().find(|d| &d.name.name == name).or_else(|| self.parent.and_then(|p| p.get(name)))
    }
}*/

struct Error {
    message: String,
    line: u32,
    col: u32,
}

impl Error {
    fn new(message: String, line: u32, col: u32) -> Error {
        Error {
            message: message.to_string(),
            line,
            col,
        }
    }
}

struct TypeDeclLookup {
    decls: Vec<TypeId>,
}

impl TypeDeclLookup {
    fn new(decls: Vec<TypeId>) -> TypeDeclLookup {
        TypeDeclLookup {
            decls,
        }
    }

    fn root_scope(&self) -> TypeDeclLookupScope {
        TypeDeclLookupScope {
            lookup: self,
            scope: QNameList::Empty,
        }
    }
}

struct TypeDeclLookupScope<'lookup, 'ast, 'parent> {
    lookup: &'lookup TypeDeclLookup,
    scope: QNameList<'ast, 'parent>,
}

impl<'ast> TypeDeclLookupScope<'_, 'ast, '_> {
    fn enter_scope(&self, name: &'ast QName) -> TypeDeclLookupScope {
        TypeDeclLookupScope {
            lookup: self.lookup,
            scope: self.scope.append(name),
        }
    }

    fn get_type(&self, name: &QName) -> Option<TypeId> {
        let mut current_scope = self.scope;
        loop {
            let full_name = current_scope.append(name);
            let id = self.lookup.decls
                .iter()
                .find(|&decl| full_name.matches(decl))
                .cloned();
            if id.is_some() { return id }
            if let QNameList::Empty = current_scope {
                break
            }
            current_scope = current_scope.pop_namespace();
        }
        None
    }
}

#[derive(Clone, Copy)]
enum QNameList<'ast, 'parent> {
    Empty,
    List(&'parent QNameList<'ast, 'parent>, &'ast [String])
}

impl<'parent, 'ast> QNameList<'parent, 'ast> {
    fn append(&'parent self, qname: &'ast QName) -> QNameList<'parent, 'ast> {
        QNameList::List(self, &qname.parts)
    }

    fn len(&self) -> usize {
        match self {
            QNameList::Empty => 0,
            QNameList::List(parent, name) => parent.len() + name.len()
        }
    }

    fn iter(&self) -> QNameListIter {
        QNameListIter::new(self)
    }

    fn matches(&self, type_id: &TypeId) -> bool {
        let names = &(type_id.0).1;
        if names.len() != self.len() { return false }
        for (a, b) in names.iter().zip(self.iter()) {
            if a != b { return false }
        }
        true
    }

    fn pop_namespace(&self) -> QNameList<'parent, 'ast> {
        match self {
            QNameList::Empty => QNameList::Empty,
            QNameList::List(parent, names) => {
                let names_len = names.len();
                if names_len == 1 {
                    **parent
                } else {
                    QNameList::List(parent, &names[0..names_len-1])
                }
            }
        }
    }

    fn to_qname(&self) -> QName {
        QName {
            parts: self.iter().cloned().collect()
        }
    }
}

struct QNameListIter<'ast> {
    parent: Option<Box<QNameListIter<'ast>>>,
    name: Option<std::slice::Iter<'ast, String>>
}

impl<'ast> QNameListIter<'ast> {
    fn new(list: &'ast QNameList<'ast, '_>) -> QNameListIter<'ast> {
        match list {
            QNameList::Empty => QNameListIter {
                parent: None,
                name: None,
            },
            QNameList::List(parent, name) => QNameListIter {
                parent: Some(Box::new(parent.iter())),
                name: Some(name.iter()),
            },
        }
    }
}

impl<'ast> Iterator for QNameListIter<'ast> {
    type Item = &'ast String;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(parent) = &mut self.parent {
            parent.next().or_else(|| self.name.as_mut().unwrap().next())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::QName;
    use crate::resolver::{QNameList, TypeId, TypeDeclLookup};
    use std::rc::Rc;
    use std::fmt::{Debug, Formatter};

    #[test]
    fn qname_list_iter() {
        let a = vec![String::from("a"), String::from("b")];
        let b = vec![String::from("c")];
        let c = vec![String::from("d"), String::from("e")];

        let l1 = QNameList::Empty;
        let l2 = QNameList::List(&l1, &a);
        let l3 = QNameList::List(&l2, &b);
        let l4 = QNameList::List(&l3, &c);

        assert_eq!(5, l4.len());
        let full: Vec<_> = l4.iter().collect();
        assert_eq!(vec!["a", "b", "c", "d", "e"], full);
    }

    #[test]
    fn qname_list_matches() {
        let type_id = TypeId(Rc::new((Rc::new(String::new()), vec![String::from("a"), String::from("b"), String::from("c")])));

        let a = vec![String::from("a"), String::from("b")];
        let b = vec![String::from("c")];

        let l1 = QNameList::Empty;
        let l2 = QNameList::List(&l1, &a);
        let l3 = QNameList::List(&l2, &b);

        assert!(l3.matches(&type_id))
    }

    #[test]
    fn type_decl_lookup() {
        let type_id = TypeId(Rc::new((Rc::new(String::from("test")), vec![String::from("A"), String::from("A")])));
        let lookup = TypeDeclLookup {
            decls: vec![type_id.clone()]
        };

        let A = QName { parts: vec![String::from("A")] };
        let A_A = QName { parts: vec![String::from("A"), String::from("A")] };

        {
            let scope1 = lookup.root_scope();
            let scope2 = scope1.enter_scope(&A);

            let a = scope2.get_type(&A);
            assert_eq!(type_id, a.unwrap());

            let a_a = scope2.get_type(&A_A);
            assert_eq!(type_id, a_a.unwrap());
        }
    }

    impl Debug for TypeId {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}:{:?}", (self.0).0, (self.0).1)
        }
    }
}