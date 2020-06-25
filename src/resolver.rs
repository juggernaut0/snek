use std::collections::HashMap;
use crate::ast::*;
use std::rc::Rc;
use std::fmt::{Debug, Formatter, Display};

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

    fn get_type(&self, _name: &QName) -> Option<&TypeDeclaration> {
        /*let ns = self.goto_ns(name)?;
        let last = name.parts.last().unwrap();
        ns.types.iter().find(|it| &it.name == last)*/
        todo!()
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
    id: TypeId,
    num_type_params: usize,
    definition: TypeDefinition,
    visibility: QName,
}
#[derive(Clone)]
pub enum TypeDefinition {
    Record(Vec<(String, ResolvedType)>),
    Union(Vec<ResolvedType>),
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ResolvedType {
    Id(TypeId, Vec<ResolvedType>),
    TypeParam(usize),
    Func(ResolvedFuncType),
    Unit,
    Any,
    Nothing,
    Error,
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedFuncType {
    //pub type_params: Vec<String>, TODO
    pub params: Vec<ResolvedType>,
    pub return_type: Box<ResolvedType>,
}
struct UndefinedType<'ast> {
    id: TypeId,
    visibility: QName,
    ast_node: UndefinedTypeNode<'ast>
}
enum UndefinedTypeNode<'ast> {
    Normal(&'ast Type),
    Case(&'ast TypeCaseRecord, &'ast Vec<String>),
}

impl UndefinedType<'_> {
    fn define(self, definition: TypeDefinition, num_type_params: usize) -> TypeDeclaration {
        TypeDeclaration {
            id: self.id,
            num_type_params,
            definition,
            visibility: self.visibility,
        }
    }
}

pub struct ValueDeclaration {
    name: String,
    type_id: TypeId,
    id: ValueId,
}

#[derive(Copy, Clone)]
pub struct ValueId(u16);
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct TypeId(Rc<(Rc<String>, Vec<String>)>);

impl TypeId {
    fn new(mod_name: Rc<String>, fqn: QName) -> TypeId {
        TypeId(Rc::new((mod_name, fqn.parts)))
    }

    fn fqn(&self) -> &[String] {
        &(self.0).1
    }

    fn namespace(&self) -> &[String] {
        let fqn = self.fqn();
        &fqn[0..fqn.len()-1]
    }
}

impl Debug for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{:?}", (self.0).0, (self.0).1)
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fqn().join("."))
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
    type_declarations: HashMap<TypeId, TypeDeclaration>,
    usages: HashMap<QNameExpr, (ValueId, String)>,
    dependencies: HashMap<&'deps String, &'deps ModuleDecls>,
    exports: ModuleDecls,
    value_id_seq: u16,
    errors: Vec<Error>
}

impl Resolver<'_> {
    pub fn new(name: Rc<String>, deps: &[ModuleDecls]) -> Resolver {
        let mut dependencies = HashMap::new();
        for dep in deps {
            dependencies.insert(dep.name.as_ref(), dep);
        }
        let exports = ModuleDecls {
            name,
            root_ns: NamespaceDeclaration::default(),
        };
        Resolver {
            declarations: HashMap::new(),
            type_declarations: HashMap::new(),
            usages: HashMap::new(),
            dependencies,
            exports,
            value_id_seq: 0,
            errors: Vec::new(),
        }
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

    pub fn resolve(&mut self, ast: &Ast) {
        let _val_decls = self.import_names(&ast.imports);
        /*for decl in &ast.root_namespace.decls {
            self.add_type_decl(decl, QNameList::Empty, QNameList::Empty, &mut type_decls)
        }*/
        let mut type_declarations = Vec::new();
        self.add_types(&mut type_declarations, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        /*let type_lookup = TypeDeclLookup {
            decls: type_decls.iter().map(|d| d.id.clone()).collect()
        };*/
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

    fn import_names(&mut self, imports: &[Import]) -> Vec<ValueDeclaration> {
        let mut value_decls = Vec::new();
        for import in imports {
            let dep = *self.dependencies.get(&import.filename).expect("Missing dependency");
            for name in &import.names {
                let mut found = false;
                if let Some(td) = dep.get_type(&name.name) {
                    let imported_type = TypeDeclaration {
                        id: td.id.clone(),
                        num_type_params: td.num_type_params,
                        definition: td.definition.clone(),
                        visibility: QName {
                            parts: td.visibility.parts.clone()
                        },
                    };
                    self.type_declarations.insert(td.id.clone(), imported_type);
                    found = true;
                }
                if let Some(vd) = dep.get_value(&name.name) {
                    value_decls.push(self.import_value(vd));
                    found = true;
                }
                if !found {
                    self.errors.push(Error {
                        message: format!("{} not found in imported file {}", name.name, import.filename),
                        line: name.line,
                        col: name.col
                    });
                }
            }
        }
        value_decls
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

    fn add_types<'ast>(&self, type_declarations: &mut Vec<UndefinedType<'ast>>, ns_decl: &'ast Namespace, namespace: QNameList, visibility: QNameList) {
        for decl in &ns_decl.decls {
            let vis = if decl.is_public() { visibility } else { namespace };
            match decl {
                Decl::Namespace(ns) => {
                    self.add_types(type_declarations, ns, namespace.append(&ns.name), vis);
                },
                Decl::Type(t) => {
                    let fqn = {
                        let mut ns = namespace.to_qname();
                        ns.parts.push(t.name.name.clone());
                        ns
                    };
                    type_declarations.push(UndefinedType {
                        id: TypeId::new(Rc::clone(&self.exports.name), fqn),
                        visibility: vis.to_qname(),
                        ast_node: UndefinedTypeNode::Normal(t),
                    });
                    if let TypeContents::Union(cases) = &t.contents {
                        for case in cases {
                            if let TypeCase::Record(record) = case {
                                let vis = if record.public { visibility } else { namespace };
                                let fqn = {
                                    let mut ns = namespace.to_qname();
                                    ns.parts.push(record.name.name.clone());
                                    ns
                                };
                                type_declarations.push(UndefinedType {
                                    id: TypeId::new(Rc::clone(&self.exports.name), fqn),
                                    visibility: vis.to_qname(),
                                    ast_node: UndefinedTypeNode::Case(record, &t.name.type_params),
                                })
                            }
                        }
                    }
                }
                _ => ()
            }
        }
    }

    fn define_types(&mut self, undefined_types: Vec<UndefinedType>, root_scope: TypeDeclLookupScope) {
        for type_decl in undefined_types {
            let scope = root_scope.enter_scope(type_decl.id.namespace());
            let (definition, num_type_params) = match type_decl.ast_node {
                UndefinedTypeNode::Normal(t) => {
                    let type_params = &t.name.type_params;
                    let num_type_params = type_params.len();

                    let definition = match &t.contents {
                        TypeContents::Record(fields) => {
                            let res_fields = fields.iter()
                                .map(|f| (f.name.clone(), self.resolve_field_type(&f.type_name, type_params, &scope, false)))
                                .collect();
                            TypeDefinition::Record(res_fields)
                        },
                        TypeContents::Union(cases) => {
                            let res_cases = cases.iter()
                                .map(|case| {
                                    match case {
                                        TypeCase::Case(type_name) => {
                                            self.resolve_field_type(type_name, type_params, &scope, false)
                                        },
                                        TypeCase::Record(tcr) => {
                                            let name = std::slice::from_ref(&tcr.name.name);
                                            // An inline record type is always visible to the union
                                            let (id, _) = scope.get_type(name).unwrap();
                                            let rts = (0..num_type_params).map(|i| ResolvedType::TypeParam(i)).collect();
                                            ResolvedType::Id(id, rts)
                                        },
                                    }
                                })
                                .collect();
                            TypeDefinition::Union(res_cases)
                        },
                    };

                    (definition, num_type_params)
                },
                UndefinedTypeNode::Case(tcr, type_params) => {
                    let res_fields = tcr.fields.iter()
                        .map(|f| (f.name.clone(), self.resolve_field_type(&f.type_name, type_params, &scope, false)))
                        .collect();
                    (TypeDefinition::Record(res_fields), type_params.len())
                },
            };

            if self.type_declarations.contains_key(&type_decl.id) {
                let name = match type_decl.ast_node {
                    UndefinedTypeNode::Normal(t) => &t.name,
                    UndefinedTypeNode::Case(tcr, _) => &tcr.name,
                };
                self.errors.push(Error {
                    message: format!("Duplicate type definition: {}", type_decl.id),
                    line: name.line,
                    col: name.col
                })
            } else {
                let decl = type_decl.define(definition, num_type_params);
                self.type_declarations.insert(decl.id.clone(), decl);
            }

        }
    }

    fn resolve_field_type(&mut self, type_name: &TypeName, type_params: &[String], scope: &TypeDeclLookupScope, nothing_allowed: bool) -> ResolvedType {
        match &type_name.type_name_type {
            TypeNameType::Named(nt) => {
                let tp = if nt.name.parts.len() == 1 && nt.params.is_empty() {
                    let name = &nt.name.parts[0];
                    type_params.iter().position(|p| p == name)
                } else {
                    None
                };

                if let Some(tpi) = tp {
                    ResolvedType::TypeParam(tpi)
                } else if let Some((id, visible)) = scope.get_type(&nt.name.parts) {
                    if !visible {
                        self.errors.push(Error {
                            message: format!("Cannot access type '{}'", &nt.name.parts.join(".")),
                            line: type_name.line,
                            col: type_name.col,
                        });
                        ResolvedType::Error
                    } else {
                        let params = nt.params.iter()
                            .map(|param| self.resolve_field_type(param, type_params, scope, true))
                            .collect();
                        ResolvedType::Id(id, params)
                    }
                } else {
                    self.errors.push(Error {
                        message: format!("Unknown type name '{}'", &nt.name.parts.join(".")),
                        line: type_name.line,
                        col: type_name.col,
                    });
                    ResolvedType::Error
                }
            },
            TypeNameType::Func(_) => todo!(),
            TypeNameType::Unit => ResolvedType::Unit,
            TypeNameType::Any => ResolvedType::Any,
            TypeNameType::Nothing => {
                if nothing_allowed {
                    ResolvedType::Nothing
                } else {
                    self.errors.push(Error {
                        message: "Cannot use '!' as a record field type".to_string(),
                        line: type_name.line,
                        col: type_name.col,
                    });
                    ResolvedType::Error
                }
            },
            TypeNameType::Inferred => {
                self.errors.push(Error {
                    message: "Cannot infer types in record fields".to_string(),
                    line: type_name.line,
                    col: type_name.col,
                });
                ResolvedType::Error
            },
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

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub line: u32,
    pub col: u32,
}

struct TypeDeclLookup {
    decls: Vec<(TypeId, Vec<String>)>,
}

impl TypeDeclLookup {
    fn new(undefined_types: &[UndefinedType]) -> TypeDeclLookup {
        // TODO don't clone visibility
        TypeDeclLookup {
            decls: undefined_types
                .iter()
                .map(|ut| (ut.id.clone(), ut.visibility.parts.clone()))
                .collect()
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
    fn enter_scope(&self, name: &'ast [String]) -> TypeDeclLookupScope {
        TypeDeclLookupScope {
            lookup: self.lookup,
            scope: self.scope.append_slice(name),
        }
    }

    fn get_type(&self, name: &[String]) -> Option<(TypeId, bool)> {
        let mut current_scope = self.scope;
        loop {
            let full_name = current_scope.append_slice(name);
            let found = self.lookup.decls
                .iter()
                .find(|&(id, _)| full_name.matches(id));
            if let Some((id, visibility)) = found {
                let visible = is_visible(self.scope, &visibility);
                return Some((id.clone(), visible))
            }
            if let QNameList::Empty = current_scope {
                break
            }
            current_scope = current_scope.pop_namespace();
        }
        None
    }
}

// Is a type with visibility visible from scope?
fn is_visible(scope: QNameList, visibility: &[String]) -> bool {
    if scope.len() < visibility.len() { return false }
    scope.iter().zip(visibility).all(|(sp, vp)| sp == vp)
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

    fn append_slice(&'parent self, slice: &'ast [String]) -> QNameList<'ast, 'parent> {
        QNameList::List(self, slice)
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

    fn pop_namespace(self) -> QNameList<'parent, 'ast> {
        match self {
            QNameList::Empty => QNameList::Empty,
            QNameList::List(parent, names) => {
                let names_len = names.len();
                if names_len == 1 {
                    *parent
                } else {
                    //parent.append_slice(&names[0..names_len-1])
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
            // TODO short circuit when parent returns None
            parent.next().or_else(|| self.name.as_mut().unwrap().next())
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use crate::ast::QName;
    use crate::resolver::{QNameList, TypeId, TypeDeclLookup, Resolver, TypeDefinition, ResolvedType, UndefinedType};
    use std::rc::Rc;

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
    fn type_decl_visibility() {
        let src = "
namespace A {
    namespace B {
        public type A # visibility: A
        type B # visibility: A.B
    }
    public namespace B {
        public type C # visibility: <root>
        type D # visibility: A.B
    }
    type E # visibility: A
}
type F # visibility: <root>
        ";
        let (ast, errs) = crate::parser::parse(src);
        assert!(errs.is_empty());
        let resolver = Resolver::new(Rc::new(String::new()), &[]);
        let mut type_declarations = Vec::new();
        resolver.add_types(&mut type_declarations, &ast.root_namespace, QNameList::Empty, QNameList::Empty);

        println!("{:?}", resolver.type_declarations.keys());
        let assert_vis = |name: Vec<&str>, expected_vis: Vec<&str>| {
            println!("{:?}", name);
            let t = type_declarations.iter().find(|it| it.id.fqn() == name.as_slice()).unwrap();
            assert_eq!(expected_vis, t.visibility.parts, "type: {:?}", name)
        };

        assert_vis(vec!["A", "B", "A"], vec!["A"]);
        assert_vis(vec!["A", "B", "B"], vec!["A", "B"]);
        assert_vis(vec!["A", "B", "C"], vec![]);
        assert_vis(vec!["A", "B", "D"], vec!["A", "B"]);
        assert_vis(vec!["A", "E"], vec!["A"]);
        assert_vis(vec!["F"], vec![]);
    }

    #[test]
    fn type_definition() {
        let src = "
type Foo { a: Foo.Bar.A }
namespace Foo {
    public namespace Bar {
        public type A { b: Foo.B }
        type X { b: B }
    }
    type B { c: C }
    type Y { a: Bar.A }
}
type C
        ";
        let (ast, errs) = crate::parser::parse(src);
        assert!(errs.is_empty());
        let mod_name = Rc::new(String::new());
        let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
        let mut undefined_types = Vec::new();
        resolver.add_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        assert!(resolver.errors.is_empty());
        let lookup = TypeDeclLookup::new(&undefined_types);
        resolver.define_types(undefined_types, lookup.root_scope());
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

        let check_type_def = |name: Vec<&str>, expected_fields: Vec<(&str, Vec<&str>)>| {
            let id = TypeId::new(Rc::clone(&mod_name), to_qname(&name));
            let t = resolver.type_declarations.get(&id).unwrap();

            if let TypeDefinition::Record(fields) = &t.definition {
                assert_eq!(expected_fields.len(), fields.len(), "In type {:?}", id);
                 for ((name, res_type), (exp_name, exp_type)) in fields.iter().zip(&expected_fields) {
                     assert_eq!(exp_name, name, "In type {:?}", id);
                     assert_eq!(&ResolvedType::Id(TypeId::new(Rc::clone(&mod_name), to_qname(exp_type)), Vec::new()), res_type, "In type {:?}", id)
                 }
            } else {
                panic!("expected a record for {:?}", id.fqn())
            }
        };

        check_type_def(vec!["Foo"], vec![("a", vec!["Foo", "Bar", "A"])]);
        check_type_def(vec!["Foo", "Bar", "A"], vec![("b", vec!["Foo", "B"])]);
        check_type_def(vec!["Foo", "Bar", "X"], vec![("b", vec!["Foo", "B"])]);
        check_type_def(vec!["Foo", "B"], vec![("c", vec!["C"])]);
        check_type_def(vec!["Foo", "Y"], vec![("a", vec!["Foo", "Bar", "A"])]);
        check_type_def(vec!["C"], vec![]);
    }

    #[test]
    fn visibility_error() {
        let src = "
namespace Foo {
    type Hidden
}
type A { f: Foo.Hidden }
";
        let (ast, errs) = crate::parser::parse(src);
        assert!(errs.is_empty());
        let mod_name = Rc::new(String::new());
        let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
        let mut undefined_types = Vec::new();
        resolver.add_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        assert!(resolver.errors.is_empty());
        let lookup = TypeDeclLookup::new(&undefined_types);
        resolver.define_types(undefined_types, lookup.root_scope());
        assert!(!resolver.errors.is_empty());
    }

    #[test]
    fn union() {
        let src = "type Option<T> = Some { t: T } | None { }";
        let (ast, errs) = crate::parser::parse(src);
        assert!(errs.is_empty());
        let mod_name = Rc::new(String::new());
        let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
        let mut undefined_types = Vec::new();
        resolver.add_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);
        let lookup = TypeDeclLookup::new(&undefined_types);
        resolver.define_types(undefined_types, lookup.root_scope());
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

        assert_eq!(3, resolver.type_declarations.len());
        // TODO more asserts?
    }

    fn to_qname(strs: &Vec<&str>) -> QName {
        QName { parts: strs.into_iter().map(|s| s.to_string()).collect() }
    }
}