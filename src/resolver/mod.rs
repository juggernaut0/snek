use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use types::*;

use crate::ast::*;

mod types;

pub struct ModuleDecls {
    name: Rc<String>,
    types: Vec<TypeDeclaration>,
    globals: Vec<GlobalDeclaration>,
}

impl ModuleDecls {
    fn get_type(&self, name: &QName) -> Option<&TypeDeclaration> {
        self.types.iter().find(|it| it.id.fqn() == name.parts.as_slice())
    }

    fn get_global(&self, name: &QName) -> Option<&GlobalDeclaration> {
        self.globals.iter().find(|it| it.fqn == name.parts)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct GlobalId(u16);
struct GlobalDeclaration {
    fqn: Vec<String>,
    type_id: TypeId,
    id: GlobalId,
}
struct UndefinedGlobal<'ast> {
    id: GlobalId,
    fqn: Vec<String>,
    declared_type: ResolvedType,
    ast_node: &'ast Binding,
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
    types: HashMap<TypeId, TypeDeclaration>,
    globals: HashMap<GlobalId, GlobalDeclaration>,
    //usages: HashMap<QNameExpr, (ValueId, String)>,
    dependencies: HashMap<&'deps String, &'deps ModuleDecls>,
    exports: ModuleDecls,
    global_id_seq: u16,
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
            types: Vec::default(),
            globals: Vec::default(),
        };
        Resolver {
            types: HashMap::new(),
            globals: HashMap::new(),
            //usages: HashMap::new(),
            dependencies,
            exports,
            global_id_seq: 0,
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
        self.import_names(&ast.imports);
        let mut undefined_types = Vec::new();
        self.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        let lookup = TypeDeclLookup::new(&undefined_types);
        self.define_types(undefined_types, lookup.root_scope());

        let mut undefined_globals = Vec::new();
        self.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, lookup.root_scope());
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

    fn add_type(&mut self, type_decl: TypeDeclaration) {
        self.types.insert(type_decl.id.clone(), type_decl);
    }

    fn add_global(&mut self, global_decl: GlobalDeclaration) {
        // TODO gen id here?
        self.globals.insert(global_decl.id.clone(), global_decl);
    }

    fn import_names(&mut self, imports: &[Import]) {
        for import in imports {
            let dep = *self.dependencies.get(&import.filename).expect("Missing dependency");
            for name in &import.names {
                let mut found = false;
                if let Some(td) = dep.get_type(&name.name) {
                    let imported_type = TypeDeclaration {
                        id: td.id.clone(),
                        num_type_params: td.num_type_params,
                        definition: td.definition.clone(),
                        visibility: QName { parts: Vec::new() },
                    };
                    self.add_type(imported_type);
                    found = true;
                }
                if let Some(gd) = dep.get_global(&name.name) {
                    let imported_global = GlobalDeclaration {
                        id: gd.id.clone(), // TODO gen a new global id for imported value
                        fqn: gd.fqn.clone(),
                        type_id: gd.type_id.clone(),
                    };
                    self.add_global(imported_global);
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
    }

    fn find_types<'ast>(&self, type_declarations: &mut Vec<UndefinedType<'ast>>, ns_decl: &'ast Namespace, namespace: QNameList, visibility: QNameList) {
        for decl in &ns_decl.decls {
            let vis = if decl.is_public() { visibility } else { namespace };
            match decl {
                Decl::Namespace(ns) => {
                    self.find_types(type_declarations, ns, namespace.append(&ns.name), vis);
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

            if self.types.contains_key(&type_decl.id) {
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
                self.types.insert(decl.id.clone(), decl);
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
                    }
                    let params = nt.params.iter()
                        .map(|param| self.resolve_field_type(param, type_params, scope, true))
                        .collect();
                    ResolvedType::Id(id, params)
                } else {
                    self.errors.push(Error {
                        message: format!("Unknown type name '{}'", &nt.name.parts.join(".")),
                        line: type_name.line,
                        col: type_name.col,
                    });
                    ResolvedType::Error
                }
            },
            TypeNameType::Func(ft) => {
                let params = ft.params.iter()
                    .map(|tn| self.resolve_field_type(tn, type_params, scope, false))
                    .collect();
                let return_type = self.resolve_field_type(&ft.return_type, type_params, scope, true);
                ResolvedType::Func(ResolvedFuncType {
                    params,
                    return_type: Box::new(return_type)
                })
            },
            TypeNameType::Unit => ResolvedType::Unit,
            TypeNameType::Any => ResolvedType::Any,
            TypeNameType::Nothing => {
                if nothing_allowed {
                    ResolvedType::Nothing
                } else {
                    self.errors.push(Error {
                        message: "Cannot use '!' here".to_string(),
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

    fn find_globals<'ast>(&mut self, undefined_globals: &mut Vec<UndefinedGlobal<'ast>>, ns_decl: &'ast Namespace, namespace: QNameList, visibility: QNameList, scope: TypeDeclLookupScope) {
        for decl in &ns_decl.decls {
            let vis = if decl.is_public() { visibility } else { namespace };
            match decl {
                Decl::Namespace(ns) => {
                    self.find_globals(undefined_globals, ns, namespace.append(&ns.name), vis, scope.enter_scope(&ns.name.parts));
                },
                Decl::Binding(b) => {
                    let names = self.extract_names(&b.pattern, &scope);
                    for (name, declared_type) in names {
                        let fqn = {
                            let mut ns = namespace.to_qname().parts;
                            ns.push(name.to_owned());
                            ns
                        };
                        let ug = UndefinedGlobal {
                            id: self.gen_global_id(),
                            fqn,
                            declared_type,
                            ast_node: b,
                        };
                        undefined_globals.push(ug)
                    }

                }
                _ => ()
            }
        }
    }

    fn extract_names<'ast>(&mut self, pattern: &'ast Pattern, scope: &TypeDeclLookupScope) -> Vec<(&'ast str, ResolvedType)> {
        let mut names = Vec::new();
        match pattern {
            Pattern::Wildcard(_) | Pattern::Constant(_) => {},
            Pattern::Name(np) => {
                let rt = np.type_name.as_ref()
                    .map(|it| self.resolve_binding_type(it, &[], scope))
                    .unwrap_or(ResolvedType::Inferred);
                names.push((np.name.as_str(), rt))
            },
            Pattern::Destruct(_, _) => { todo!() },
            Pattern::List(_) => { todo!() },
        }
        names
    }

    fn resolve_binding_type(&mut self, type_name: &TypeName, type_params: &[String], scope: &TypeDeclLookupScope) -> ResolvedType {
        todo!("resolve_binding_type")
    }

    fn gen_global_id(&mut self) -> GlobalId {
        let id = self.global_id_seq;
        self.global_id_seq += 1;
        GlobalId(id)
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

trait Lookup {
    type Id;

    fn find(&self, fqn: QNameList) -> Option<&(Self::Id, Vec<String>)>;

    fn root_scope(&self) -> LookupScope<Self> {
        LookupScope {
            lookup: self,
            scope: QNameList::Empty,
        }
    }
}

struct LookupScope<'lookup, 'ast, 'parent, TLookup : Lookup + ?Sized> {
    lookup: &'lookup TLookup,
    scope: QNameList<'ast, 'parent>,
}

impl<'ast, TLookup : Lookup> LookupScope<'_, 'ast, '_, TLookup> where TLookup::Id : Clone {
    fn enter_scope(&self, name: &'ast [String]) -> LookupScope<TLookup> {
        LookupScope {
            lookup: self.lookup,
            scope: self.scope.append_slice(name),
        }
    }

    fn get_type(&self, name: &[String]) -> Option<(TLookup::Id, bool)> {
        let mut current_scope = self.scope;
        loop {
            let full_name = current_scope.append_slice(name);
            let found = self.lookup.find(full_name);
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

// Is an object with visibility visible from scope?
fn is_visible(scope: QNameList, visibility: &[String]) -> bool {
    if scope.len() < visibility.len() { return false }
    scope.iter().zip(visibility).all(|(sp, vp)| sp == vp)
}

type TypeDeclLookupScope<'lookup, 'ast, 'parent> = LookupScope<'lookup, 'ast, 'parent, TypeDeclLookup>;

#[derive(Clone, Copy)]
enum QNameList<'parent, 'ast> {
    Empty,
    List(&'parent QNameList<'parent, 'ast>, &'ast [String])
}

impl<'parent, 'ast> QNameList<'parent, 'ast> {
    fn append(&'parent self, qname: &'ast QName) -> QNameList<'parent, 'ast> {
        self.append_slice(&qname.parts)
    }

    fn append_slice(&'parent self, slice: &'ast [String]) -> QNameList<'parent, 'ast> {
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

    // element-wise equality
    fn matches(&self, names: &[String]) -> bool {
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
                    parent.append_slice(&names[0..names_len-1])
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
    fn new(list: &'ast QNameList<'_, 'ast>) -> QNameListIter<'ast> {
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
            let p_next = parent.next();
            if p_next.is_some() {
                return p_next
            } else {
                self.parent = None
            }
        }
        if let Some(name) = &mut self.name {
            return name.next()
        }
        None
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;

    use crate::ast::QName;

    use super::*;

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
        let type_id = TypeId::new(
            Rc::new(String::new()),
            to_qname(vec!["a", "b", "c"])
        );

        let a = vec![String::from("a"), String::from("b")];
        let b = vec![String::from("c")];

        let l1 = QNameList::Empty;
        let l2 = QNameList::List(&l1, &a);
        let l3 = QNameList::List(&l2, &b);

        assert!(l3.matches(type_id.fqn()))
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
        resolver.find_types(&mut type_declarations, &ast.root_namespace, QNameList::Empty, QNameList::Empty);

        println!("{:?}", resolver.types.keys());
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
        resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        assert!(resolver.errors.is_empty());
        let lookup = TypeDeclLookup::new(&undefined_types);
        resolver.define_types(undefined_types, lookup.root_scope());
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

        let check_type_def = |name: Vec<&str>, expected_fields: Vec<(&str, Vec<&str>)>| {
            let id = TypeId::new(Rc::clone(&mod_name), to_qname(name));
            let t = resolver.types.get(&id).unwrap();

            if let TypeDefinition::Record(fields) = &t.definition {
                assert_eq!(expected_fields.len(), fields.len(), "In type {:?}", id);
                 for ((name, res_type), (exp_name, exp_type)) in fields.iter().zip(&expected_fields) {
                     assert_eq!(exp_name, name, "In type {:?}", id);
                     assert_eq!(&ResolvedType::Id(TypeId::new(Rc::clone(&mod_name), to_qname(exp_type.clone())), Vec::new()), res_type, "In type {:?}", id)
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
        resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
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
        resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);
        let lookup = TypeDeclLookup::new(&undefined_types);
        resolver.define_types(undefined_types, lookup.root_scope());
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

        assert_eq!(3, resolver.types.len());
        {
            let option = get_type(&resolver, "Option");
            assert_eq!(1, option.num_type_params);
            if let TypeDefinition::Union(cases) = &option.definition {
                assert_eq!(2, cases.len());
            } else {
                panic!()
            }
        }
        {
            let some = get_type(&resolver, "Some");
            assert_eq!(1, some.num_type_params);
            if let TypeDefinition::Record(fields) = &some.definition {
                assert_eq!(1, fields.len());
                assert_eq!("t", &fields[0].0)
            } else {
                panic!()
            }
        }
        {
            let none = get_type(&resolver, "None");
            assert_eq!(1, none.num_type_params);
            if let TypeDefinition::Record(fields) = &none.definition {
                assert_eq!(0, fields.len());
            } else {
                panic!()
            }
        }
    }

    #[test]
    fn func_field() {
        let src = "type BiConsumer<T> { consume: { T T -> () } }";
        let (ast, errs) = crate::parser::parse(src);
        assert!(errs.is_empty());
        let mod_name = Rc::new(String::new());
        let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
        let mut undefined_types = Vec::new();
        resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);
        let lookup = TypeDeclLookup::new(&undefined_types);
        resolver.define_types(undefined_types, lookup.root_scope());
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

        let t = resolver.types.values().next().unwrap();
        let fields = if let TypeDefinition::Record(fields) = &t.definition { fields } else { panic!() };
        let consume = fields.first().unwrap();
        assert_eq!("consume", consume.0);
        let rft = if let ResolvedType::Func(rft) = &consume.1 { rft } else { panic!() };
        assert_eq!(ResolvedType::TypeParam(0), rft.params[0]);
        assert_eq!(ResolvedType::TypeParam(0), rft.params[1]);
        assert_eq!(&ResolvedType::Unit, rft.return_type.as_ref());
    }

    #[test]
    fn named_globals() {
        let src = "\
let a = 5
namespace A { let a = 1 }
namespace B { let a = 2 }
namespace A.B.C { let a = 3 }
namespace A.B {
    namespace C.D {
        let a = 4
    }
}";
        let (ast, errs) = crate::parser::parse(src);
        assert!(errs.is_empty());
        let mod_name = Rc::new(String::new());
        let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
        let mut undefined_globals = Vec::new();
        let lookup = TypeDeclLookup::new(&[]);
        resolver.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, lookup.root_scope());
        assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

        assert_eq!(5, undefined_globals.len());
        undefined_globals.iter().find(|it| it.fqn == vec!["a"]).expect("Expected a");
        undefined_globals.iter().find(|it| it.fqn == vec!["A", "a"]).expect("Expected A.a");
        undefined_globals.iter().find(|it| it.fqn == vec!["B", "a"]).expect("Expected B.a");
        undefined_globals.iter().find(|it| it.fqn == vec!["A", "B", "C", "a"]).expect("Expected A.B.C.a");
        undefined_globals.iter().find(|it| it.fqn == vec!["A", "B", "C", "D", "a"]).expect("Expected A.B.C.D.a");
    }

    fn to_qname(strs: Vec<&str>) -> QName {
        QName { parts: strs.into_iter().map(|s| s.to_string()).collect() }
    }

    fn get_type<'a>(resolver: &'a Resolver, name: &str) -> &'a TypeDeclaration {
        resolver.types.values().find(|it| it.id.fqn().last().unwrap() == name).unwrap()
    }
}