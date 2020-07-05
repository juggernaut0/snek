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

        #[cfg(debug_assertions)] {
            println!("Undefined Globals: ");
            for global in &undefined_globals {
                println!("{}: {:?}", global.fqn.join("."), global.declared_type)
            }
        }
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
                // If the name has is not a qualified name and has no args of its own, it may be a type param
                let tp = if nt.name.parts.len() == 1 && nt.type_args.is_empty() {
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
                    let params = nt.type_args.iter()
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
            Pattern::Destruct(fields, tn) => {
                let declared_type_fields = tn.as_ref()
                    .map(|it| (self.resolve_binding_type(it, &[], scope), it))
                    .and_then(|(rt, tn)| {
                        match rt {
                            ResolvedType::Id(id, args) => {
                                let type_decl = self.types.get(&id).expect("Missing type def");
                                if let TypeDefinition::Record(fs) = &type_decl.definition {
                                    // instantiate type params
                                    let subs: Vec<_> = fs
                                        .iter()
                                        .map(|(name, ty)| {
                                            let new_ty = if let ResolvedType::TypeParam(i) = ty {
                                                &args[*i]
                                            } else {
                                                ty
                                            };
                                            (name.clone(), new_ty.clone())
                                        })
                                        .collect();
                                    Some(subs)
                                } else {
                                    self.errors.push(Error {
                                        message: "Destructuring pattern must be for a record type".to_string(),
                                        line: tn.line,
                                        col: tn.col,
                                    });
                                    None
                                }
                            },
                            ResolvedType::Inferred => { None },
                            _ => {
                                self.errors.push(Error {
                                    message: "Destructuring pattern must be for a record type".to_string(),
                                    line: tn.line,
                                    col: tn.col,
                                });
                                None
                            }
                        }
                    });

                for field in fields {
                    match field {
                        FieldPattern::Name(np) => {
                            let inferred_field_type = declared_type_fields
                                .as_ref()
                                .and_then(|fs| {
                                    fs.iter()
                                        .find(|(name, _)| &np.name == name)
                                        .map(|(_, ty)| ty)
                                })
                                .cloned()
                                .unwrap_or(ResolvedType::Inferred);

                            let declared_type = np.type_name.as_ref()
                                .map(|it| self.resolve_binding_type(it, &[], scope))
                                .unwrap_or(ResolvedType::Inferred);

                            names.push((&np.name, self.unify(inferred_field_type, declared_type, np.line, np.col)))
                        },
                        FieldPattern::Binding(_, _) => { todo!("destructure pattern binding case") },
                    }
                }
            },
            Pattern::List(_) => { todo!("list pattern") },
        }
        names
    }

    fn resolve_binding_type(&mut self, type_name: &TypeName, type_params: &[String], scope: &TypeDeclLookupScope) -> ResolvedType {
        match &type_name.type_name_type {
            TypeNameType::Named(nt) => {
                // If the name has is not a qualified name and has no args of its own, it may be a type param
                let tp = if nt.name.parts.len() == 1 && nt.type_args.is_empty() {
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
                    let args = nt.type_args.iter()
                        .map(|arg| self.resolve_binding_type(arg, type_params, scope))
                        .collect();
                    ResolvedType::Id(id, args)
                } else {
                    self.errors.push(Error {
                        message: format!("Unknown type name '{}'", &nt.name.parts.join(".")),
                        line: type_name.line,
                        col: type_name.col,
                    });
                    ResolvedType::Error
                }
            },
            TypeNameType::Func(_) => { todo!("resolve_binding_type Func") },
            TypeNameType::Unit => ResolvedType::Unit,
            TypeNameType::Any => ResolvedType::Any,
            TypeNameType::Nothing => ResolvedType::Nothing,
            TypeNameType::Inferred => ResolvedType::Inferred,
        }
    }

    fn unify(&mut self, expected: ResolvedType, actual: ResolvedType, line: u32, col: u32) -> ResolvedType {
        let mut error = None;
        let unified = match (expected, actual) {
            (ResolvedType::Inferred, actual) => actual,
            (ResolvedType::Any, _) => ResolvedType::Any,
            (expected, ResolvedType::Nothing) => expected,
            (expected, ResolvedType::Inferred) => expected,
            (ResolvedType::Id(e_id, e_type_args), ResolvedType::Id(a_id, a_type_args)) => {
                if e_id != a_id {
                    error = Some((e_id.fqn().join("."), a_id.fqn().join(".")));
                    ResolvedType::Id(e_id, e_type_args)
                } else {
                    let unified_args = e_type_args
                        .into_iter()
                        .zip(a_type_args)
                        .map(|(e, a)| self.unify(e, a, line, col))
                        .collect();
                    ResolvedType::Id(e_id, unified_args)
                }
            },
            (ResolvedType::TypeParam(e_i), ResolvedType::TypeParam(a_i)) => {
                if e_i != a_i {
                    // TODO sub in type param names
                    error = Some((format!("type param {}", e_i), format!("type param {}", a_i)));
                }
                ResolvedType::TypeParam(e_i)
            }
            (ResolvedType::Unit, ResolvedType::Unit) => ResolvedType::Unit,
            (expected, actual) => {
                // TODO make type names better
                error = Some((format!("{:?}", expected), format!("{:?}", actual)));
                expected
            }
        };
        if let Some((e_name, a_name)) = error {
            self.errors.push(Error {
                message: format!("Type mismatch. Expected: {} Actual: {}", e_name, a_name),
                line,
                col,
            });
        }
        unified
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
        names.iter().zip(self.iter()).all(|(a, b)| a == b)
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
mod test;
