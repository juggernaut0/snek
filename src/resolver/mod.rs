use std::collections::HashMap;
use std::fmt::Debug;
use std::rc::Rc;

use crate::ast::*;

mod types;
mod globals;
mod lookup;
mod qname_list;

use types::*;
use globals::*;
use lookup::*;
use qname_list::*;
use crate::resolver::DefineGlobalResult::Success;
use std::iter::repeat;

struct BuiltinTypeNames {
    number: String,
    string: String,
    boolean: String,
}

impl BuiltinTypeNames {
    fn new() -> BuiltinTypeNames {
        BuiltinTypeNames {
            number: String::from("Number"),
            string: String::from("String"),
            boolean: String::from("Boolean"),
        }
    }
}

lazy_static! {
    static ref BUILTIN_TYPE_NAMES: BuiltinTypeNames = BuiltinTypeNames::new();
}

pub struct ModuleDecls {
    name: Rc<String>,
    types: Vec<TypeDeclaration>,
    globals: Vec<GlobalDeclaration>,
}

impl ModuleDecls {
    fn get_type(&self, name: &QName) -> Option<&TypeDeclaration> {
        self.types.iter().find(|it| it.id.fqn().as_slice() == name.parts.as_slice())
    }

    fn get_global(&self, name: &QName) -> Option<&GlobalDeclaration> {
        self.globals.iter().find(|it| it.fqn.as_slice() == name.parts.as_slice())
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
        let type_lookup = TypeDeclLookup::new(&undefined_types);
        self.define_types(undefined_types, type_lookup.root_scope());

        let mut undefined_globals = Vec::new();
        self.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, type_lookup.root_scope());
        let global_lookup = GlobalLookup::new(&undefined_globals);
        self.define_globals(undefined_globals, type_lookup.root_scope(), global_lookup.root_scope());

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
                        visibility: Vec::new(),
                    };
                    self.add_type(imported_type);
                    found = true;
                }
                if let Some(gd) = dep.get_global(&name.name) {
                    let imported_global = GlobalDeclaration {
                        id: self.gen_global_id(),
                        fqn: gd.fqn.clone(),
                        resolved_type: gd.resolved_type.clone(),
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
                    let fqn = Fqn::new(namespace, t.name.name.clone());
                    type_declarations.push(UndefinedType {
                        id: TypeId::new(Rc::clone(&self.exports.name), fqn),
                        visibility: vis.to_vec(),
                        ast_node: UndefinedTypeNode::Normal(t),
                    });
                    if let TypeContents::Union(cases) = &t.contents {
                        for case in cases {
                            if let TypeCase::Record(record) = case {
                                let vis = if record.public { visibility } else { namespace };
                                let fqn = Fqn::new(namespace, record.name.name.clone());
                                type_declarations.push(UndefinedType {
                                    id: TypeId::new(Rc::clone(&self.exports.name), fqn),
                                    visibility: vis.to_vec(),
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

    fn define_types(&mut self, undefined_types: Vec<UndefinedType>, root_scope: LookupScope<TypeDeclLookup>) {
        for type_decl in undefined_types {
            let scope = root_scope.enter_scope(type_decl.id.fqn().namespace());
            let (definition, num_type_params) = match type_decl.ast_node {
                UndefinedTypeNode::Normal(t) => {
                    let type_params = &t.name.type_params;
                    let num_type_params = type_params.len();

                    let definition = match &t.contents {
                        TypeContents::Record(fields) => {
                            let res_fields = fields.iter()
                                .map(|f| {
                                    ResolvedField {
                                        name: f.name.clone(),
                                        public: f.public, // TODO does this need to be a full visibility vec?
                                        resolved_type: self.resolve_field_type(&f.type_name, type_params, &scope, false)
                                    }
                                })
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
                                            let (id, _) = scope.get(name).unwrap();
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
                        .map(|f| {
                            ResolvedField {
                                name: f.name.clone(),
                                public: f.public,
                                resolved_type: self.resolve_field_type(&f.type_name, type_params, &scope, false)
                            }
                        })
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

    fn resolve_field_type(&mut self, type_name: &TypeName, type_params: &[String], scope: &LookupScope<TypeDeclLookup>, nothing_allowed: bool) -> ResolvedType {
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
                } else if let Some((id, visible)) = scope.get(&nt.name.parts) {
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

    fn find_globals<'ast>(
        &mut self,
        undefined_globals: &mut Vec<UndefinedGlobalBinding<'ast>>,
        ns_decl: &'ast Namespace,
        namespace: QNameList,
        visibility: QNameList,
        scope: LookupScope<TypeDeclLookup>
    ) {
        for decl in &ns_decl.decls {
            let vis = if decl.is_public() { visibility } else { namespace };
            match decl {
                Decl::Namespace(ns) => {
                    self.find_globals(undefined_globals, ns, namespace.append(&ns.name), vis, scope.enter_scope(&ns.name.parts));
                },
                Decl::Binding(b) => {
                    let (names, expected_type) = self.extract_names(&b.pattern, &scope);
                    let decls = names
                        .into_iter()
                        .map(|(name, declared_type)| {
                            let id = self.gen_global_id();
                            let fqn = Fqn::new(namespace, name.to_string());
                            if !declared_type.is_inferred() {
                                self.add_global(GlobalDeclaration {
                                    id: id.clone(),
                                    fqn: fqn.clone(),
                                    resolved_type: declared_type.clone(),
                                })
                            }
                            UndefinedGlobal {
                                id,
                                fqn,
                                visibility: vis.to_vec(),
                                declared_type,
                            }
                        })
                        .collect();

                    let ugb = UndefinedGlobalBinding {
                        decls,
                        expected_type,
                        ast_node: b,
                    };
                    undefined_globals.push(ugb);
                }
                _ => ()
            }
        }
    }

    fn extract_names<'ast>(&mut self, pattern: &'ast Pattern, scope: &LookupScope<TypeDeclLookup>) -> (Vec<(&'ast str, ResolvedType)>, ResolvedType) {
        let mut names = Vec::new();
        let expected_type = match pattern {
            Pattern::Wildcard(otn) => {
                otn.as_ref()
                    .map(|tn| self.resolve_binding_type(tn, &[], scope))
                    .unwrap_or(ResolvedType::Inferred)
            },
            Pattern::Constant(lit) => {
                match lit.lit_type {
                    LiteralType::NUMBER => ResolvedType::Id(scope.get(std::slice::from_ref(&BUILTIN_TYPE_NAMES.number)).unwrap().0, Vec::new()),
                    LiteralType::STRING => ResolvedType::Id(scope.get(std::slice::from_ref(&BUILTIN_TYPE_NAMES.string)).unwrap().0, Vec::new()),
                    LiteralType::BOOL => ResolvedType::Id(scope.get(std::slice::from_ref(&BUILTIN_TYPE_NAMES.boolean)).unwrap().0, Vec::new()),
                    LiteralType::UNIT => ResolvedType::Unit,
                }
            },
            Pattern::Name(np) => {
                let rt = np.type_name.as_ref()
                    .map(|it| self.resolve_binding_type(it, &[], scope))
                    .unwrap_or(ResolvedType::Inferred);
                names.push((np.name.as_str(), rt.clone()));
                rt
            },
            Pattern::Destruct(fields, tn) => {
                let declared_type = tn.as_ref()
                    .map(|it| (self.resolve_binding_type(it, &[], scope), it));
                let declared_type_fields = declared_type
                    .clone()
                    .and_then(|(rt, tn)| {
                        match rt {
                            ResolvedType::Id(id, args) => {
                                let type_decl = self.types.get(&id).expect("Missing type def");
                                if let TypeDefinition::Record(fs) = &type_decl.definition {
                                    // instantiate type params
                                    let mut subs: Vec<_> = fs.clone();
                                    for rf in &mut subs {
                                        if let ResolvedType::TypeParam(i) = &rf.resolved_type {
                                            rf.resolved_type = args[*i].clone()
                                        };
                                    }
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
                                    let declared_field = fs.iter()
                                        .find(|rf| &np.name == &rf.name);
                                    if declared_field.is_none() {
                                        self.errors.push(Error {
                                            message: format!("Unknown field {}", np.name),
                                            line: np.line,
                                            col: np.col,
                                        })
                                    }
                                    declared_field.map(|rf| rf.resolved_type.clone())
                                })
                                .unwrap_or(ResolvedType::Inferred);

                            let declared_type = np.type_name.as_ref()
                                .map(|it| self.resolve_binding_type(it, &[], scope))
                                .unwrap_or(ResolvedType::Inferred);

                            names.push((&np.name, self.unify(inferred_field_type, declared_type, np.line, np.col)))
                        },
                        FieldPattern::Binding(_, _) => { todo!("destructure pattern binding case") },
                    }
                }
                declared_type.map(|(rt, _)| rt).unwrap_or(ResolvedType::Inferred)
            },
            Pattern::List(_) => { todo!("list pattern") },
        };
        (names, expected_type)
    }

    fn resolve_binding_type(&mut self, type_name: &TypeName, type_params: &[String], scope: &LookupScope<TypeDeclLookup>) -> ResolvedType {
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
                } else if let Some((id, visible)) = scope.get(&nt.name.parts) {
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

    fn define_globals(&mut self, undefined_globals: Vec<UndefinedGlobalBinding>, root_type_scope: LookupScope<TypeDeclLookup>, root_global_scope: LookupScope<GlobalLookup>) {
        let ugbs_by_gid = {
            let mut map = HashMap::new();
            for (i, ugb) in undefined_globals.iter().enumerate() {
                for ug in &ugb.decls {
                    map.insert(ug.id.clone(), (i, ugb));
                }
            }
            map
        };

        let mut stack: Vec<_> = undefined_globals.iter().enumerate().collect();
        stack.reverse();

        let mut result = vec![None; undefined_globals.len()];
        while let Some((i, ugb)) = stack.pop() {
            if result[i].is_some() { continue }

            let ns = ugb.namespace();
            let type_scope = root_type_scope.enter_scope(ns);
            let global_scope = root_global_scope.enter_scope(ns);
            let dgr = self.resolve_expr(ugb.expected_type.clone(), &ugb.ast_node.expr, &type_scope, &global_scope, &LocalScope::new(None));
            match dgr {
                DefineGlobalResult::NeedsType(id) => {
                    stack.push(ugbs_by_gid.get(&id).copied().unwrap());
                    continue
                },
                DefineGlobalResult::Success(unified_type) => {
                    result[i] = Some(unified_type);
                    todo!("never thought I'd get this far")
                }
            }
        }
        todo!("something with result")
    }

    fn resolve_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        type_scope: &LookupScope<TypeDeclLookup>,
        global_scope: &LookupScope<GlobalLookup>,
        local_scope: &LocalScope
    ) -> DefineGlobalResult {
        match &expr.expr_type {
            ExprType::QName(qn) => {
                // TODO local scope
                if let Some((global, visible)) = global_scope.get(&qn.parts) {
                    if !visible {
                        self.errors.push(Error {
                            message: format!("{} cannot be accessed here.", qn.parts.join(".")),
                            line: expr.line,
                            col: expr.col,
                        })
                    }

                    let actual_type = if let Some(g_decl) =  self.globals.get(&global) {
                        g_decl.resolved_type.clone()
                    } else {
                        return DefineGlobalResult::NeedsType(global)
                    };

                    Success(self.unify(expected_type, actual_type, expr.line, expr.col))
                } else {
                    self.errors.push(Error {
                        message: format!("Unknown name {}", qn.parts.join(".")),
                        line: expr.line,
                        col: expr.col,
                    });
                    Success(ResolvedType::Error)
                }
            },
            ExprType::Constant(_) => todo!(),
            ExprType::Unary(_, _) => todo!(),
            ExprType::Binary(_, _, _) => todo!(),
            ExprType::Call(call_expr) => {
                let callee = self.resolve_expr(ResolvedType::Callable(Box::new(expected_type)), &call_expr.callee, type_scope, global_scope, local_scope);
                let rft = match callee {
                    DefineGlobalResult::Success(ResolvedType::Func(rft)) => rft,
                    DefineGlobalResult::Success(_) => unreachable!(),
                    DefineGlobalResult::NeedsType(_) => return callee,
                };

                if rft.params.len() < call_expr.args.len() {
                    self.errors.push(Error {
                        message: "Too many arguments provided for function".to_string(),
                        line: expr.line,
                        col: expr.col,
                    })
                } else if rft.params.len() < call_expr.args.len() {
                    // TODO currying
                    self.errors.push(Error {
                        message: "Partial application not yet supported".to_string(),
                        line: expr.line,
                        col: expr.col,
                    })
                }

                for (exp_arg_type, arg_expr) in rft.params.iter().cloned().chain(repeat(ResolvedType::Inferred)).zip(&call_expr.args) {
                    self.resolve_expr(exp_arg_type, arg_expr, type_scope, global_scope, local_scope);
                }
                DefineGlobalResult::Success(*rft.return_type)
            },
            ExprType::Lambda(_) => todo!(),
            ExprType::List(_) => todo!(),
            ExprType::New(_, _) => todo!(),
            ExprType::Dot => todo!(),
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
                    error = Some((e_id.fqn().to_string(), a_id.fqn().to_string()));
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
        GlobalId::new(id)
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

struct LocalScope<'a> {
    parent: Option<&'a LocalScope<'a>>,
}

impl<'a> LocalScope<'a> {
    fn new(parent: Option<&'a LocalScope<'a>>) -> LocalScope<'a> {
        LocalScope {
            parent,
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub message: String,
    pub line: u32,
    pub col: u32,
}

#[cfg(test)]
mod test;

enum DefineGlobalResult {
    NeedsType(GlobalId),
    Success(ResolvedType),
}
