use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::repeat;
use std::rc::Rc;

use globals::*;
use lookup::*;
use qname_list::*;
use types::*;

use crate::ast::*;
use crate::resolver::irt::{IrTree, Save, Statement, Expr as IrtExpr, ExprType as IrtExprType};

mod types;
mod globals;
mod lookup;
mod qname_list;
pub mod irt;
#[cfg(test)]
mod test;

type ResolveResult = Result<(ModuleDecls, IrTree), Vec<Error>>;

pub fn resolve(name: Rc<String>, deps: &[ModuleDecls], ast: &Ast) -> ResolveResult {
    let mut resolver = Resolver::new(name, deps);
    let irt = resolver.resolve(ast);
    if resolver.errors.is_empty() {
        Ok((resolver.exports, irt))
    } else {
        Err(resolver.errors)
    }
}

struct BuiltinTypeNames {
    mod_name: Rc<String>,
    number: TypeId,
    string: TypeId,
    boolean: TypeId,
}

impl BuiltinTypeNames {
    fn new() -> BuiltinTypeNames {
        let builtin_mod = Rc::new(String::from("__builtin"));
        BuiltinTypeNames {
            number: TypeId::new(Rc::clone(&builtin_mod), Fqn::from(&["Number"][..])),
            string: TypeId::new(Rc::clone(&builtin_mod), Fqn::from(&["String"][..])),
            boolean: TypeId::new(Rc::clone(&builtin_mod), Fqn::from(&["Boolean"][..])),
            mod_name: builtin_mod,
        }
    }
}

thread_local! {
    static BUILTIN_TYPE_NAMES: BuiltinTypeNames = BuiltinTypeNames::new();
}

pub struct ModuleDecls {
    name: Rc<String>,
    types: Vec<Rc<TypeDeclaration>>,
    globals: Vec<Rc<GlobalDeclaration>>,
}

impl ModuleDecls {
    fn get_type(&self, name: &QName) -> Option<&Rc<TypeDeclaration>> {
        self.types.iter().find(|it| it.id.fqn().as_slice() == name.parts.as_slice())
    }

    fn get_global(&self, name: &QName) -> Option<&Rc<GlobalDeclaration>> {
        self.globals.iter().find(|it| it.fqn().as_slice() == name.parts.as_slice())
    }
}

fn make_primitive_type(id: TypeId) -> Rc<TypeDeclaration> {
    Rc::new(TypeDeclaration {
        id,
        num_type_params: 0,
        definition: TypeDefinition::Primitive,
        visibility: Vec::new(),
        export: true,
    })
}

fn make_builtins() -> ModuleDecls {
    let builtin_mod_name = BUILTIN_TYPE_NAMES.with(|btn| Rc::clone(&btn.mod_name));
    ModuleDecls {
        types: vec![
            make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone())),
            make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.string.clone())),
            make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone())),
        ],
        globals: vec![
            Rc::new(GlobalDeclaration {
                id: GlobalId::new(Rc::clone(&builtin_mod_name), Fqn::new(Vec::new(), "println".to_string())),
                resolved_type: ResolvedType::Func(ResolvedFuncType {
                    params: vec![ResolvedType::Any],
                    return_type: Box::new(ResolvedType::Unit)
                }),
                visibility: Vec::new(),
                export: true,
            })
        ],
        name: builtin_mod_name,
    }
}

pub struct Resolver<'deps> {
    // in scope items, including imports
    types: HashMap<TypeId, Rc<TypeDeclaration>>,
    globals: HashMap<GlobalId, Rc<GlobalDeclaration>>,
    // This module's types
    exports: ModuleDecls,

    //usages: HashMap<QNameExpr, (ValueId, String)>,
    dependencies: HashMap<&'deps String, &'deps ModuleDecls>,
    global_id_seq: u16,
    errors: Vec<Error>,

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

    pub fn resolve(&mut self, ast: &Ast) -> IrTree {
        let stmts = self.resolve_4(ast);

        IrTree {
            statements: stmts
        }
    }

    fn resolve_1<'ast>(&mut self, ast: &'ast Ast) -> Vec<UndefinedType<'ast>> {
        self.import_from_module(&make_builtins(), &[]);
        self.import_names(&ast.imports);

        let mut undefined_types = Vec::new();
        self.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty, true);
        undefined_types
    }

    fn resolve_2(&mut self, ast: &Ast) -> TypeDeclLookup {
        let undefined_types = self.resolve_1(ast);
        let type_lookup = TypeDeclLookup::new(&undefined_types, self.types.values().map(Rc::as_ref));
        self.define_types(undefined_types, type_lookup.root_scope());
        type_lookup
    }

    fn resolve_3<'ast>(&mut self, ast: &'ast Ast) -> (Vec<UndefinedGlobalBinding<'ast>>, TypeDeclLookup) {
        let type_lookup = self.resolve_2(ast);
        let mut undefined_globals = Vec::new();
        self.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, true, type_lookup.root_scope());
        (undefined_globals, type_lookup)
    }

    fn resolve_4(&mut self, ast: &Ast) -> Vec<Statement> {
        let (undefined_globals, type_lookup) = self.resolve_3(ast);
        let global_lookup = GlobalLookup::new(&undefined_globals, self.globals.values().map(Rc::as_ref));
        self.define_globals(undefined_globals, type_lookup.root_scope(), global_lookup.root_scope())
    }

    fn import_type(&mut self, type_decl: Rc<TypeDeclaration>) {
        self.types.insert(type_decl.id.clone(), type_decl);
    }

    fn import_global(&mut self, global_decl: Rc<GlobalDeclaration>) {
        self.globals.insert(global_decl.id.clone(), global_decl);
    }

    fn import_names(&mut self, imports: &[Import]) {
        for import in imports {
            let exports = *self.dependencies.get(&import.filename).expect("Missing dependency");
            self.import_from_module(exports, &import.names)
        }
    }

    fn import_from_module<'ast>(&mut self, module: &ModuleDecls, names: &'ast [ImportedName]) {
        // if names is empty, import everything from module
        if names.is_empty() {
            for td in &module.types {
                if td.is_exported() {
                    self.import_type(Rc::clone(td))
                }
            }
            for gd in &module.globals {
                if gd.is_exported() {
                    self.import_global(Rc::clone(gd))
                }
            }
        }
        for name in names {
            let mut found = false;
            if let Some(td) = module.get_type(&name.name) {
                if td.is_exported() {
                    self.import_type(Rc::clone(td))
                }
                found = true;
            }
            if let Some(gd) = module.get_global(&name.name) {
                self.import_global(Rc::clone(&gd));
                found = true;
            }
            if !found {
                self.errors.push(Error {
                    message: format!("{} not found in imported file {}", name.name, module.name),
                    line: name.line,
                    col: name.col
                });
            }
        }
    }

    // walks the namespace tree, inserting type names into `type_declarations`
    fn find_types<'ast>(&self, type_declarations: &mut Vec<UndefinedType<'ast>>, ns_decl: &'ast Namespace, namespace: QNameList, visibility: QNameList, can_export: bool) {
        for decl in &ns_decl.decls {
            let vis = if decl.is_public() { visibility } else { namespace };
            match decl {
                Decl::Namespace(ns) => {
                    self.find_types(type_declarations, ns, namespace.append(&ns.name), vis, can_export && ns.public);
                },
                Decl::Type(t) => {
                    let fqn = Fqn::new_from_qname_list(namespace, t.name.name.clone());
                    type_declarations.push(UndefinedType {
                        id: TypeId::new(Rc::clone(&self.exports.name), fqn),
                        visibility: vis.to_vec(),
                        export: can_export && t.public,
                        ast_node: UndefinedTypeNode::Normal(t),
                    });
                    if let TypeContents::Union(cases) = &t.contents {
                        for case in cases {
                            if let TypeCase::Record(record) = case {
                                let vis = if record.public { visibility } else { namespace };
                                let fqn = Fqn::new_from_qname_list(namespace, record.name.name.clone());
                                type_declarations.push(UndefinedType {
                                    id: TypeId::new(Rc::clone(&self.exports.name), fqn),
                                    visibility: vis.to_vec(),
                                    export: can_export && t.public,
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
                let decl = Rc::new(type_decl.define(definition, num_type_params));
                self.types.insert(decl.id.clone(), Rc::clone(&decl));
                self.exports.types.push(decl);
            }

        }
    }

    // given a ast TypeName and a scope, resolve the type
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

    // walks the namespace tree, inserting global binding names into `undefined_globals`
    fn find_globals<'ast>(
        &mut self,
        undefined_globals: &mut Vec<UndefinedGlobalBinding<'ast>>,
        ns_decl: &'ast Namespace,
        namespace: QNameList,
        visibility: QNameList,
        can_export: bool,
        scope: LookupScope<TypeDeclLookup>
    ) {
        for decl in &ns_decl.decls {
            let vis = if decl.is_public() { visibility } else { namespace };
            match decl {
                Decl::Namespace(ns) => {
                    self.find_globals(undefined_globals, ns, namespace.append(&ns.name), vis, can_export && ns.public, scope.enter_scope(&ns.name.parts));
                },
                Decl::Binding(b) => {
                    let (names, expected_type) = self.extract_names(&b.pattern, &scope);
                    let decls = names
                        .into_iter()
                        .map(|(name, declared_type, from)| {
                            let fqn = Fqn::new_from_qname_list(namespace, name.to_string());
                            let id = GlobalId::new(Rc::clone(&self.exports.name), fqn.clone());
                            if !declared_type.is_inferred() {
                                self.import_global(Rc::new(GlobalDeclaration {
                                    id: id.clone(),
                                    resolved_type: declared_type.clone(),
                                    visibility: vis.to_vec(),
                                    export: can_export && b.public
                                }));
                            }
                            UndefinedGlobal {
                                id,
                                fqn,
                                visibility: vis.to_vec(),
                                export: can_export && b.public,
                                declared_type,
                                from,
                            }
                        })
                        .collect();

                    let ugb = UndefinedGlobalBinding {
                        namespace: namespace.to_vec(),
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

    // returns a pair of (list of tuples of (name, type for that name, BindingFrom for that name), type for whole pattern)
    fn extract_names<'ast>(&mut self, pattern: &'ast Pattern, scope: &LookupScope<TypeDeclLookup>) -> (Vec<(&'ast str, ResolvedType, BindingFrom<'ast>)>, ResolvedType) {
        let mut names = Vec::new();
        let expected_type = match pattern {
            Pattern::Wildcard(otn) => {
                otn.as_ref()
                    .map(|tn| self.resolve_binding_type(tn, &[], scope))
                    .unwrap_or(ResolvedType::Inferred)
            },
            Pattern::Constant(lit) => {
                match lit.lit_type {
                    LiteralType::NUMBER => ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone()), Vec::new()),
                    LiteralType::STRING => ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.string.clone()), Vec::new()),
                    LiteralType::BOOL => ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone()), Vec::new()),
                    LiteralType::UNIT => ResolvedType::Unit,
                }
            },
            Pattern::Name(np) => {
                let rt = np.type_name.as_ref()
                    .map(|it| self.resolve_binding_type(it, &[], scope))
                    .unwrap_or(ResolvedType::Inferred);
                names.push((np.name.as_str(), rt.clone(), BindingFrom::Direct));
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

                            names.push((&np.name, self.unify(inferred_field_type, declared_type, np.line, np.col), BindingFrom::Destructured(&np.name)))
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
                // If the name is not a qualified name and has no args of its own, it may be a type param
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

    fn define_globals(
        &mut self,
        undefined_globals: Vec<UndefinedGlobalBinding>,
        root_type_scope: LookupScope<TypeDeclLookup>,
        root_global_scope: LookupScope<GlobalLookup>
    ) -> Vec<Statement> {
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
        stack.reverse(); // so that pop comes off the "top" of the code

        let mut result = Vec::new();
        result.resize_with(undefined_globals.len(), || None);
        while let Some((i, ugb)) = stack.pop() {
            if result[i].is_some() { continue }

            let ns = ugb.namespace.as_slice();
            let type_scope = root_type_scope.enter_scope(ns);
            let global_scope = root_global_scope.enter_scope(ns);
            let dgr = self.resolve_expr(ugb.expected_type.clone(), &ugb.ast_node.expr, &type_scope, &global_scope, &LocalScope::new(None));
            match dgr {
                DefineGlobalResult::NeedsType(id) => {
                    let dep = ugbs_by_gid.get(&id).copied().unwrap();
                    if std::ptr::eq(ugb, dep.1) {
                        self.errors.push(Error {
                            message: "Recursive error while resolving expression".to_string(),
                            line: ugb.ast_node.expr.line,
                            col: ugb.ast_node.expr.col,
                        })
                    } else {
                        stack.push((i, ugb));
                        stack.push(dep);
                    }
                    continue
                },
                DefineGlobalResult::Success(irt_expr) => {
                    if ugb.decls.len() > 0 && irt_expr.resolved_type.is_inferred() {
                        self.errors.push(Error {
                            message: "Unable to infer type of global binding".into(),
                            line: ugb.ast_node.expr.line,
                            col: ugb.ast_node.expr.col,
                        })
                    }
                    let mut save: Option<Save<GlobalId>> = None;
                    for ug in &ugb.decls {
                        let resolved_type = match ug.from {
                            BindingFrom::Direct => {
                                save = Some(Save::Normal(ug.id.clone()));
                                irt_expr.resolved_type.clone()
                            },
                            BindingFrom::Destructured(_) => todo!("destructured globals") // get type of field
                        };
                        let decl = Rc::new(ug.define(resolved_type));
                        self.globals.insert(ug.id.clone(), Rc::clone(&decl));
                        self.exports.globals.push(decl);
                    }
                    let stmt = match save {
                        None => Statement::Discard(irt_expr),
                        Some(save) => Statement::SaveGlobal(save, irt_expr),
                    };
                    result[i] = Some(stmt);
                }
            }
        }

        result.into_iter()
            .flatten()
            .collect()
    }

    fn resolve_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        type_scope: &LookupScope<TypeDeclLookup>,
        global_scope: &LookupScope<GlobalLookup>,
        local_scope: &LocalScope
    ) -> DefineGlobalResult {
        let (actual_type, irt_expr_type) = match &expr.expr_type {
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

                    (actual_type, irt::ExprType::LoadGlobal(global))
                } else {
                    self.errors.push(Error {
                        message: format!("Unknown name {}", qn.parts.join(".")),
                        line: expr.line,
                        col: expr.col,
                    });
                    return DefineGlobalResult::error_expr()
                }
            },
            ExprType::Constant(lit) => {
                let actual_type = match lit.lit_type {
                    LiteralType::NUMBER => ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone()), Vec::new()),
                    LiteralType::STRING => ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.string.clone()), Vec::new()),
                    LiteralType::BOOL => ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone()), Vec::new()),
                    LiteralType::UNIT => ResolvedType::Unit,
                };
                let cons = match lit.lit_type {
                    LiteralType::NUMBER => irt::Constant::Number(lit.value.parse().unwrap()), // TODO handle out of range numbers?
                    LiteralType::STRING => {
                        let raw = match unescape(&lit.value) {
                            Ok(raw) => raw,
                            Err(msg) => {
                                self.errors.push(Error {
                                    message: msg,
                                    line: expr.line,
                                    col: expr.col,
                                });
                                String::new()
                            }
                        };
                        irt::Constant::String(raw)
                    },
                    LiteralType::BOOL => irt::Constant::Boolean(lit.value.parse().unwrap()), // lit.value is guaranteed to be "true" or "false" by parser
                    LiteralType::UNIT => irt::Constant::Unit,
                };
                (actual_type, irt::ExprType::LoadConstant(cons))
            },
            ExprType::Unary(_, _) => todo!(),
            ExprType::Binary(_, _, _) => todo!(),
            ExprType::Call(call_expr) => {
                match self.resolve_call_expr(expected_type.clone(), expr, type_scope, global_scope, local_scope, call_expr) {
                    Ok(it) => it,
                    Err(dfg) => return dfg
                }
            },
            ExprType::Lambda(_) => todo!(),
            ExprType::List(_) => todo!(),
            ExprType::New(_, _) => todo!(),
            ExprType::Dot => todo!(),
        };
        let resolved_type = self.unify(expected_type, actual_type, expr.line, expr.col);
        DefineGlobalResult::Success(irt::Expr {
            resolved_type,
            expr_type: irt_expr_type,
        })
    }

    fn resolve_call_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        type_scope: &LookupScope<TypeDeclLookup>,
        global_scope: &LookupScope<GlobalLookup>,
        local_scope: &LocalScope,
        call_expr: &CallExpr,
    ) -> Result<(ResolvedType, IrtExprType), DefineGlobalResult> {
        let callee_result = self.resolve_expr(ResolvedType::Callable(Box::new(expected_type)), &call_expr.callee, type_scope, global_scope, local_scope);
        let callee = match callee_result {
            DefineGlobalResult::NeedsType(_) => return Err(callee_result),
            DefineGlobalResult::Success(callee_expr) => callee_expr,
        };
        let rft = match callee.resolved_type.clone() {
            ResolvedType::Func(rft) => rft,
            ResolvedType::Callable(et) => {
                self.errors.push(Error {
                    message: "Unable to infer callable type".into(),
                    line: expr.line,
                    col: expr.col,
                });
                // TODO is this a good idea?
                // let rt = if et.is_inferred() { ResolvedType::Error } else { *et };
                return Ok((*et, IrtExprType::Error))
            },
            ResolvedType::Error => return Err(DefineGlobalResult::error_expr()),
            _ => unreachable!() // Callable does not unify with anything except Func or Callable
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

        let arg_results = rft.params
            .iter()
            .cloned()
            .chain(repeat(ResolvedType::Inferred))
            .zip(&call_expr.args)
            .map(|(exp_arg_type, arg_expr)| {
                self.resolve_expr(exp_arg_type, arg_expr, type_scope, global_scope, local_scope)
            });

        let mut args = Vec::new();
        for arg_result in arg_results {
            match arg_result {
                DefineGlobalResult::NeedsType(_) => return Err(arg_result),
                DefineGlobalResult::Success(expr) => args.push(expr),
            }
        }
        Ok((
            *rft.return_type,
            irt::ExprType::Call(irt::CallExpr {
                callee: Box::new(callee),
                args,
            })
        ))
    }

    fn unify(&mut self, expected: ResolvedType, actual: ResolvedType, line: u32, col: u32) -> ResolvedType {
        let mut error = None;
        print!("{:?} {:?}", expected, actual);
        let unified = match (expected, actual) {
            (ResolvedType::Inferred, actual) => actual,
            (ResolvedType::Any, _) => ResolvedType::Any,
            (expected, ResolvedType::Nothing) => expected,
            (expected, ResolvedType::Inferred) => expected,
            (expected, ResolvedType::Error) => expected,
            (ResolvedType::Error, _) => ResolvedType::Error,
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
                error = Some((format!("{}", expected), format!("{}", actual)));
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
        println!(" -> {:?}", unified);
        unified
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

enum DefineGlobalResult {
    NeedsType(GlobalId),
    Success(IrtExpr),
}

impl DefineGlobalResult {
    fn error_expr() -> DefineGlobalResult {
        DefineGlobalResult::Success(irt::Expr {
            resolved_type: ResolvedType::Error,
            expr_type: irt::ExprType::Error
        })
    }
}

// takes a quoted, escaped string
fn unescape(s: &str) -> Result<String, String> {
    let mut res = String::new();
    let mut chars = s[..s.len()-1].chars(); // strip off last quote, scanner already checked it matches
    chars.next().unwrap();
    while let Some(c) = chars.next() {
        if c == '\\' {
            res.push(match chars.next() {
                None => return Err("Unexpected end of string literal".to_string()),
                Some('"') => '"',
                Some('\'') => '\'',
                Some('n') => '\n',
                Some('t') => '\t',
                Some('\\') => '\\',
                Some(huh) => return Err(format!("Unknown escape sequence: \\{}", huh))
            });
        } else {
            res.push(c);
        }
    }
    Ok(res)
}
