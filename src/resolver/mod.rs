use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::iter::repeat;
use std::rc::Rc;

use globals::*;
pub use globals::GlobalId;
use lookup::*;
use qname_list::*;
use types::*;

use crate::ast::*;
use irt::{BinaryOp as IrtBinaryOp, Expr as IrtExpr, ExprType as IrtExprType, IrTree, Save, Statement, ResolvedPattern, PatternType as IrtPatternType};
#[cfg(debug_assertions)]
pub use locals::LocalId;
#[cfg(not(debug_assertions))]
use locals::LocalId;
use unifier::Unifier;
use crate::resolver::irt::Constant;

mod types;
mod globals;
mod lookup;
mod qname_list;
pub mod irt;
mod locals;
mod unifier;
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

    fn mod_name() -> Rc<String> {
        BUILTIN_TYPE_NAMES.with(|btn| Rc::clone(&btn.mod_name))
    }

    fn number() -> ResolvedType {
        ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone()), Vec::new())
    }

    fn string() -> ResolvedType {
        ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.string.clone()), Vec::new())
    }

    fn boolean() -> ResolvedType {
        ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone()), Vec::new())
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

fn make_builtin_global(name: &str, resolved_type: ResolvedType) -> Rc<GlobalDeclaration> {
    Rc::new(GlobalDeclaration {
        id: GlobalId::new(BuiltinTypeNames::mod_name(), Fqn::new(Vec::new(), name.to_string())),
        resolved_type,
        visibility: Vec::new(),
        export: true,
    })
}

fn make_builtins() -> ModuleDecls {
    ModuleDecls {
        types: vec![
            make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone())),
            make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.string.clone())),
            make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone())),
        ],
        globals: vec![
            make_builtin_global("println", ResolvedType::Func {
                params: vec![ResolvedType::Any],
                return_type: Box::new(ResolvedType::Unit)
            }),
            make_builtin_global("TODO", ResolvedType::Func {
                params: Vec::new(),
                return_type: Box::new(ResolvedType::Nothing)
            }),
        ],
        name: BuiltinTypeNames::mod_name(),
    }
}

// TODO move to submod so it can be private
pub struct Resolver<'deps> {
    // in scope items, including imports
    types: HashMap<TypeId, Rc<TypeDeclaration>>,
    globals: HashMap<GlobalId, Rc<GlobalDeclaration>>,
    // This module's types
    exports: ModuleDecls,

    //usages: HashMap<QNameExpr, (ValueId, String)>,
    dependencies: HashMap<&'deps String, &'deps ModuleDecls>,
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
                ResolvedType::Func {
                    params,
                    return_type: Box::new(return_type)
                }
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
                    let resolved_pattern = self.resolve_pattern(&b.pattern, &scope, None);
                    let expected_type = resolved_pattern.resolved_type.clone();
                    let names = self.extract_names(resolved_pattern);
                    let decls = names
                        .into_iter()
                        .map(|(name, declared_type, from)| {
                            let fqn = Fqn::new_from_qname_list(namespace, name);
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

    // returns a list of tuples of (name, type for that name, field navigation path for that name)
    fn extract_names(&mut self, pattern: ResolvedPattern) -> Vec<(String, ResolvedType, FieldPath)> {
        let mut names = Vec::new();
        self.extract_names_impl(&mut names, pattern, Vec::new());
        names
    }

    fn extract_names_impl<'a>(&mut self, names: &mut Vec<(String, ResolvedType, FieldPath)>, pattern: ResolvedPattern, nav: Vec<String>) {
        match pattern.pattern_type {
            IrtPatternType::Discard => {}
            IrtPatternType::Constant(_) => {}
            IrtPatternType::Name(name) => {
                names.push((name, pattern.resolved_type.clone(), FieldPath(nav)))
            }
            IrtPatternType::Destructuring(fields) => {
                for (name, field) in fields {
                    let mut nav = nav.clone(); // TODO avoid this clone by being smarter about sharing nav parents (tree structure)?
                    nav.push(name);
                    self.extract_names_impl(names, field, nav);
                }
            }
        }
    }

    fn resolve_pattern(
        &mut self,
        pattern: &Pattern,
        scope: &LookupScope<TypeDeclLookup>,
        expr_type: Option<ResolvedType>,
    ) -> ResolvedPattern {
        match &pattern.pattern {
            PatternType::Wildcard(otn) => {
                let resolved_type = otn.as_ref()
                    .map(|tn| self.resolve_binding_type(tn, &[], scope))
                    .or(expr_type)
                    .unwrap_or(ResolvedType::Inferred);
                ResolvedPattern { resolved_type, pattern_type: IrtPatternType::Discard }
            }
            PatternType::Name(np) => {
                let resolved_type = np.type_name.as_ref()
                    .map(|it| self.resolve_binding_type(it, &[], scope))
                    .or(expr_type)
                    .unwrap_or(ResolvedType::Inferred);
                ResolvedPattern { resolved_type, pattern_type: IrtPatternType::Name(np.name.as_str().into()) }
            }
            PatternType::Constant(lit) => {
                let resolved_type = match lit.lit_type {
                    LiteralType::NUMBER => BuiltinTypeNames::number(),
                    LiteralType::STRING => BuiltinTypeNames::string(),
                    LiteralType::BOOL => BuiltinTypeNames::boolean(),
                    LiteralType::UNIT => ResolvedType::Unit,
                };
                let constant = self.literal_to_constant(lit, pattern.line, pattern.col);
                ResolvedPattern { resolved_type, pattern_type: IrtPatternType::Constant(constant) }
            }
            PatternType::Destruct(fps, otn) => {
                let declared_type = otn.as_ref()
                    .map(|tn| self.resolve_binding_type(tn, &[], scope))
                    .or(expr_type);

                let declared_type_fields = declared_type
                    .as_ref()
                    .and_then(|rt| {
                        match rt {
                            ResolvedType::Id(id, args) => {
                                let type_decl = self.types.get(id).expect("Missing type def");
                                if let TypeDefinition::Record(fs) = type_decl.definition.clone().instantiate_into(&args) {
                                    Some(fs)
                                } else {
                                    self.errors.push(Error {
                                        message: "Destructuring pattern must be for a record type".to_string(),
                                        line: pattern.line,
                                        col: pattern.col,
                                    });
                                    None
                                }
                            },
                            ResolvedType::Inferred => { None },
                            _ => {
                                self.errors.push(Error {
                                    message: "Destructuring pattern must be for a record type".to_string(),
                                    line: pattern.line,
                                    col: pattern.col,
                                });
                                None
                            }
                        }
                    });

                let mut subpatterns = Vec::new();
                for field in fps {
                    let field_name = field.field_name();

                    let inferred_field_type = declared_type_fields
                        .as_ref()
                        .and_then(|fs| {
                            let declared_field = fs.iter()
                                .find(|rf| field_name == &rf.name);
                            if declared_field.is_none() {
                                self.errors.push(Error {
                                    message: format!("Unknown field {}", field_name),
                                    line: pattern.line, // TODO get more accurate location for field_name
                                    col: pattern.col,
                                })
                            }
                            declared_field.map(|rf| rf.resolved_type.clone())
                        })
                        .unwrap_or(ResolvedType::Inferred);

                    let resolved_pattern = match field {
                        FieldPattern::Name(np) => {
                            let declared_field_type = np.type_name.as_ref()
                                .map(|it| self.resolve_binding_type(it, &[], scope))
                                .unwrap_or(ResolvedType::Inferred);

                            let resolved_type = self.unify(declared_field_type, inferred_field_type, np.line, np.col);

                            ResolvedPattern {
                                resolved_type,
                                pattern_type: IrtPatternType::Name(np.name.as_str().into())
                            }
                        },
                        FieldPattern::Binding(subpat, _) => {
                            let rp = self.resolve_pattern(subpat, scope, None);

                            let resolved_type = self.unify(rp.resolved_type, inferred_field_type, subpat.line, subpat.col);

                            ResolvedPattern {
                                resolved_type,
                                pattern_type: rp.pattern_type,
                            }
                        },
                    };

                    subpatterns.push((field_name.as_str().into(), resolved_pattern));
                }

                let resolved_type = declared_type.unwrap_or(ResolvedType::Inferred);
                ResolvedPattern { resolved_type, pattern_type: IrtPatternType::Destructuring(subpatterns) }
            }
            PatternType::List(_) => { todo!("list pattern") }
        }
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
            TypeNameType::Func(ft) => {
                // TODO type params
                let params = ft.params.iter().map(|it| self.resolve_binding_type(it, type_params, scope)).collect();
                let return_type = Box::new(self.resolve_binding_type(&ft.return_type, type_params, scope));

                ResolvedType::Func { params, return_type }
            },
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
                    let pattern = &ugb.ast_node.pattern;
                    if irt_expr.resolved_type.is_inferred() {
                        self.errors.push(Error {
                            message: "Unable to infer type of global binding".into(),
                            line: pattern.line,
                            col: pattern.col,
                        });
                    }
                    if !self.is_exhaustive(std::slice::from_ref(pattern), irt_expr.resolved_type.clone(), &type_scope) {
                        self.errors.push(Error {
                            message: "Binding pattern must be exhaustive".into(),
                            line: pattern.line,
                            col: pattern.col,
                        });
                    }
                    let mut paths = Vec::new();
                    // TODO I would really love to own ugb here to clean up these clones
                    for ug in &ugb.decls {
                        let path = ug.from.clone();
                        let resolved_type = if path.is_empty() {
                            irt_expr.resolved_type.clone()
                        } else {
                            let expected_type = ug.declared_type.clone();
                            let actual_type = self.navigate_type_fields(&path, irt_expr.resolved_type.clone(), pattern.line, pattern.col);
                            self.unify(expected_type, actual_type, pattern.line, pattern.col)
                        };
                        paths.push((path, ug.id.clone()));
                        let decl = Rc::new(ug.define(resolved_type));
                        self.globals.insert(ug.id.clone(), Rc::clone(&decl));
                        self.exports.globals.push(decl);
                    }
                    let stmt = if paths.is_empty() {
                        Statement::Discard(irt_expr)
                    } else {
                        Statement::SaveGlobal(Save { paths }, irt_expr)
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
                if let Some(local) = local_scope.get_from_qname(qn) {
                    (local.typ.clone(), IrtExprType::LoadLocal(local.id))
                } else if let Some((global, visible)) = global_scope.get(&qn.parts) {
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
                    LiteralType::NUMBER => BuiltinTypeNames::number(),
                    LiteralType::STRING => BuiltinTypeNames::string(),
                    LiteralType::BOOL => BuiltinTypeNames::boolean(),
                    LiteralType::UNIT => ResolvedType::Unit,
                };
                let cons = self.literal_to_constant(lit, expr.line, expr.col);
                (actual_type, IrtExprType::LoadConstant(cons))
            },
            ExprType::Unary(_, _) => todo!(),
            ExprType::Binary(op, left, right) => {
                let res_left = match self.resolve_expr(ResolvedType::Inferred, &left, type_scope, global_scope, local_scope) {
                    DefineGlobalResult::Success(it) => it,
                    DefineGlobalResult::NeedsType(id) => return DefineGlobalResult::NeedsType(id),
                };
                let res_right = match self.resolve_expr(ResolvedType::Inferred, &right, type_scope, global_scope, local_scope) {
                    DefineGlobalResult::Success(it) => it,
                    DefineGlobalResult::NeedsType(id) => return DefineGlobalResult::NeedsType(id),
                };
                let (rt, op) = match (op, &res_left.resolved_type, &res_right.resolved_type) {
                    (BinaryOp::EQ, _, _) => (BuiltinTypeNames::boolean(), IrtBinaryOp::Eq),
                    (BinaryOp::NEQ, _, _) => (BuiltinTypeNames::boolean(), IrtBinaryOp::Neq),
                    (BinaryOp::LT, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::LessThan),
                    (BinaryOp::GT, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::GreaterThan),
                    (BinaryOp::LEQ, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::LessEq),
                    (BinaryOp::GEQ, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::GreaterEq),
                    (BinaryOp::PLUS, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberAdd),
                    (BinaryOp::MINUS, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberSub),
                    (BinaryOp::TIMES, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberMul),
                    (BinaryOp::DIV, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberDiv),
                    (BinaryOp::PLUS, l, r) if l == &BuiltinTypeNames::string() && r == &BuiltinTypeNames::string() => (BuiltinTypeNames::string(), IrtBinaryOp::StringConcat),
                    (op, left, right) => {
                        let message = if left.is_inferred() {
                            format!("Unable to infer type of left-hand operand")
                        } else if right.is_inferred() {
                            format!("Unable to infer type of right-hand operand")
                        } else {
                            format!("Incompatible operand types for binary operator {:?}: {} & {}", op, left, right)
                        };
                        self.errors.push(Error {
                            message,
                            line: expr.line,
                            col: expr.col,
                        });
                        (ResolvedType::Error, IrtBinaryOp::Error)
                    },
                };
                (rt, IrtExprType::Binary { op, left: Box::new(res_left), right: Box::new(res_right) })
            },
            ExprType::Call(call_expr) => {
                match self.resolve_call_expr(expected_type.clone(), expr, type_scope, global_scope, local_scope, call_expr) {
                    Ok(it) => it,
                    Err(dfg) => return dfg
                }
            },
            ExprType::Lambda(lambda_expr) => {
                match self.resolve_lambda_expr(expected_type.clone(), expr, type_scope, global_scope, local_scope, lambda_expr) {
                    Ok(it) => it,
                    Err(dfg) => return dfg
                }
            }
            ExprType::List(_) => todo!(),
            ExprType::New(_, _) => todo!("resolve_expr new"),
            ExprType::Dot => todo!(),
            ExprType::Match => unreachable!("match handled in resolve_call_expr")
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
        if let ExprType::Match = &call_expr.callee.expr_type {
            todo!("match expression")
        }

        let callee_result = self.resolve_expr(ResolvedType::Callable(Box::new(expected_type)), &call_expr.callee, type_scope, global_scope, local_scope);
        let callee = match callee_result {
            DefineGlobalResult::NeedsType(_) => return Err(callee_result),
            DefineGlobalResult::Success(callee_expr) => callee_expr,
        };
        let (params, return_type) = match callee.resolved_type.clone() {
            ResolvedType::Func { params, return_type } => (params, return_type),
            ResolvedType::Callable(et) => {
                self.errors.push(Error {
                    message: "Unable to infer callee type".into(),
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

        if params.len() < call_expr.args.len() {
            self.errors.push(Error {
                message: "Too many arguments provided for function".to_string(),
                line: expr.line,
                col: expr.col,
            })
        } else if params.len() < call_expr.args.len() {
            // TODO currying
            self.errors.push(Error {
                message: "Partial application not yet supported".to_string(),
                line: expr.line,
                col: expr.col,
            })
        }

        let arg_results = params
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
            *return_type,
            irt::ExprType::Call {
                callee: Box::new(callee),
                args,
            }
        ))
    }

    fn resolve_lambda_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        type_scope: &LookupScope<TypeDeclLookup>,
        global_scope: &LookupScope<GlobalLookup>,
        local_scope: &LocalScope,
        lambda_expr: &LambdaExpr,
    ) -> Result<(ResolvedType, IrtExprType), DefineGlobalResult> {
        let (expected_params, expected_return_type) = match expected_type {
            ResolvedType::Func { params, return_type } =>{
                if params.len() != lambda_expr.params.len() {
                    self.errors.push(Error {
                        message: format!("Parameter count mismatch. Expected {} Actual {}", params.len(), lambda_expr.params.len()),
                        line: expr.line,
                        col: expr.col,
                    });
                }
                (params, *return_type)
            }
            ResolvedType::Inferred => (Vec::new(), ResolvedType::Inferred),
            _ => {
                self.errors.push(Error {
                    message: "Type mismatch".to_string(),
                    line: expr.line,
                    col: expr.col,
                });
                (Vec::new(), ResolvedType::Inferred)
            }
        };

        let mut statements = Vec::new();
        let mut param_types = Vec::new();
        let mut scope = LocalScope::new(Some(local_scope));

        let params = lambda_expr.params.iter()
            .zip(expected_params.into_iter().chain(repeat(ResolvedType::Inferred)));
        for (param, expected_param_type) in params {
            let resolved_pattern = self.resolve_pattern(param, type_scope, Some(expected_param_type));
            let resolved_type = resolved_pattern.resolved_type.clone();
            let names = self.extract_names(resolved_pattern);
            if resolved_type.is_inferred() {
                self.errors.push(Error {
                    message: "Unable to infer parameter type, specify the type explicitly".to_string(),
                    line: param.line,
                    col: param.col,
                });
            }
            let irt_expr = IrtExpr { resolved_type: resolved_type.clone(), expr_type: IrtExprType::LoadParam };
            param_types.push(resolved_type);
            let paths: Vec<_> = names.into_iter()
                .map(|(name, declared_type, path)| {
                    let resolved_type = if path.is_empty() {
                        declared_type
                    } else {
                        let actual_type = self.navigate_type_fields(&path, irt_expr.resolved_type.clone(), param.line, param.col);
                        self.unify(declared_type, actual_type, param.line, param.col)
                    };
                    (path, scope.insert(name.into(), resolved_type))
                })
                .collect();
            let statement = if paths.is_empty() {
                Statement::Discard(irt_expr)
            } else {
                Statement::SaveLocal(Save { paths }, irt_expr)
            };
            statements.push(statement);
        }

        for binding in &lambda_expr.bindings {
            #[cfg(debug_assertions)] {
                if let Expr { expr_type: ExprType::QName(qn), .. } = &binding.expr {
                    if qn.parts.len() == 1 && &qn.parts[0] == "diag" {
                        for item in &scope.declarations {
                            println!("local {} is {}: {}", item.id.0, item.name, item.typ)
                        }
                        continue
                    }
                }
            }

            let pattern = &binding.pattern;
            let resolved_pattern = self.resolve_pattern(pattern, type_scope, None);
            let expected_type = resolved_pattern.resolved_type.clone();
            let names = self.extract_names(resolved_pattern);
            let dfg = self.resolve_expr(expected_type, &binding.expr, type_scope, global_scope, &scope);
            // TODO can I accumulate these across all bindings and return them all at once?
            let irt_expr = match dfg {
                DefineGlobalResult::NeedsType(_) => return Err(dfg),
                DefineGlobalResult::Success(irt_expr) => irt_expr,
            };
            let paths: Vec<_> = names.into_iter()
                .map(|(name, declared_type, path)| {
                    let actual_type = self.navigate_type_fields(&path, irt_expr.resolved_type.clone(), pattern.line, pattern.col);
                    let resolved_type = self.unify(declared_type, actual_type, pattern.line, pattern.col);
                    (path, scope.insert(name.into(), resolved_type))
                })
                .collect();
            let statement = if paths.is_empty() {
                Statement::Discard(irt_expr)
            } else {
                Statement::SaveLocal(Save { paths }, irt_expr)
            };
            statements.push(statement);
        }

        let dfg = self.resolve_expr(expected_return_type, &lambda_expr.expr, type_scope, global_scope, &scope);
        let irt_expr = match dfg {
            DefineGlobalResult::NeedsType(_) => return Err(dfg),
            DefineGlobalResult::Success(irt_expr) => irt_expr,
        };
        let return_type = Box::new(irt_expr.resolved_type.clone());

        statements.push(Statement::Return(irt_expr));

        let rt = ResolvedType::Func { params: param_types, return_type };
        let irt_expr_type = IrtExprType::Func { statements };
        Ok((rt, irt_expr_type))
    }

    fn unify(&mut self, expected: ResolvedType, actual: ResolvedType, line: u32, col: u32) -> ResolvedType {
        Unifier::new(self, line, col).unify(expected, actual)
    }

    fn unifies(&mut self, expected: ResolvedType, actual: ResolvedType, line: u32, col: u32) -> bool {
        Unifier::new(self, line, col).unifies(expected, actual)
    }

    //noinspection RsSelfConvention
    // Do the set of given patterns cover all possible values of typ?
    fn is_exhaustive(&mut self, patterns: &[Pattern], typ: ResolvedType, scope: &LookupScope<TypeDeclLookup>) -> bool {
        let is_catch_all = |pattern: &Pattern| -> bool {
            let type_name = match &pattern.pattern {
                PatternType::Wildcard(type_name) => type_name,
                PatternType::Name(np) => &np.type_name,
                _ => return false,
            };
            let expected = type_name.as_ref()
                .map(|tn| self.resolve_binding_type(tn, &[], scope))
                .unwrap_or(ResolvedType::Inferred);

            self.unifies(expected, typ.clone(), pattern.line, pattern.col)
        };

        if patterns.iter().any(is_catch_all) {
            return true;
        }

        match typ.clone() {
            ResolvedType::Id(id, args) => {
                let type_def = self.types.get(&id).expect("missing type").definition.clone().instantiate_into(&args);
                if let TypeDefinition::Union(cases) = type_def {
                    cases.into_iter().all(|case| {
                        self.is_exhaustive(patterns, case, scope)
                    })
                } else if let TypeDefinition::Record(fields) = type_def {
                    todo!("is_exhaustive destructure")
                    /*
                    def is_exh_for_fields(fields, patterns):
                        if fields is empty:
                            return True
                        field = fields[0]
                        relevant_patterns = patterns.filter { it is destructure of typ }.map { it.fields[field.name] }
                        if not is_exhaustive(relevant_patterns, field.type)
                            return False
                        return relevant_patterns.all { pattern -> is_exh_for_fields(fields[1:], relevant_patterns.filter { it.fields[field.name] == (pattern || is_catchall) }) }
                    */
                } else if id == BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone()) {
                    let mut has_true = false;
                    let mut has_false = false;
                    for pattern in patterns {
                        if let PatternType::Constant(Literal { lit_type: LiteralType::BOOL, value }) = &pattern.pattern {
                            if value == "true" {
                                has_true = true;
                            } else if value == "false" {
                                has_false = true;
                            }
                        }
                    }
                    has_true && has_false
                } else {
                    false
                }
            }
            ResolvedType::Unit => {
                patterns.iter().any(|pattern| {
                    if let PatternType::Constant(lit) = &pattern.pattern {
                        lit.lit_type == LiteralType::UNIT
                    } else {
                        false
                    }
                })
            }
            ResolvedType::TypeParam(_) | ResolvedType::Func { .. } | ResolvedType::Callable(_) | ResolvedType::Any => false,
            ResolvedType::Nothing => true,
            ResolvedType::Inferred => false,
            ResolvedType::Error => false,
        }
    }

    fn literal_to_constant(&mut self, lit: &Literal, line: u32, col: u32) -> Constant {
        match lit.lit_type {
            LiteralType::NUMBER => irt::Constant::Number(lit.value.parse().unwrap()), // TODO handle out of range numbers?
            LiteralType::STRING => {
                let raw = match unescape(&lit.value) {
                    Ok(raw) => raw,
                    Err(msg) => {
                        self.errors.push(Error {
                            message: msg,
                            line,
                            col,
                        });
                        String::new()
                    }
                };
                irt::Constant::String(raw)
            },
            LiteralType::BOOL => irt::Constant::Boolean(lit.value.parse().unwrap()), // lit.value is guaranteed to be "true" or "false" by parser
            LiteralType::UNIT => irt::Constant::Unit,
        }
    }

    // Navigate fields on record types
    fn navigate_type_fields(&mut self, path: &FieldPath, base_type: ResolvedType, line: u32, col: u32) -> ResolvedType {
        let mut res = base_type;
        for field_name in &path.0 {
            res = if let ResolvedType::Id(id, args) = res {
                let type_decl = self.types.get(&id).expect("Missing type def");
                if let TypeDefinition::Record(fs) = type_decl.definition.clone().instantiate_into(&args) {
                    let orf = fs.iter().find(|rf| &rf.name == field_name);
                    if let Some(rf) = orf {
                        rf.resolved_type.clone()
                    } else {
                        self.errors.push(Error {
                            message: format!("Unknown field {}", field_name),
                            line,
                            col,
                        });
                        return ResolvedType::Error
                    }
                } else {
                    self.errors.push(Error {
                        message: "Destructuring pattern must be for a record type".to_string(),
                        line,
                        col,
                    });
                    return ResolvedType::Error
                }
            } else {
                self.errors.push(Error {
                    message: "Destructuring pattern must be for a record type".to_string(),
                    line,
                    col,
                });
                return ResolvedType::Error
            }
        }
        res
    }
}

struct LocalScope<'a> {
    parent: Option<&'a LocalScope<'a>>,
    declarations: Vec<ScopeItem>,
    local_id_seq: u32,
}

struct ScopeItem {
    pub name: String,
    pub id: LocalId,
    pub typ: ResolvedType,
}

impl<'a> LocalScope<'a> {
    fn new(parent: Option<&'a LocalScope<'a>>) -> LocalScope<'a> {
        LocalScope {
            parent,
            declarations: Vec::new(),
            local_id_seq: parent.map(|p| p.local_id_seq).unwrap_or(0),
        }
    }

    fn insert(&mut self, name: String, typ: ResolvedType) -> LocalId {
        let id = LocalId(self.local_id_seq);
        self.local_id_seq += 1; // TODO I don't think this is right, parent scopes also need their seqs incremented
        self.declarations.push(ScopeItem { name, id, typ });
        id
    }

    fn get(&self, name: &str) -> Option<&ScopeItem> {
        self.declarations.iter().find(|d| &d.name == name).or_else(|| self.parent.and_then(|p| p.get(name)))
    }

    fn get_from_qname(&self, qn: &QName) -> Option<&ScopeItem> {
        if qn.parts.len() != 1 { return None }
        self.get(qn.parts.last().unwrap())
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

// TODO make this cheaper to clone
#[derive(Clone)]
pub struct FieldPath(Vec<String>);

impl FieldPath {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Display for FieldPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("::"))
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
