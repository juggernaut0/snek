use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use globals::*;
use lookup::*;
use qname_list::*;
use types::*;

use crate::ast::{self, *};
use irt::{Expr as IrtExpr, IrTree, Save, Statement, Constant};
use unifier::Unifier;
use patterns::{ExhaustivenessChecker, PatternType as IrtPatternType};

pub use functions::{FunctionId, FunctionDeclaration};
pub use globals::GlobalId;
pub use locals::LocalId;
pub use patterns::{ResolvedPattern, PatternType};
pub use types::{TypeId, ResolvedType, TypeDefinition, TypeDeclaration, ResolvedField};
use crate::resolver::expr::{ExprResolver, ExprResolverContext};
use crate::resolver::locals::LocalScope;

mod expr;
mod functions;
pub mod irt;
mod globals;
mod locals;
mod lookup;
mod patterns;
mod qname_list;
mod types;
mod unifier;

#[cfg(test)]
mod test;

type ResolveResult = Result<IrTree, Vec<Error>>;

pub fn resolve(name: Rc<String>, deps: &[ModuleDecls], ast: &Ast) -> ResolveResult {
    let mut resolver = Resolver::new(name, deps);
    let statements = resolver.resolve(ast);
    let irt = IrTree { decls: resolver.exports, statements };
    if resolver.errors.is_empty() {
        Ok(irt)
    } else {
        Err(resolver.errors)
    }
}

pub struct BuiltinTypeNames {
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

    pub fn mod_name() -> Rc<String> {
        BUILTIN_TYPE_NAMES.with(|btn| Rc::clone(&btn.mod_name))
    }

    pub fn number_id() -> TypeId {
        BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone())
    }
    fn number() -> ResolvedType {
        ResolvedType::Id(BuiltinTypeNames::number_id(), Vec::new())
    }

    pub fn string_id() -> TypeId {
        BUILTIN_TYPE_NAMES.with(|btn| btn.string.clone())
    }
    fn string() -> ResolvedType {
        ResolvedType::Id(BuiltinTypeNames::string_id(), Vec::new())
    }

    pub fn boolean_id() -> TypeId {
        BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone())
    }
    fn boolean() -> ResolvedType {
        ResolvedType::Id(BuiltinTypeNames::boolean_id(), Vec::new())
    }
}

thread_local! {
    static BUILTIN_TYPE_NAMES: BuiltinTypeNames = BuiltinTypeNames::new();
}

pub struct ModuleDecls {
    name: Rc<String>,
    types: Vec<Rc<TypeDeclaration>>,
    globals: Vec<Rc<GlobalDeclaration>>,
    functions: Vec<Rc<FunctionDeclaration>>,
}

impl ModuleDecls {
    pub fn name(&self) -> &Rc<String> {
        &self.name
    }

    pub fn get_type(&self, name: &QName) -> Option<&Rc<TypeDeclaration>> {
        self.types.iter().find(|it| it.id.fqn().as_slice() == name.parts.as_slice())
    }

    pub fn get_global(&self, name: &QName) -> Option<&Rc<GlobalDeclaration>> {
        self.globals.iter().find(|it| it.fqn().as_slice() == name.parts.as_slice())
    }

    pub fn functions(&self) -> &Vec<Rc<FunctionDeclaration>> {
        &self.functions
    }

    pub fn types(&self) -> &Vec<Rc<TypeDeclaration>> {
        &self.types
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

pub fn get_builtin_id(name: &str) -> GlobalId {
    GlobalId::new(BuiltinTypeNames::mod_name(), Fqn::new(Vec::new(), name.to_string()))
}

fn make_builtin_global(name: &str, resolved_type: ResolvedType) -> Rc<GlobalDeclaration> {
    Rc::new(GlobalDeclaration {
        id: get_builtin_id(name),
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
                num_type_params: 0,
                params: vec![ResolvedType::Any],
                return_type: Box::new(ResolvedType::Unit)
            }),
            make_builtin_global("TODO", ResolvedType::Func {
                num_type_params: 0,
                params: Vec::new(),
                return_type: Box::new(ResolvedType::Nothing)
            }),
        ],
        functions: Vec::new(),
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

    func_seq: u32,

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
            functions: Vec::default(),
        };
        Resolver {
            types: HashMap::new(),
            globals: HashMap::new(),
            func_seq: 0,
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

    pub fn resolve(&mut self, ast: &Ast) -> Vec<Statement> {
        self.resolve_4(ast)
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
                self.import_global(Rc::clone(gd));
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
                                            let rts = (0..num_type_params).map(ResolvedType::TypeParam).collect();
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
        //todo!("move to TypeResolver");
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
                    num_type_params: ft.type_params.len(),
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
                    let names = resolved_pattern.extract_names();
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

    fn resolve_pattern(
        &mut self,
        pattern: &Pattern,
        scope: &LookupScope<TypeDeclLookup>,
        expr_type: Option<ResolvedType>,
    ) -> ResolvedPattern {
        match &pattern.pattern {
            ast::PatternType::Wildcard(otn) => {
                let resolved_type = otn.as_ref()
                    .map(|tn| self.resolve_binding_type(tn, &[], scope))
                    .or(expr_type)
                    .unwrap_or(ResolvedType::Inferred);
                ResolvedPattern { resolved_type, pattern_type: IrtPatternType::Discard }
            }
            ast::PatternType::Name(np) => {
                let resolved_type = np.type_name.as_ref()
                    .map(|it| self.resolve_binding_type(it, &[], scope))
                    .or(expr_type)
                    .unwrap_or(ResolvedType::Inferred);
                ResolvedPattern { resolved_type, pattern_type: IrtPatternType::Name(np.name.as_str().into()) }
            }
            ast::PatternType::Constant(lit) => {
                let resolved_type = match lit.lit_type {
                    LiteralType::Number => BuiltinTypeNames::number(),
                    LiteralType::String => BuiltinTypeNames::string(),
                    LiteralType::Bool => BuiltinTypeNames::boolean(),
                    LiteralType::Unit => ResolvedType::Unit,
                };
                let constant = self.literal_to_constant(lit, pattern.line, pattern.col);
                ResolvedPattern { resolved_type, pattern_type: IrtPatternType::Constant(constant) }
            }
            ast::PatternType::Destruct(fps, otn) => {
                let declared_type = otn.as_ref()
                    .map(|tn| self.resolve_binding_type(tn, &[], scope))
                    .or(expr_type);

                let declared_type_fields = declared_type
                    .as_ref()
                    .and_then(|rt| {
                        match rt {
                            ResolvedType::Id(id, args) => {
                                let type_decl = self.types.get(id).expect("Missing type def");
                                if let TypeDefinition::Record(fs) = type_decl.definition.clone().instantiate_into(args) {
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

                            let resolved_type = self.unify(declared_field_type, inferred_field_type, None, np.line, np.col);

                            ResolvedPattern {
                                resolved_type,
                                pattern_type: IrtPatternType::Name(np.name.as_str().into())
                            }
                        },
                        FieldPattern::Binding(subpat, _) => {
                            let rp = self.resolve_pattern(subpat, scope, None);

                            let resolved_type = self.unify(rp.resolved_type, inferred_field_type, None, subpat.line, subpat.col);

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
            ast::PatternType::List(_) => { todo!("list pattern") }
        }
    }

    fn resolve_binding_type(
        &mut self,
        type_name: &TypeName,
        f_type_params: &[String],
        scope: &LookupScope<TypeDeclLookup>,
    ) -> ResolvedType {
        struct Context<'a, 'b, 'c, 'd, 'e, 'f, 'g> {
            resolver: &'a mut Resolver<'b>,
            scope: &'c LookupScope<'d, 'e, 'f, TypeDeclLookup>,
        }
        impl TypeResolverContext for Context<'_, '_, '_, '_, '_, '_, '_> {
            fn push_error(&mut self, error: Error) {
                self.resolver.errors.push(error);
            }

            fn get_type(&self, qn: &QName) -> Option<(TypeId, bool)> {
                self.scope.get(&qn.parts)
            }

            fn get_type_param(&self, name: &str) -> Option<usize> {
                None
            }
        }
        TypeResolver::new(&mut Context { resolver: self, scope }).resolve_binding_type(type_name)
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
            let dgr = self.resolve_expr(ugb.expected_type.clone(), &ugb.ast_node.expr, &type_scope, &global_scope);
            match dgr {
                DefineGlobalResult::NeedsType(id) => {
                    let dep = ugbs_by_gid.get(&id).copied().unwrap();
                    if i == dep.0 {
                        self.errors.push(Error {
                            message: "Recursive error while resolving expression".to_string(),
                            line: ugb.ast_node.expr.line,
                            col: ugb.ast_node.expr.col,
                        });
                        result[i] = Some(Statement::Discard(error_expr()));
                        for ug in &ugb.decls {
                            let decl = Rc::new(ug.define(ResolvedType::Error));
                            self.globals.insert(ug.id.clone(), Rc::clone(&decl));
                            self.exports.globals.push(decl);
                        }
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
                    let resolved_pattern = self.resolve_pattern(pattern, &type_scope, Some(irt_expr.resolved_type.clone()));
                    if !self.exhaustive(std::slice::from_ref(&&resolved_pattern), irt_expr.resolved_type.clone()) {
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
                            self.unify(expected_type, actual_type, None, pattern.line, pattern.col)
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
    ) -> DefineGlobalResult {
        struct Context<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j> {
            resolver: &'a mut Resolver<'b>,
            type_scope: &'c LookupScope<'d, 'e, 'f, TypeDeclLookup>,
            global_scope: &'g LookupScope<'h, 'i, 'j, GlobalLookup>,
        }
        impl ExprResolverContext for Context<'_, '_, '_, '_, '_, '_, '_, '_, '_, '_> {
            fn push_error(&mut self, error: Error) {
                self.resolver.errors.push(error);
            }

            fn get_type_decl(&self, id: &TypeId) -> &TypeDeclaration {
                self.resolver.get_type(id)
            }

            fn get_global(&self, qn: &QName) -> Option<(GlobalId, bool)> {
                self.global_scope.get(&qn.parts)
            }

            fn get_global_decl(&self, id: &GlobalId) -> Option<&GlobalDeclaration> {
                self.resolver.globals.get(id).map(|it| it.as_ref())
            }

            fn resolve_named_type(&mut self, named_type: &NamedType, type_params: &[String], line: u32, col: u32) -> ResolvedType {
                self.resolver.resolve_named_type(named_type, type_params, self.type_scope, line, col)
            }

            fn resolve_pattern(&mut self, pattern: &Pattern, expr_type: Option<ResolvedType>) -> ResolvedPattern {
                self.resolver.resolve_pattern(pattern, self.type_scope, expr_type)
            }

            fn navigate_type_fields(&mut self, path: &FieldPath, base_type: ResolvedType, line: u32, col: u32) -> ResolvedType {
                self.resolver.navigate_type_fields(path, base_type, line, col)
            }

            fn is_visible(&self, visibility: &[String]) -> bool {
                self.type_scope.is_visible(visibility)
            }

            fn define_function(&mut self, resolved_type: ResolvedType, statements: Vec<Statement>, captures: Vec<LocalId>) -> FunctionId {
                self.resolver.define_function(resolved_type, statements, captures)
            }

            fn literal_to_constant(&mut self, lit: &Literal, line: u32, col: u32) -> Constant {
                self.resolver.literal_to_constant(lit, line, col)
            }
        }
        let mut context = Context { resolver: self, type_scope, global_scope };
        ExprResolver::new(&mut context).resolve_top_expr(expected_type, expr)
    }

    fn unify(&mut self, expected: ResolvedType, actual: ResolvedType, holes: Option<&mut [Hole]>, line: u32, col: u32) -> ResolvedType {
        let (rt, errors) = Unifier::new(self, holes, line, col).unify(expected, actual);
        self.errors.extend(errors);
        rt
    }

    fn exhaustive<'a>(&self, patterns: &[&'a ResolvedPattern], typ: ResolvedType) -> bool {
        ExhaustivenessChecker::new(self).exhaustive(patterns, typ)
    }

    fn literal_to_constant(&mut self, lit: &Literal, line: u32, col: u32) -> Constant {
        match lit.lit_type {
            LiteralType::Number => irt::Constant::Number(lit.value.parse().unwrap()), // TODO handle out of range numbers?
            LiteralType::String => {
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
            LiteralType::Bool => irt::Constant::Boolean(lit.value.parse().unwrap()), // lit.value is guaranteed to be "true" or "false" by parser
            LiteralType::Unit => irt::Constant::Unit,
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

    fn define_function(&mut self, resolved_type: ResolvedType, statements: Vec<Statement>, captures: Vec<LocalId>) -> FunctionId {
        let seq_id = self.func_seq;
        self.func_seq += 1;
        let decl = FunctionDeclaration::new(Rc::clone(&self.exports.name), seq_id, resolved_type, statements, captures);
        let id = decl.id();
        self.exports.functions.push(Rc::new(decl));
        id
    }
}

trait TypeStore {
    fn get_type(&self, id: &TypeId) -> &TypeDeclaration;
}

impl TypeStore for Resolver<'_> {
    fn get_type(&self, id: &TypeId) -> &TypeDeclaration {
        self.types.get(id).expect("missing type")
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
        DefineGlobalResult::Success(error_expr())
    }
}

fn error_expr() -> IrtExpr {
    irt::Expr {
        resolved_type: ResolvedType::Error,
        expr_type: irt::ExprType::Error
    }
}

// TODO make this cheaper to clone
#[derive(Clone)]
pub struct FieldPath(Vec<String>);

impl FieldPath {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Display for FieldPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("::"))
    }
}

trait CopyRef<T> {
    fn copy_ref(&mut self) -> Option<&mut T>;
}
impl<T> CopyRef<T> for Option<&mut T> {
    fn copy_ref(&mut self) -> Option<&mut T> {
        self.as_mut().map(|x| &mut **x)
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
