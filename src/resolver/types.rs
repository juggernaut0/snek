use std::rc::Rc;
use crate::ast::{NamedType, QName, Type, TypeCaseRecord, TypeName, TypeNameType};
use std::fmt::{Debug, Formatter, Display};
use crate::resolver::Error;
use crate::resolver::qname_list::{QNameList, Fqn};
use crate::resolver::lookup::Lookup;
use crate::util::join;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct TypeId(Rc<String>, Fqn);

impl TypeId {
    pub fn new(mod_name: Rc<String>, fqn: Fqn) -> TypeId {
        TypeId(mod_name, fqn)
    }

    pub fn module(&self) -> &str {
        &self.0
    }

    pub fn fqn(&self) -> &Fqn {
        &self.1
    }
}

impl Debug for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{}", self.0, self.1)
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fqn())
    }
}

pub struct TypeDeclaration {
    pub id: TypeId,
    pub num_type_params: usize,
    pub definition: TypeDefinition,
    pub visibility: Vec<String>,
    pub export: bool,
}

impl TypeDeclaration {
    pub fn is_exported(&self) -> bool {
        self.visibility.is_empty() && self.export
    }
}

#[derive(Clone)]
pub enum TypeDefinition {
    Record(Vec<ResolvedField>),
    Union(Vec<ResolvedType>),
    Primitive,
}
#[derive(Clone)]
pub struct ResolvedField {
    pub name: String,
    pub public: bool,
    pub resolved_type: ResolvedType
}

impl TypeDefinition {
    pub fn instantiate_into(mut self, args: &[ResolvedType]) -> TypeDefinition {
        self.instantiate(args);
        self
    }

    pub fn instantiate(&mut self, args: &[ResolvedType]) {
        match self {
            TypeDefinition::Record(fields) => {
                for field in fields {
                    field.resolved_type.instantiate(args);
                }
            }
            TypeDefinition::Union(cases) => {
                for case in cases {
                    case.instantiate(args);
                }
            }
            TypeDefinition::Primitive => {}
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum ResolvedType {
    Id(TypeId, Vec<ResolvedType>),
    TypeParam(usize),
    TypeArg(usize, u32), // (index, nest level)
    Func {
        num_type_params: usize,
        params: Vec<ResolvedType>,
        return_type: Box<ResolvedType>,
    },
    Callable(Box<ResolvedType>),
    Unit,      // ()
    Any,       // *
    Nothing,   // !
    Inferred,  // _
    Error,
    Hole(usize),
}

#[derive(Debug)]
pub enum Hole {
    Empty,
    Fixed,
    Filled(ResolvedType),
}

impl ResolvedType {
    pub fn is_inferred(&self) -> bool {
        match self {
            ResolvedType::Inferred => true,
            ResolvedType::Id(_, args) => args.iter().any(|rt| rt.is_inferred()),
            ResolvedType::Func { params, return_type, .. } => {
                params.iter().any(|rt| rt.is_inferred()) || return_type.is_inferred()
            },
            _ => false
        }
    }
/*
    pub fn is_error(&self) -> bool {
        match self {
            ResolvedType::Error => true,
            ResolvedType::Id(_, args) => args.iter().any(|rt| rt.is_error()),
            ResolvedType::Func { params, return_type } => params.iter().any(|rt| rt.is_error()) || return_type.is_error(),
            _ => false
        }
    }
*/
    pub fn instantiate(&mut self, args: &[ResolvedType]) {
        match self {
            ResolvedType::Id(_, my_args) => {
                my_args.iter_mut().for_each(|it| it.instantiate(args))
            },
            ResolvedType::TypeParam(i) => *self = args[*i].clone(),
            ResolvedType::Func { params, return_type, .. } => {
                params.iter_mut().for_each(|it| it.instantiate(args));
                return_type.instantiate(args);
            },
            ResolvedType::Callable(return_type) => {
                return_type.instantiate(args);
            },
            _other => {}
        }
    }
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Id(id, args) => {
                write!(f, "{}", id)?;
                if !args.is_empty() {
                    write!(f, "<")?;
                    join(f, args, " ")?;
                    write!(f, ">")?;
                }
                Ok(())
            }
            ResolvedType::TypeParam(id) => {
                write!(f, "type param {}", id)
            }
            ResolvedType::TypeArg(id, level) => {
                write!(f, "type arg {} {}", id, level)
            }
            ResolvedType::Func { num_type_params, params, return_type } => {
                write!(f, "{{ ")?;
                if *num_type_params > 0 {
                    write!(f, "<{} params> ", num_type_params)?;
                }
                join(f, params, " ")?;
                write!(f, " -> {} }}", return_type)
            },
            ResolvedType::Callable(ret) => write!(f, "{{ ? -> {} }}", ret),
            ResolvedType::Unit => write!(f, "()"),
            ResolvedType::Any => write!(f, "*"),
            ResolvedType::Nothing => write!(f, "!"),
            ResolvedType::Inferred => write!(f, "_"),
            ResolvedType::Error => write!(f, "Error"),
            ResolvedType::Hole(i) => write!(f, "hole {}", i),
        }
    }
}

pub struct UndefinedType<'ast> {
    pub id: TypeId,
    pub visibility: Vec<String>,
    pub export: bool,
    pub ast_node: UndefinedTypeNode<'ast>
}
pub enum UndefinedTypeNode<'ast> {
    Normal(&'ast Type),
    Case(&'ast TypeCaseRecord, &'ast Vec<String>),
}

impl UndefinedType<'_> {
    pub fn define(self, definition: TypeDefinition, num_type_params: usize) -> TypeDeclaration {
        TypeDeclaration {
            id: self.id,
            num_type_params,
            definition,
            visibility: self.visibility,
            export: self.export,
        }
    }
}

pub struct TypeDeclLookup {
    decls: Vec<(TypeId, Vec<String>)>,
}

impl TypeDeclLookup {
    pub fn new<'a>(undefined_types: &[UndefinedType], imported_types: impl IntoIterator<Item=&'a TypeDeclaration>) -> TypeDeclLookup {
        // TODO avoid cloning visibility
        TypeDeclLookup {
            decls: undefined_types
                .iter()
                .map(|ut| (ut.id.clone(), ut.visibility.clone()))
                .chain(imported_types.into_iter().map(|td| (td.id.clone(), Vec::new())))
                .collect()
        }
    }
}

impl Lookup for TypeDeclLookup {
    type Id = TypeId;

    fn find(&self, fqn: QNameList) -> Option<(TypeId, &[String])> {
        self.decls
            .iter()
            .find(|(id, _)| fqn.matches(id.fqn()))
            .map(|(id, vis)| (id.clone(), vis.as_slice()))
    }
}

#[derive(Clone, Copy)]
pub struct TypeArgStack<'ast, 'parent> {
    args: &'ast [String],
    parent: Option<&'parent TypeArgStack<'ast, 'parent>>,
    level: u32,
}

pub const TYPE_ARG_STACK_ROOT: TypeArgStack<'static, 'static> = TypeArgStack { args: &[], parent: None, level: 0 };

impl<'ast> TypeArgStack<'ast, '_> {
    pub fn push_level(&self, args: &'ast [String]) -> TypeArgStack {
        TypeArgStack {
            args,
            parent: Some(self),
            level: self.level + 1,
        }
    }

    pub fn level(&self) -> u32 {
        self.level
    }
}

pub(super) trait TypeResolverContext {
    fn push_error(&mut self, error: Error);
    fn get_type(&self, qn: &QName) -> Option<(TypeId, bool)>;
    fn get_type_arg(&self, name: &str) -> Option<usize>;
}

// turns Ast TypeName into ResolvedType
pub(super) struct TypeResolver<'ctx, 'ast, 'args> {
    context: &'ctx mut dyn TypeResolverContext,
    function_nest_level: u32,
    type_params: Vec<&'ast [String]>,
    type_args: TypeArgStack<'ast, 'args>,
}

impl TypeResolver<'_, '_, '_> {
    pub(super) fn new<'ctx, 'ast, 'args>(context: &'ctx mut dyn TypeResolverContext, type_args: TypeArgStack<'ast, 'args>) -> TypeResolver<'ctx, 'ast, 'args> {
        TypeResolver {
            context,
            function_nest_level: 0,
            type_params: Vec::new(),
            type_args,
        }
    }
}

impl<'ast> TypeResolver<'_, 'ast, '_> {
    fn new_child(&mut self, f_type_params: &'ast [String]) -> TypeResolver {
        let type_params = {
            let mut tmp: Vec<_> = self.type_params.iter().cloned().collect();
            tmp.push(f_type_params);
            tmp
        };
        TypeResolver {
            context: self.context,
            function_nest_level: self.function_nest_level + 1,
            type_params,
            type_args: self.type_args,
        }
    }

    pub(super) fn resolve_binding_type(&mut self, type_name: &'ast TypeName) -> ResolvedType {
        match &type_name.type_name_type {
            TypeNameType::Named(nt) => {
                self.resolve_named_type(nt, type_name.line, type_name.col)
            },
            TypeNameType::Func(ft) => {
                let num_type_params = ft.type_params.len();

                if self.function_nest_level > 0 && num_type_params > 0 {
                    self.context.push_error(Error {
                        message: "Generic functions as function parameters or return types are not supported".to_string(),
                        line: type_name.line,
                        col: type_name.col,
                    })
                }

                let mut subresolver = self.new_child(&ft.type_params);
                let params = ft.params.iter()
                    .map(|it| subresolver.resolve_binding_type(it))
                    .collect();
                let return_type = Box::new(subresolver.resolve_binding_type(&ft.return_type));

                ResolvedType::Func { num_type_params, params, return_type }
            },
            TypeNameType::Unit => ResolvedType::Unit,
            TypeNameType::Any => ResolvedType::Any,
            TypeNameType::Nothing => ResolvedType::Nothing,
            TypeNameType::Inferred => ResolvedType::Inferred,
        }
    }

    // resolve the NamedType that appears in a "new" expression
    pub(super) fn resolve_new_type(&mut self, named_type: &'ast NamedType, line: u32, col: u32) -> ResolvedType {
        match self.resolve_named_type(named_type, line, col) {
            ResolvedType::TypeArg(_, _) => {
                self.context.push_error(Error {
                    message: "Can not create instance of type arguments using new".to_string(),
                    line,
                    col,
                });
                ResolvedType::Error
            },
            ok @ (ResolvedType::Error | ResolvedType::Id(_, _)) => ok,
            otherwise => panic!("unexpected resolved type from resolved_named_type: {:?}", otherwise)
        }
    }

    fn resolve_named_type(&mut self, named_type: &'ast NamedType, line: u32, col: u32) -> ResolvedType {
        // If the name is not a qualified name and has no args of its own, it may be a type param or a type arg
        if named_type.name.parts.len() == 1 && named_type.type_args.is_empty() {
            let name = &named_type.name.parts[0];
            if let Some((tpi, _)) = self.find_type_param(name) {
                return ResolvedType::TypeParam(tpi);
            } else if let Some((tai, level)) = self.find_type_arg(name) {
                return ResolvedType::TypeArg(tai, level);
            }
        }

        if let Some((id, visible)) = self.context.get_type(&named_type.name) {
            if !visible {
                self.context.push_error(Error {
                    message: format!("Cannot access type '{}'", &named_type.name.parts.join(".")),
                    line,
                    col,
                });
            }
            let args = named_type.type_args.iter()
                .map(|arg| self.resolve_binding_type(arg))
                .collect();
            ResolvedType::Id(id, args)
        } else {
            self.context.push_error(Error {
                message: format!("Unknown type name '{}'", &named_type.name.parts.join(".")),
                line,
                col,
            });
            ResolvedType::Error
        }
    }

    fn find_type_param(&self, name: &str) -> Option<(usize, u32)> {
        for (level, names) in self.type_params.iter().rev().enumerate() {
            if let Some(i) = names.iter().position(|it| name == it) {
                return Some((i, level as u32))
            }
        }
        None
    }

    fn find_type_arg(&self, name: &str) -> Option<(usize, u32)> {
        let mut opt_head = Some(self.type_args);
        while let Some(head) = opt_head {
            let i = head.args.iter().position(|it| it.as_str() == name);
            if let Some(i) = i {
                return Some((i, head.level));
            }
            opt_head = head.parent.copied()
        }
        None
    }
}
