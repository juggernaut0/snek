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
    FTypeParam(usize, u32),
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
            ResolvedType::Func { params, return_type, .. } => params.iter().any(|rt| rt.is_inferred()) || return_type.is_inferred(),
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
            ResolvedType::FTypeParam(id, level) => {
                write!(f, "f type param {} {}", id, level)
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
                .chain(imported_types.into_iter().map(|td| (td.id.clone(), Vec::new()))) // Double check that imported types must have root vis
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

struct FTypeParamStack<'a> {
    head: Rc<&'a [String]>
}

pub(super) trait TypeResolverContext {
    fn push_error(&mut self, error: Error);
    fn get_type(&self, qn: &QName) -> Option<(TypeId, bool)>;
    fn get_type_param(&self, name: &str) -> Option<usize>;
}

// turns Ast TypeName into ResolvedType
pub(super) struct TypeResolver<'ctx, 'ftp> {
    context: &'ctx mut dyn TypeResolverContext,
    function_nest_level: u32,
    f_type_params: FTypeParamStack<'ftp>,
}

impl TypeResolver<'_, '_> {
    pub(super) fn new<'ctx>(context: &'ctx mut dyn TypeResolverContext) -> TypeResolver<'ctx, 'static> {
        TypeResolver {
            context,
            function_nest_level: 0,
            f_type_params: Vec::new(),
        }
    }

    fn new_child(&mut self, f_type_params: &[String]) -> TypeResolver {
        let f_type_params = {
            let mut tmp: Vec<_> = self.f_type_params.iter().cloned().collect();
            tmp.push(f_type_params);
            tmp
        };
        TypeResolver {
            context: &mut self.context,
            function_nest_level: self.function_nest_level + 1,
            f_type_params,
        }
    }

    pub(super) fn resolve_binding_type(&mut self, type_name: &TypeName) -> ResolvedType {
        match &type_name.type_name_type {
            TypeNameType::Named(nt) => {
                self.resolve_named_type(nt, type_name.line, type_name.col)
            },
            TypeNameType::Func(ft) => {
                let num_type_params = ft.type_params.len();

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

    fn resolve_named_type(&mut self, named_type: &NamedType, line: u32, col: u32) -> ResolvedType {
        // If the name is not a qualified name and has no args of its own, it may be a type param
        if named_type.name.parts.len() == 1 && named_type.type_args.is_empty() {
            let name = &named_type.name.parts[0];
            if let Some(tpi) = self.find_type_param(name) {
                return ResolvedType::TypeParam(tpi);
            } else if let Some((tpi, level)) = self.find_f_tye_param(name) {
                return ResolvedType::FTypeParam(tpi, level);
            }
        }

        if let Some((id, visible)) = self.context.get_type(&named_type.name) {
            if !visible {
                self.errors.push(Error {
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
            self.errors.push(Error {
                message: format!("Unknown type name '{}'", &named_type.name.parts.join(".")),
                line,
                col,
            });
            ResolvedType::Error
        }
    }

    fn find_f_tye_param(&self, name: &str) -> Option<(usize, u32)> {
        for (level, names) in self.f_type_params.iter().rev().enumerate() {
            if let Some(i) = names.iter().position(|it| name == it) {
                return Some((i, level as u32))
            }
        }
        None
    }
}
