use std::rc::Rc;
use crate::ast::{Type, TypeCaseRecord};
use std::fmt::{Debug, Formatter, Display};
use crate::resolver::qname_list::{QNameList, Fqn};
use crate::resolver::lookup::Lookup;

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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ResolvedType {
    Id(TypeId, Vec<ResolvedType>),
    TypeParam(usize),
    Func(ResolvedFuncType),
    Callable(Box<ResolvedType>),
    Unit,      // ()
    Any,       // *
    Nothing,   // !
    Inferred,  // _
    Error,
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedFuncType {
    //pub type_params: Vec<String>, TODO
    pub params: Vec<ResolvedType>,
    pub return_type: Box<ResolvedType>,
}

impl ResolvedType {
    pub fn is_inferred(&self) -> bool {
        match self {
            ResolvedType::Inferred => true,
            ResolvedType::Id(_, args) => args.iter().any(|rt| rt.is_inferred()),
            ResolvedType::Func(rft) => rft.params.iter().any(|rt| rt.is_inferred()) || rft.return_type.is_inferred(),
            _ => false
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            ResolvedType::Error => true,
            ResolvedType::Id(_, args) => args.iter().any(|rt| rt.is_error()),
            ResolvedType::Func(rft) => rft.params.iter().any(|rt| rt.is_error()) || rft.return_type.is_error(),
            _ => false
        }
    }
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Id(id, args) => {
                write!(f, "{}", id)?;
                if args.len() > 0 {
                    write!(f, "<")?;
                    join(f, &args, " ")?;
                    write!(f, ">")?;
                }
                Ok(())
            }
            ResolvedType::TypeParam(id) => todo!("Need type param name"),
            ResolvedType::Func(rft) => {
                write!(f, "{{ ")?;
                join(f, &rft.params, " ")?;
                write!(f, " -> {} }}", rft.return_type)
            },
            ResolvedType::Callable(ret) => write!(f, "{{ ? -> {} }}", ret),
            ResolvedType::Unit => write!(f, "()"),
            ResolvedType::Any => write!(f, "*"),
            ResolvedType::Nothing => write!(f, "!"),
            ResolvedType::Inferred => write!(f, "_"),
            ResolvedType::Error => write!(f, "Error"),
        }
    }
}

fn join<T: Display>(f: &mut Formatter<'_>, ts: &[T], sep: &str) -> std::fmt::Result {
    if ts.len() == 0 {
        return Ok(());
    }
    let mut it = ts.iter();
    write!(f, "{}", it.next().unwrap())?;
    for t in it {
        write!(f, "{}{}", sep, t)?;
    }
    Ok(())
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
