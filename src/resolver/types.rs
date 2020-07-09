use std::rc::Rc;
use crate::ast::{Type, TypeCaseRecord};
use std::fmt::{Debug, Formatter, Display};
use crate::resolver::qname_list::{QNameList, Fqn};
use crate::resolver::lookup::Lookup;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct TypeId(Rc<String>, Fqn);
pub struct TypeDeclaration {
    pub id: TypeId,
    pub num_type_params: usize,
    pub definition: TypeDefinition,
    pub visibility: Vec<String>,
}
#[derive(Clone)]
pub enum TypeDefinition {
    Record(Vec<ResolvedField>),
    Union(Vec<ResolvedType>),
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
    Unit,
    Any,
    Nothing,
    Inferred,
    Error,
}
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ResolvedFuncType {
    //pub type_params: Vec<String>, TODO
    pub params: Vec<ResolvedType>,
    pub return_type: Box<ResolvedType>,
}
pub struct UndefinedType<'ast> {
    pub id: TypeId,
    pub visibility: Vec<String>,
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
        }
    }
}

impl TypeId {
    pub fn new(mod_name: Rc<String>, fqn: Fqn) -> TypeId {
        TypeId(mod_name, fqn)
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

pub struct TypeDeclLookup {
    decls: Vec<(TypeId, Vec<String>)>,
}

impl TypeDeclLookup {
    pub fn new(undefined_types: &[UndefinedType]) -> TypeDeclLookup {
        // TODO avoid cloning visibility
        TypeDeclLookup {
            decls: undefined_types
                .iter()
                .map(|ut| (ut.id.clone(), ut.visibility.clone()))
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
