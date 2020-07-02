use std::rc::Rc;
use crate::ast::{QName, Type, TypeCaseRecord};
use std::fmt::{Debug, Formatter, Display};
use crate::resolver::{Lookup, QNameList};

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct TypeId(Rc<(Rc<String>, Vec<String>)>);
pub struct TypeDeclaration {
    pub id: TypeId,
    pub num_type_params: usize,
    pub definition: TypeDefinition,
    pub visibility: QName,
}
#[derive(Clone)]
pub enum TypeDefinition {
    Record(Vec<(String, ResolvedType)>),
    Union(Vec<ResolvedType>),
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
    pub visibility: QName,
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
    pub fn new(mod_name: Rc<String>, fqn: QName) -> TypeId {
        TypeId(Rc::new((mod_name, fqn.parts)))
    }

    pub fn fqn(&self) -> &[String] {
        &(self.0).1
    }

    pub fn namespace(&self) -> &[String] {
        let fqn = self.fqn();
        &fqn[0..fqn.len()-1]
    }
}

impl Debug for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{:?}", (self.0).0, (self.0).1)
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fqn().join("."))
    }
}

pub struct TypeDeclLookup {
    decls: Vec<(TypeId, Vec<String>)>,
}

impl TypeDeclLookup {
    pub fn new(undefined_types: &[UndefinedType]) -> TypeDeclLookup {
        // TODO don't clone visibility
        TypeDeclLookup {
            decls: undefined_types
                .iter()
                .map(|ut| (ut.id.clone(), ut.visibility.parts.clone()))
                .collect()
        }
    }
}

impl Lookup for TypeDeclLookup {
    type Id = TypeId;

    fn find(&self, fqn: QNameList) -> Option<&(TypeId, Vec<String>)> {
        self.decls
            .iter()
            .find(|&(id, _)| fqn.matches(id.fqn()))
    }
}
