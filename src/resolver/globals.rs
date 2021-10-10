use crate::ast::Binding;
use crate::resolver::lookup::Lookup;
use crate::resolver::qname_list::{Fqn, QNameList};
use crate::resolver::types::ResolvedType;

// TODO this will need to be more like TypeId
#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct GlobalId(u16);

impl GlobalId {
    pub fn new(id: u16) -> GlobalId {
        GlobalId(id)
    }
}

pub struct GlobalDeclaration {
    pub id: GlobalId,
    pub fqn: Fqn,
    pub resolved_type: ResolvedType,
}
pub struct UndefinedGlobalBinding<'ast> {
    pub namespace: Vec<String>,
    pub decls: Vec<UndefinedGlobal<'ast>>, // may be empty
    pub expected_type: ResolvedType,
    pub ast_node: &'ast Binding,
}

pub struct UndefinedGlobal<'ast> {
    pub id: GlobalId,
    pub fqn: Fqn,
    pub visibility: Vec<String>,
    pub declared_type: ResolvedType,
    pub from: BindingFrom<'ast>,
}

impl UndefinedGlobal<'_> {
    // TODO why can't I own self
    pub fn define(&self, resolved_type: ResolvedType) -> GlobalDeclaration {
        GlobalDeclaration {
            id: self.id,
            fqn: self.fqn.clone(),
            resolved_type,
        }
    }
}

pub enum BindingFrom<'ast> {
    Direct,                  // let x = ...
    Destructured(&'ast str), // let { x } = ...
}

pub struct GlobalLookup {
    globals: Vec<(Fqn, GlobalId, Vec<String>)>,
}

impl GlobalLookup {
    pub fn new(undefined_globals: &[UndefinedGlobalBinding]) -> GlobalLookup {
        // TODO eliminate visibility clone?
        let globals = undefined_globals.iter()
            .flat_map(|it| &it.decls)
            .map(|ug| (ug.fqn.clone(), ug.id.clone(), ug.visibility.clone()))
            .collect();
        GlobalLookup {
            globals,
        }
    }
}

impl Lookup for GlobalLookup {
    type Id = GlobalId;

    fn find(&self, qn: QNameList) -> Option<(GlobalId, &[String])> {
        self.globals.iter()
            .find(|(fqn, _, _)| qn.matches(fqn))
            .map(|(_, id, vis)| (id.clone(), vis.as_slice()))
    }
}