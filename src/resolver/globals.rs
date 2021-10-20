use std::rc::Rc;
use crate::ast::Binding;
use crate::resolver::FieldPath;
use crate::resolver::lookup::Lookup;
use crate::resolver::qname_list::{Fqn, QNameList};
use crate::resolver::types::ResolvedType;

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct GlobalId(Rc<String>, Fqn);

impl GlobalId {
    pub fn new(mod_name: Rc<String>, fqn: Fqn) -> GlobalId {
        GlobalId(mod_name, fqn)
    }
}

impl GlobalId {
    pub fn module(&self) -> &str {
        &self.0
    }

    pub fn fqn(&self) -> &Fqn {
        &self.1
    }
}

pub struct GlobalDeclaration {
    pub id: GlobalId,
    pub resolved_type: ResolvedType,
    pub visibility: Vec<String>,
    pub export: bool,
}

impl GlobalDeclaration {
    pub fn fqn(&self) -> &Fqn {
        &self.id.fqn()
    }
    pub fn is_exported(&self) -> bool {
        self.visibility.is_empty() && self.export
    }

}

pub struct UndefinedGlobalBinding<'ast> {
    pub namespace: Vec<String>,
    pub decls: Vec<UndefinedGlobal>, // may be empty
    pub expected_type: ResolvedType,
    pub ast_node: &'ast Binding,
}

pub struct UndefinedGlobal {
    pub id: GlobalId,
    pub fqn: Fqn,
    pub visibility: Vec<String>,
    pub export: bool,
    pub declared_type: ResolvedType,
    pub from: FieldPath,
}

impl UndefinedGlobal {
    // TODO why can't I own self
    pub fn define(&self, resolved_type: ResolvedType) -> GlobalDeclaration {
        GlobalDeclaration {
            id: self.id.clone(),
            resolved_type,
            visibility: self.visibility.clone(),
            export: self.export,
        }
    }
}

pub struct GlobalLookup {
    globals: Vec<(GlobalId, Vec<String>)>,
}

impl GlobalLookup {
    pub fn new<'a>(undefined_globals: &[UndefinedGlobalBinding], imported_globals: impl IntoIterator<Item=&'a GlobalDeclaration>) -> GlobalLookup {
        // TODO eliminate visibility clone?
        let globals = undefined_globals.iter()
            .flat_map(|it| &it.decls)
            .map(|ug| (ug.id.clone(), ug.visibility.clone()))
            .chain(imported_globals.into_iter().map(|gd| (gd.id.clone(), Vec::new())))
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
            .find_map(|(id, vis)| {
                if qn.matches(&id.1) {
                    Some((id.clone(), vis.as_slice()))
                } else {
                    None
                }
            })
    }
}