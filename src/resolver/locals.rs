use crate::ast::QName;
use crate::resolver::ResolvedType;

#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq)]
pub struct LocalId(pub u32);

pub struct LocalScope<'a> {
    parent: Option<&'a LocalScope<'a>>,
    level: u32,
    declarations: Vec<ScopeItem>,
    local_id_seq: u32,
}

pub struct ScopeItem {
    pub name: String,
    pub id: LocalId,
    pub level: u32,
    pub typ: ResolvedType,
}

impl<'a> LocalScope<'a> {
    pub fn new(parent: Option<&'a LocalScope<'a>>, capturing: bool) -> LocalScope<'a> {
        LocalScope {
            parent,
            level: parent.map_or(0, |p| p.level + if capturing { 1 } else { 0 }),
            declarations: Vec::new(),
            local_id_seq: parent.map_or(0, |p| p.local_id_seq),
        }
    }

    pub fn level(&self) -> u32 {
        self.level
    }

    pub fn insert(&mut self, name: String, typ: ResolvedType) -> LocalId {
        let id = LocalId(self.local_id_seq);
        self.local_id_seq += 1;
        self.declarations.push(ScopeItem { name, id, level: self.level, typ });
        id
    }

    fn get(&self, name: &str) -> Option<&ScopeItem> {
        self.declarations.iter().rev().find(|d| d.name == name)
            .or_else(|| self.parent.and_then(|p| p.get(name)))
    }

    pub fn get_from_qname(&self, qn: &QName) -> Option<&ScopeItem> {
        if qn.parts.len() != 1 { return None }
        self.get(qn.parts.last().unwrap())
    }
}
