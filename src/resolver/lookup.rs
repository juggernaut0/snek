use crate::resolver::qname_list::QNameList;

pub trait Lookup {
    type Id;

    fn find(&self, fqn: QNameList) -> Option<(Self::Id, &[String])>;

    fn root_scope(&self) -> LookupScope<Self> {
        LookupScope {
            lookup: self,
            scope: QNameList::Empty,
        }
    }
}

pub struct LookupScope<'lookup, 'ast, 'parent, TLookup : Lookup + ?Sized> {
    lookup: &'lookup TLookup,
    scope: QNameList<'ast, 'parent>,
}

impl<'ast, TLookup : Lookup> LookupScope<'_, 'ast, '_, TLookup> {
    pub fn enter_scope(&self, name: &'ast [String]) -> LookupScope<TLookup> {
        LookupScope {
            lookup: self.lookup,
            scope: self.scope.append_slice(name),
        }
    }

    pub fn get(&self, name: &[String]) -> Option<(TLookup::Id, bool)> {
        let mut current_scope = self.scope;
        loop {
            let full_name = current_scope.append_slice(name);
            let found = self.lookup.find(full_name);
            if let Some((id, visibility)) = found {
                let visible = self.is_visible(visibility);
                return Some((id, visible))
            }
            if let QNameList::Empty = current_scope {
                break
            }
            current_scope = current_scope.pop_namespace();
        }
        None
    }

    // Is an object with visibility visible from scope?
    pub fn is_visible(&self, visibility: &[String]) -> bool {
        self.scope.prefix_matches(visibility)
    }
}
