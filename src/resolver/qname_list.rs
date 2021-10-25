use crate::ast::QName;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Clone, Copy)]
pub enum QNameList<'parent, 'ast> {
    Empty,
    List(&'parent QNameList<'parent, 'ast>, &'ast [String])
}

impl<'parent, 'ast> QNameList<'parent, 'ast> {
    pub fn append(&'parent self, qname: &'ast QName) -> QNameList<'parent, 'ast> {
        self.append_slice(&qname.parts)
    }

    pub fn append_slice(&'parent self, slice: &'ast [String]) -> QNameList<'parent, 'ast> {
        if slice.is_empty() { *self } else { QNameList::List(self, slice) }
    }

    pub fn len(&self) -> usize {
        match self {
            QNameList::Empty => 0,
            QNameList::List(parent, name) => parent.len() + name.len()
        }
    }

    pub fn iter(&self) -> QNameListIter {
        QNameListIter::new(self)
    }

    // element-wise equality
    pub fn matches(&self, fqn: &Fqn) -> bool {
        let names = fqn.as_slice();
        if self.len() != names.len() { return false }
        self.iter().zip(names.iter()).all(|(a, b)| a == b)
    }

    // Is `prefix` a prefix of self?
    pub fn prefix_matches(&self, prefix: &[String]) -> bool {
        if self.len() < prefix.len() { return false }
        self.iter().zip(prefix).all(|(a, b)| a == b)
    }

    pub fn pop_namespace(self) -> QNameList<'parent, 'ast> {
        match self {
            QNameList::Empty => QNameList::Empty,
            QNameList::List(parent, names) => {
                let names_len = names.len();
                if names_len == 1 {
                    *parent
                } else {
                    parent.append_slice(&names[0..names_len-1])
                }
            }
        }
    }

    pub fn to_vec(self) -> Vec<String> {
        self.iter().cloned().collect()
    }
}

pub struct QNameListIter<'ast> {
    parent: Option<Box<QNameListIter<'ast>>>,
    name: Option<std::slice::Iter<'ast, String>>
}

impl<'ast> QNameListIter<'ast> {
    fn new(list: &'ast QNameList<'_, 'ast>) -> QNameListIter<'ast> {
        match list {
            QNameList::Empty => QNameListIter {
                parent: None,
                name: None,
            },
            QNameList::List(parent, name) => QNameListIter {
                parent: Some(Box::new(parent.iter())),
                name: Some(name.iter()),
            },
        }
    }
}

impl<'ast> Iterator for QNameListIter<'ast> {
    type Item = &'ast String;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(parent) = &mut self.parent {
            let p_next = parent.next();
            if p_next.is_some() {
                return p_next
            } else {
                self.parent = None
            }
        }
        if let Some(name) = &mut self.name {
            return name.next()
        }
        None
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct Fqn {
    parts: Rc<Vec<String>>,
}

impl Fqn {
    pub fn new(namespace: Vec<String>, name: String) -> Fqn {
        let mut parts = namespace;
        parts.push(name);
        Fqn {
            parts: Rc::new(parts),
        }
    }

    pub fn new_from_qname_list(namespace: QNameList, name: String) -> Fqn {
        Fqn::new(namespace.to_vec(), name)
    }

    pub fn as_slice(&self) -> &[String] {
        &self.parts
    }

    pub fn namespace(&self) -> &[String] {
        let len = self.parts.len();
        &self.parts[..len-1]
    }
}

impl Display for Fqn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parts.join("."))
    }
}

impl<S: Into<String>> From<Vec<S>> for Fqn {
    fn from(its: Vec<S>) -> Self {
        Fqn { parts: Rc::new(its.into_iter().map(|it| it.into()).collect()) }
    }
}

impl<S: ToString> From<&[S]> for Fqn {
    fn from(its: &[S]) -> Self {
        Fqn { parts: Rc::new(its.iter().map(|it| it.to_string()).collect()) }
    }
}
