use std::collections::HashMap;
use crate::ast::QName;

struct Declaration(u32);

struct Resolver {
    declarations: HashMap<String, Declaration>,
    usages: HashMap<QName, Declaration>
}