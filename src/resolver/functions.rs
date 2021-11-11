use std::rc::Rc;
use crate::resolver::irt::Statement;
use crate::resolver::{LocalId, ResolvedType};

#[derive(Clone, Eq, PartialEq)]
pub struct FunctionId {
    module: Rc<String>,
    seq_id: u32,
}

impl FunctionId {
    pub fn module(&self) -> &str {
        &self.module
    }
    pub fn id(&self) -> u32 {
        self.seq_id
    }
}

pub struct FunctionDeclaration {
    id: FunctionId,
    resolved_type: ResolvedType,
    statements: Vec<Statement>,
    captures: Vec<LocalId>,
}

impl FunctionDeclaration {
    pub fn new(module: Rc<String>, seq_id: u32, resolved_type: ResolvedType, statements: Vec<Statement>, captures: Vec<LocalId>) -> FunctionDeclaration {
        FunctionDeclaration {
            id: FunctionId { module, seq_id },
            resolved_type,
            statements,
            captures,
        }
    }

    pub fn id(&self) -> FunctionId {
        self.id.clone()
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn resolved_type(&self) -> &ResolvedType {
        &self.resolved_type
    }

    pub fn captures(&self) -> &Vec<LocalId> {
        &self.captures
    }
}
