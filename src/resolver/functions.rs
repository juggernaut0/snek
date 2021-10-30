use std::rc::Rc;
use crate::resolver::irt::Statement;
use crate::resolver::ResolvedType;

#[derive(Clone, Eq, PartialEq)]
pub struct FunctionId {
    module: Rc<String>,
    seq_id: u32,
}

impl FunctionId {
    pub fn id(&self) -> u32 {
        self.seq_id
    }
}

pub struct FunctionDeclaration {
    id: FunctionId,
    resolved_type: ResolvedType,
    statements: Vec<Statement>,
}

impl FunctionDeclaration {
    pub fn new(module: Rc<String>, seq_id: u32, resolved_type: ResolvedType, statements: Vec<Statement>) -> FunctionDeclaration {
        FunctionDeclaration {
            id: FunctionId { module, seq_id },
            resolved_type,
            statements,
        }
    }

    pub fn id(&self) -> FunctionId {
        self.id.clone()
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}
