use crate::resolver::globals::GlobalId;
use crate::resolver::types::ResolvedType;

pub struct IrTree {
    pub statements: Vec<Statement>,
}

pub enum Statement {
    SaveGlobal(Save<GlobalId>, Expr),
    SaveLocal(Save<()>, Expr), // TODO LocalId
    Discard(Expr),
}

pub enum Save<Target> {
    Normal(Target), // Save the whole expr result to T
    Destructure(Vec<(String, Target)>), // List of field names to T
}

pub struct Expr {
    pub resolved_type: ResolvedType,
    pub expr_type: ExprType,
}

pub enum ExprType {
    Error,
    LoadConstant(Constant),
    LoadGlobal(GlobalId),
    Call(CallExpr),
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
}

pub enum Constant {
    Unit,
    Number(f64),
    String(String),
    Boolean(bool),
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

pub enum BinaryOp {
    Error,
    Eq,
    Neq,
    LessThan,
    LessEq,
    GreaterThan,
    GreaterEq,
}
