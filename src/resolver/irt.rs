use crate::resolver::types::ResolvedType;
use crate::resolver::globals::GlobalId;

pub struct Expr {
    pub resolved_type: ResolvedType,
    pub expr_type: ExprType,
}

pub enum ExprType {
    Error,
    LoadConstant(Constant),
    LoadGlobal(GlobalId),
    Call(CallExpr),
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