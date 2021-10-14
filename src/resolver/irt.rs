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
    Call { callee: Box<Expr>, args: Vec<Expr> },
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },
}

pub enum Constant {
    Unit,
    Number(f64),
    String(String),
    Boolean(bool),
}

pub enum BinaryOp {
    Error,
    Eq,
    Neq,
    LessThan,
    LessEq,
    GreaterThan,
    GreaterEq,
    NumberAdd,
    NumberSub,
    NumberMul,
    NumberDiv,
    StringConcat,
}

pub trait IrtVisitor {
    fn visit_ir_tree(&mut self, tree: &IrTree) {}
    fn visit_statement(&mut self, statement: &Statement) {}
    fn visit_save_global(&mut self, save: &Save<GlobalId>) {}
    fn visit_expr(&mut self, expr: &Expr) {}
}

pub trait IrtNode {
    fn accept(&self, visitor: &mut impl IrtVisitor);
}

impl IrtNode for IrTree {
    fn accept(&self, visitor: &mut impl IrtVisitor) {
        visitor.visit_ir_tree(self);
    }
}

impl IrtNode for Statement {
    fn accept(&self, visitor: &mut impl IrtVisitor) {
        visitor.visit_statement(self);
    }
}

impl IrtNode for Save<GlobalId> {
    fn accept(&self, visitor: &mut impl IrtVisitor) {
        visitor.visit_save_global(self);
    }
}

impl IrtNode for Expr {
    fn accept(&self, visitor: &mut impl IrtVisitor) {
        visitor.visit_expr(self);
    }
}
