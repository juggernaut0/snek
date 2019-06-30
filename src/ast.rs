pub struct Ast {
    pub imports: Vec<Import>,
    pub root_namespace: Namespace,
    pub expr: Option<Expr>
}

pub struct Import {
    pub filename: String,
    pub names: Vec<QName>
}

pub enum Decl {
    Namespace(Namespace),
    Type(Type),
    Binding(Binding)
}

pub struct Namespace {
    pub name: QName,
    pub public: bool,
    pub decls: Vec<Decl>
}

pub struct Type {
    pub name: String,
    pub public: bool,
    pub cases: Vec<TypeCase>,
    pub namespace: Option<Namespace>
}

pub struct TypeCase {
    pub name: String,
    pub num_params: u32
}

pub struct Binding {
    pub public: bool,
    pub pattern: Pattern,
    pub expr: Expr
}

pub enum Pattern {
    Wildcard,
    Name(String),
    Constant(Literal),
    Type(QName, Vec<Pattern>),
    List(Vec<Pattern>)
}

pub enum Expr {
    QName(QName),
    Constant(Literal),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(CallExpr),
    Lambda(LambdaExpr),
    List(Vec<Expr>)
}

pub struct QName {
    pub parts: Vec<String>
}

pub struct Literal {
    pub lit_type: LiteralType,
    pub value: String
}

#[derive(Debug)]
pub enum LiteralType {
    NUMBER,
    STRING,
    BOOL,
    UNIT
}

#[derive(Debug)]
pub enum UnaryOp {
    PLUS,
    MINUS,
    BANG
}

#[derive(Debug)]
pub enum BinaryOp {
    PLUS,
    MINUS,
    TIMES,
    DIV,
    LT,
    GT,
    LEQ,
    GEQ,
    EQ,
    NEQ,
    AND,
    OR
}

impl BinaryOp {
    pub fn precedence(&self) -> u32 {
        match self {
            BinaryOp::EQ => 1,
            BinaryOp::NEQ => 1,
            BinaryOp::LT => 1,
            BinaryOp::GT => 1,
            BinaryOp::LEQ => 1,
            BinaryOp::GEQ => 1,
            BinaryOp::AND => 2,
            BinaryOp::OR => 2,
            BinaryOp::PLUS => 3,
            BinaryOp::MINUS => 3,
            BinaryOp::TIMES => 4,
            BinaryOp::DIV => 4,
        }
    }
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

pub struct LambdaExpr {
    pub params: Vec<Pattern>,
    pub bindings: Vec<Binding>,
    pub expr: Box<Expr>
}
