use std::rc::Rc;

pub struct Ast {
    pub imports: Vec<Import>,
    pub root_namespace: Namespace,
    pub expr: Option<Expr>
}

pub struct Import {
    pub filename: String,
    pub names: Vec<ImportedName>,
}

pub struct ImportedName {
    pub name: QName,
    pub line: u32,
    pub col: u32,
}

pub enum Decl {
    Namespace(Namespace),
    Type(Type),
    Binding(Binding)
}

impl Decl {
    pub fn is_public(&self) -> bool {
        match self {
            Decl::Namespace(ns) => ns.public,
            Decl::Type(t) => t.public,
            Decl::Binding(b) => b.public,
        }
    }
}

pub struct Namespace {
    pub name: QName,
    pub public: bool,
    pub decls: Vec<Decl>
}

pub struct Type {
    pub name: TypeNameDecl,
    pub public: bool,
    pub contents: TypeContents
}

pub struct TypeNameDecl {
    pub name: String,
    pub type_params: Vec<String>
}

pub enum TypeContents {
    Record(Vec<TypeField>),
    Union(Vec<TypeCase>)
}

pub struct TypeField {
    pub name: String,
    pub public: bool,
    pub type_name: TypeName
}

pub enum TypeCase {
    Case(TypeName),
    Record(TypeCaseRecord)
}

pub struct TypeCaseRecord {
    pub name: TypeNameDecl,
    pub public: bool,
    pub fields: Vec<TypeField>
}

pub struct Binding {
    pub public: bool,
    pub pattern: Pattern,
    pub expr: Expr
}

pub enum Pattern {
    Wildcard(Option<TypeName>),
    Name(Rc<NamePattern>),
    Constant(Literal),
    Destruct(Vec<FieldPattern>, Option<TypeName>),
    List(Vec<Pattern>)
}

pub enum FieldPattern {
    Name(Rc<NamePattern>),
    Binding(Pattern, String)
}

#[derive(Eq, PartialEq, Hash)]
pub struct NamePattern {
    pub line: u32,
    pub col: u32,
    pub name: String,
    pub type_name: Option<TypeName>,
}

#[derive(Eq, PartialEq, Hash)]
pub enum TypeName {
    Named(NamedType),
    Func(FuncType),
    Unit,
    Any,
    Nothing,
    Inferred,
}

#[derive(Eq, PartialEq, Hash)]
pub struct NamedType {
    pub name: QName,
    pub params: Vec<TypeName>
}

#[derive(Eq, PartialEq, Hash)]
pub struct FuncType {
    pub type_params: Vec<String>,
    pub params: Vec<TypeName>,
    pub return_type: Box<TypeName>,
}

pub struct Expr {
    pub line: u32,
    pub col: u32,
    pub expr_type: ExprType
}

pub enum ExprType {
    QName(QName),
    Constant(Literal),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(CallExpr),
    Lambda(LambdaExpr),
    List(Vec<Expr>),
    New(Option<NamedType>, Vec<FieldInit>),
    Dot
}

#[derive(Eq, PartialEq, Hash)]
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
            BinaryOp::AND => 1,
            BinaryOp::OR => 1,
            BinaryOp::EQ => 2,
            BinaryOp::NEQ => 2,
            BinaryOp::LT => 2,
            BinaryOp::GT => 2,
            BinaryOp::LEQ => 2,
            BinaryOp::GEQ => 2,
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

pub struct FieldInit {
    pub field_name: String,
    pub expr: Expr,
}
