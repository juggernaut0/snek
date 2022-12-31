use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

pub struct Ast {
    pub imports: Vec<Import>,
    pub root_namespace: Namespace,
    //pub expr: Option<Expr>
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
    pub type_params: Vec<String>,
    pub line: u32,
    pub col: u32,
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

pub struct Pattern {
    pub line: u32,
    pub col: u32,
    pub pattern: PatternType,
}

pub enum PatternType {
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

impl FieldPattern {
    pub fn field_name(&self) -> &String {
        match self {
            FieldPattern::Name(np) => &np.name,
            FieldPattern::Binding(_, name) => name,
        }
    }
}

#[derive(Eq, PartialEq, Hash)]
pub struct NamePattern {
    pub line: u32,
    pub col: u32,
    pub name: String,
    pub type_name: Option<TypeName>,
}

// TODO line and column on TypeName
#[derive(Eq, PartialEq, Hash)]
pub struct TypeName {
    pub line: u32,
    pub col: u32,
    pub type_name_type: TypeNameType,
}
#[derive(Eq, PartialEq, Hash)]
pub enum TypeNameType {
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
    pub type_args: Vec<TypeName>
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
    Dot,
    Match,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct QName {
    pub parts: Vec<String>
}

impl Display for QName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.parts.join("."))
    }
}

pub struct Literal {
    pub lit_type: LiteralType,
    pub value: String
}

#[derive(Debug, PartialEq, Eq)]
pub enum LiteralType {
    Number,
    String,
    Bool,
    Unit,
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Bang,
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Div,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

impl BinaryOp {
    pub fn precedence(&self) -> u32 {
        match self {
            BinaryOp::And => 1,
            BinaryOp::Or => 1,
            BinaryOp::Equal => 2,
            BinaryOp::NotEqual => 2,
            BinaryOp::LessThan => 2,
            BinaryOp::GreaterThan => 2,
            BinaryOp::LessEqual => 2,
            BinaryOp::GreaterEqual => 2,
            BinaryOp::Plus => 3,
            BinaryOp::Minus => 3,
            BinaryOp::Times => 4,
            BinaryOp::Div => 4,
        }
    }
}

pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

pub struct LambdaExpr {
    pub type_params: Vec<String>,
    pub params: Vec<Pattern>,
    pub bindings: Vec<Binding>,
    pub expr: Box<Expr>
}

pub struct FieldInit {
    pub field_name: String,
    pub expr: Expr,
}
