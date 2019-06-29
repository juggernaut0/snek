pub struct Ast {
    pub imports: Vec<Import>,
    pub root_namespace: Namespace,
    //expr: CallExpr
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

pub enum Pattern { // TODO
    Wildcard,
    Name(String),
    Constant(Literal)
}

pub enum Expr { // TODO
    QName(QName),
    Constant(Literal),
}

pub struct QName {
    pub parts: Vec<String>
}

pub struct Literal {
    pub lit_type: LiteralType,
    pub value: String
}

pub enum LiteralType {
    NUMBER,
    STRING,
    BOOL,
    UNIT
}