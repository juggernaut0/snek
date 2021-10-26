use crate::scanner::{Scanner, Token, TokenName::*, TokenName};
use crate::ast::*;
use std::rc::Rc;

const MAX_ERRORS: usize = 500;

pub fn parse(src: &str) -> (Ast, Vec<ParseError>) {
    Parser::new(Scanner::new(src)).parse()
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
    line: u32,
    col: u32,
}

impl ParseError {
    pub fn new(message: String, line: u32, col: u32) -> ParseError {
        ParseError { message, line, col }
    }

    pub fn message(&self) -> &String {
        &self.message
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn col(&self) -> u32 {
        self.col
    }
}

struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Token<'a>,
    errors: Vec<ParseError>
}

impl<'a> Parser<'a> {
    fn new(mut scanner: Scanner) -> Parser {
        match scanner.get_token() {
            Ok(t) => Parser {
                scanner,
                current: t,
                errors: Vec::new()
            },
            Err(_) => unreachable!() // first token cannot be an error
        }

    }

    fn pos(&self) -> (u32, u32) {
        (self.current.line(), self.current.col())
    }

    fn parse(mut self) -> (Ast, Vec<ParseError>) {
        let imports = self.imports();
        let mut decls = self.decls(true);
        if !self.current.is_eof() {
            let (line, col) = self.pos();
            if let Some(expr) = self.call_expr() {
                decls.push(Decl::Binding(Binding {
                    public: false,
                    pattern: Pattern { line, col, pattern: PatternType::Wildcard(None) },
                    expr
                }));
            }
        }
        if !self.current.is_eof() {
            self.error_at_current("EOF")
        }
        let root_namespace = Namespace { name: QName { parts: Vec::new() }, public: true, decls };

        let ast = Ast {
            imports,
            root_namespace,
        };
        (ast, self.errors)
    }

    fn error(&mut self, error: ParseError) {
        if self.errors.len() == MAX_ERRORS {
            eprintln!("{:?}", self.errors);
            panic!("Error limit reached")
        }
        self.errors.push(error)
    }

    fn error_at_current(&mut self, expected: &str) {
        let token = &self.current;
        let message = format!("{} expected, got '{}'", expected, if token.is_eof() { "end of file" } else { token.value() });
        let error = ParseError {
            message,
            line: token.line(),
            col: token.col()
        };
        self.error(error);
        //self.advance();
        self.synchronize();
    }

    fn synchronize(&mut self) {
        loop {
            let current = &self.current;
            if current.matches_value(Keyword, "import")
                || current.matches_value(Keyword, "public")
                || current.matches_value(Keyword, "namespace")
                || current.matches_value(Keyword, "type")
                || current.matches_value(Keyword, "let")
                || current.is_eof() {
                break;
            }
            self.advance();
        }
    }

    fn extract<'b>(&mut self, scan_result: Result<Token<'b>, ParseError>) -> Option<Token<'b>> {
        match scan_result {
            Ok(t) => Some(t),
            Err(e) => {
                self.error(e);
                None
            }
        }
    }

    fn advance(&mut self) {
        loop {
            let sr = self.scanner.get_token();
            let ot = self.extract(sr);
            if let Some(t) = ot {
                self.current = t;
                break;
            }
        }
    }

    fn advance_if_matches(&mut self, token_name: TokenName) -> Option<&'a str> {
        let value = self.current.matches(token_name);
        if value.is_some() {
            self.advance();
        }
        value
    }

    fn advance_if_matches_value(&mut self, token_name: TokenName, value: &str) -> bool {
        let b = self.current.matches_value(token_name, value);
        if b {
            self.advance();
        }
        b
    }

    fn require(&mut self, expected: &str, token_name: TokenName) -> Option<&'a str> {
        match self.current.matches(token_name) {
            Some(value) => {
                self.advance();
                Some(value)
            }
            None => {
                self.error_at_current(expected);
                None
            }
        }
    }

    fn require_value(&mut self, token_name: TokenName, value: &str) -> Option<()> {
        if self.current.matches_value(token_name, value) {
            self.advance();
            Some(())
        } else {
            self.error_at_current(value);
            None
        }
    }

    fn imports(&mut self) -> Vec<Import> {
        let mut imports = Vec::new();
        while self.current.matches_value(Keyword, "import") {
            self.advance(); // advance past "import"
            let mut names = Vec::new();
            while !self.current.matches_value(Keyword, "from") {
                let (line, col) = self.pos();
                match self.qualified_name() {
                    Some(name) => {
                        let imported_name = ImportedName {
                            name,
                            line,
                            col,
                        };
                        names.push(imported_name)
                    },
                    None => continue
                }
            }
            self.advance(); // advance past "from"
            if let Some(filename) = self.require("string literal", Str) {
                imports.push(Import { filename: trim_quotes(filename).to_string(), names });
            }
        }
        imports
    }

    fn qualified_name(&mut self) -> Option<QName> {
        let init = self.require("identifier", Ident)?;
        self.qualified_name_with_init(init)
    }

    fn qualified_name_with_init(&mut self, init: &str) -> Option<QName> {
        let mut parts = vec![init.to_string()];
        while self.current.matches_value(Symbol, ".") {
            self.advance(); // advance past "."
            let ident = self.require("identifier", Ident)?;
            parts.push(ident.to_string())
        }
        Some(QName { parts })
    }

    fn decls(&mut self, top_level: bool) -> Vec<Decl> {
        let mut decls = Vec::new();
        while self.current.matches_value(Keyword, "public")
            || self.current.matches_value(Keyword, "namespace")
            || self.current.matches_value(Keyword, "type")
            || self.current.matches_value(Keyword, "let") {
            match self.decl() {
                Some(decl) => decls.push(decl),
                None => continue
            }
            if top_level
                && !(self.current.matches_value(Keyword, "public")
                || self.current.matches_value(Keyword, "namespace")
                || self.current.matches_value(Keyword, "type")
                || self.current.matches_value(Keyword, "let")
                || self.current.matches_value(Symbol, "(")
                || self.current.is_eof()) {
                self.error_at_current("declaration or call");
            }
        }
        decls
    }

    fn decl(&mut self) -> Option<Decl> {
        let public = self.advance_if_matches_value(Keyword, "public");
        if self.current.matches_value(Keyword, "namespace") {
            self.namespace(public).map(Decl::Namespace)
        } else if self.current.matches_value(Keyword, "type") {
            self.type_decl(public).map(Decl::Type)
        } else if self.current.matches_value(Keyword, "let") {
            self.binding(public).map(Decl::Binding)
        } else {
            self.error_at_current("namespace, type, or binding");
            None
        }
    }

    fn namespace(&mut self, public: bool) -> Option<Namespace> {
        self.advance(); // advance past "namespace"
        let name = self.qualified_name()?;
        self.require_value(Symbol, "{")?;
        let decls = self.decls(false);
        self.require_value(Symbol, "}")?;
        Some(Namespace { name, public, decls })
    }

    fn type_decl(&mut self, public: bool) -> Option<Type> {
        self.advance(); // advance past "type"
        let name = self.type_name_decl()?;
        let contents = if self.advance_if_matches_value(Symbol, "=") {
            TypeContents::Union(self.type_cases()?)
        } else if self.current.matches_value(Symbol, "{") {
            TypeContents::Record(self.type_fields()?)
        } else {
            TypeContents::Record(Vec::new())
        };

        Some(Type { public, name, contents })
    }

    fn type_name_decl(&mut self) -> Option<TypeNameDecl> {
        let (line, col) = self.pos();
        let name = self.require("identifier", Ident)?;
        let type_params = if self.current.matches_value(Symbol, "<") {
            self.type_params()?
        } else {
            Vec::new()
        };
        Some(TypeNameDecl { name: name.to_string(), type_params, line, col })
    }

    fn type_params(&mut self) -> Option<Vec<String>> {
        self.require_value(Symbol, "<")?;
        let mut params = Vec::new();
        while let Some(param) = self.advance_if_matches(Ident) {
            params.push(param.to_string())
        }
        self.require_value(Symbol, ">")?;
        Some(params)
    }

    fn type_cases(&mut self) -> Option<Vec<TypeCase>> {
        let mut cases = vec![self.type_case()?];
        while self.advance_if_matches_value(Symbol, "|") {
            cases.push(self.type_case()?);
        }
        Some(cases)
    }

    fn type_case(&mut self) -> Option<TypeCase> {
        let public = self.advance_if_matches_value(Keyword, "public");

        if public {
            // For sure a record
            Some(TypeCase::Record(self.type_case_record(public)?))
        } else if self.current.matches_value(Symbol, "{") || self.current.matches_value(Symbol, "(") {
            // Either func_type or unit, let type_name sort it out
            Some(TypeCase::Case(self.type_name()?))
        } else if let Some(first_ident) = self.current.matches(Ident) {
            let (line, col) = self.pos();
            self.advance();
            // Check if record or case based on next token
            if self.current.matches_value(Symbol, "{") {
                // For sure a record
                Some(TypeCase::Record(self.type_case_record_with_init(public, first_ident, line, col)?))
            } else {
                let type_name = TypeName {
                    line, col,
                    type_name_type: TypeNameType::Named(self.named_type_with_init(first_ident)?)
                };
                Some(TypeCase::Case(type_name))
            }
        } else {
            self.error_at_current("type declaration or type");
            None
        }
    }

    fn type_case_record(&mut self, public: bool) -> Option<TypeCaseRecord> {
        let (line, col) = self.pos();
        let init = self.require("identifier", Ident)?;
        self.type_case_record_with_init(public, init, line, col)
    }

    fn type_case_record_with_init(&mut self, public: bool, init: &str, line: u32, col: u32) -> Option<TypeCaseRecord> {
        let name = TypeNameDecl { name: init.to_string(), type_params: Vec::new(), line, col };
        let fields = self.type_fields()?;
        Some(TypeCaseRecord { public, name, fields })
    }

    fn type_fields(&mut self) -> Option<Vec<TypeField>> {
        let mut fields = Vec::new();
        self.require_value(Symbol, "{")?;
        while !self.current.matches_value(Symbol, "}") {
            let public = self.advance_if_matches_value(Keyword, "public");
            let name = self.require("identifier", Ident)?.to_string();
            self.require_value(Symbol, ":")?;
            let type_name = self.type_name()?;
            fields.push(TypeField { public, name, type_name });

            if self.current.matches_value(Symbol, "}") {
                break;
            } else {
                self.require_value(Symbol, ",");
            }
        }
        self.require_value(Symbol, "}")?;

        Some(fields)
    }

    fn binding(&mut self, public: bool) -> Option<Binding> {
        self.advance(); // advance past "let"
        let pattern = self.pattern()?;
        self.require_value(Symbol, "=")?;
        let expr = self.expr()?;
        Some(Binding { public, pattern, expr })
    }

    fn pattern(&mut self) -> Option<Pattern> {
        let (line, col) = self.pos();
        let pattern = if self.advance_if_matches_value(Ident, "_") {
            let type_name = self.opt_type_annotation()?;
            Some(PatternType::Wildcard(type_name))
        } else if let Some(name) = self.current.matches(Ident) {
            Some(PatternType::Name(self.name_pattern(name)?))
        } else if let Some(l) = self.literal() {
            Some(PatternType::Constant(l))
        } else if self.advance_if_matches_value(Symbol, "(") {
            self.require_value(Symbol, ")")?;
            Some(PatternType::Constant(Literal { lit_type: LiteralType::Unit, value: String::new() }))
        } else if self.advance_if_matches_value(Symbol, "[") {
            let mut patterns = Vec::new();
            while !self.current.matches_value(Symbol, "]") {
                let pattern = self.pattern()?;
                patterns.push(pattern);
            }
            self.advance(); // advance past "]"
            Some(PatternType::List(patterns))
        } else if self.advance_if_matches_value(Symbol, "{") {
            let mut fields = vec![self.field_pattern()?];
            if !self.current.matches_value(Symbol, "}") {
                self.require_value(Symbol, ",");
            }
            while !self.current.matches_value(Symbol, "}") {
                fields.push(self.field_pattern()?);
                if self.current.matches_value(Symbol, "}") {
                    break;
                } else {
                    self.require_value(Symbol, ",");
                }
            }
            self.require_value(Symbol, "}")?;
            let type_name = self.opt_type_annotation()?;
            Some(PatternType::Destruct(fields, type_name))
        } else {
            self.error_at_current("pattern");
            None
        };
        pattern.map(|pattern| { Pattern { line, col, pattern} })
    }

    fn name_pattern(&mut self, name: &str) -> Option<Rc<NamePattern>> {
        let (line, col) = self.pos();
        self.advance(); // advance past ident
        let type_name = self.opt_type_annotation()?;
        Some(Rc::new(NamePattern { line, col, name: name.to_string(), type_name }))
    }

    fn opt_type_annotation(&mut self) -> Option<Option<TypeName>> {
        let ta = if self.advance_if_matches_value(Symbol, ":") {
            Some(self.type_name()?)
        } else {
            None
        };
        Some(ta)
    }

    fn field_pattern(&mut self) -> Option<FieldPattern> {
        if self.advance_if_matches_value(Keyword, "let") {
            let pattern = self.pattern()?;
            self.require_value(Symbol, "=")?;
            let field = self.require("identified", Ident)?.to_string();
            Some(FieldPattern::Binding(pattern, field))
        } else if let Some(name) = self.current.matches(Ident) {
            Some(FieldPattern::Name(self.name_pattern(name)?))
        } else {
            self.error_at_current("identifier");
            None
        }
    }

    fn type_name(&mut self) -> Option<TypeName> {
        let (line, col) = self.pos();
        let type_name_type = if let Some(init) = self.advance_if_matches(Ident) {
            if init == "_" {
                TypeNameType::Inferred
            } else {
                let named = self.named_type_with_init(init)?;
                TypeNameType::Named(named)
            }
        } else if self.advance_if_matches_value(Symbol, "{") {
            let type_params = if self.current.matches_value(Symbol, "<") {
                self.type_params()?
            } else {
                Vec::new()
            };
            let mut params = Vec::new();
            while !self.current.matches_value(Symbol, "->") {
                params.push(self.type_name()?);
            }
            self.advance(); // advance past ->
            let ret = self.type_name()?;
            self.require_value(Symbol, "}")?;
            TypeNameType::Func(FuncType { type_params, params, return_type: Box::new(ret) })
        } else if self.advance_if_matches_value(Symbol, "*") {
            TypeNameType::Any
        } else if self.advance_if_matches_value(Symbol, "(") {
            self.require_value(Symbol, ")")?;
            TypeNameType::Unit
        } else if self.advance_if_matches_value(Symbol, "!") {
            TypeNameType::Nothing
        } else {
            self.error_at_current("type identifier");
            return None
        };
        Some(TypeName { line, col, type_name_type })
    }

    fn named_type(&mut self) -> Option<NamedType> {
        let init = self.require("identifier", Ident)?;
        self.named_type_with_init(init)
    }

    fn named_type_with_init(&mut self, init: &str) -> Option<NamedType> {
        let name = self.qualified_name_with_init(init)?;
        let mut params = Vec::new();
        if self.advance_if_matches_value(Symbol, "<") {
            params.push(self.type_name()?);
            while !self.current.matches_value(Symbol, ">") {
                params.push(self.type_name()?);
            }
            self.advance(); // advance past >
        }
        Some(NamedType { name, type_args: params })
    }

    fn expr(&mut self) -> Option<Expr> {
        let (line, col) = self.pos();
        if self.current.matches(Ident).is_some() {
            self.qualified_name().map(|qn| Expr { line, col, expr_type: ExprType::QName(qn) })
        } else if let Some(l) = self.literal() {
            Some(Expr { line, col, expr_type: ExprType::Constant(l) })
        } else if self.current.matches_value(Symbol, "(") {
            self.call_expr()
        } else if self.current.matches_value(Symbol, "{") {
            self.lambda_expr()
        } else if self.advance_if_matches_value(Symbol, "[") {
            let mut exprs = Vec::new();
            while !self.current.matches_value(Symbol, "]") {
                let expr = self.expr()?;
                exprs.push(expr);
            }
            self.advance(); // advance past "]"
            Some(Expr { line, col, expr_type: ExprType::List(exprs) })
        } else if self.advance_if_matches_value(Keyword, "new") {
            let type_name = if !self.current.matches_value(Symbol, "{") {
                Some(self.named_type()?)
            } else {
                None
            };
            self.require_value(Symbol, "{")?;
            let mut inits = Vec::new();
            while !self.current.matches_value(Symbol, "}") {
                inits.push(self.field_init()?);
                if self.current.matches_value(Symbol, "}") {
                    break;
                } else {
                    self.require_value(Symbol, ",");
                }
            }
            self.require_value(Symbol, "}")?;
            Some(Expr { line, col, expr_type: ExprType::New(type_name, inits) })
        } else {
            self.error_at_current("expression");
            None
        }
    }

    fn call_expr(&mut self) -> Option<Expr> {
        let (line, col) = self.pos();
        self.advance(); // advance past "("
        if self.advance_if_matches_value(Symbol, ")") {
            let expr_type = ExprType::Constant(Literal { lit_type: LiteralType::Unit, value: String::new() });
            return Some(Expr { line, col, expr_type })
        }
        let first = if self.advance_if_matches_value(Symbol, ".") {
            let expr_type = ExprType::Dot;
            let (line, col) = self.pos();
            Expr { line, col, expr_type }
        } else if self.advance_if_matches_value(Keyword, "match") {
            let expr_type = ExprType::Match;
            let (line, col) = self.pos();
            Expr { line, col, expr_type }
        } else {
            self.unary()?
        };
        match first.expr_type {
            ExprType::Unary(_, _) => self.finish_binary(first),
            _ if self.peek_binary_op().is_some() => self.finish_binary(first),
            _ => {
                let mut args = Vec::new();
                while !self.current.matches_value(Symbol, ")") {
                    let expr = self.expr()?;
                    args.push(expr);
                }
                self.require_value(Symbol, ")")?;
                let expr_type = ExprType::Call(CallExpr { callee: Box::new(first), args });
                Some(Expr { line, col, expr_type })
            }
        }
    }

    fn finish_binary(&mut self, first: Expr) -> Option<Expr> {
        let (line, col) = (first.line, first.col);
        let mut res = first;
        while !self.advance_if_matches_value(Symbol, ")") {
            let op = self.binary_op()?;
            let second = self.binary(op.precedence())?;
            let expr_type = ExprType::Binary(op, Box::new(res), Box::new(second));
            res = Expr { line, col, expr_type };
        }
        Some(res)
    }

    fn binary(&mut self, prec: u32) -> Option<Expr> {
        let (line, col) = self.pos();
        let first = self.unary()?;
        let mut res = first;
        while let Some(op) = self.peek_binary_op().filter(|o| o.precedence() > prec) {
            self.advance(); // advance past binary_op
            let second = self.binary(op.precedence())?;
            let expr_type = ExprType::Binary(op, Box::new(res), Box::new(second));
            res = Expr { line, col, expr_type }
        }
        Some(res)
    }

    fn unary(&mut self) -> Option<Expr> {
        let (line, col) = self.pos();
        if let Some(op) = self.unary_op() {
            let expr = self.expr()?;
            let expr_type = ExprType::Unary(op, Box::new(expr));
            Some(Expr { line, col, expr_type })
        } else {
            self.expr()
        }
    }

    fn unary_op(&mut self) -> Option<UnaryOp> {
        if self.advance_if_matches_value(Symbol, "+") {
            Some(UnaryOp::Plus)
        } else if self.advance_if_matches_value(Symbol, "-") {
            Some(UnaryOp::Minus)
        } else if self.advance_if_matches_value(Symbol, "!") {
            Some(UnaryOp::Bang)
        } else {
            None
        }
    }

    fn peek_binary_op(&self) -> Option<BinaryOp> {
        if self.current.matches_value(Symbol, "+") {
            Some(BinaryOp::Plus)
        } else if self.current.matches_value(Symbol, "-") {
            Some(BinaryOp::Minus)
        } else if self.current.matches_value(Symbol, "*") {
            Some(BinaryOp::Times)
        } else if self.current.matches_value(Symbol, "/") {
            Some(BinaryOp::Div)
        } else if self.current.matches_value(Symbol, "&&") {
            Some(BinaryOp::And)
        } else if self.current.matches_value(Symbol, "||") {
            Some(BinaryOp::Or)
        } else if self.current.matches_value(Symbol, "<") {
            Some(BinaryOp::LessThan)
        } else if self.current.matches_value(Symbol, ">") {
            Some(BinaryOp::GreaterThan)
        } else if self.current.matches_value(Symbol, "<=") {
            Some(BinaryOp::LessEqual)
        } else if self.current.matches_value(Symbol, ">=") {
            Some(BinaryOp::GreaterEqual)
        } else if self.current.matches_value(Symbol, "==") {
            Some(BinaryOp::Equal)
        } else if self.current.matches_value(Symbol, "!=") {
            Some(BinaryOp::NotEqual)
        } else {
            None
        }
    }

    fn binary_op(&mut self) -> Option<BinaryOp> {
        let op = self.peek_binary_op();
        if op.is_some() {
            self.advance();
        } else {
            self.error_at_current("binary operator");
        }
        op
    }

    fn literal(&mut self) -> Option<Literal> {
        if let Some(v) = self.advance_if_matches(Str) {
            Some(Literal { lit_type: LiteralType::String, value: v.to_string() })
        } else if let Some(v) = self.advance_if_matches(Number) {
            Some(Literal { lit_type: LiteralType::Number, value: v.to_string() })
        } else if self.advance_if_matches_value(Keyword, "true") {
            Some(Literal { lit_type: LiteralType::Bool, value: "true".to_string() })
        } else if self.advance_if_matches_value(Keyword, "false") {
            Some(Literal { lit_type: LiteralType::Bool, value: "false".to_string() })
        } else {
            None
        }
    }

    fn lambda_expr(&mut self) -> Option<Expr> {
        let (line, col) = self.pos();
        self.advance(); // advance past "{"
        let mut params = Vec::new();
        while !self.current.matches_value(Symbol, "->") {
            let param = self.pattern()?;
            params.push(param);
        }
        self.advance(); // advance past "->"
        let mut bindings = Vec::new();
        loop {
            {
                let (line, col) = self.pos();
                if self.advance_if_matches_value(Symbol, "public") {
                    self.error(ParseError {
                        message: "A local binding may not be public".to_string(),
                        line,
                        col,
                    })
                }
            }
            let binding = if self.current.matches_value(Keyword, "let") {
                self.binding(false)?
            } else {
                let expr = self.expr()?;
                if self.advance_if_matches_value(Symbol, "}") {
                    let expr_type = ExprType::Lambda(LambdaExpr { params, bindings, expr: Box:: new(expr) });
                    return Some(Expr { line, col, expr_type })
                }
                let pattern = Pattern { line: expr.line, col: expr.col, pattern: PatternType::Wildcard(None) };
                Binding { public: false, pattern, expr }
            };
            bindings.push(binding);
        }
    }

    fn field_init(&mut self) -> Option<FieldInit> {
        let field_name = self.require("field name", Ident)?.to_string();
        self.require_value(Symbol, ":")?;
        let expr = self.expr()?;
        Some(FieldInit { field_name, expr })
    }
}

fn trim_quotes(str_lit: &str) -> &str {
    str_lit.trim_matches(str_lit.chars().next().unwrap())
}
