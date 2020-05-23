use crate::scanner::{Scanner, Token, TokenName::*, TokenName};
use crate::ast::*;
use std::rc::Rc;

const MAX_ERRORS: usize = 500;

pub fn parse(src: &str) -> Result<Ast, Vec<ParseError>> {
    Parser::new(Scanner::new(src)).parse()
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
    line: u32,
    col: u32
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

    fn parse(mut self) -> Result<Ast, Vec<ParseError>> {
        let imports = self.imports();
        let decls = self.decls(true);
        let root_namespace = Namespace { name: QName { parts: Vec::new() }, public: true, decls };
        let expr = if !self.current.is_eof() {
            self.call_expr()
        } else {
            None
        };
        let at_end = self.current.is_eof();
        if self.errors.is_empty() && at_end {
            Ok(Ast {
                imports,
                root_namespace,
                expr
            })
        } else {
            if !at_end {
                self.error_at_current("EOF")
            }
            Err(self.errors)
        }
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
            if current.matches_value(KEYWORD, "import")
                || current.matches_value(KEYWORD, "public")
                || current.matches_value(KEYWORD, "namespace")
                || current.matches_value(KEYWORD, "type")
                || current.matches_value(KEYWORD, "let")
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
        while self.current.matches_value(KEYWORD, "import") {
            self.advance(); // advance past "import"
            let mut names = Vec::new();
            while !self.current.matches_value(KEYWORD, "from") {
                match self.qualified_name() {
                    Some(qname) => names.push(qname),
                    None => continue
                }
            }
            self.advance(); // advance past "from"
            if let Some(filename) = self.require("string literal", STRING) {
                imports.push(Import { filename: filename.to_string(), names });
            }
        }
        imports
    }

    fn qualified_name(&mut self) -> Option<QName> {
        let mut parts = Vec::new();
        let ident = self.require("identifier", IDENT)?;
        parts.push(ident.to_string());
        while self.current.matches_value(SYMBOL, ".") {
            self.advance(); // advance past "."
            let ident = self.require("identifier", IDENT)?;
            parts.push(ident.to_string())
        }
        Some(QName { parts })
    }

    fn decls(&mut self, top_level: bool) -> Vec<Decl> {
        let mut decls = Vec::new();
        while self.current.matches_value(KEYWORD, "public")
            || self.current.matches_value(KEYWORD, "namespace")
            || self.current.matches_value(KEYWORD, "type")
            || self.current.matches_value(KEYWORD, "let") {
            match self.decl() {
                Some(decl) => decls.push(decl),
                None => continue
            }
            if top_level
                && !(self.current.matches_value(KEYWORD, "public")
                || self.current.matches_value(KEYWORD, "namespace")
                || self.current.matches_value(KEYWORD, "type")
                || self.current.matches_value(KEYWORD, "let")
                || self.current.matches_value(SYMBOL, "(")
                || self.current.is_eof()) {
                self.error_at_current("declaration or call");
            }
        }
        decls
    }

    fn decl(&mut self) -> Option<Decl> {
        let public = self.advance_if_matches_value(KEYWORD, "public");
        if self.current.matches_value(KEYWORD, "namespace") {
            self.namespace(public).map(|ns| Decl::Namespace(ns))
        } else if self.current.matches_value(KEYWORD, "type") {
            self.type_decl(public).map(|t| Decl::Type(t))
        } else if self.current.matches_value(KEYWORD, "let") {
            self.binding(public).map(|b| Decl::Binding(b))
        } else {
            self.error_at_current("namespace, type, or binding");
            None
        }
    }

    fn namespace(&mut self, public: bool) -> Option<Namespace> {
        self.advance(); // advance past "namespace"
        let name = self.qualified_name()?;
        self.require_value(SYMBOL, "{")?;
        let decls = self.decls(false);
        self.require_value(SYMBOL, "}")?;
        Some(Namespace { name, public, decls })
    }

    fn type_decl(&mut self, public: bool) -> Option<Type> {
        self.advance(); // advance past "type"
        let name = self.type_name_decl()?;
        let contents = if self.advance_if_matches_value(SYMBOL, "=") {
            TypeContents::Union(self.type_cases()?)
        } else if self.current.matches_value(SYMBOL, "{") {
            TypeContents::Record(self.type_fields()?)
        } else {
            TypeContents::Record(Vec::new())
        };

        Some(Type { public, name, contents })
    }

    fn type_name_decl(&mut self) -> Option<TypeNameDecl> {
        let init = self.require("identifier", IDENT)?;
        self.type_name_decl_with_init(init)
    }

    fn type_name_decl_with_init(&mut self, init: &str) -> Option<TypeNameDecl> {
        let mut params = Vec::new();
        if self.advance_if_matches_value(SYMBOL, "<") {
            while let Some(param) = self.advance_if_matches(IDENT) {
                params.push(param.to_string())
            }
            self.require_value(SYMBOL, ">")
        }

        Some(TypeNameDecl { name: init.to_string(), params })
    }

    fn type_cases(&mut self) -> Option<Vec<TypeCase>> {
        let mut cases = Vec::new();
        while !self.advance_if_matches_value(SYMBOL, "|") {
            let case = self.type_case()?;
            cases.push(case);
        }
        Some(cases)
    }

    fn type_case(&mut self) -> Option<TypeCase> {
        let public = self.advance_if_matches_value(KEYWORD, "public");

        if public {
            // For sure a record
            Some(TypeCase::Record(self.type_case_record(public)?))
        } else if self.current.matches_value(SYMBOL, "{") {
            // For sure a func_type
            Some(TypeCase::Case(self.type_name()?))
        } else if let Some(first_ident) = self.advance_if_matches(IDENT) {
            // Check if record or case based on next token
            if self.current.matches_value(SYMBOL, "{") {
                // For sure a record
                Some(TypeCase::Record(self.type_case_record_with_init(public, first_ident)?))
            } else {
                Some(TypeCase::Case(self.named_type_with_init(first_ident)?))
            }
        } else {
            self.error_at_current("type declaration or case");
            None
        }
    }

    fn type_case_record(&mut self, public: bool) -> Option<TypeCaseRecord> {
        let init = self.require("identifier", IDENT)?;
        self.type_case_record_with_init(public, init)
    }

    fn type_case_record_with_init(&mut self, public: bool, init: &str) -> Option<TypeCaseRecord> {
        let name = self.type_name_decl_with_init(init)?;
        let fields = self.type_fields()?;
        Some(TypeCaseRecord { public, name, fields })
    }
    
    fn type_fields(&mut self) -> Option<Vec<TypeField>> {
        let mut fields = Vec::new();
        self.require_value(SYMBOL, "{")?;
        while !self.current.matches_value(SYMBOL, "}") {
            let public = self.advance_if_matches_value(KEYWORD, "public");
            let name = self.require("identifier", IDENT)?.to_string();
            self.require_value(SYMBOL, ":")?;
            let type_name = self.type_name()?;
            fields.push(TypeField { public, name, type_name })
        }
        self.require_value(SYMBOL, "}")?;

        Some(fields)
    }

    fn binding(&mut self, public: bool) -> Option<Binding> {
        self.advance(); // advance past "let"
        let pattern = self.pattern()?;
        self.require_value(SYMBOL, "=")?;
        let expr = self.expr()?;
        Some(Binding { public, pattern, expr })
    }

    fn pattern(&mut self) -> Option<Pattern> {
        if self.advance_if_matches_value(IDENT, "_") {
            Some(Pattern::Wildcard)
        } else if let Some(name) = self.current.matches(IDENT) {
            let (line, col) = self.pos();
            self.advance();
            Some(Pattern::Name(Rc::new(NamePattern { line, col, name: name.to_string() })))
        } else if let Some(l) = self.literal() {
            Some(Pattern::Constant(l))
        } else if self.current.matches_value(SYMBOL, "(") {
            self.type_pattern()
        } else if self.advance_if_matches_value(SYMBOL, "[") {
            let mut patterns = Vec::new();
            while !self.current.matches_value(SYMBOL, "]") {
                let pattern = self.pattern()?;
                patterns.push(pattern);
            }
            self.advance(); // advance past "]"
            Some(Pattern::List(patterns))
        } else {
            self.error_at_current("pattern");
            None
        }
    }

    fn type_pattern(&mut self) -> Option<Pattern> {
        self.advance(); // advance past "("
        if self.advance_if_matches_value(SYMBOL, ")") {
            return Some(Pattern::Constant(Literal { lit_type: LiteralType::UNIT, value: "".to_string() }))
        }
        let qname = self.qualified_name()?;
        let mut patterns = Vec::new();
        while !self.current.matches_value(SYMBOL, ")") {
            let pattern = self.pattern()?;
            patterns.push(pattern);
        }
        self.advance(); // advance past ")"
        Some(Pattern::Type(qname, patterns))
    }

    fn type_name(&mut self) -> Option<TypeName> {
        todo!("type_name")
    }

    fn named_type_with_init(&mut self, init: &str) -> Option<TypeName> {
        todo!("named_type")
    }

    fn expr(&mut self) -> Option<Expr> {
        let (line, col) = self.pos();
        if self.current.matches(IDENT).is_some() {
            self.qualified_name().map(|qn| Expr { line, col, expr_type: ExprType::QName(qn) })
        } else if let Some(l) = self.literal() {
            Some(Expr { line, col, expr_type: ExprType::Constant(l) })
        } else if self.current.matches_value(SYMBOL, "(") {
            self.call_expr()
        } else if self.current.matches_value(SYMBOL, "{") {
            self.lambda_expr()
        } else if self.advance_if_matches_value(SYMBOL, "[") {
            let mut exprs = Vec::new();
            while !self.current.matches_value(SYMBOL, "]") {
                let expr = self.expr()?;
                exprs.push(expr);
            }
            self.advance(); // advance past "]"
            Some(Expr { line, col, expr_type: ExprType::List(exprs) })
        } else {
            self.error_at_current("expression");
            None
        }
    }

    fn call_expr(&mut self) -> Option<Expr> {
        let (line, col) = self.pos();
        self.advance(); // advance past "("
        if self.advance_if_matches_value(SYMBOL, ")") {
            let expr_type = ExprType::Constant(Literal { lit_type: LiteralType::UNIT, value: "".to_string() });
            return Some(Expr { line, col, expr_type })
        }
        let first = if self.advance_if_matches_value(SYMBOL, ".") {
            let expr_type = ExprType::Dot;
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
                while !self.current.matches_value(SYMBOL, ")") {
                    let expr = self.expr()?;
                    args.push(expr);
                }
                self.require_value(SYMBOL, ")")?;
                let expr_type = ExprType::Call(CallExpr { callee: Box::new(first), args });
                Some(Expr { line, col, expr_type })
            }
        }
    }

    fn finish_binary(&mut self, first: Expr) -> Option<Expr> {
        let (line, col) = (first.line, first.col);
        let mut res = first;
        while !self.advance_if_matches_value(SYMBOL, ")") {
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
        if self.advance_if_matches_value(SYMBOL, "+") {
            Some(UnaryOp::PLUS)
        } else if self.advance_if_matches_value(SYMBOL, "-") {
            Some(UnaryOp::MINUS)
        } else if self.advance_if_matches_value(SYMBOL, "!") {
            Some(UnaryOp::BANG)
        } else {
            None
        }
    }

    fn peek_binary_op(&self) -> Option<BinaryOp> {
        if self.current.matches_value(SYMBOL, "+") {
            Some(BinaryOp::PLUS)
        } else if self.current.matches_value(SYMBOL, "-") {
            Some(BinaryOp::MINUS)
        } else if self.current.matches_value(SYMBOL, "*") {
            Some(BinaryOp::TIMES)
        } else if self.current.matches_value(SYMBOL, "/") {
            Some(BinaryOp::DIV)
        } else if self.current.matches_value(SYMBOL, "&&") {
            Some(BinaryOp::AND)
        } else if self.current.matches_value(SYMBOL, "||") {
            Some(BinaryOp::OR)
        } else if self.current.matches_value(SYMBOL, "<") {
            Some(BinaryOp::LT)
        } else if self.current.matches_value(SYMBOL, ">") {
            Some(BinaryOp::GT)
        } else if self.current.matches_value(SYMBOL, "<=") {
            Some(BinaryOp::LEQ)
        } else if self.current.matches_value(SYMBOL, ">=") {
            Some(BinaryOp::GEQ)
        } else if self.current.matches_value(SYMBOL, "==") {
            Some(BinaryOp::EQ)
        } else if self.current.matches_value(SYMBOL, "!=") {
            Some(BinaryOp::NEQ)
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
        if let Some(v) = self.advance_if_matches(STRING) {
            Some(Literal { lit_type: LiteralType::STRING, value: v.to_string() })
        } else if let Some(v) = self.advance_if_matches(NUMBER) {
            Some(Literal { lit_type: LiteralType::NUMBER, value: v.to_string() })
        } else if self.advance_if_matches_value(KEYWORD, "true") {
            Some(Literal { lit_type: LiteralType::BOOL, value: "true".to_string() })
        } else if self.advance_if_matches_value(KEYWORD, "false") {
            Some(Literal { lit_type: LiteralType::BOOL, value: "false".to_string() })
        } else {
            None
        }
    }

    fn lambda_expr(&mut self) -> Option<Expr> {
        let (line, col) = self.pos();
        self.advance(); // advance past "{"
        let mut params = Vec::new();
        while !self.current.matches_value(SYMBOL, "->") {
            let param = self.pattern()?;
            params.push(param);
        }
        self.advance(); // advance past "->"
        let mut bindings = Vec::new();
        loop {
            let binding = if self.current.matches_value(KEYWORD, "let") {
                self.binding(false)?
            } else {
                let expr = self.expr()?;
                if self.advance_if_matches_value(SYMBOL, "}") {
                    let expr_type = ExprType::Lambda(LambdaExpr { params, bindings, expr: Box:: new(expr) });
                    return Some(Expr { line, col, expr_type })
                }
                Binding { public: false, pattern: Pattern::Wildcard, expr }
            };
            bindings.push(binding);
        }
    }
}
