use crate::scanner::{Scanner, Token, TokenName::*, TokenName};
use crate::ast::*;

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
        let name = self.require("identifier", IDENT)?;
        let mut cases = Vec::new();
        if self.advance_if_matches_value(SYMBOL, "=") {
            let first = self.type_case()?;
            cases.push(first);
            while self.advance_if_matches_value(SYMBOL, "|") {
                let case = self.type_case()?;
                cases.push(case);
            }
        } else {
            let mut num_params = 0;
            while let Some(_) = self.advance_if_matches(IDENT) {
                num_params += 1;
            }
            cases.push(TypeCase { name: name.to_string(), num_params })
        }
        let namespace = if self.advance_if_matches_value(SYMBOL, "{") {
            let decls = self.decls(false);
            self.require_value(SYMBOL, "}")?;
            Some(Namespace { name: QName { parts: vec!(name.to_string()) }, public, decls })
        } else {
            None
        };
        Some(Type { name: name.to_string(), public, cases, namespace })
    }

    fn type_case(&mut self) -> Option<TypeCase> {
        let name = self.require("identifier", IDENT)?.to_string();
        let mut num_params = 0;
        while let Some(_) = self.advance_if_matches(IDENT) {
            num_params += 1;
        }
        Some(TypeCase { name, num_params })
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
        } else if let Some(name) = self.advance_if_matches(IDENT) {
            Some(Pattern::Name(name.to_string()))
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

    fn expr(&mut self) -> Option<Expr> {
        if self.current.matches(IDENT).is_some() {
            self.qualified_name().map(|qn| Expr::QName(qn))
        } else if let Some(l) = self.literal() {
            Some(Expr::Constant(l))
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
            Some(Expr::List(exprs))
        } else {
            self.error_at_current("expression");
            None
        }
    }

    fn call_expr(&mut self) -> Option<Expr> {
        self.advance(); // advance past "("
        if self.current.matches_value(SYMBOL, ")") {
            return Some(Expr::Constant(Literal { lit_type: LiteralType::UNIT, value: "".to_string() }))
        }
        let first = self.unary()?;
        match first {
            Expr::Unary(_, _) => self.finish_binary(first),
            _ if self.peek_binary_op().is_some() => self.finish_binary(first),
            _ => {
                let mut args = Vec::new();
                while !self.current.matches_value(SYMBOL, ")") {
                    let expr = self.expr()?;
                    args.push(expr);
                }
                self.require_value(SYMBOL, ")")?;
                Some(Expr::Call(CallExpr { callee: Box::new(first), args }))
            }
        }
    }

    fn finish_binary(&mut self, first: Expr) -> Option<Expr> {
        let mut res = first;
        while !self.advance_if_matches_value(SYMBOL, ")") {
            let op = self.binary_op()?;
            let second = self.binary(op.precedence())?;
            res = Expr::Binary(op, Box::new(res), Box::new(second));
        }
        return Some(res);
    }

    fn binary(&mut self, prec: u32) -> Option<Expr> {
        let first = self.unary()?;
        if let Some(op) = self.peek_binary_op().filter(|o| o.precedence() > prec) {
            self.advance(); // advance past binary_op
            let second = self.binary(op.precedence())?;
            Some(Expr::Binary(op, Box::new(first), Box::new(second)))
        } else {
            Some(first)
        }
    }

    fn unary(&mut self) -> Option<Expr> {
        if let Some(op) = self.unary_op() {
            let expr = self.expr()?;
            Some(Expr::Unary(op, Box::new(expr)))
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
                if self.current.matches_value(SYMBOL, "}") {
                    self.advance();
                    return Some(Expr::Lambda(LambdaExpr { params, bindings, expr: Box:: new(expr) }))
                }
                Binding { public: false, pattern: Pattern::Wildcard, expr }
            };
            bindings.push(binding);
        }
    }
}
