use crate::scanner::{Scanner, Token, TokenName::*, TokenName};
use crate::ast::*;

const MAX_ERRORS: usize = 500;

pub fn parse(src: &str) -> Result<Ast, Vec<ParseError>> {
    /*let mut scanner = Scanner::new(src);
    loop {
        let rt = scanner.get_token();
        match rt {
            Ok(t) => {
                println!("{:?}", t);
                if t.name() == EOF {
                    break;
                }
            }
            Err(e) => {
                eprintln!("{:?}", e);
            }
        }
    }*/
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
            Err(_) => unimplemented!() // TODO handle error on first token
        }

    }

    fn parse(mut self) -> Result<Ast, Vec<ParseError>> {
        let imports = self.imports();
        let decls = self.decls(true);
        let root_namespace = Namespace { name: QName { parts: Vec::new() }, public: true, decls };
        // TODO call
        let at_end = self.current.matches(EOF).is_some();
        if self.errors.is_empty() && at_end {
            Ok(Ast {
                imports,
                root_namespace
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
        let message = format!("Expected {}, got '{}'", expected, token.value());
        let error = ParseError {
            message,
            line: token.line(),
            col: token.col()
        };
        self.error(error);
        self.advance();
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
                || current.matches(EOF).is_some() {
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
                || self.current.matches_value(KEYWORD, "let")) {
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
        // TODO (), list, type patterns
        if self.advance_if_matches_value(IDENT, "_") {
            Some(Pattern::Wildcard)
        } else if let Some(name) = self.advance_if_matches(IDENT) {
            Some(Pattern::Name(name.to_string()))
        } else if let Some(l) = self.literal() {
            Some(Pattern::Constant(l))
        } else {
            self.error_at_current("pattern");
            None
        }
    }

    fn expr(&mut self) -> Option<Expr> {
        // TODO call, binary, lambda, list exprs
        if self.current.matches(IDENT).is_some() {
            self.qualified_name().map(|qn| Expr::QName(qn))
        } else if let Some(l) = self.literal() {
            Some(Expr::Constant(l))
        } else if self.current.matches_value(SYMBOL, "(") {
            self.call_expr()
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
        unimplemented!()
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
}
