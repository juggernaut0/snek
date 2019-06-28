use crate::scanner::{Scanner, Token, TokenName::*, TokenName};

const MAX_ERRORS: usize = 500;

pub fn parse(src: &str) -> Result<Ast, Vec<ParseError>> {
    let mut scanner = Scanner::new(src);
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
    }
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

pub struct Ast {
    imports: Vec<Import>,
    root_namespace: Namespace,
    //expr: CallExpr
}

pub struct Import {
    filename: String,
    names: Vec<QName>
}

pub struct Namespace {
    name: QName,
    public: bool,
    decls: Vec<Decl>
}

enum Decl {
    Namespace(Namespace),
    //Type(Type),
    //Binding(Binding)
}

pub struct QName {
    parts: Vec<String>
}

struct Parser<'a> {
    scanner: Scanner<'a>,
    current: Token<'a>,
    //previous: Option<Token<'a>>,
    errors: Vec<ParseError>
}

impl<'a> Parser<'a> {
    fn new(mut scanner: Scanner) -> Parser {
        match scanner.get_token() {
            Ok(t) => Parser {
                scanner,
                current: t,
                //previous: None,
                errors: Vec::new()
            },
            Err(_) => unimplemented!() // TODO handle error on first token
        }

    }

    fn parse(mut self) -> Result<Ast, Vec<ParseError>> {
        let imports = self.imports();
        let decls = self.decls();
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

    fn error_at(& mut self, token: &Token, expected: &str) {
        let message = format!("Expected {}, got {}", expected, token.value());
        let error = ParseError {
            message,
            line: token.line(),
            col: token.col()
        };
        self.error(error)
    }

    fn error_at_current(&mut self, expected: &str) {
        let token = &self.current;
        let message = format!("Expected {}, got {}", expected, token.value());
        let error = ParseError {
            message,
            line: token.line(),
            col: token.col()
        };
        self.error(error)
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

    fn require(&mut self, expected: &str, token_name: TokenName) -> Option<&'a str> {
        match self.current.matches(token_name) {
            Some(value) => {
                self.advance();
                Some(value)
            }
            None => {
                self.error_at_current(expected);
                self.advance();
                self.synchronize();
                None
            }
        }
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

    fn decls(&mut self) -> Vec<Decl> {
        let mut decls = Vec::new();
        while self.current.matches_value(KEYWORD, "public")
            || self.current.matches_value(KEYWORD, "namespace")
            || self.current.matches_value(KEYWORD, "type")
            || self.current.matches_value(KEYWORD, "let") {
            match self.decl() {
                Some(decl) => decls.push(decl),
                None => continue
            }
        }
        decls
    }

    fn decl(&mut self) -> Option<Decl> {
        let public = self.current.matches_value(KEYWORD, "public");
        if public {
            self.advance();
        }
        if self.current.matches_value(KEYWORD, "namespace") {
            self.namespace(public).map(|ns| Decl::Namespace(ns))
        } else { // TODO recognize type and let
            self.error_at_current("namespace, type, or binding");
            self.advance();
            self.synchronize();
            None
        }
    }

    fn namespace(&mut self, public: bool) -> Option<Namespace> {
        unimplemented!()
    }
}
