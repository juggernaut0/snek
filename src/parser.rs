use crate::scanner::{Scanner, Token, TokenName::*};

const MAX_ERRORS: usize = 500;

pub fn parse(src: &str) -> Result<Ast, Vec<ParseError>> {
    let mut scanner = Scanner::new(src);
    loop {
        let t = scanner.get_token().unwrap();
        println!("{:?}", t);
        if t.name() == EOF {
            break;
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
    //rootNamespace: NameSpace,
    //expr: CallExpr
}

pub struct Import {
    filename: String,
    names: Vec<QName>
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
        // TODO decls & call
        let current = self.current.matches(EOF);
        if self.errors.is_empty() && current.is_some() {
            Ok(Ast {
                imports
            })
        } else {
            if current.is_none() {
                self.error_at_current("EOF")
            }
            Err(self.errors)
        }
    }

    fn error(&mut self, error: ParseError) {
        if self.errors.len() == MAX_ERRORS {
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

    fn imports(&mut self) -> Vec<Import> {
        let mut imports = Vec::new();
        while self.current.matches_value(KEYWORD, "import") {
            self.advance(); // advance past "import"
            let mut names = Vec::new();
            while !self.current.matches_value(KEYWORD, "from") {
                names.push(self.qualified_name());
            }
            self.advance(); // advance past "from"
            match self.current.matches(STRING) {
                Some(filename) => {
                    imports.push(Import { filename: filename.to_string(), names });
                    self.advance();
                }
                None => self.error_at_current("string literal") // TODO synchronize
            }

        }
        imports
    }

    fn qualified_name(&mut self) -> QName {
        unimplemented!() // TODO qualified_name
    }
}
