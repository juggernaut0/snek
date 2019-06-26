use std::iter::Peekable;
use std::str::CharIndices;

use crate::scanner::TokenName::*;
use crate::parse::ParseError;

type ScanResult = Result<Token, ParseError>;

pub struct Scanner<'a> {
    src: &'a str,
    chars: Peekable<CharIndices<'a>>,
    current_line: u32,
    start_col: u32,
    current_col: u32,
    start_index: usize,
    current_index: usize
}

impl<'a> Scanner<'a> {
    pub fn new(src: &str) -> Scanner {
        Scanner {
            src,
            chars: src.char_indices().peekable(),
            current_line: 1,
            start_col: 1,
            current_col: 1,
            start_index: 0,
            current_index: 0
        }
    }

    pub fn get_token(&mut self) -> ScanResult {
        loop {
            let t = self.match_token()?;
            if t.name == IGNORE {
                continue
            } else if t.name == NEWLINE {
                self.current_line += 1;
                self.current_col = 1;
                continue
            } else {
                return Ok(t)
            }
        }
    }


    // ===== Result constructors =====

    fn token(&self, name: TokenName) -> ScanResult {
        let value = (&self.src[self.start_index..self.current_index]).to_string();
        Ok(Token { name, value, line: self.current_line, col: self.start_col })
    }

    fn error(&self, message: String) -> ParseError {
        ParseError::new(message, self.current_line, self.current_col)
    }


    // ===== Matching utility functions =====

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    // called when EOF is acceptable
    fn advance(&mut self) -> Option<char> {
        let ic = self.chars.next();
        if let Some((i, c)) = ic {
            self.current_col += 1;
            // i is the index of the char we just advanced past, so we need to add c.len
            // to get the index of the current
            self.current_index = i + c.len_utf8()
        }
        ic.map(|(_, c)| c)
    }

    // called when EOF is *not* expected (i.e. in the middle of an incomplete token)
    fn consume(&mut self) -> Result<char, ParseError> {
        self.advance().ok_or_else(|| self.error("Unexpected EOF".to_string()))
    }

    fn consume_if(&mut self, f: impl FnOnce(char) -> bool) -> bool {
        if self.peek().filter(|it| f(*it)).is_some() {
            self.advance();
            return true
        } else {
            return false
        }
    }

    fn consume_while(&mut self, f: impl Fn(char) -> bool) {
        loop {
            if !self.consume_if(|it| f(it)) {
                break
            }
        }
    }

    fn require(&mut self, f: impl Fn(char) -> bool, message: String) -> Result<(), ParseError> {
        let c = self.consume()?;
        if !f(c) {
            Err(self.error(message))
        } else {
            Ok(())
        }
    }


    // ===== Scanning functions =====

    fn match_token(&mut self) -> ScanResult {
        self.start_col = self.current_col;
        self.start_index = self.current_index;
        match self.peek() {
            Some(c) => {
                match c {
                    '#' => self.match_comment(),
                    c if c.is_ascii_digit() => self.match_number(),
                    c if c == '\'' || c == '"' => self.match_string(),
                    c if starts_symbol(c) => self.match_symbol(),
                    c if is_whitespace(c) => self.match_whitespace(),
                    c if c == '\n' => self.match_newline(),
                    _ => unimplemented!("Default")
                }
            },
            None => self.token(EOF)
        }
    }

    fn match_comment(&mut self) -> ScanResult {
        self.consume()?;
        self.consume_while(|c| c != '\n');
        self.token(IGNORE)
    }

    fn match_number(&mut self) -> ScanResult {
        self.consume()?;
        self.consume_while(|it| it.is_ascii_digit());
        if self.consume_if(|it| it == '.') {
            self.consume_while(|it| it.is_ascii_digit());
        }
        self.token(NUMBER)
    }

    fn match_string(&mut self) -> ScanResult {
        let start = self.consume()?;
        self.consume_while(|c| c != '\n' && c != start);
        self.require(|c| c == start, format!("Expected {}", start))?;
        self.token(STRING)
    }

    fn match_symbol(&mut self) -> ScanResult {
        unimplemented!()
    }

    fn match_whitespace(&mut self) -> ScanResult {
        self.consume()?;
        self.consume_while(is_whitespace);
        self.token(IGNORE)
    }

    fn match_newline(&mut self) -> ScanResult {
        self.consume()?;
        self.token(NEWLINE)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenName {
    IGNORE,
    NEWLINE,
    IDENT,
    NUMBER,
    STRING,
    KEYWORD,
    SYMBOL,
    EOF
}

#[derive(Debug)]
pub struct Token {
    name: TokenName,
    value: String,
    line: u32,
    col: u32
}

impl Token {
    pub fn name(&self) -> TokenName {
        self.name
    }
}

fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r'
}

fn starts_symbol(c: char) -> bool {
    c == '.'
        || c == '{'
        || c == '}'
        || c == '('
        || c == ')'
        || c == '['
        || c == ']'
        || c == '='
        || c == '-'
        || c == '+'
        || c == '*'
        || c == '/'
}
