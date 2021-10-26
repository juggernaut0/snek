use std::iter::Peekable;
use std::str::CharIndices;

use crate::scanner::TokenName::*;
use crate::parser::ParseError;

type ScanResult<'a> = Result<Token<'a>, ParseError>;

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

    pub fn get_token(&mut self) -> ScanResult<'a> {
        loop {
            let t = self.match_token()?;
            if t.name == Ignore {
                continue
            } else if t.name == Newline {
                self.current_line += 1;
                self.current_col = 1;
                continue
            } else {
                return Ok(t)
            }
        }
    }


    // ===== Result constructors =====

    fn value(&self) -> &'a str {
        &self.src[self.start_index..self.current_index]
    }

    fn token(&self, name: TokenName) -> ScanResult<'a> {
        let value = self.value();
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
            true
        } else {
            false
        }
    }

    fn consume_while(&mut self, f: impl Fn(char) -> bool) {
        loop {
            if !self.consume_if(|it| f(it)) {
                break
            }
        }
    }

    fn require(&mut self, f: impl Fn(char) -> bool, message: &str) -> Result<char, ParseError> {
        let c = self.consume()?;
        if !f(c) {
            Err(self.error(message.to_string()))
        } else {
            Ok(c)
        }
    }


    // ===== Scanning functions =====

    fn match_token(&mut self) -> ScanResult<'a> {
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
                    _ => self.match_ident()
                }
            },
            None => self.token(Eof)
        }
    }

    fn match_comment(&mut self) -> ScanResult<'a> {
        self.consume()?;
        self.consume_while(|c| c != '\n');
        self.token(Ignore)
    }

    fn match_number(&mut self) -> ScanResult<'a> {
        self.consume()?;
        self.consume_while(|it| it.is_ascii_digit());
        if self.consume_if(|it| it == '.') {
            self.consume_while(|it| it.is_ascii_digit());
        }
        self.token(Number)
    }

    fn match_string(&mut self) -> ScanResult<'a> {
        let start = self.consume()?;
        loop {
            if self.consume_if(|it| it == '\\') {
                self.consume()?;
            } else if !self.consume_if(|c| c != '\n' && c != start) {
                break
            }
        }
        self.require(|c| c == start, &format!("Expected {}", start))?;
        self.token(Str)
    }

    fn match_symbol(&mut self) -> ScanResult<'a> {
        match self.consume()? {
            '-' => { self.consume_if(|c| c == '>'); }
            '<' => { self.consume_if(|c| c == '='); }
            '>' => { self.consume_if(|c| c == '='); }
            '=' => { self.consume_if(|c| c == '='); }
            '!' => { self.consume_if(|c| c == '='); }
            '&' => { self.require(|c| c == '&', "Expected &")?; }
            '|' => { self.consume_if(|c| c == '|'); }
            _ => {}
        }
        self.token(Symbol)
    }

    fn match_whitespace(&mut self) -> ScanResult<'a> {
        self.consume()?;
        self.consume_while(is_whitespace);
        self.token(Ignore)
    }

    fn match_newline(&mut self) -> ScanResult<'a> {
        self.consume()?;
        self.token(Newline)
    }

    fn match_ident(&mut self) -> ScanResult<'a> {
        self.consume()?;
        self.consume_while(|c| {
            c != '#'
                //&& !c.is_ascii_digit()
                && c != '\''
                && c != '"'
                && !starts_symbol(c)
                && !c.is_ascii_whitespace()
        });
        let value = self.value();
        match value {
            "import"
            | "from"
            | "public"
            | "namespace"
            | "type"
            | "new"
            | "let"
            | "match"
            | "true"
            | "false" => self.token(Keyword),
            _ => self.token(Ident)
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenName {
    Ignore,
    Newline,
    Ident,
    Number,
    Str,
    Keyword,
    Symbol,
    Eof,
}

#[derive(Debug)]
pub struct Token<'a> {
    name: TokenName,
    value: &'a str,
    line: u32,
    col: u32
}

impl<'a> Token<'a> {
    pub fn value(&self) -> &'a str {
        self.value
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn col(&self) -> u32 {
        self.col
    }

    pub fn matches(&self, name: TokenName) -> Option<&'a str> {
        if self.name == name {
            Some(self.value)
        } else {
            None
        }
    }

    pub fn matches_value(&self, name: TokenName, value: &str) -> bool {
        self.name == name && self.value == value
    }

    pub fn is_eof(&self) -> bool {
        self.name == Eof
    }
}

// excludes /n so lines can be tracked
fn is_whitespace(c: char) -> bool {
    c == ' ' || c == '\t' || c == '\r'
}

fn starts_symbol(c: char) -> bool {
    c == ':'
        || c == '.'
        || c == ','
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
        || c == '<'
        || c == '>'
        || c == '!'
        || c == '&'
        || c == '|'
}
