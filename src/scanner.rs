use std::iter::Peekable;
use std::str::Chars;

use crate::scanner::TokenName::*;
use crate::parse::ParseError;

type ScanResult = Result<Token, ParseError>;

pub struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
    current_line: u32,
    start_col: u32,
    current_col: u32
}

impl<'a> Scanner<'a> {
    pub fn new(src: &str) -> Scanner {
        Scanner {
            chars: src.chars().peekable(),
            current_line: 1,
            start_col: 1,
            current_col: 1
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

    fn match_token(&mut self) -> ScanResult {
        self.start_col = self.current_col;
        match self.peek() {
            Some(c) => {
                match c {
                    '#' => self.match_comment(),
                    c if c.is_ascii_digit() => self.match_number(),
                    c if is_whitespace(c) => self.match_whitespace(),
                    c if c == '\n' => self.match_newline(),
                    _ => unimplemented!("Default")
                }
            },
            None => self.token(EOF, String::new())
        }
    }

    fn token(&self, name: TokenName, value: String) -> ScanResult {
        Ok(Token { name, value, line: self.current_line, col: self.start_col })
    }

    fn error(&self, message: String) -> ParseError {
        ParseError::new(message, self.current_line, self.current_col)
    }


    // ===== Matching utility functions =====

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().copied()
    }

    // called when EOF is acceptable
    fn advance(&mut self) -> Option<char> {
        let c = self.chars.next();
        if c.is_some() {
            self.current_col += 1
        }
        c
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

    fn consume_into(&mut self, value: &mut String) -> Result<(), ParseError> {
        let c = self.consume()?;
        value.push(c);
        Ok(())
    }

    fn consume_into_if(&mut self, value: &mut String, f: impl FnOnce(char) -> bool) -> bool {
        if let Some(c) = self.peek().filter(|it| f(*it)) {
            value.push(c);
            self.advance();
            return true
        } else {
            return false
        }
    }

    fn consume_into_while(&mut self, value: &mut String, f: impl Fn(char) -> bool) {
        loop {
            if !self.consume_into_if(value, |it| f(it)) {
                break
            }
        }
    }

    fn consume_if_match(&mut self, expect: char) -> Result<(), ParseError> {
        let c = self.consume()?;
        if expect != c {
            Err(self.error(format!("Expected {}, got {}", expect, c)))
        } else {
            Ok(())
        }
    }


    // ===== Scanning functions =====

    fn match_comment(&mut self) -> ScanResult {
        self.consume_if_match('#')?;
        self.consume_while(|c| c != '\n');
        self.token(IGNORE, String::new())
    }

    fn match_number(&mut self) -> ScanResult {
        let mut value = String::new();
        self.consume_into(&mut value)?;
        self.consume_into_while(&mut value, |it| it.is_ascii_digit());
        if self.consume_into_if(&mut value, |it| it == '.') {
            self.consume_into_while(&mut value, |it| it.is_ascii_digit());
        }
        self.token(NUMBER, value)
    }

    fn match_whitespace(&mut self) -> ScanResult {
        self.consume()?;
        self.consume_while(is_whitespace);
        self.token(IGNORE, String::new())
    }

    fn match_newline(&mut self) -> ScanResult {
        self.consume()?;
        self.token(NEWLINE, String::new())
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
