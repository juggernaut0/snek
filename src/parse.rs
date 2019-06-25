use crate::scanner::Scanner;
use crate::scanner::TokenName;

pub trait Ast {

}

pub fn parse(src: &str) -> Box<dyn Ast> {
    let mut scanner = Scanner::new(src);
    loop {
        let token = scanner.get_token().unwrap(); // TODO don't unwrap
        println!("{:?}", token);
        if token.name() == TokenName::EOF {
            break;
        }
    }
    unimplemented!()
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
}
