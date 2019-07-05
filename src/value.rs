use std::rc::Rc;
use std::fmt::{Debug, Display, Formatter, Error};
use crate::opcode::Code;
use crate::interpreter::{Interpreter, RuntimeError};

#[derive(Clone)]
pub enum Value {
    Unit,
    Number(f64),
    Boolean(bool),
    String(Rc<String>),
    Function(Rc<FunctionValue>),
    // TODO other object types
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(_) => write!(f, "<function>") // TODO line number
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        Display::fmt(self, f)
    }
}

pub struct FunctionValue {
    func_type: FunctionType,
    num_args: u16,
}

impl FunctionValue {
    pub fn new(func_type: FunctionType, num_args: u16) -> FunctionValue {
        FunctionValue {
            func_type,
            num_args
        }
    }

    pub fn from_closure(f: impl Fn(&mut Interpreter) -> Result<(), RuntimeError> + 'static, num_args: u16) -> FunctionValue {
        FunctionValue::new(FunctionType::Native(Box::new(f)), num_args)
    }

    pub fn num_args(&self) -> u16 {
        self.num_args
    }

    pub fn func_type(&self) -> &FunctionType {
        &self.func_type
    }
}

pub enum FunctionType {
    Compiled(Box<Code>),
    Native(Box<dyn Fn(&mut Interpreter) -> Result<(), RuntimeError>>),
    Partial(Rc<FunctionValue>, Vec<Value>)
}
