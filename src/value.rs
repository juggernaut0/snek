use std::cell::RefCell;
use std::fmt::{Debug, Display, Error, Formatter};
use std::rc::{Rc, Weak};

use crate::interpreter::{Environment, Interpreter, RuntimeError};
use crate::opcode::Code;

#[derive(Clone)]
pub enum Value<'a> {
    Uninitialized,
    Unit,
    Number(f64),
    Boolean(bool),
    String(&'a String),
    Function(&'a FunctionValue),
    // TODO other object types
}

impl<'a> Value<'a> {
    pub fn require_number(self) -> Result<f64, RuntimeError> {
        if let Value::Number(n) = self {
            Ok(n)
        } else {
            Err(RuntimeError { message: format!("Required number, got {}", self) })
        }
    }

    pub fn require_boolean(self) -> Result<bool, RuntimeError> {
        if let Value::Boolean(b) = self {
            Ok(b)
        } else {
            Err(RuntimeError { message: format!("Required boolean, got {}", self) })
        }
    }

    pub fn require_function(self) -> Result<&'a FunctionValue, RuntimeError> {
        if let Value::Function(fv) = self {
            Ok(fv)
        } else {
            Err(RuntimeError { message: format!("Required function, got {}", self) })
        }
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Uninitialized => write!(f, "Uninitialized"),
            Value::Unit => write!(f, "()"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(_) => write!(f, "<function>"), // TODO line number
        }
    }
}

impl Debug for Value<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        Display::fmt(self, f)
    }
}

pub struct FunctionValue {
    func_type: FunctionType,
    num_params: u16,
}

impl FunctionValue {
    pub fn new(func_type: FunctionType, num_params: u16) -> FunctionValue {
        FunctionValue {
            func_type,
            num_params
        }
    }

    pub fn from_closure(f: impl Fn(&mut Interpreter) -> Result<(), RuntimeError> + 'static, num_params: u16) -> FunctionValue {
        FunctionValue::new(FunctionType::Native(Box::new(f)), num_params)
    }

    pub fn num_params(&self) -> u16 {
        self.num_params
    }

    pub fn func_type(&self) -> &FunctionType {
        &self.func_type
    }
}

pub enum FunctionType {
    Compiled(CompiledFunction),
    Native(Box<dyn Fn(&mut Interpreter) -> Result<(), RuntimeError>>),
    //Partial(&'a FunctionValue<'a>, Vec<Value<'a>>) TODO partial functions
}

pub struct CompiledFunction {
    code: Rc<Code>,
    environment: Weak<Environment>,
}

impl CompiledFunction {
    pub fn new(code: Rc<Code>, environment: &Rc<Environment>) -> CompiledFunction {
        CompiledFunction {
            code,
            environment: Rc::downgrade(environment),
        }
    }

    pub fn code(&self) -> &Rc<Code> {
        &self.code
    }

    pub fn environment(&self) -> Rc<Environment> {
        self.environment.upgrade().unwrap()
    }
}

