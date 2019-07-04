use std::rc::Rc;
use std::fmt::{Debug, Formatter, Error};

#[derive(Clone)]
pub enum Value {
    Unit,
    Number(f64),
    Boolean(bool),
    String(Rc<String>),
    // TODO other object types
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::String(s) => write!(f, "{}", s),
        }
    }
}
