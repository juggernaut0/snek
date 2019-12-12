use std::collections::HashMap;
use crate::opcode::{ConstantValue, Code};
use std::rc::Rc;
use crate::mem::heap_allocate;

#[derive(Copy, Clone)]
pub enum OwnedValue {
    Uninitialized,
    Unit,
    Number(f64),
    Boolean(bool),
    String(*const String), // TODO can I use Box for these (and other raw pointers in mem module)?
    Function(*const FunctionValue),
    // TODO other object types
}

pub trait IntoOwnedValue {
    fn into_owned_value(self) -> OwnedValue;
}

impl IntoOwnedValue for String {
    fn into_owned_value(self) -> OwnedValue {
        OwnedValue::String(heap_allocate(self))
    }
}

impl IntoOwnedValue for FunctionValue {
    fn into_owned_value(self) -> OwnedValue {
        OwnedValue::Function(heap_allocate(self))
    }
}

impl IntoOwnedValue for ConstantValue {
    fn into_owned_value(self) -> OwnedValue {
        match self {
            ConstantValue::Unit => OwnedValue::Unit,
            ConstantValue::Number(n) => OwnedValue::Number(n),
            ConstantValue::Boolean(b) => OwnedValue::Boolean(b),
            ConstantValue::String(s) => s.into_owned_value(),
        }
    }
}
