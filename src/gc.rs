use crate::value::{Value, FunctionValue};
use std::collections::HashMap;
use crate::opcode::ConstantValue;

pub struct GcRoot {
    exec_stack: Vec<OwnedValue>,
    //call_stack: Vec<CallFrame>,
    root_namespace: Namespace,
    strings: Vec<*const String>,
    functions: Vec<*const FunctionValue>
}

impl GcRoot {
    pub fn new() -> GcRoot {
        GcRoot {
            exec_stack: Vec::new(),
            root_namespace: Namespace::default(),
            strings: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn allocate(&mut self, obj: impl IntoOwnedValue) -> Value {
        let owned = obj.into_owned_value();
        match owned {
            OwnedValue::String(p) => self.strings.push(p),
            OwnedValue::Function(p) => self.functions.push(p),
            _ => {}
        };
        to_value(owned)
    }

    pub fn exec_stack_push(&mut self, value: Value) {
        self.exec_stack.push(to_owned(value))
    }

    pub fn exec_stack_peek(&self) -> Option<Value> {
        self.exec_stack.last().map(|&it| to_value(it) )
    }

    pub fn exec_stack_pop(&mut self) -> Option<Value> {
        self.exec_stack.pop().map(|&it| to_value(it) )
    }

    // TODO WIP API for namespace put/gets
    pub fn put_name(&mut self, name: String, value: Value) {
        self.root_namespace.values.insert(name, to_owned(value));
    }

    pub fn get_name(&self, name: &String) -> Option<Value> {
        self.root_namespace.values.get(name).map(|&ov| to_value(ov))
    }

    pub fn collect(&mut self) {
        // TODO
    }
}

#[derive(Default)]
struct Namespace {
    values: HashMap<String, OwnedValue>,
}

#[derive(Copy, Clone)]
pub enum OwnedValue {
    Uninitialized,
    Unit,
    Number(f64),
    Boolean(bool),
    String(*const String),
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

fn heap_allocate<T>(t: T) -> *const T {
    Box::into_raw(Box::new(t))
}

fn to_value<'a>(ov: OwnedValue) -> Value<'a> {
    unsafe {
        match ov {
            OwnedValue::Uninitialized => Value::Uninitialized,
            OwnedValue::Unit => Value::Unit,
            OwnedValue::Number(n) => Value::Number(n),
            OwnedValue::Boolean(b) => Value::Boolean(b),
            OwnedValue::String(s) => Value::String(&*s),
            OwnedValue::Function(f) => Value::Function(&*f),
        }
    }
}

fn to_owned(v: Value) -> OwnedValue {
    match v {
        Value::Uninitialized => OwnedValue::Uninitialized,
        Value::Unit => OwnedValue::Unit,
        Value::Number(n) => OwnedValue::Number(n),
        Value::Boolean(b) => OwnedValue::Boolean(b),
        Value::String(s) => OwnedValue::String(s),
        Value::Function(f) => OwnedValue::Function(f),
    }
}
