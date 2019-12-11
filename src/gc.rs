use crate::value::{Value, FunctionValue};
use std::collections::HashMap;
use crate::opcode::{ConstantValue, Code};
use std::cell::RefCell;
use fnv::FnvHashMap;
use std::rc::Rc;

#[derive(Default)]
pub struct GcRoot {
    exec_stack: Vec<OwnedValue>,
    call_stack: Vec<CallFrame>,
    root_namespace: Namespace,
    environments: Vec<*const Environment>,
    strings: Vec<*const String>,
    functions: Vec<*const FunctionValue>
}

impl GcRoot {
    pub fn new() -> GcRoot {
        GcRoot::default()
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
enum OwnedValue {
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

struct CallFrame {
    code: Rc<Code>,
    ip: usize,
    environment: Rc<Environment>
}

// TODO find a faster implementation than this (Vec instead of HashMap)
// NOTE this implementation is safe in a single thread
#[derive(Default)]
struct Environment {
    parent: Option<Rc<Environment>>, // TODO maybe not use Rc?
    bindings: RefCell<FnvHashMap<u16, OwnedValue>>,
    marked: RefCell<bool>,
}

impl Environment {
    fn new_child(parent: Rc<Environment>) -> Environment {
        Environment {
            parent: Some(parent),
            //bindings: RefCell::new(vec![Value::Uninitialized; size]),
            bindings: RefCell::new(FnvHashMap::default()),
            marked: RefCell::new(false)
        }
    }

    /*fn save(&self, slot: u16, value: Value) {
        let i = slot as usize - self.offset;
        self.bindings.borrow_mut()[i] = value;
    }

    fn load(&self, slot: u16) -> Option<Value> {
        if slot as usize > self.offset {
            let i = slot as usize - self.offset;
            let v = &self.bindings.borrow()[i];
            if let Value::Uninitialized = v {
                None
            } else {
                Some(v.clone())
            }
        } else if let Some(p) = self.parent {
            p.load(slot)
        } else {
            None
        }
    }*/

    fn save(&self, slot: u16, value: Value) {
        self.bindings.borrow_mut().insert(slot, to_owned(value));
    }

    fn load(&self, slot: u16) -> Option<Value> {
        if let Some(&v) = self.bindings.borrow().get(&slot) {
            Some(to_value(v))
        } else if let Some(p) = &self.parent {
            p.load(slot)
        } else {
            None
        }
    }

    fn values(&self) -> Vec<Value> {
        self.bindings.borrow().values().cloned().collect()
    }

    fn mark(&self) {
        *self.marked.borrow_mut() = true;
    }

    fn unmark(&self) {
        *self.marked.borrow_mut() = false;
    }

    fn marked(&self) -> bool {
        *self.marked.borrow()
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
