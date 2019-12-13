use std::collections::HashMap;
use std::rc::Rc;

pub use crate::mem::environment::Environment;
use crate::mem::environment::{OwnedEnv, get_inner};
use crate::mem::owned_value::{IntoOwnedValue, OwnedValue};
pub use crate::mem::value::{Value, FunctionType, CompiledFunction, FunctionValue};
use crate::opcode::Code;
use std::cell::RefCell;
use std::borrow::Borrow;
use crate::mem::value::get_cf_environment;

mod environment;
mod owned_value;
mod value;

#[derive(Default)]
pub struct GcRoot {
    exec_stack: Vec<OwnedValue>,
    call_stack: Vec<OwnedCallFrame>,
    root_namespace: Namespace,
    environments: Vec<*const OwnedEnv>,
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

    pub fn new_env(&mut self) -> Environment {
        let env = heap_allocate(OwnedEnv::default());
        self.environments.push(env);
        unsafe {
            (*env).borrow()
        }
    }

    pub fn new_child_env(&mut self, cf: &CompiledFunction) -> Environment {
        let env = heap_allocate(OwnedEnv::new_child(get_cf_environment(cf)));
        self.environments.push(env);
        unsafe {
            (*env).borrow()
        }
    }

    pub fn exec_stack_push(&mut self, value: Value) {
        self.exec_stack.push(to_owned(value))
    }

    pub fn exec_stack_peek(&self) -> Option<Value> {
        self.exec_stack.last().map(|&it| to_value(it) )
    }

    pub fn exec_stack_pop(&mut self) -> Option<Value> {
        self.exec_stack.pop().map(|it| to_value(it) )
    }

    pub fn call_stack_push(&mut self, code: Rc<Code>, env: Environment) {
        self.call_stack.push(OwnedCallFrame { code, ip: 0, environment: get_inner(env) })
    }

    /*pub fn call_stack_peek(&self) -> Option<CallFrame> {
        let last = self.call_stack.last()?;
        Some(unsafe { last.borrow() })
    }*/

    pub fn call_stack_pop(&mut self) -> Option<CallFrame> {
        let top = self.call_stack.pop()?;
        Some(unsafe { top.borrow() })
    }

    // TODO WIP API for namespace put/gets
    pub fn put_name(&mut self, name: String, value: Value) {
        self.root_namespace.values.insert(name, to_owned(value));
    }

    pub fn get_name(&self, name: &String) -> Option<Value> {
        self.root_namespace.values.get(name).map(|&ov| to_value(ov))
    }

    pub fn gc(&mut self) {
        // TODO
    }
}

#[derive(Default)]
struct Namespace {
    values: HashMap<String, OwnedValue>,
}

pub struct CallFrame<'a> {
    code: Rc<Code>,
    pub ip: usize,
    environment: Environment<'a>,
}

impl CallFrame<'_> {
    pub fn environment(&self) -> Environment {
        self.environment
    }

    pub fn code(&self) -> &Code {
        &self.code
    }
}

struct OwnedCallFrame {
    code: Rc<Code>,
    ip: usize,
    environment: *const OwnedEnv,
}

impl OwnedCallFrame {
    unsafe fn borrow<'a>(self) -> CallFrame<'a> {
        CallFrame {
            code: self.code,
            ip: self.ip,
            environment: (*self.environment).borrow(),
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
