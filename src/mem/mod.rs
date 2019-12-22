use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::rc::Rc;

use crate::mem::environment::{get_inner, OwnedEnv};
pub use crate::mem::environment::Environment;
use crate::mem::owned_value::{IntoOwnedValue, OwnedValue};
pub use crate::mem::value::{CompiledFunction, FunctionType, FunctionValue, Value};
use crate::mem::value::get_cf_environment;
use crate::opcode::Code;
use crate::mem::mark::Mark;
use std::cmp::min;

mod environment;
mod mark;
mod owned_value;
mod value;

const MIN_LIMIT: usize = 1 << 20;

pub struct GcRoot {
    inner: RefCell<GcRootImpl>,
    limit: usize,
}

#[derive(Default)]
struct GcRootImpl {
    exec_stack: Vec<OwnedValue>,
    call_stack: Vec<OwnedCallFrame>,
    root_namespace: Namespace,
    environments: Mark<OwnedEnv>,
    strings: Mark<String>,
    functions: Mark<FunctionValue>
}

impl GcRoot {
    pub fn new() -> GcRoot {
        GcRoot {
            inner: RefCell::new(GcRootImpl::default()),
            limit: MIN_LIMIT,
        }
    }

    pub fn allocate(&self, obj: impl IntoOwnedValue) -> Value {
        let owned = obj.into_owned_value();
        match owned {
            OwnedValue::String(p) => self.inner.borrow_mut().strings.insert(p),
            OwnedValue::Function(p) => self.inner.borrow_mut().functions.insert(p),
            _ => {}
        };
        to_value(owned)
    }

    pub fn new_env(&self) -> Environment {
        let env = heap_allocate(OwnedEnv::default());
        self.inner.borrow_mut().environments.insert(env);
        unsafe {
            (*env).borrow()
        }
    }

    pub fn new_child_env(&self, cf: &CompiledFunction) -> Environment {
        let env = heap_allocate(OwnedEnv::new_child(get_cf_environment(cf)));
        self.inner.borrow_mut().environments.insert(env);
        unsafe {
            (*env).borrow()
        }
    }

    pub fn exec_stack_push(&self, value: Value) {
        self.inner.borrow_mut().exec_stack.push(to_owned(value))
    }

    pub fn exec_stack_peek(&self) -> Option<Value> {
        self.inner.borrow().exec_stack.last().map(|&it| to_value(it) )
    }

    pub fn exec_stack_pop(&self) -> Option<Value> {
        self.inner.borrow_mut().exec_stack.pop().map(|it| to_value(it) )
    }

    /*pub fn call_stack_push(&self, code: Rc<Code>, env: Environment) {
        self.inner.borrow_mut().call_stack.push(OwnedCallFrame { code, ip: 0, environment: get_inner(env) })
    }*/

    pub fn call_stack_push(&self, frame: CallFrame) {
        let owned = OwnedCallFrame {
            code: frame.code,
            ip: frame.ip,
            environment: get_inner(frame.environment)
        };
        self.inner.borrow_mut().call_stack.push(owned);
    }

    /*pub fn call_stack_peek(&self) -> Option<CallFrame> {
        let last = self.call_stack.last()?;
        Some(unsafe { last.borrow() })
    }*/

    pub fn call_stack_pop(&self) -> Option<CallFrame> {
        let top = self.inner.borrow_mut().call_stack.pop()?;
        Some(unsafe { top.borrow() })
    }

    // TODO WIP API for namespace put/gets
    pub fn put_name(&self, name: String, value: Value) {
        self.inner.borrow_mut().root_namespace.values.insert(name, to_owned(value));
    }

    pub fn get_name(&self, name: &String) -> Option<Value> {
        self.inner.borrow().root_namespace.values.get(name).map(|&ov| to_value(ov))
    }

    pub fn gc(&mut self) {
        if self.inner.borrow().usage() > self.limit {
            unsafe {
                self.inner.borrow_mut().gc();
            }
            let usage = self.inner.borrow().usage();
            if usage > (self.limit / 4) {
                self.limit *= 2;
            } else if usage < (self.limit / 8) && self.limit > MIN_LIMIT {
                self.limit /= 2;
            }
        }
    }

    pub fn debug_exec_stack(&self) {
        let s: Vec<_> = self.inner.borrow().exec_stack.iter().map(|&it| to_value(it)).collect();
        println!("[DEBUG] {:?}", s)
    }
}

impl GcRootImpl {
    fn usage(&self) -> usize {
        self.environments.usage() + self.strings.usage() + self.functions.usage()
    }

    unsafe fn gc(&mut self) {
        let call_stack = std::mem::replace(&mut self.call_stack, Vec::new());
        for e in &call_stack {
            self.mark_env(e.environment)
        }
        std::mem::replace(&mut self.call_stack, call_stack);

        let exec_stack = std::mem::replace(&mut self.exec_stack, Vec::new());
        for v in &exec_stack {
            self.mark_value(v);
        }
        std::mem::replace(&mut self.exec_stack, exec_stack);

        let root_namespace = std::mem::replace(&mut self.root_namespace, Namespace::default());
        root_namespace.values.values().for_each(|v| self.mark_value(v));
        std::mem::replace(&mut self.root_namespace, root_namespace);

        self.strings.collect();
        self.functions.collect();
        self.environments.collect();
    }

    unsafe fn mark_value(&mut self, v: *const OwnedValue) {
        match *v {
            OwnedValue::Function(fv) => {
                if self.functions.is_marked(fv) {
                    return;
                }
                self.functions.mark(fv);
                match (*fv).func_type() {
                    FunctionType::Compiled(cf) => {
                        self.mark_env(cf.environment);
                    },
                    _ => {}
                }
            }
            OwnedValue::String(s) => {
                self.strings.mark(s);
            }
            _ => {} // do nothing
        }
    }

    unsafe fn mark_env(&mut self, e: *const OwnedEnv) {
        if self.environments.is_marked(e) {
            return
        }
        self.environments.mark(e);
        (&*e).values_for_each(|v| {
            self.mark_value(v);
        });
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
    pub fn new(code: Rc<Code>, environment: Environment) -> CallFrame {
        CallFrame { code, ip: 0, environment }
    }

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
