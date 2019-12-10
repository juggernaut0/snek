use crate::value::{Value, OwnedValue};

pub struct GcRoot {
    exec_stack: Vec<OwnedValue>,
    //call_stack: Vec<CallFrame>,
}

impl GcRoot {
    pub fn new() -> GcRoot {
        GcRoot {
            exec_stack: Vec::new(),
        }
    }

    pub fn exec_stack_push(&mut self, value: impl Into<OwnedValue>) {
        self.exec_stack.push(value.into())
    }

    pub fn exec_stack_peek(&self) -> Option<Value> {
        self.exec_stack.last().map(|&it| unsafe { to_value(it) })
    }

    pub fn exec_stack_pop(&mut self) -> Option<Value> {
        self.exec_stack.pop().map(|&it| unsafe { to_value(it) })
    }

    fn collect(self) -> GcRoot {
        unimplemented!()
    }
}

unsafe fn to_value<'a>(ov: OwnedValue) -> Value<'a> {
    match ov {
        OwnedValue::Uninitialized => Value::Uninitialized,
        OwnedValue::Unit => Value::Unit,
        OwnedValue::Number(n) => Value::Number(n),
        OwnedValue::Boolean(b) => Value::Boolean(b),
        OwnedValue::String(s) => Value::String(&*s),
        OwnedValue::Function(f) => Value::Function(&*f),
    }
}
