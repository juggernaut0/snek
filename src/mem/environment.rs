use std::cell::RefCell;

use fnv::FnvHashMap;

use crate::mem::{to_owned, to_value, Value};
use crate::mem::owned_value::OwnedValue;

// TODO find a faster implementation than this (Vec instead of HashMap)
// NOTE this implementation is safe in a single thread
#[derive(Default)]
pub struct OwnedEnv {
    parent: Option<*const OwnedEnv>,
    bindings: RefCell<FnvHashMap<u16, OwnedValue>>,
    marked: RefCell<bool>,
}

impl OwnedEnv {
    pub fn new_child(parent: *const OwnedEnv) -> OwnedEnv {
        OwnedEnv {
            parent: Some(parent),
            bindings: RefCell::new(FnvHashMap::default()),
            marked: RefCell::new(false)
        }
    }

    pub fn borrow(&self) -> Environment {
        Environment { inner: self }
    }

    fn save(&self, slot: u16, value: Value) {
        self.bindings.borrow_mut().insert(slot, to_owned(value));
    }

    fn load(&self, slot: u16) -> Option<Value> {
        if let Some(&v) = self.bindings.borrow().get(&slot) {
            Some(to_value(v))
        } else if let Some(p) = self.parent {
            unsafe { *p }.load(slot)
        } else {
            None
        }
    }

    /*fn values(&self) -> Vec<Value> {
        self.bindings.borrow().values().cloned().collect()
    }*/

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

#[derive(Copy, Clone)]
pub struct Environment<'a> {
    inner: &'a OwnedEnv
}

impl Environment<'_> {
    pub fn save(&self, slot: u16, value: Value) {
        self.inner.save(slot, value)
    }

    pub fn load(&self, slot: u16) -> Option<Value> {
        self.inner.load(slot)
    }
}

pub fn get_inner(env: Environment) -> *const OwnedEnv {
    env.inner
}
