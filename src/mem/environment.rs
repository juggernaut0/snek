use std::cell::{RefCell, Ref};

use fnv::FnvHashMap;

use crate::mem::{to_owned, to_value, Value};
use crate::mem::owned_value::OwnedValue;
use std::collections::hash_map::Values;
use std::collections::HashMap;

// TODO find a faster implementation than this (Vec instead of HashMap)
// NOTE this implementation is safe in a single thread
#[derive(Default)]
pub struct OwnedEnv {
    parent: Option<*const OwnedEnv>,
    bindings: RefCell<FnvHashMap<u16, OwnedValue>>,
}

impl OwnedEnv {
    pub fn new_child(parent: *const OwnedEnv) -> OwnedEnv {
        OwnedEnv {
            parent: Some(parent),
            bindings: RefCell::new(FnvHashMap::default()),
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
            unsafe { &*p }.load(slot)
        } else {
            None
        }
    }

    pub fn values_for_each(&self, mut f: impl FnMut(*const OwnedValue)) {
        self.bindings.borrow().values().for_each(|v| f(v))
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
