use std::collections::HashMap;
use std::mem::size_of;

pub struct Mark<T> {
    store: HashMap<*const T, bool>,
}

impl<T> Mark<T> {
    pub fn usage(&self) -> usize {
        self.store.len() * size_of::<T>()
    }

    pub fn insert(&mut self, t: *const T) {
        self.store.insert(t, false);
    }

    pub fn is_marked(&self, t: *const T) -> bool {
        self.store.get(&t).copied().unwrap_or(false)
    }

    pub fn mark(&mut self, t: *const T) {
        self.store.insert(t, true);
    }

    pub unsafe fn collect(&mut self) {
        self.store.retain(|&t, m| {
            let marked = *m;
            if !marked {
                drop(Box::from_raw(t as *mut T));
            }
            *m = false;
            marked
        })
    }
}

impl<T> Default for Mark<T> {
    fn default() -> Self {
        Mark {
            store: HashMap::new(),
        }
    }
}
