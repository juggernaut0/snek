use std::hash::{Hash, Hasher};
use std::cell::{RefCell, Cell};
use std::collections::hash_map::DefaultHasher;
use std::ops::Deref;

pub struct IdBox<T>(Box<T>);

impl<T> IdBox<T> {
    pub fn new(t: T) -> IdBox<T> {
        IdBox(Box::new(t))
    }

    fn as_ptr(&self) -> *const T {
        self.0.as_ref() as *const T
    }
}

impl<T: Hash> Hash for IdBox<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state);
    }
}

impl<T: PartialEq> PartialEq for IdBox<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}
impl<T: Eq> Eq for IdBox<T> {}

impl<T: Clone> Clone for IdBox<T> {
    fn clone(&self) -> Self {
        IdBox(self.0.clone())
    }
}

impl<T> Deref for IdBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub fn test() {
    let a = IdBox::new(123);
    let b = IdBox::new(123);
    let c = IdBox::new(234);
    println!("a == b {}", a == b);
    println!("a == c {}", a == c);
    println!("hash(a) {}", hash(&a));
    println!("hash(b) {}", hash(&b));
    println!("hash(c) {}", hash(&c));
    let moved_a = a;
    println!("hash(moved_a) {}", hash(&moved_a));
    let cloned_b = b.clone();
    println!("b == cloned_b {}", b == cloned_b);
    println!("hash(cloned_b) {}", hash(&cloned_b));
    println!("234 == *c {}", 234 == *c);
}

fn hash<T: Hash>(t: &T) -> u64 {
    let mut h = DefaultHasher::new();
    t.hash(&mut h);
    h.finish()
}
