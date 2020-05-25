use std::cell::RefCell;
use std::borrow::Borrow;
use std::ops::Deref;

pub struct Collector {}

pub trait Trace {
    //fn trace(&self, collector: &Collector);
}

pub struct Gc<Root : Trace> {
    root: Box<Root>,
    objs: RefCell<Vec<Box<dyn Trace>>>,
}

impl<Root : Trace> Gc<Root> {
    fn new(root: Root) -> Gc<Root> {
        Gc {
            root: Box::new(root),
            objs: RefCell::new(Vec::new()),
        }
    }

    fn root(&self) -> &Root {
        &self.root
    }

    fn allocate<T : 'static + Trace>(&self, t: T) -> GcRef<T> {
        /*let bx = Box::new(t);
        self.objs.borrow_mut().push(bx);
        &bx*/
        todo!("allocate")
    }

    fn collect(&mut self) {
        todo!("collect")
    }
}

struct GcRef<T : Trace> {
    ptr: *const T
}

impl<T : Trace> GcRef<T> {
    // panics if ptr has been collected
    fn borrow(&self) -> &T {
        todo!()
    }
}




struct GcRoot {
    stack: RefCell<Vec<GcRef<i32>>>
}

impl Trace for i32 {}

impl Trace for GcRoot {}

fn foo() {
    let mut gc = Gc::new(GcRoot { stack: RefCell::new(Vec::new()) });
    {
        let a = gc.allocate(1);
        gc.root().stack.borrow_mut().push(a);
    }
    gc.collect();
    {
        let a = gc.root().stack.borrow_mut().pop().unwrap();
        println!("{:?}", a.borrow());
    }
}
