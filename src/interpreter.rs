use crate::opcode::{Code, OpCode};
use crate::opcode::OpCode::*;
use crate::value::{Value, FunctionValue, FunctionType, CompiledFunction};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::time::{Duration, Instant};
use std::mem::{Discriminant, discriminant};
use std::ops::AddAssign;

pub fn execute(code: Rc<Code>) {
    let mut int = Interpreter::new(code);
    match int.run() {
        Ok(()) => {
            println!("decode: {:?}", int.decode_time);
            let mut exec_times = int.exec_time.iter().collect::<Vec<_>>();
            exec_times.sort_by(|(_, a), (_, b)| b.cmp(a));
            for (d, t) in exec_times {
                println!("exec {:?}: {:?}", d, t);
            }
            println!("gc: {:?}", int.gc_time);
        },
        Err(e) => {
            eprintln!("[ERROR] {}", e.message)
        }
    }
}

pub struct RuntimeError {
    message: String
}

pub struct Interpreter {
    exec_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    root_namespace: Namespace,
    environments: Vec<Rc<Environment>>,
    env_limit: usize,
    env_max: usize,
    decode_time: Duration,
    exec_time: HashMap<Discriminant<OpCode>, Duration>,
    gc_time: Duration,
}

impl Interpreter {
    fn new(code: Rc<Code>) -> Interpreter {
        Interpreter {
            exec_stack: Vec::new(),
            call_stack: vec![CallFrame { code, ip: 0, environment: Rc::new(Environment::default()) }],
            root_namespace: make_builtins(),
            environments: Vec::new(),
            env_limit: 16,
            env_max: 1 << 16,
            decode_time: Duration::default(),
            exec_time: HashMap::new(),
            gc_time: Duration::default(),
        }
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        while !self.call_stack.is_empty() {
            let mut frame = self.call_stack.pop().unwrap();
            while frame.ip < frame.code.len() {
                let di = Instant::now();
                let (opcode, d) = frame.code.get_op_code(frame.ip);
                frame.ip += d;
                self.decode_time += di.elapsed();
                let ei = Instant::now();
                let disc = discriminant(&opcode);
                match opcode {
                    NoOp => (),
                    Fail(msg) => {
                        return Err(self.error(String::clone(&msg)))
                    }
                    Pop => {
                        let _ = self.pop()?;
                    },
                    Duplicate => {
                        let v = self.peek()?;
                        self.exec_stack.push(v);
                    },
                    Jump(d) => {
                        frame.ip += d as usize;
                    },
                    JumpIfFalse(d) => {
                        let b = self.pop_bool()?;
                        if !b {
                            frame.ip += d as usize;
                        }
                    }
                    LoadLocal(name, id) => {
                        let v = frame.environment.load(id).ok_or(self.error(format!("Variable '{}' accessed before assignment", name)))?;
                        self.exec_stack.push(v);
                    },
                    LoadName(name) => {
                        let mut ns = &self.root_namespace;
                        for part in &name[0..(name.len() - 1)] {
                            if let Some(subname) = ns.subnames.get(part) {
                                ns = subname
                            } else {
                                return Err(self.error(format!("No such namespace: {}", part)))
                            }
                        }
                        let last = &name[name.len() - 1];
                        if let Some(v) = ns.values.get(last) {
                            self.exec_stack.push(v.clone())
                        } else {
                            return Err(self.error(format!("No such value: {}", last)))
                        }
                    },
                    LoadConstant(v) => self.exec_stack.push(v),
                    SaveLocal(id) => {
                        let v = self.pop()?;
                        frame.environment.save(id, v);
                    },
                    Call(nargs) => {
                        let f = self.pop()?;
                        let fv = if let Value::Function(fv) = f {
                            fv
                        } else {
                            return Err(self.error(format!("Can only call functions, got {:?}", f)))
                        };
                        let func_params = fv.num_params();
                        if nargs == func_params {
                            match fv.func_type() {
                                FunctionType::Compiled(cf) => {
                                    let environment = self.new_env(cf.environment());
                                    let new_frame = CallFrame { code: Rc::clone(cf.code()), ip: 0, environment };
                                    self.call_stack.push(frame);
                                    frame = new_frame;
                                },
                                FunctionType::Native(f) => f(self)?,
                                FunctionType::Partial(_, _) => unimplemented!("partial functions"), // TODO
                            }
                        } else if nargs < func_params {
                            unimplemented!("partial functions") // TODO
                        } else {
                            return Err(self.error(format!("Too many arguments passed for function")))
                        }
                    },
                    MakeClosure(code, nparams) => {
                        let cf = CompiledFunction::new(code, &frame.environment);
                        let fv = Rc::new(FunctionValue::new(FunctionType::Compiled(cf), nparams));
                        let f = Value::Function(fv);
                        self.exec_stack.push(f);
                    }
                    _ => unimplemented!("unsupported opcode @ {}: {:?}", frame.ip - d, opcode)
                }
                let ed = ei.elapsed();
                self.exec_time.entry(disc).or_default().add_assign(ed);
            }
            let gci = Instant::now();
            // TODO figure out a better strategy for GC
            if self.environments.len() > self.env_limit {
                self.cleanup_envs();
                self.env_limit *= 2;
                if self.env_limit > self.env_max {
                    self.env_limit = self.env_max
                }
            }
            self.gc_time += gci.elapsed();
        }
        Ok(())
    }

    fn error(&self, message: String) -> RuntimeError {
        RuntimeError {
            message
        }
    }

    fn debug(&self) {
        println!("[DEBUG] {:?}", self.exec_stack)
    }

    fn peek(&self) -> Result<Value, RuntimeError> {
        self.exec_stack.last().cloned().ok_or_else(|| { self.error("Cannot peek empty stack".to_string()) })
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.exec_stack.pop().ok_or_else(|| { self.error("Cannot pop empty stack".to_string()) })
    }

    fn pop_number(&mut self) -> Result<f64, RuntimeError> {
        match self.pop()? {
            Value::Number(n) => Ok(n),
            _ => Err(self.error("Expected a number".to_string()))
        }
    }

    fn pop_bool(&mut self) -> Result<bool, RuntimeError> {
        match self.pop()? {
            Value::Boolean(b) => Ok(b),
            _ => Err(self.error("Expected a boolean".to_string()))
        }
    }

    fn new_env(&mut self, parent: Rc<Environment>) -> Rc<Environment> {
        let env = Rc::new(Environment::new_child(parent));
        self.environments.push(Rc::clone(&env));
        env
    }

    fn cleanup_envs(&mut self) {
        fn mark_value(v: &Value) {
            if let Value::Function(fv) = v {
                if let FunctionType::Compiled(cf) = fv.func_type() {
                    mark_env(cf.environment());
                }
            }
        }
        fn mark_env(e: Rc<Environment>) {
            if e.marked() {
                return;
            }
            e.mark();
            for v in e.values() {
                mark_value(&v);
            }
        }
        for cf in &self.call_stack {
            mark_env(cf.environment.clone())
        }
        for v in &self.exec_stack {
            mark_value(v)
        }
        let mut i = 0;
        while i < self.environments.len() {
            let e = &self.environments[i];
            if !e.marked() {
                self.environments.swap_remove(i);
            } else {
                i += 1;
            }
        }
    }
}

struct CallFrame {
    code: Rc<Code>,
    ip: usize,
    environment: Rc<Environment>
}

#[derive(Default)]
struct Namespace {
    values: HashMap<String, Value>,
    subnames: HashMap<String, Namespace>
}

// TODO find a faster implementation than this (Vec instead of HashMap)
// NOTE this implementation is safe in a single thread
#[derive(Default)]
pub struct Environment {
    parent: Option<Rc<Environment>>,
    bindings: RefCell<HashMap<u16, Value>>,
    marked: RefCell<bool>,
}

impl Environment {
    fn new_child(parent: Rc<Environment>) -> Environment {
        Environment {
            parent: Some(parent),
            //bindings: RefCell::new(vec![Value::Uninitialized; size]),
            bindings: RefCell::new(HashMap::new()),
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
        self.bindings.borrow_mut().insert(slot, value);
    }

    fn load(&self, slot: u16) -> Option<Value> {
        if let Some(v) = self.bindings.borrow().get(&slot) {
            Some(v.clone())
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

fn make_builtins() -> Namespace {
    let mut root = Namespace::default();
    let ops = make_ops();
    root.subnames.insert("ops".to_string(), ops);

    let println = Value::Function(Rc::new(FunctionValue::from_closure(|int| {
        let v = int.pop()?;
        println!("{}", v);
        Ok(int.exec_stack.push(Value::Unit))
    }, 1)));
    root.values.insert("println".to_string(), println);

    root
}

macro_rules! make_binary_op {
    ($oper: tt) => {
        Value::Function(Rc::new(FunctionValue::from_closure(|int| {
            let b = int.pop_number()?;
            let a = int.pop_number()?;
            Ok(int.exec_stack.push(Value::Number(a $oper b)))
        }, 2)))
    };
}

fn make_ops() -> Namespace {
    let mut ops = Namespace::default();
    ops.values.insert("plus".to_string(), make_binary_op!(+));
    ops.values.insert("minus".to_string(), make_binary_op!(-));
    ops.values.insert("times".to_string(), make_binary_op!(*));
    ops.values.insert("div".to_string(), make_binary_op!(/));

    let eq = Value::Function(Rc::new(FunctionValue::from_closure(|int| {
        let b = int.pop()?;
        let a = int.pop()?;
        let is_eq = match (a, b) { // TODO object values
            (Value::Unit, Value::Unit) => true,
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            _ => false
        };
        Ok(int.exec_stack.push(Value::Boolean(is_eq)))
    }, 2)));
    ops.values.insert("eq".to_string(), eq);

    ops
}
