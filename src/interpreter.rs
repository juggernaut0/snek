use crate::opcode::{Code, ConstantValue, OpCode};
use crate::opcode::OpCode::*;
use crate::value::{Value, FunctionValue, FunctionType, CompiledFunction};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use fnv::FnvHashMap;

pub fn execute(code: Rc<Code>) {
    let mut int = Interpreter::new(code);
    match int.run() {
        Ok(()) => {},
        Err(e) => {
            eprintln!("[ERROR] {}", e.message)
        }
    }
}

pub struct RuntimeError {
    pub message: String
}

pub struct Interpreter {
    exec_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    root_namespace: Namespace,
    environments: Vec<Rc<Environment>>,
    env_limit: usize,
    env_max: usize,
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
            //decode_time: HashMap::new(),
            //exec_time: HashMap::new(),
            //gc_time: Duration::default(),
        }
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        while !self.call_stack.is_empty() {
            let mut frame = self.call_stack.pop().unwrap();
            while frame.ip < frame.code.len() {
                let (opcode, d) = frame.code.get_op_code(frame.ip);
                frame.ip += d;
                self.debug(&opcode);
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
                        self.push(v);
                    },
                    Jump(d) => {
                        frame.ip += d as usize;
                    },
                    JumpIfFalse(d) => {
                        let b = self.pop()?.require_boolean()?;
                        if !b {
                            frame.ip += d as usize;
                        }
                    }
                    LoadLocal(id) => {
                        let v = frame.environment.load(id)
                            .ok_or_else(|| self.error(format!("Variable '{}' accessed before assignment", frame.code.get_local_name(id))))?;
                        self.push(v);
                    },
                    LoadName(name) => {
                        let name: &String = &name;
                        if let Some(v) = self.root_namespace.values.get(name) {
                            let v = v.clone();
                            self.push(v);
                        } else {
                            return Err(self.error(format!("No such value: {}", &name)))
                        }
                    },
                    LoadConstant(v) => {
                        self.exec_stack.push(match v {
                            ConstantValue::Unit => Value::Unit,
                            ConstantValue::Number(n) => Value::Number(n),
                            ConstantValue::Boolean(b) => Value::Boolean(b),
                            ConstantValue::String(s) => Value::String(Rc::new(s)),
                        })
                    },
                    SaveLocal(id) => {
                        let v = self.pop()?;
                        frame.environment.save(id, v);
                    },
                    Call(nargs) => {
                        let fv = self.pop()?.require_function()?;
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
                    TailCall => {
                        //frame.ip = 0;
                        todo!("tail call") // TODO this is acting weird
                    }
                    MakeClosure(code, nparams) => {
                        let cf = CompiledFunction::new(code, &frame.environment);
                        let fv = Rc::new(FunctionValue::new(FunctionType::Compiled(cf), nparams));
                        let f = Value::Function(fv);
                        self.push(f);
                    }
                    _ => unimplemented!("unsupported opcode @ {}: {:?}", frame.ip - d, opcode)
                }
            }
            // TODO figure out a better strategy for GC
            if self.environments.len() > self.env_limit {
                self.cleanup_envs();
                self.env_limit *= 2;
                if self.env_limit > self.env_max {
                    self.env_limit = self.env_max
                }
            }
        }
        Ok(())
    }

    fn error(&self, message: String) -> RuntimeError {
        RuntimeError {
            message
        }
    }

    fn debug(&self, opcode: &OpCode) {
        println!("[DEBUG] {:?} {:?}", opcode, self.exec_stack)
    }

    fn push(&mut self, value: Value) {
        self.exec_stack.push(value);
    }

    fn peek(&self) -> Result<Value, RuntimeError> {
        self.exec_stack.last().cloned().ok_or_else(|| { self.error("Cannot peek empty stack".to_string()) })
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.exec_stack.pop().ok_or_else(|| { self.error("Cannot pop empty stack".to_string()) })
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
}

// TODO find a faster implementation than this (Vec instead of HashMap)
// NOTE this implementation is safe in a single thread
#[derive(Default)]
pub struct Environment {
    parent: Option<Rc<Environment>>,
    bindings: RefCell<FnvHashMap<u16, Value>>,
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

    fn marked(&self) -> bool {
        *self.marked.borrow()
    }
}

fn make_builtins() -> Namespace {
    let mut root = Namespace::default();
    make_ops(&mut root);

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
            let b = int.pop()?.require_number()?;
            let a = int.pop()?.require_number()?;
            Ok(int.exec_stack.push(Value::Number(a $oper b)))
        }, 2)))
    };
}

fn make_ops(ns: &mut Namespace) {
    ns.values.insert("ops.plus".to_string(), make_binary_op!(+));
    ns.values.insert("ops.minus".to_string(), make_binary_op!(-));
    ns.values.insert("ops.times".to_string(), make_binary_op!(*));
    ns.values.insert("ops.div".to_string(), make_binary_op!(/));

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
    ns.values.insert("ops.eq".to_string(), eq);
}
