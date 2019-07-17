use crate::opcode::Code;
use crate::opcode::OpCode::*;
use crate::value::{Value, FunctionValue, FunctionType, CompiledFunction};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub fn execute(code: &Code) {
    match Interpreter::new().run(code) {
        Ok(()) => {},
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
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            exec_stack: Vec::new(),
            call_stack: vec![CallFrame { environment: Rc::new(Environment::default()) }],
            root_namespace: make_builtins(),
            environments: Vec::new(),
        }
    }

    fn run(&mut self, code: &Code) -> Result<(), RuntimeError> {
        let mut ip: usize = 0;
        while ip < code.len() {
            let (opcode, d) = code.get_op_code(ip);
            ip += d;
            match opcode {
                NoOp => (),
                Pop => {
                    let _ = self.pop()?;
                },
                LoadLocal(name, id) => {
                    let v = self.get_env().load(id).ok_or(self.error(format!("Variable '{}' accessed before assignment", name)))?;
                    self.exec_stack.push(v);
                }
                LoadName(name) => {
                    let mut ns = &self.root_namespace;
                    for part in &name[0..(name.len()-1)] {
                        if let Some(subname) = ns.subnames.get(part) {
                            ns = subname
                        } else {
                            return Err(self.error(format!("No such namespace: {}", part)))
                        }
                    }
                    let last = &name[name.len()-1];
                    if let Some(v) = ns.values.get(last) {
                        self.exec_stack.push(v.clone())
                    } else {
                        return Err(self.error(format!("No such value: {}", last)))
                    }
                }
                LoadConstant(v) => self.exec_stack.push(v),
                SaveLocal(id) => {
                    let v = self.pop()?;
                    self.get_env().save(id, v);
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
                                let frame = CallFrame { environment };
                                self.call_stack.push(frame);
                                self.run(cf.code())?;
                                self.call_stack.pop().unwrap();
                                // TODO cleanup_envs
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
                    let cf = CompiledFunction::new(code, &self.get_env());
                    let fv = Rc::new(FunctionValue::new(FunctionType::Compiled(cf), nparams));
                    let f = Value::Function(fv);
                    self.exec_stack.push(f);
                }
                _ => unimplemented!("unsupported opcode @ {}: {:?}", ip - d, opcode)
            }
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

    fn get_env(&self) -> &Rc<Environment> {
        &self.call_stack.last().unwrap().environment
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

    fn new_env(&mut self, parent: Rc<Environment>) -> Rc<Environment> {
        let env = Rc::new(Environment::new_child(parent));
        self.environments.push(Rc::clone(&env));
        env
    }

    fn cleanup_envs(&mut self) {
        unimplemented!("cleanup_envs") // TODO
    }
}

struct CallFrame {
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
