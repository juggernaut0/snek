use crate::opcode::Code;
use crate::opcode::OpCode::*;
use crate::value::{Value, FunctionValue, FunctionType};
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
    root_namespace: Namespace,
    root_environment: Environment,
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            exec_stack: Vec::new(),
            root_namespace: make_builtins(),
            root_environment: Environment::default(),
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
                    // TODO replace with call_stack environment
                    let v = self.root_environment.load(id).ok_or(self.error(format!("Variable '{}' accessed before assignment", name)))?;
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
                    self.root_environment.save(id, v); // TODO replace with call_stack environment
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
                            FunctionType::Compiled(_) => unimplemented!("compiled functions"), // TODO
                            FunctionType::Native(f) => f(self)?,
                            FunctionType::Partial(_, _) => unimplemented!("partial functions"), // TODO
                        }
                    } else if nargs < func_params {
                        unimplemented!("partial functions") // TODO
                    } else {
                        return Err(self.error(format!("Too many arguments passed for function")))
                    }
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

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.exec_stack.pop().ok_or_else(|| { self.error("Cannot pop empty stack".to_string()) })
    }

    fn pop_number(&mut self) -> Result<f64, RuntimeError> {
        match self.pop()? {
            Value::Number(n) => Ok(n),
            _ => Err(self.error("Expected a number".to_string()))
        }
    }
}

#[derive(Default)]
struct Namespace {
    values: HashMap<String, Value>,
    subnames: HashMap<String, Namespace>
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

fn make_ops() -> Namespace {
    let mut ops = Namespace::default();
    let ops_plus = Value::Function(Rc::new(FunctionValue::from_closure(|int| {
        let b = int.pop_number()?;
        let a = int.pop_number()?;
        Ok(int.exec_stack.push(Value::Number(a + b)))
    }, 2)));
    ops.values.insert("plus".to_string(), ops_plus);
    ops
}

// TODO find a faster implementation than this
#[derive(Default)]
pub struct Environment {
    parent: Option<Rc<RefCell<Environment>>>,
    bindings: HashMap<u16, Value>
}

impl Environment {
    fn new(parent: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            parent,
            bindings: HashMap::new()
        }
    }

    fn save(&mut self, slot: u16, value: Value) {
        self.bindings.insert(slot, value);
    }

    fn load(&self, slot: u16) -> Option<Value> {
        if let Some(v) = self.bindings.get(&slot) {
            v.clone().into()
        } else if let Some(p) = &self.parent {
            p.borrow().load(slot)
        } else {
            None
        }
    }
}
