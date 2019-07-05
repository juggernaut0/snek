use crate::opcode::Code;
use crate::opcode::OpCode::*;
use crate::value::{Value, FunctionValue, FunctionType};
use std::collections::HashMap;
use std::rc::Rc;

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
    root_namespace: Namespace
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            exec_stack: Vec::new(),
            root_namespace: make_builtins()
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
                Call(nargs) => {
                    let f = self.pop()?;
                    if let Value::Function(fv) = f {
                        let func_args = fv.num_args();
                        if nargs == func_args {
                            match fv.func_type() {
                                FunctionType::Compiled(_) => unimplemented!("compiled functions"),
                                FunctionType::Native(f) => f(self)?,
                                FunctionType::Partial(_, _) => unimplemented!("partial functions"),
                            }
                        } else if nargs < func_args {
                            unimplemented!("partial functions")
                        } else {
                            return Err(self.error(format!("Too many arguments passed for function")))
                        }
                    } else {
                        return Err(self.error(format!("Can only call functions, got {:?}", f)))
                    }
                }
                _ => unimplemented!()
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
        println!("{:?}", v); // TODO real stringify
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
