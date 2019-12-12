use crate::opcode::Code;
use crate::opcode::OpCode::*;
use std::rc::Rc;
use std::cell::RefCell;
use fnv::FnvHashMap;
use crate::mem::{GcRoot, Value};

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
    gc_root: GcRoot,
}

impl Interpreter {
    fn new(code: Rc<Code>) -> Interpreter {
        let mut gc_root = GcRoot::new();
        make_builtins(&mut gc_root);
        let env = gc_root.new_env();
        gc_root.call_stack_push(code, env);
        Interpreter {
            gc_root,
        }
    }

    fn run(&mut self) -> Result<(), RuntimeError> {
        while !self.call_stack.is_empty() {
            let frame = self.gc_root.call_stack_pop();
            while frame.ip < frame.code().len() {
                let (opcode, d) = frame.code().get_op_code(frame.ip);
                frame.ip += d;
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
                        let v = frame.environment().load(id)
                            .ok_or_else(|| self.error(format!("Variable '{}' accessed before assignment", frame.code().get_local_name(id))))?;
                        self.push(v);
                    },
                    LoadName(name) => {
                        let name: &String = &name;
                        if let Some(v) = self.gc_root.get_name(name) {
                            self.push(v.clone())
                        } else {
                            return Err(self.error(format!("No such value: {}", &name)))
                        }
                    },
                    LoadConstant(v) => self.gc_root.exec_stack_push(self.gc_root.allocate(v)),
                    SaveLocal(id) => {
                        let v = self.pop()?;
                        frame.environment().save(id, v);
                    },
                    Call(nargs) => {
                        let fv = self.pop()?.require_function()?;
                        let func_params = fv.num_params();
                        if nargs == func_params {
                            match fv.func_type() {
                                FunctionType::Compiled(cf) => {
                                    let environment = self.gc_root.new_child_env(cf.environment());
                                    /*let new_frame = CallFrame { code: , ip: 0, environment };
                                    self.call_stack.push(frame);*/
                                    self.gc_root.call_stack_push(Rc::clone(cf.code()), environment);
                                    //frame = new_frame;
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
                        let fv = FunctionValue::new(FunctionType::Compiled(cf), nparams);
                        let f = self.gc_root.allocate(fv);
                        self.push(f);
                    }
                    _ => unimplemented!("unsupported opcode @ {}: {:?}", frame.ip - d, opcode)
                }
            }
            self.gc_root.gc();
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

    fn push(&mut self, value: Value) {
        self.gc_root.exec_stack_push(value);
    }

    fn peek(&self) -> Result<Value, RuntimeError> {
        self.gc_root.exec_stack_peek().ok_or_else(|| { self.error("Cannot peek empty stack".to_string()) })
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.gc_root.exec_stack_pop().ok_or_else(|| { self.error("Cannot pop empty stack".to_string()) })
    }

    /*fn cleanup_envs(&mut self) {
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
    }*/
}

fn make_builtins(gc_root: &mut GcRoot) {
    make_ops(gc_root);

    let println = FunctionValue::from_closure(|int| {
        let v = int.pop()?;
        println!("{}", v);
        Ok(int.exec_stack.push(Value::Unit))
    }, 1);
    gc_root.put_name("println".to_string(), gc_root.allocate(println));
}

macro_rules! make_binary_op {
    ($oper: tt) => {
        FunctionValue::from_closure(|int| {
            let b = int.pop()?.require_number()?;
            let a = int.pop()?.require_number()?;
            Ok(int.exec_stack.push(Value::Number(a $oper b)))
        }, 2)
    };
}

fn make_ops(gc_root: &mut GcRoot) {
    gc_root.put_name("ops.plus".to_string(), gc_root.allocate(make_binary_op!(+)));
    gc_root.put_name("ops.minus".to_string(), gc_root.allocate(make_binary_op!(-)));
    gc_root.put_name("ops.times".to_string(), gc_root.allocate(make_binary_op!(*)));
    gc_root.put_name("ops.div".to_string(), gc_root.allocate(make_binary_op!(/)));

    let eq = FunctionValue::from_closure(|int| {
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
    }, 2);
    gc_root.put_name("ops.eq".to_string(), gc_root.allocate(eq));
}
