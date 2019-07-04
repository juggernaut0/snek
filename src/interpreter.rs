use crate::opcode::Code;
use crate::opcode::OpCode::*;
use crate::value::Value;

pub fn execute(code: &Code) {
    Interpreter::new().run(code);
}

struct Interpreter {
    exec_stack: Vec<Value>
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            exec_stack: Vec::new()
        }
    }

    fn run(&mut self, code: &Code) {
        let mut ip: usize = 0;
        while ip < code.len() {
            let (opcode, d) = code.get_op_code(ip);
            ip += d;
            match opcode {
                NoOp => (),
                Pop => {
                    let _ = self.exec_stack.pop().expect("cannot pop empty stack");
                },
                LoadConstant(v) => self.exec_stack.push(v),
                _ => unimplemented!()
            }
        }
    }
}
