use crate::opcode::Code;
use crate::opcode::OpCode::*;
use crate::debug::print_opcode;

pub fn execute(code: &Code) {
    let mut ip: usize = 0;
    while ip < code.len() {
        let (opcode, d) = code.get_op_code(ip);
        ip += d;
        print_opcode(&opcode);
    }
}

struct Interpreter {

}
