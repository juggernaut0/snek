use crate::resolver::LocalId;
use crate::opcode::OpCode::*;

type StringId = u16;
type Name = Vec<String>;
type NameId = u16;
type LabelId = u16;
type ConstId = u16;
type TypeId = u16;

pub enum OpCode {
    NoOp,
    Fail(String),
    Pop,
    Duplicate,
    Swap,
    Decompose(NameId, u16),
    Jump(LabelId),
    Match(MatchPattern),
    JumpIfMatch(MatchPattern, u16),
    LoadLocal(LocalId),
    LoadName(NameId),
    LoadNumber(ConstId),
    LoadString(ConstId),
    SaveLocal(LocalId),
    SaveNamespace(StringId, NameId),
    Call(u16),
    TailCall,
    MakeClosure(ConstId),
    MakeNamespace(NameId, bool),
    MakeType(NameId, TypeId),
    ImportAll(StringId),
    ImportNames(StringId, Vec<NameId>)
}

enum MatchPattern {
    // TODO
}

#[derive(Default)]
pub struct Code {
    ops: Vec<u16>,
    strings: Vec<String>,
    names: Vec<Name>,
    //constants: Vec<Value>,
    //types: Vec<TypeDecl>,
}

impl Code {
    pub fn new() -> Code {
        Code::default()
    }

    pub fn add_op_code(&mut self, op_code: OpCode) {
        match op_code {
            NoOp => self.add_op(0),
            Fail(msg) => {
                self.add_op(1);
                self.add_string_op(msg);
            },
            _ => unimplemented!("add_op")
        }
    }

    fn add_op(&mut self, op: u16) {
        self.ops.push(op)
    }

    fn add_string_op(&mut self, s: String) {
        if self.strings.len() >= std::u16::MAX as usize {
            panic!("Too many strings in Code")
        }
        self.add_op(self.strings.len() as u16);
        self.strings.push(s);
    }

    pub fn finalize(&mut self) {
        self.ops.shrink_to_fit();
        self.strings.shrink_to_fit();
        self.names.shrink_to_fit();
    }
}
