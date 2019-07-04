use std::collections::HashMap;
use std::rc::Rc;

use crate::opcode::OpCode::*;
use crate::resolver::LocalId;
use crate::value::Value;

type StringId = u16;
type Name = Vec<String>;
type LabelId = u16;
type TypeId = u16;

#[derive(Debug)]
pub enum OpCode {
    NoOp,
    Fail(Rc<String>),
    Pop,
    Duplicate,
    Swap,
    Decompose(Rc<Name>, u16),
    Jump(LabelId),
    /*Match(MatchPattern),
    JumpIfMatch(MatchPattern, u16),*/
    LoadLocal(Rc<String>, LocalId),
    LoadName(Rc<Name>),
    LoadConstant(Value),
    SaveLocal(LocalId),
    /*SaveNamespace(StringId, NameId),*/
    Call(u16),
    /*TailCall,
    MakeClosure(ConstId),*/
    MakeNamespace(Rc<Name>, bool),
    /*MakeType(NameId, TypeId),
    ImportAll(StringId),
    ImportNames(StringId, Vec<NameId>)*/
}

enum MatchPattern {
    // TODO
}

#[derive(Default)]
pub struct CodeBuilder {
    ops: Vec<u16>,
    strings: Vec<Rc<String>>,
    names: Vec<Rc<Name>>,
    constants: Vec<Value>,
    //types: Vec<TypeDecl>,
    labels: HashMap<LabelId, usize>, // labelId to index in ops
    locals: HashMap<LocalId, Rc<String>>,
    label_seq: u16
}

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder::default()
    }

    pub fn add_op_code(&mut self, op_code: OpCode) {
        match op_code {
            NoOp => self.add_op(0),
            Fail(msg) => {
                self.add_op(1);
                self.add_string_op(msg);
            },
            Pop => self.add_op(2),
            Duplicate => self.add_op(3),
            Swap => self.add_op(4),
            Decompose(name, size) => {
                self.add_op(5);
                self.add_name_op(name);
                self.add_op(size);
            },
            Jump(label) => {
                self.add_op(5);
                self.add_op(label);
            },
            LoadLocal(name, id) => {
                self.add_op(8);
                self.add_op(id);
                self.locals.insert(id, name);
            },
            LoadName(name) => {
                self.add_op(9);
                self.add_name_op(name);
            }
            LoadConstant(value) => {
                self.add_op(10);
                self.add_constant_op(value);
            },
            SaveLocal(id) => {
                self.add_op(11);
                self.add_op(id);
            }
            Call(nargs) => {
                self.add_op(13);
                self.add_op(nargs);
            }
            _ => unimplemented!("add_op_code")
        }
    }

    fn add_op(&mut self, op: u16) {
        self.ops.push(op)
    }

    fn add_string_op(&mut self, s: Rc<String>) {
        if self.strings.len() >= std::u16::MAX as usize {
            panic!("Too many strings in Code")
        }
        self.add_op(self.strings.len() as u16);
        self.strings.push(s);
    }

    fn add_name_op(&mut self, n: Rc<Name>) {
        if let Some((i, _)) = self.names.iter().enumerate().find(|(i, e)| e == &&n) {
            self.add_op(i as u16);
        } else {
            if self.names.len() >= std::u16::MAX as usize {
                panic!("Too many names in Code")
            }
            self.add_op(self.names.len() as u16);
            self.names.push(n);
        }
    }

    fn add_constant_op(&mut self, c: Value) {
        if self.constants.len() >= std::u16::MAX as usize {
            panic!("Too many constants in Code")
        }
        self.add_op(self.constants.len() as u16);
        self.constants.push(c);
    }

    pub fn create_label(&mut self) -> LabelId {
        let id = self.label_seq;
        self.label_seq += 1;
        id
    }

    pub fn attach_label(&mut self, label: LabelId) {
        self.labels.insert(label, self.ops.len());
    }

    pub fn finalize(mut self) -> Code {
        self.ops.shrink_to_fit();
        self.strings.shrink_to_fit();
        self.names.shrink_to_fit();
        self.constants.shrink_to_fit();
        self.labels.shrink_to_fit();
        self.locals.shrink_to_fit();
        Code {
            ops: self.ops,
            strings: self.strings,
            names: self.names,
            constants: self.constants,
            labels: self.labels,
            locals: self.locals
        }
    }
}

pub struct Code {
    ops: Vec<u16>,
    strings: Vec<Rc<String>>,
    names: Vec<Rc<Name>>,
    constants: Vec<Value>,
    labels: HashMap<LabelId, usize>, // labelId to index in ops
    locals: HashMap<LocalId, Rc<String>>,
}

impl Code {
    pub fn get_op_code(&self, index: usize) -> (OpCode, usize) {
        let opcode = self.ops[index];
        match opcode {
            0 => (NoOp, 1),
            2 => (Pop, 1),
            8 => {
                let id = self.ops[index + 1];
                let name = Rc::clone(self.locals.get(&id).unwrap());
                (LoadLocal(name, id), 2)
            }
            9 => {
                let name = Rc::clone(self.get_arg(&self.names, index, 1));
                (LoadName(name), 2)
            },
            10 => {
                let v = self.get_arg(&self.constants, index, 1).clone();
                (LoadConstant(v), 2)
            },
            11 => {
                let id = self.ops[index + 1];
                (SaveLocal(id), 2)
            }
            13 => {
                let nargs = self.ops[index + 1];
                (Call(nargs), 2)
            }
            _ => unimplemented!("get_op_code: {}", opcode)
        }
    }

    pub fn len(&self) -> usize {
        self.ops.len()
    }

    fn get_arg<'a, T>(&self, vec: &'a Vec<T>, index: usize, offset: usize) -> &'a T {
        &vec[self.ops[index + offset] as usize]
    }
}
