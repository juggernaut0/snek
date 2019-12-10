use std::collections::HashMap;
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;

use crate::opcode::OpCode::*;
use crate::resolver::LocalId;

type Name = String;

#[derive(Debug)]
pub enum OpCode {
    NoOp,
    Fail(Rc<String>),
    Pop,
    Duplicate,
    Swap,
    TypeCheck(Rc<Name>),
    Decompose(Rc<Name>, u16),
    Jump(u16),
    JumpIfFalse(u16),
    LoadLocal(LocalId),
    LoadName(Rc<Name>),
    LoadConstant(ConstantValue),
    SaveLocal(LocalId),
    SaveNamespace(Rc<String>, Rc<Name>),
    Call(u16),
    TailCall,
    MakeClosure(Rc<Code>, u16),
    MakeNamespace(Rc<Name>, bool),
    /*MakeType(NameId, TypeId),
    ImportAll(StringId),
    ImportNames(StringId, Vec<NameId>)*/
}

const NO_OP_CODE: u16 = 0;
const FAIL_CODE: u16 = 1;
const POP_CODE: u16 = 2;
const DUPLICATE_CODE: u16 = 3;
const SWAP_CODE: u16 = 4;
const TYPE_CHECK_CODE: u16 = 5;
const DECOMPOSE_CODE: u16 = 6;
const JUMP_CODE: u16 = 7;
const JUMP_IF_FALSE_CODE: u16 = 8;
const LOAD_LOCAL_CODE: u16 = 9;
const LOAD_NAME_CODE: u16 = 10;
const LOAD_CONSTANT_CODE: u16 = 11;
const SAVE_LOCAL_CODE: u16 = 12;
const SAVE_NAMESPACE_CODE: u16 = 13;
const CALL_CODE: u16 = 14;
const TAIL_CALL_CODE: u16 = 15;
const MAKE_CLOSURE_CODE: u16 = 16;
const MAKE_NAMESPACE_CODE: u16 = 17;
const MAKE_TYPE_CODE: u16 = 18;
const IMPORT_ALL_CODE: u16 = 19;
const IMPORT_NAMES_CODE: u16 = 20;

#[derive(Default)]
pub struct CodeBuilder {
    ops: Vec<u16>,
    lines: Vec<(usize, u32)>,
    current_line: (usize, u32),
    strings: Vec<Rc<String>>,
    names: Vec<Rc<Name>>,
    constants: Vec<ConstantValue>,
    codes: Vec<Rc<Code>>,
    //types: Vec<TypeDecl>,
    labels: Vec<usize>, // labelId to ip
    locals: HashMap<LocalId, String>,
}

impl CodeBuilder {
    pub fn new() -> CodeBuilder {
        CodeBuilder::default()
    }

    pub fn add_op_code(&mut self, op_code: OpCode) {
        match op_code {
            NoOp => self.add_op(NO_OP_CODE),
            Fail(msg) => {
                self.add_op(FAIL_CODE);
                self.add_string_op(msg);
            },
            Pop => self.add_op(POP_CODE),
            Duplicate => self.add_op(DUPLICATE_CODE),
            Swap => self.add_op(SWAP_CODE),
            Jump(d) => {
                self.add_op(JUMP_CODE);
                self.add_op(d);
            },
            JumpIfFalse(d) => {
                self.add_op(JUMP_IF_FALSE_CODE);
                self.add_op(d);
            }
            LoadLocal(id) => {
                self.add_op(LOAD_LOCAL_CODE);
                self.add_op(id);
            },
            LoadName(name) => {
                self.add_op(LOAD_NAME_CODE);
                self.add_name_op(name);
            },
            LoadConstant(value) => {
                self.add_op(LOAD_CONSTANT_CODE);
                self.add_constant_op(value);
            },
            SaveLocal(id) => {
                self.add_op(SAVE_LOCAL_CODE);
                self.add_op(id);
            },
            Call(nargs) => {
                self.add_op(CALL_CODE);
                self.add_op(nargs);
            },
            MakeClosure(code, nparams) => {
                self.add_op(MAKE_CLOSURE_CODE);
                self.add_code_op(code);
                self.add_op(nparams);
            },
            _ => unimplemented!("add_op_code")
        }
    }

    fn add_op(&mut self, op: u16) {
        self.ops.push(op)
    }

    fn add_string_op(&mut self, s: Rc<String>) {
        if self.strings.len() >= std::u16::MAX as usize {
            panic!("Too many strings in Code") // TODO replace these panics with CompileError
        }
        self.add_op(self.strings.len() as u16);
        self.strings.push(s);
    }

    fn add_name_op(&mut self, n: Rc<Name>) {
        if let Some((i, _)) = self.names.iter().enumerate().find(|(_, e)| e == &&n) {
            self.add_op(i as u16);
        } else {
            if self.names.len() >= std::u16::MAX as usize {
                panic!("Too many names in Code")
            }
            self.add_op(self.names.len() as u16);
            self.names.push(n);
        }
    }

    fn add_constant_op(&mut self, c: ConstantValue) {
        if self.constants.len() >= std::u16::MAX as usize {
            panic!("Too many constants in Code")
        }
        self.add_op(self.constants.len() as u16);
        self.constants.push(c);
    }

    fn add_code_op(&mut self, code: Rc<Code>) {
        if self.codes.len() >= std::u16::MAX as usize {
            panic!("Too many functions in Code")
        }
        self.add_op(self.codes.len() as u16);
        self.codes.push(code);
    }

    pub fn set_line(&mut self, line: u32) {
        let len = self.ops.len();
        if len == self.current_line.0 { // no ops have been added since last set
            self.current_line.1 = line // overwrite line
        } else if line != self.current_line.1 {
            self.lines.push(self.current_line);
            self.current_line = (len, line)
        }
    }

    pub fn add_jump_op(&mut self, jump_op: OpCode) -> usize {
        let i = self.ops.len() + 1;
        self.add_op_code(jump_op);
        i
    }

    pub fn attach_label(&mut self, label: usize) {
        let d = self.ops.len() - label - 1;
        if d > std::u16::MAX as usize {
            panic!("Too far to jump")
        }
        self.ops[label] = d as u16;
    }

    pub fn set_local_name(&mut self, id: LocalId, name: String) {
        self.locals.insert(id, name);
    }

    pub fn finalize(mut self) -> Code {
        self.ops.shrink_to_fit();
        self.lines.push(self.current_line);
        self.lines.shrink_to_fit();
        self.strings.shrink_to_fit();
        self.names.shrink_to_fit();
        self.constants.shrink_to_fit();
        self.codes.shrink_to_fit();
        self.locals.shrink_to_fit();
        // TODO populate locals_offset & locals_size
        Code {
            ops: self.ops,
            lines: self.lines,
            strings: self.strings,
            names: self.names,
            constants: self.constants,
            codes: self.codes,
            locals: self.locals,
        }
    }
}

pub struct Code {
    ops: Vec<u16>,
    lines: Vec<(usize, u32)>,
    strings: Vec<Rc<String>>,
    names: Vec<Rc<Name>>,
    constants: Vec<ConstantValue>,
    codes: Vec<Rc<Code>>,
    locals: HashMap<LocalId, String>,
}

impl Code {
    pub fn get_op_code(&self, index: usize) -> (OpCode, usize) {
        let opcode = self.ops[index];
        match opcode {
            NO_OP_CODE => (NoOp, 1),
            FAIL_CODE => {
                let msg = Rc::clone(self.get_arg(&self.strings, index, 1));
                (Fail(msg), 2)
            },
            POP_CODE => (Pop, 1),
            DUPLICATE_CODE => (Duplicate, 1),
            JUMP_CODE => {
                let d = self.ops[index + 1];
                (Jump(d), 2)
            }
            JUMP_IF_FALSE_CODE => {
                let d = self.ops[index + 1];
                (JumpIfFalse(d), 2)
            }
            LOAD_LOCAL_CODE => {
                let id = self.ops[index + 1];
                (LoadLocal(id), 2)
            }
            LOAD_NAME_CODE => {
                let name = Rc::clone(self.get_arg(&self.names, index, 1));
                (LoadName(name), 2)
            },
            LOAD_CONSTANT_CODE => {
                let v = self.get_arg(&self.constants, index, 1).clone();
                (LoadConstant(v), 2)
            },
            SAVE_LOCAL_CODE => {
                let id = self.ops[index + 1];
                (SaveLocal(id), 2)
            }
            CALL_CODE => {
                let nargs = self.ops[index + 1];
                (Call(nargs), 2)
            },
            MAKE_CLOSURE_CODE => {
                let code = Rc::clone(self.get_arg(&self.codes, index, 1));
                let nparams = self.ops[index + 2];
                (MakeClosure(code, nparams), 3)
            }
            _ => unimplemented!("get_op_code: {}", opcode)
        }
    }

    pub fn get_line(&self, index: usize) -> u32 {
        let mut iter = self.lines.iter();
        let mut last = iter.next().unwrap().1; // guaranteed to have at least one element
        loop {
            if let Some((i, line)) = iter.next() {
                if *i == index {
                    return *line
                } else if *i < index {
                    last = *line;
                    continue
                }
            }
            return last
        }
    }

    pub fn len(&self) -> usize {
        self.ops.len()
    }

    pub fn get_local_name(&self, local: LocalId) -> &String {
        self.locals.get(&local).unwrap()
    }

    fn get_arg<'a, T>(&self, vec: &'a Vec<T>, index: usize, offset: usize) -> &'a T {
        &vec[self.ops[index + offset] as usize]
    }
}

impl Debug for Code {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<Code>")
    }
}

#[derive(Clone)]
pub enum ConstantValue {
    Unit,
    Number(f64),
    Boolean(bool),
    String(String),
}
