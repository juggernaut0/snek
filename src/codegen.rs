use crate::opcode::{Code, CodeBuilder};
use crate::ast::*;
use crate::resolver::Resolver;
use crate::opcode::OpCode::{MakeNamespace, Pop, LoadConstant};
use std::rc::Rc;
use crate::value::Value;
use std::str::ParseBoolError;

pub fn compile(ast: &Ast) -> Result<Code, Vec<CompileError>> {
    CodeGenerator::new(Resolver::new(ast)).compile(ast)
}

pub struct CompileError {
    message: String,
    line: u32
}

type CodeGenResult = Result<(), CompileError>;

struct CodeGenerator {
    resolver: Resolver,
    code: CodeBuilder,
    errors: Vec<CompileError>
}

impl CodeGenerator {
    fn new(resolver: Resolver) -> CodeGenerator {
        CodeGenerator {
            resolver,
            code: CodeBuilder::new(),
            errors: Vec::new()
        }
    }

    fn error(&mut self, message: &str) {
        self.errors.push(CompileError { message: message.to_string(), line: 0 });
    }

    fn compile(mut self, ast: &Ast) -> Result<Code, Vec<CompileError>> {
        for import in &ast.imports {
            self.gen_import(import);
        }
        self.gen_namespace(&ast.root_namespace, &Vec::new());
        if let Some(expr) = &ast.expr {
            self.gen_expr(expr);
        }
        if self.errors.is_empty() {
            Ok(self.code.finalize())
        } else {
            Err(self.errors)
        }
    }

    fn gen_import(&mut self, import: &Import) {
        unimplemented!("gen_import")
    }

    fn gen_decl(&mut self, decl: &Decl, parent: &Vec<String>) {
        match decl {
            Decl::Namespace(ns) => self.gen_namespace(ns, parent),
            Decl::Type(ty) =>  self.gen_type(ty, parent),
            Decl::Binding(b) => self.gen_binding(b)
        }
    }

    fn gen_namespace(&mut self, namespace: &Namespace, parent: &Vec<String>) {
        let mut name = Vec::new();
        name.clone_from(parent);
        name.clone_from(&namespace.name.parts);
        let name = Rc::new(name);

        if !name.is_empty() {
            self.code.add_op_code(MakeNamespace(Rc::clone(&name), namespace.public));
        }
        for decl in &namespace.decls {
            self.gen_decl(decl, &name);
        }
    }

    fn gen_type(&mut self, type_decl: &Type, parent: &Vec<String>) {
        unimplemented!("gen_type")
    }

    fn gen_binding(&mut self, binding: &Binding) {
        self.gen_expr(&binding.expr);
        if binding.public {
            unimplemented!("public bindings")
        } else {
            self.gen_pattern(&binding.pattern)
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match &expr.expr_type {
            ExprType::QName(qn) => self.gen_name(qn),
            ExprType::Constant(lit) => self.gen_literal(lit),
            ExprType::Unary(op, e) => {
                self.gen_expr(e);
                // TODO gen unary op
            }
            ExprType::Binary(op, e1, e2) => {
                self.gen_expr(e1);
                self.gen_expr(e2);
                self.code.add_op_code(Pop) // TODO add binary op
            }
            _ => unimplemented!("gen_expr")
        }
    }

    fn gen_name(&mut self, qname: &QName) {
        unimplemented!("gen_name")
    }

    fn gen_literal(&mut self, literal: &Literal) {
        match literal.lit_type {
            LiteralType::NUMBER => {
                let val = match literal.value.parse::<f64>() {
                    Ok(n) => n,
                    Err(e) => {
                        self.error(&e.to_string());
                        0.0
                    }
                };
                self.code.add_op_code(LoadConstant(Rc::new(Value::Number(val))))
            },
            LiteralType::STRING => {
                unimplemented!("string literal")
            },
            LiteralType::BOOL => {
                let val = match literal.value.parse::<bool>() {
                    Ok(n) => n,
                    Err(e) => {
                        self.error(&e.to_string());
                        false
                    }
                };
                self.code.add_op_code(LoadConstant(Rc::new(Value::Boolean(val))))
            },
            LiteralType::UNIT => {
                self.code.add_op_code(LoadConstant(Rc::new(Value::Unit)))
            },
        }
    }

    fn gen_pattern(&mut self, pattern: &Pattern) {
        unimplemented!("gen_pattern")
    }
}
