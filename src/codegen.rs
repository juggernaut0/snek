use crate::opcode::{Code, CodeBuilder};
use crate::ast::*;
use crate::resolver::Resolver;
use crate::opcode::OpCode::*;
use std::rc::Rc;
use crate::value::Value;

pub fn compile(ast: &Ast) -> Result<Code, Vec<CompileError>> {
    AstCodeGenerator::new(Resolver::new(ast)).compile(ast)
}

pub struct CompileError {
    message: String,
    line: u32
}

impl CompileError {
    fn message(&self) -> &String {
        &self.message
    }

    fn line(&self) -> u32 {
        self.line
    }
}

type CodeGenResult = Result<(), CompileError>;

struct AstCodeGenerator {
    resolver: Resolver,
    errors: Vec<CompileError>
}

impl AstCodeGenerator {
    fn new(resolver: Resolver) -> AstCodeGenerator {
        AstCodeGenerator {
            resolver,
            errors: Vec::new()
        }
    }

    fn error(&mut self, message: &str) {
        self.errors.push(CompileError { message: message.to_string(), line: 0 });
    }

    fn compile(mut self, ast: &Ast) -> Result<Code, Vec<CompileError>> {
        let mut gen = CodeGenerator::from(&mut self);
        gen.gen_ast(ast);
        let code = gen.code;
        if self.errors.is_empty() {
            Ok(code.finalize())
        } else {
            Err(self.errors)
        }
    }
}

struct CodeGenerator<'a> {
    base: &'a mut AstCodeGenerator,
    code: CodeBuilder
}

impl<'a> CodeGenerator<'a> {
    fn from(base: &mut AstCodeGenerator) -> CodeGenerator {
        CodeGenerator {
            base,
            code: CodeBuilder::new()
        }
    }

    fn gen_ast(&mut self, ast: &Ast) {
        for import in &ast.imports {
            self.gen_import(import);
        }
        self.gen_namespace(&ast.root_namespace, &Vec::new());
        if let Some(expr) = &ast.expr {
            self.gen_expr(expr);
        }
    }

    fn gen_import(&mut self, import: &Import) {
        unimplemented!("gen_import") // TODO
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
        unimplemented!("gen_type") // TODO
    }

    fn gen_binding(&mut self, binding: &Binding) {
        self.gen_expr(&binding.expr);
        if binding.public {
            unimplemented!("public bindings") // TODO
        } else {
            self.gen_pattern(&binding.pattern)
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        self.code.set_line(expr.line);
        match &expr.expr_type {
            ExprType::QName(_) => self.gen_name(expr),
            ExprType::Constant(lit) => self.gen_literal(lit),
            ExprType::Unary(op, e) => {
                self.gen_expr(e);
                // TODO switch on ops
                // TODO dedup operator names
                self.code.add_op_code(LoadName(Rc::new(vec!("ops".to_string(), "unary_plus".to_string()))));
                self.code.add_op_code(Call(1));
            },
            ExprType::Binary(op, e1, e2) => {
                self.gen_expr(e1);
                self.gen_expr(e2);
                // TODO switch on ops
                // TODO dedup operator names
                // TODO control flow for && and ||
                self.code.add_op_code(LoadName(Rc::new(vec!("ops".to_string(), "plus".to_string()))));
                self.code.add_op_code(Call(2));
            },
            ExprType::Call(ce) => {
                if let ExprType::QName(qn) = &ce.callee.expr_type {
                    if qn.parts.len() == 1 && &qn.parts[0] == "match" {
                        unimplemented!("match expr") // TODO
                    }
                }
                for e in &ce.args {
                    self.gen_expr(e);
                }
                self.gen_expr(&ce.callee);
                self.code.add_op_code(Call(ce.args.len() as u16));
            },
            ExprType::Lambda(le) => {
                if le.params.len() > std::u16::MAX as usize {
                    self.base.error("Too many parameters for function");
                }
                let mut code = CodeGenerator::from(&mut self.base);
                for p in &le.params {
                    code.gen_pattern(p);
                }
                for b in &le.bindings {
                    code.gen_binding(b);
                }
                code.gen_expr(&le.expr);
                self.code.add_op_code(MakeClosure(Rc::new(code.code.finalize()), le.params.len() as u16))
            }
            _ => unimplemented!("gen_expr") // TODO
        }
    }

    fn gen_name(&mut self, qname: &Expr) {
        if let Some((id, name)) = self.base.resolver.get_usage(qname) {
            self.code.add_op_code(LoadLocal(Rc::clone(name), *id));
        } else if let ExprType::QName(qn) = &qname.expr_type {
            self.code.add_op_code(LoadName(Rc::new(qn.parts.clone())));
        } else {
            unreachable!()
        }
    }

    fn gen_literal(&mut self, literal: &Literal) {
        match literal.lit_type {
            LiteralType::NUMBER => {
                let val = match literal.value.parse::<f64>() {
                    Ok(n) => n,
                    Err(e) => {
                        self.base.error(&e.to_string());
                        0.0
                    }
                };
                self.code.add_op_code(LoadConstant(Value::Number(val)))
            },
            LiteralType::STRING => {
                let val = literal.value.trim_matches(|c| c == '\'' || c == '"').to_string();
                self.code.add_op_code(LoadConstant(Value::String(Rc::new(val))));
            },
            LiteralType::BOOL => {
                let val = match literal.value.parse::<bool>() {
                    Ok(n) => n,
                    Err(e) => {
                        self.base.error(&e.to_string());
                        false
                    }
                };
                self.code.add_op_code(LoadConstant(Value::Boolean(val)))
            },
            LiteralType::UNIT => {
                self.code.add_op_code(LoadConstant(Value::Unit))
            },
        }
    }

    fn gen_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => self.code.add_op_code(Pop),
            Pattern::Name(name) => {
                let id = self.base.resolver.get_declaration(name);
                self.code.add_op_code(SaveLocal(id));
            },
            _ => unimplemented!("gen_pattern") // TODO
        }
    }
}