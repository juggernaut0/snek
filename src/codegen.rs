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
            self.code.add_op_code(Pop)
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
            unimplemented!("MakeNamespace"); // TODO
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
                match op {
                    UnaryOp::PLUS => self.gen_ops_call("unary_plus", 1),
                    UnaryOp::MINUS => self.gen_ops_call("unary_minus", 1),
                    UnaryOp::BANG => self.gen_ops_call("bang", 1),
                }
            },
            ExprType::Binary(op, e1, e2) => {
                self.gen_expr(e1);
                self.gen_expr(e2);
                // TODO compare ops
                // TODO control flow for && and ||
                match op {
                    BinaryOp::PLUS => self.gen_ops_call("plus", 2),
                    BinaryOp::MINUS => self.gen_ops_call("minus", 2),
                    BinaryOp::TIMES => self.gen_ops_call("times", 2),
                    BinaryOp::DIV => self.gen_ops_call("div", 2),
                    BinaryOp::LT => unimplemented!("comp ops"),
                    BinaryOp::GT => unimplemented!("comp ops"),
                    BinaryOp::LEQ => unimplemented!("comp ops"),
                    BinaryOp::GEQ => unimplemented!("comp ops"),
                    BinaryOp::EQ => self.gen_ops_call("eq", 2),
                    BinaryOp::NEQ => {
                        self.gen_ops_call("eq", 2);
                        self.gen_ops_call("bang", 1);
                    },
                    BinaryOp::AND => unimplemented!("logical ops"),
                    BinaryOp::OR => unimplemented!("logical ops"),
                }
            },
            ExprType::Call(ce) => {
                if let ExprType::QName(qn) = &ce.callee.expr_type {
                    if qn.parts.len() == 1 && &qn.parts[0] == "match" {
                        self.gen_match(ce);
                        return;
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
                    return;
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

    fn gen_ops_call(&mut self, name: &str, arity: u16) {
        // TODO dedup operator names to prevent unnecessary allocations
        self.code.add_op_code(LoadName(Rc::new(format!("ops.{}", name))));
        self.code.add_op_code(Call(arity));
    }

    fn gen_match(&mut self, expr: &CallExpr) {
        if expr.args.len() == 0 {
            self.base.error("Match expression must have a parameter");
            return;
        }

        self.gen_expr(&expr.args[0]);

        let mut labels = Vec::new();
        let mut ends = Vec::new();
        for case in &expr.args[1..] {
            self.code.set_line(case.line);
            for l in &labels {
                self.code.attach_label(*l);
            }
            labels.clear();
            if let ExprType::Lambda(le) = &case.expr_type {
                if le.params.len() != 1 {
                    self.base.error("Every case in a match expression must have exactly one parameter");
                    continue;
                }
                let pattern = &le.params[0];
                match pattern {
                    Pattern::Wildcard => {},
                    Pattern::Name(_) => {
                        unimplemented!()
                    },
                    Pattern::Constant(l) => {
                        self.code.add_op_code(Duplicate);
                        self.gen_literal(l);
                        self.gen_ops_call("eq", 2);
                        let l = self.code.add_jump_op(JumpIfFalse(0));
                        labels.push(l);
                    },
                    Pattern::Type(_, _) => {
                        unimplemented!("type matching") // TODO
                    },
                    Pattern::List(_) => {
                        unimplemented!("type matching")
                    },
                }
                self.code.add_op_code(Pop);
                for b in &le.bindings {
                    self.gen_binding(b);
                }
                self.gen_expr(&le.expr);
                let l = self.code.add_jump_op(Jump(0));
                ends.push(l);
            } else {
                self.base.error("Every case in a match expression must be a lambda expression");
            }
        }
        for l in labels {
            self.code.attach_label(l);
        }
        self.code.add_op_code(Fail(Rc::new("No match".to_string())));
        for l in ends {
            self.code.attach_label(l);
        }
        self.code.add_op_code(NoOp);
    }

    fn gen_name(&mut self, qname: &Expr) {
        if let Some((id, name)) = self.base.resolver.get_usage(qname) {
            self.code.add_op_code(LoadLocal(*id));
            self.code.set_local_name(*id, name.clone())
        } else if let ExprType::QName(qn) = &qname.expr_type {
            self.code.add_op_code(LoadName(Rc::new(qn.parts.join("."))));
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