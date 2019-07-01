use crate::ast::*;
use std::fmt::{Display, Error, Formatter};

pub struct AstPrinter {
    indent: usize
}

impl AstPrinter {
    pub fn new() -> AstPrinter {
        AstPrinter {
            indent: 0
        }
    }

    fn print(&self, s: &str) {
        let sp = "  ".repeat(self.indent);
        println!("{}{}", sp, s);
    }

    fn print_open(&mut self, s: &str) {
        self.print(&format!("{} {{", s));
        self.indent += 1;
    }

    fn print_close(&mut self) {
        self.indent -= 1;
        self.print("}")
    }

    fn print_all<T: AstNode>(&mut self, items: &Vec<T>) {
        for item in items {
            item.print(self)
        }
    }

    pub fn print_ast(&mut self, a: &Ast) {
        self.print_open("AST");
        self.print_all(&a.imports);
        self.print_namespace(&a.root_namespace);
        if let Some(e) = &a.expr {
            self.print_expr(e);
        }
        self.print_close();
    }

    fn print_import(&mut self, import: &Import) {
        self.print_open(&format!("import from {}", import.filename));
        self.print_all(&import.names);
        self.print_close();
    }

    fn print_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Namespace(ns) => self.print_namespace(ns),
            Decl::Type(t) => self.print_type(t),
            Decl::Binding(b) => self.print_binding(b),
        }
    }

    fn print_namespace(&mut self, namespace: &Namespace) {
        self.print_open(&format!("namespace {} public = {}", namespace.name, namespace.public));
        self.print_all(&namespace.decls);
        self.print_close();
    }

    fn print_type(&mut self, type_decl: &Type) {
        self.print_open(&format!("type {} public = {}", type_decl.name, type_decl.public));
        self.print_all(&type_decl.cases);
        if let Some(ns) = &type_decl.namespace {
            self.print_namespace(ns);
        }
        self.print_close();
    }

    fn print_case(&mut self, type_case: &TypeCase) {
        self.print(&format!("case {}({})", type_case.name, type_case.num_params));
    }

    fn print_binding(&mut self, binding: &Binding) {
        self.print_open(&format!("binding public = {}", binding.public));
        self.print_pattern(&binding.pattern);
        self.print_expr(&binding.expr);
        self.print_close();
    }

    fn print_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => self.print("pattern wildcard"),
            Pattern::Name(n) => self.print(&format!("pattern name = {}", n.name)),
            Pattern::Constant(l) => {
                self.print_open("pattern constant");
                self.print_literal(l);
                self.print_close();
            },
            Pattern::Type(qname, nested) => {
                self.print_open(&format!("pattern type name = {}", qname));
                self.print_all(nested);
                self.print_close();
            }
            Pattern::List(nested) => {
                self.print_open("pattern list");
                self.print_all(nested);
                self.print_close();
            }
        }
    }

    fn print_expr(&mut self, expr: &Expr) {
        match &expr.expr_type {
            ExprType::QName(qn) => self.print_qname(qn),
            ExprType::Constant(l) => self.print_literal(l),
            ExprType::Unary(op, e1) => {
                self.print_open(&format!("unary op = {:?}", op));
                self.print_expr(e1);
                self.print_close();
            },
            ExprType::Binary(op, e1, e2) => {
                self.print_open(&format!("binary op = {:?}", op));
                self.print_expr(e1);
                self.print_expr(e2);
                self.print_close();
            },
            ExprType::Call(ce) => {
                self.print_open("call");
                self.print_expr(&ce.callee);
                self.print_all(&ce.args);
                self.print_close();
            },
            ExprType::Lambda(le) => {
                self.print_open("lambda");
                self.print_all(&le.params);
                self.print_all(&le.bindings);
                self.print_expr(&le.expr);
                self.print_close();
            },
            ExprType::List(es) => {
                self.print_open("list");
                self.print_all(es);
                self.print_close();
            },
            ExprType::Dot => {
                self.print("dot")
            }
        }
    }

    fn print_literal(&mut self, literal: &Literal) {
        self.print(&format!("literal type = {:?} value = {}", literal.lit_type, literal.value));
    }

    fn print_qname(&self, qname: &QName) {
        self.print(&qname.to_string())
    }
}

trait AstNode {
    fn print(&self, printer: &mut AstPrinter);
}

impl AstNode for Import {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_import(self);
    }
}

impl AstNode for Decl {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_decl(self);
    }
}

impl AstNode for TypeCase {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_case(self);
    }
}

impl AstNode for QName {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_qname(self);
    }
}

impl AstNode for Expr {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_expr(self);
    }
}

impl AstNode for Pattern {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_pattern(self);
    }
}

impl AstNode for Binding {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_binding(self);
    }
}

impl Display for QName {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.parts.join("."))
    }
}
