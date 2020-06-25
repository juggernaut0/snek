use crate::ast::*;
use std::fmt::{Display, Error, Formatter};
//use crate::opcode::{OpCode, Code};

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
        self.print_all(&import.names.iter().map(|it| &it.name).collect::<Vec<_>>());
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
        match &type_decl.contents {
            TypeContents::Record(fields) => self.print_all(fields),
            TypeContents::Union(cases) => self.print_all(cases),
        }
        self.print_close();
    }

    fn print_case(&mut self, type_case: &TypeCase) {
        match type_case {
            TypeCase::Record(record) => {
                self.print_open(&format!("case type {} public = {}", record.name, record.public));
                self.print_all(&record.fields);
                self.print_close();
            },
            TypeCase::Case(name) => self.print(&format!("case {}", name)),
        }
    }

    fn print_field(&mut self, field: &TypeField) {
        self.print(&format!("field {} : {} public = {}", field.name, field.type_name, field.public))
    }

    fn print_binding(&mut self, binding: &Binding) {
        self.print_open(&format!("binding public = {}", binding.public));
        self.print_pattern(&binding.pattern);
        self.print_expr(&binding.expr);
        self.print_close();
    }

    fn print_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard(tn) => self.print(&format!("pattern wildcard type = {}", opt_display(tn))),
            Pattern::Name(n) => self.print(&format!("pattern name = {} type = {}", n.name, opt_display(&n.type_name))),
            Pattern::Constant(l) => {
                self.print_open("pattern constant");
                self.print_literal(l);
                self.print_close();
            },
            Pattern::List(nested) => {
                self.print_open("pattern list");
                self.print_all(nested);
                self.print_close();
            }
            Pattern::Destruct(nested, tn) => {
                self.print_open(&format!("pattern destructure type = {}", opt_display(tn)));
                self.print_all(nested);
                self.print_close();
            }
        }
    }

    fn print_field_pattern(&mut self, pattern: &FieldPattern) {
        match pattern {
            FieldPattern::Name(n) => self.print(&format!("field name = {}", n.name)),
            FieldPattern::Binding(pattern, field) => {
                self.print_open(&format!("field binding = {}", field));
                self.print_pattern(pattern);
                self.print_close();
            },
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
            ExprType::New(nt, inits) => {
                self.print_open(&format!("new {}", opt_display(nt)));
                for init in inits {
                    self.print_open(&format!("field init {} = expr", init.field_name));
                    self.print_expr(&init.expr);
                    self.print_close();
                }
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

impl<T: AstNode> AstNode for &'_ T {
    fn print(&self, printer: &mut AstPrinter) {
        T::print(self, printer);
    }
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

impl AstNode for TypeField {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_field(self);
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

impl AstNode for FieldPattern {
    fn print(&self, printer: &mut AstPrinter) {
        printer.print_field_pattern(self);
    }
}

impl Display for QName {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.parts.join("."))
    }
}

impl Display for TypeNameDecl {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.type_params.is_empty() {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}<{}>", self.name, self.type_params.iter().map(|it| it.to_string()).collect::<Vec<String>>().join(" "))
        }
    }
}

impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match &self.type_name_type {
            TypeNameType::Named(name) => name.fmt(f),
            TypeNameType::Func(func) => {
                write!(f, "{{ {} -> {} }}", func.params.iter().map(|it| it.to_string()).collect::<Vec<String>>().join(" "), func.return_type)
            }
            TypeNameType::Any => write!(f, "*"),
            TypeNameType::Unit => write!(f, "()"),
            TypeNameType::Nothing => write!(f, "!"),
            TypeNameType::Inferred => write!(f, "_")
        }
    }
}

impl Display for NamedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if self.params.is_empty() {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}<{}>", self.name, self.params.iter().map(|it| it.to_string()).collect::<Vec<String>>().join(" "))
        }
    }
}

fn opt_display<T : Display>(ot: &Option<T>) -> String {
    match ot {
        None => "None".to_string(),
        Some(t) => format!("Some({})", t),
    }
}

/*pub fn print_code(code: &Code) {
    let mut ip: usize = 0;
    let mut last_line = 0;
    let mut funcs = Vec::new();
    while ip < code.len() {
        let (opcode, d) = code.get_op_code(ip);
        let line = code.get_line(ip);
        if last_line != line {
            last_line = line;
            println!("{}\t{}\t{:?}", line, ip, opcode);
        } else {
            println!("\t{}\t{:?}", ip, opcode);
        }
        ip += d;
        if let OpCode::MakeClosure(code, _) = opcode {
            funcs.push((code, line));
        }
    }
    for (c, line) in funcs {
        println!("\nFunction @ line {}", line);
        print_code(&c);
    }
}*/
