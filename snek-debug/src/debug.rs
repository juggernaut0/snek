use snek::ast::*;
use std::fmt::{Display, Error, Formatter};
use snek::resolver::{GlobalId, LocalId};
use snek::resolver::irt::{self, Constant, IrtNode, IrTree, IrtVisitor, Save, Statement};
use snek::util::join_map;

trait DebugPrinter {
    fn indent(&self) -> usize;
    fn increase_indent(&mut self);
    fn decrease_indent(&mut self);

    fn print(&self, s: &str) {
        let sp = "  ".repeat(self.indent());
        println!("{}{}", sp, s);
    }

    fn print_open(&mut self, s: &str) {
        self.print(&format!("{} {{", s));
        self.increase_indent();
    }

    fn print_close(&mut self) {
        self.decrease_indent();
        self.print("}")
    }

    fn print_one<T: DebugPrinterNode<Self>>(&mut self, item: &T) {
        item.print(self);
    }

    fn print_all<T: DebugPrinterNode<Self>>(&mut self, items: &[T]) {
        for item in items {
            item.print(self)
        }
    }
}

trait DebugPrinterNode<T: DebugPrinter + ?Sized> {
    fn print(&self, printer: &mut T);
}

// === AST ===

pub struct AstPrinter {
    indent: usize
}

impl DebugPrinter for AstPrinter {
    fn indent(&self) -> usize {
        self.indent
    }

    fn increase_indent(&mut self) {
        self.indent += 1;
    }

    fn decrease_indent(&mut self) {
        self.indent -= 1;
    }
}

impl<T: AstNode> DebugPrinterNode<AstPrinter> for T {
    fn print(&self, printer: &mut AstPrinter) {
        self.print(printer)
    }
}

impl AstPrinter {
    pub fn new() -> AstPrinter {
        AstPrinter {
            indent: 0
        }
    }

    pub fn print_ast(&mut self, a: &Ast) {
        self.print_open("AST");
        self.print_all(&a.imports);
        self.print_namespace(&a.root_namespace);
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
        self.print_open(&format!("type {} public = {}", TypeNameDeclW(&type_decl.name), type_decl.public));
        match &type_decl.contents {
            TypeContents::Record(fields) => self.print_all(fields),
            TypeContents::Union(cases) => self.print_all(cases),
        }
        self.print_close();
    }

    fn print_case(&mut self, type_case: &TypeCase) {
        match type_case {
            TypeCase::Record(record) => {
                self.print_open(&format!("case type {} public = {}", TypeNameDeclW(&record.name), record.public));
                self.print_all(&record.fields);
                self.print_close();
            },
            TypeCase::Case(name) => self.print(&format!("case {}", TypeNameW(name))),
        }
    }

    fn print_field(&mut self, field: &TypeField) {
        self.print(&format!("field {} : {} public = {}", field.name, TypeNameW(&field.type_name), field.public))
    }

    fn print_binding(&mut self, binding: &Binding) {
        self.print_open(&format!("binding public = {}", binding.public));
        self.print_pattern(&binding.pattern);
        self.print_expr(&binding.expr);
        self.print_close();
    }

    fn print_pattern(&mut self, pattern: &Pattern) {
        match &pattern.pattern {
            PatternType::Wildcard(tn) => self.print(&format!("pattern wildcard type = {}", opt_display(&tn.as_ref().map(TypeNameW)))),
            PatternType::Name(n) => self.print(&format!("pattern name = {} type = {}", n.name, opt_display(&n.type_name.as_ref().map(TypeNameW)))),
            PatternType::Constant(l) => {
                self.print_open("pattern constant");
                self.print_literal(l);
                self.print_close();
            },
            PatternType::List(nested) => {
                self.print_open("pattern list");
                self.print_all(nested);
                self.print_close();
            }
            PatternType::Destruct(nested, tn) => {
                self.print_open(&format!("pattern destructure type = {}", opt_display(&tn.as_ref().map(TypeNameW))));
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
                self.print_open(&format!("new {}", opt_display(&nt.as_ref().map(NamedTypeW))));
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
            ExprType::Match => {
                self.print("match")
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

struct TypeNameDeclW<'a>(&'a TypeNameDecl);
impl Display for TypeNameDeclW<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.0.type_params.is_empty() {
            write!(f, "{}", self.0.name)
        } else {
            write!(f, "{}<{}>", self.0.name, self.0.type_params.iter().map(|it| it.to_string()).collect::<Vec<String>>().join(" "))
        }
    }
}

struct TypeNameW<'a>(&'a TypeName);
impl Display for TypeNameW<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match &self.0.type_name_type {
            TypeNameType::Named(name) => NamedTypeW(name).fmt(f),
            TypeNameType::Func(func) => {
                write!(f, "{{ {} -> {} }}", func.params.iter().map(|it| TypeNameW(it).to_string()).collect::<Vec<String>>().join(" "), TypeNameW(&func.return_type))
            }
            TypeNameType::Any => write!(f, "*"),
            TypeNameType::Unit => write!(f, "()"),
            TypeNameType::Nothing => write!(f, "!"),
            TypeNameType::Inferred => write!(f, "_")
        }
    }
}

struct NamedTypeW<'a>(&'a NamedType);
impl Display for NamedTypeW<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if self.0.type_args.is_empty() {
            write!(f, "{}", self.0.name)
        } else {
            write!(f, "{}<{}>", self.0.name, self.0.type_args.iter().map(|it| TypeNameW(it).to_string()).collect::<Vec<String>>().join(" "))
        }
    }
}

fn opt_display<T : Display>(ot: &Option<T>) -> String {
    match ot {
        None => "None".to_string(),
        Some(t) => format!("Some({})", t),
    }
}

// === IrTree ===

pub struct IrPrinter {
    indent: usize,
}

impl IrPrinter {
    pub fn new() -> IrPrinter {
        IrPrinter {
            indent: 0,
        }
    }
}

impl DebugPrinter for IrPrinter {
    fn indent(&self) -> usize {
        self.indent
    }

    fn increase_indent(&mut self) {
        self.indent += 1;
    }

    fn decrease_indent(&mut self) {
        self.indent -= 1;
    }
}

impl<T: IrtNode> DebugPrinterNode<IrPrinter> for T {
    fn print(&self, printer: &mut IrPrinter) {
        self.accept(printer)
    }
}

impl IrtVisitor for IrPrinter {
    fn visit_ir_tree(&mut self, tree: &IrTree) {
        self.print_open("IR");

        for func in tree.decls.functions() {
            self.print_open(&format!("Function {}", func.id().id()));
            self.print_all(func.statements());
            self.print_close();
        }

        self.print_all(&tree.statements);
        self.print_close();
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::SaveGlobal(save, expr) => {
                self.print_open("Save global");
                self.print_one(save);
                self.print_one(expr);
                self.print_close();
            }
            Statement::SaveLocal(save, expr) => {
                self.print_open("Save local");
                self.print_one(save);
                self.print_one(expr);
                self.print_close();
            }
            Statement::Discard(expr) => {
                self.print_open("Discard");
                self.print_one(expr);
                self.print_close();
            },
            Statement::Return(expr) => {
                self.print_open("Return");
                self.print_one(expr);
                self.print_close();
            }
        }
    }

    fn visit_save_global(&mut self, save: &Save<GlobalId>) {
        for (path, id) in &save.paths {
            self.print(&format!("{} -> {}", path, id.fqn()));
        }
    }

    fn visit_save_local(&mut self, save: &Save<LocalId>) {
        for (path, id) in &save.paths {
            self.print(&format!("{} -> {}", path, id.0));
        }
    }

    fn visit_expr(&mut self, expr: &snek::resolver::irt::Expr) {
        match &expr.expr_type {
            irt::ExprType::Error => {}
            irt::ExprType::LoadConstant(irt::Constant::Unit) => self.print("()"),
            irt::ExprType::LoadConstant(irt::Constant::Boolean(b)) => self.print(&b.to_string()),
            irt::ExprType::LoadConstant(irt::Constant::String(s)) => self.print(&format!("{:?}", s)),
            irt::ExprType::LoadConstant(irt::Constant::Number(f)) => self.print(&f.to_string()),
            irt::ExprType::LoadGlobal(g) => self.print(&format!("global {}: {}", g.fqn(), expr.resolved_type)),
            irt::ExprType::LoadLocal(l) => self.print(&format!("local {}: {}", l.0, expr.resolved_type)),
            irt::ExprType::Call { callee, args } => {
                self.print_open(&format!("Call: {}", expr.resolved_type));
                self.print_one(callee.as_ref());
                self.print_all(args);
                self.print_close();

            }
            irt::ExprType::Binary { left, right, op } => {
                let op_str = match op {
                    irt::BinaryOp::Error => "?",
                    irt::BinaryOp::Eq => "equality",
                    irt::BinaryOp::Neq => "inequality",
                    irt::BinaryOp::LessThan => "less than",
                    irt::BinaryOp::LessEq => "less than or equal",
                    irt::BinaryOp::GreaterThan => "greater than",
                    irt::BinaryOp::GreaterEq => "greater than or equal",
                    irt::BinaryOp::NumberAdd => "add",
                    irt::BinaryOp::NumberSub => "subtract",
                    irt::BinaryOp::NumberMul => "multiply",
                    irt::BinaryOp::NumberDiv => "divide",
                    irt::BinaryOp::StringConcat => "concat",
                };
                self.print_open(&format!("Binary {}: {}", op_str, expr.resolved_type));
                self.print_one(left.as_ref());
                self.print_one(right.as_ref());
                self.print_close();
            }
            irt::ExprType::LoadParam => {
                self.print("Pop param")
            }
            irt::ExprType::Func(id) => {
                self.print(&format!("Function {}: {}", id.id(), expr.resolved_type));
            }
            irt::ExprType::New { field_inits } => {
                self.print_open(&format!("New: {}", expr.resolved_type));
                for (field_name, expr) in field_inits {
                    self.print_open(field_name);
                    self.print_one(expr);
                    self.print_close();
                }
                self.print_close();
            }
            irt::ExprType::Match { expr: matched_expr, arms } => {
                self.print_open(&format!("Match: {}", expr.resolved_type));
                self.print_one(matched_expr.as_ref());
                for (pattern, arm) in arms {
                    self.print_open(&format!("{}", PatternTypeW(&pattern.pattern_type)));
                    self.print_all(arm);
                    self.print_close();
                }
                self.print_close();
            }
        }
    }
}

struct PatternTypeW<'a>(&'a snek::resolver::PatternType);
impl Display for PatternTypeW<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            snek::resolver::PatternType::Discard => write!(f, "_"),
            snek::resolver::PatternType::Constant(c) => {
                match c {
                    Constant::Unit => write!(f, "()"),
                    Constant::Number(n) => write!(f, "{}", n),
                    Constant::String(s) => write!(f, "{}", s),
                    Constant::Boolean(b) => write!(f, "{}", b),
                }
            },
            snek::resolver::PatternType::Name(name) => write!(f, "{}", name),
            snek::resolver::PatternType::Destructuring(fields) => {
                write!(f, "{{")?;
                join_map(f, fields, ", ", |f, (name, pattern)| {
                    write!(f, "{} => {}", name, PatternTypeW(&pattern.pattern_type))
                })?;
                write!(f, "}}")
            }
        }
    }
}
