use std::borrow::Cow;
use std::ops::Range;
use snek::resolver::{get_builtin_id, GlobalId, ResolvedField, ResolvedType, TypeDeclaration, TypeDefinition, TypeId};
use snek::resolver::irt::*;

pub fn generate(irt: &IrTree) -> String {
    let mut generator = JsGenerator::new();
    generator.generate(irt);
    generator.output
}

struct JsGenerator {
    output: String,
    indent: u32,
}

impl JsGenerator {
    fn new() -> JsGenerator {
        JsGenerator {
            output: String::new(),
            indent: 0,
        }
    }

    fn generate(&mut self, irt: &IrTree) {
        self.generate_builtins();
        // TODO generate imports
        // TODO generate types
        for typ in irt.decls.types() {
            self.generate_instance_of(typ);
            if let TypeDefinition::Record(fields) = &typ.definition {
                self.generate_constructor(typ, fields);
            }
        }

        for statement in &irt.statements {
            self.statement(statement)
        }
    }

    fn write(&mut self, code: &str) {
        self.output.push_str(code);
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("  ");
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn unindent(&mut self) {
        self.indent -= 1;
    }

    fn generate_builtins(&mut self) {
        self.write("var ");
        self.global_identifier(&get_builtin_id("println"));
        self.write("=console.log;\n");
    }

    fn generate_instance_of(&mut self, typ: &TypeDeclaration) {
        self.write("function ");
        self.type_identifier(&typ.id);
        self.write("_instance_of(o){\n");
        self.indent();
        match &typ.definition {
            TypeDefinition::Record(_) => {
                self.write_indent();
                self.write("return o.__proto__.constructor===");
                self.type_identifier(&typ.id);
                self.write(";\n");
            }
            TypeDefinition::Union(cases) => { todo!("instance of union") }
            TypeDefinition::Primitive => unreachable!("primitive types handled in builtin module")
        }
        self.unindent();
        self.write("}\n");
    }

    fn generate_constructor(&mut self, typ: &TypeDeclaration, fields: &[ResolvedField]) {
        self.write("function ");
        self.type_identifier(&typ.id);
        self.write("(");
        for field in fields {
            self.write(&field.name);
            self.write(",");
        }
        self.write("){\n");
        self.indent();
        for field in fields {
            self.write_indent();
            self.write("this.");
            self.write(&field.name);
            self.write("=");
            self.write(&field.name);
            self.write(";\n");
        }
        self.unindent();
        self.write("}\n");
    }

    fn statement(&mut self, statement: &Statement) {
        match statement {
            Statement::SaveGlobal(save, expr) => {
                // special case for non-destructuring assignment
                if save.paths.len() == 1 && save.paths[0].0.is_empty() {
                    let target = &save.paths[0].1;
                    self.write("var ");
                    self.global_identifier(target);
                    self.write("=");
                    self.expr(expr);
                    self.write(";\n");
                } else {
                    todo!("generate statement save_global destructured")
                }
            },
            Statement::SaveLocal(_, _) => todo!("generate statement save_local"),
            Statement::Discard(expr) => {
                self.write("(");
                self.expr(expr);
                self.write(");\n");
            },
            Statement::Return(_) => todo!("generate statement return"),
        }
    }

    fn global_identifier(&mut self, global_id: &GlobalId) {
        self.write("$");
        self.write(global_id.module()); // TODO sanitize?
        for part in global_id.fqn().as_slice() {
            self.write("_");
            self.write(part);
        }
    }

    fn type_identifier(&mut self, type_id: &TypeId) {
        self.write("$T$");
        self.write(type_id.module());
        for part in type_id.fqn().as_slice() {
            self.write("_");
            self.write(part);
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match &expr.expr_type {
            ExprType::Error => panic!("error expr in codegen"),
            ExprType::LoadConstant(c) => self.constant(c),
            ExprType::LoadGlobal(id) => self.global_identifier(id),
            ExprType::LoadLocal(_) => todo!("generate expr loadLocal"),
            ExprType::LoadParam => todo!("generate expr loadParam"),
            ExprType::Call { callee, args } => {
                self.expr(callee);
                self.write("(");
                for arg in args {
                    self.expr(arg);
                    self.write(",");
                }
                self.write(")");
            },
            ExprType::Binary { op, left, right } => {
                let op_str = match op {
                    BinaryOp::Error => panic!("error operator in codegen"),
                    BinaryOp::Eq => "===",
                    BinaryOp::Neq => "!==",
                    BinaryOp::LessThan => "<",
                    BinaryOp::LessEq => "<=",
                    BinaryOp::GreaterThan => ">",
                    BinaryOp::GreaterEq => ">=",
                    BinaryOp::NumberAdd => "+",
                    BinaryOp::NumberSub => "-",
                    BinaryOp::NumberMul => "*",
                    BinaryOp::NumberDiv => "/",
                    BinaryOp::StringConcat => "+",
                };
                self.write("(");
                self.expr(left);
                self.write(op_str);
                self.expr(right);
                self.write(")");
            },
            ExprType::Func(_) => todo!("generate expr func"),
            ExprType::New { field_inits } => {
                let type_id = if let ResolvedType::Id(id, _) = &expr.resolved_type { id } else { unreachable!() };
                self.write("new ");
                self.type_identifier(type_id);
                self.write("(");
                for (_, expr) in field_inits {
                    self.expr(expr);
                    self.write(",");
                }
                self.write(")");
            },
            ExprType::Match { .. } => todo!("generate expr match"),
        }
    }

    fn constant(&mut self, constant: &Constant) {
        match constant {
            Constant::Unit => self.write("null"),
            Constant::Number(n) => self.write(&n.to_string()),
            Constant::String(s) => {
                self.write("\"");
                self.write(escape_str(s).as_ref());
                self.write("\"");
            },
            Constant::Boolean(b) => self.write(&b.to_string()),
        }
    }
}

fn escape_str(raw: &str) -> Cow<str> {
    let mut result = Cow::from(raw);
    let mut i = 0;
    while i < result.len() {
        let c = result[i..].chars().next().unwrap();
        let c_len = c.len_utf8();
        i += match c {
            '\n' => replace_in_place(&mut result, i..i+c_len, "\\n"),
            '\r' => replace_in_place(&mut result, i..i+c_len, "\\r"),
            '\\' => replace_in_place(&mut result, i..i+c_len, "\\\\"),
            '\t' => replace_in_place(&mut result, i..i+c_len, "\\t"),
            '\'' => replace_in_place(&mut result, i..i+c_len, "\\'"),
            '"' => replace_in_place(&mut result, i..i+c_len, "\\\""),
            ' '..='~' => c_len, // non-escaped printable ascii, ignore
            c => {
                let r = c.escape_unicode().to_string(); // Rust format just happens to be the same as JS
                replace_in_place(&mut result, i..i+c_len, &r)
            }
        };
    }
    result
}

fn replace_in_place(s: &mut Cow<str>, i: Range<usize>, r: &str) -> usize {
    s.to_mut().replace_range(i, r);
    r.len()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn escape_noop() {
        assert_eq!("hello", escape_str("hello"));
    }

    #[test]
    fn escape_newline() {
        assert_eq!("hello\\nworld", escape_str("hello\nworld"));
    }

    #[test]
    fn escape_several() {
        assert_eq!("\\\"\\n\\\"\\\\", escape_str("\"\n\"\\"));
    }

    #[test]
    fn escape_empty() {
        assert_eq!("", escape_str(""));
    }

    #[test]
    fn escape_unicode() {
        assert_eq!("x\\u{4e8b}x", escape_str("x事x"));
    }

    #[test]
    fn escape_big_unicode() {
        assert_eq!("x\\u{1d306}x", escape_str("x𝌆x"));
    }
}
