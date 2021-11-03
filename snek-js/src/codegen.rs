use std::borrow::Cow;
use std::ops::Range;
use snek::resolver::{BuiltinTypeNames, get_builtin_id, GlobalId, LocalId, PatternType, ResolvedField, ResolvedPattern, ResolvedType, TypeDeclaration, TypeDefinition, TypeId};
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

        self.primitive_instance_of(&BuiltinTypeNames::number_id(), "typeof o===\"number\"");
        self.primitive_instance_of(&BuiltinTypeNames::string_id(), "typeof o===\"string\"");
        self.primitive_instance_of(&BuiltinTypeNames::boolean_id(), "typeof o===\"boolean\"");
    }

    fn primitive_instance_of(&mut self, id: &TypeId, body: &str) {
        self.write("function ");
        self.instance_of_name(id);
        self.write("(o,a){\n");
        self.indent();
        self.write_indent();
        self.write("return ");
        self.write(body);
        self.write(";\n");
        self.unindent();
        self.write_indent();
        self.write("}\n");
    }

    fn generate_instance_of(&mut self, typ: &TypeDeclaration) {
        self.write("function ");
        self.instance_of_name(&typ.id);
        self.write("(o,a){\n");
        self.indent();
        self.write_indent();
        match &typ.definition {
            TypeDefinition::Record(_) => {
                self.write("return o.__proto__.constructor===");
                self.type_identifier(&typ.id);
                self.write(";\n");
            }
            TypeDefinition::Union(cases) => {
                let mut it = cases.iter();
                self.write("return ");
                self.check_type("o", "a", it.next().unwrap());
                for case in it {
                    self.write("||");
                    self.check_type("o", "a", case);
                }
                self.write(";\n");
            }
            TypeDefinition::Primitive => unreachable!("primitive types handled in builtin module")
        }
        self.unindent();
        self.write("}\n");
    }

    fn instance_of_name(&mut self, type_id: &TypeId) {
        self.type_identifier(&type_id);
        self.write("_instance_of");
    }

    fn check_type(&mut self, ident: &str, args_ident: &str, typ: &ResolvedType) {
        match typ {
            ResolvedType::Id(id, args) => {
                self.instance_of_name(id);
                self.write("(");
                self.write(ident);
                self.write(",[");
                for arg in args {
                    self.type_check_ref(arg);
                    self.write(",");
                }
                self.write("])");
            }
            ResolvedType::TypeParam(i) => {
                self.write(args_ident);
                self.write("[");
                self.write(&i.to_string());
                self.write("](");
                self.write(ident);
                self.write(",");
                self.write(ident);
                self.write(".$__type_args)");
            },
            ResolvedType::Func { .. } => todo!("check_type func"),
            ResolvedType::Callable(_) => panic!("callable type in codegen"),
            ResolvedType::Unit => {
                self.write(ident);
                self.write("===null");
            }
            ResolvedType::Any => self.write("true"),
            ResolvedType::Nothing => self.write("false"),
            ResolvedType::Inferred => panic!("inferred type in codegen"),
            ResolvedType::Error => panic!("error type in codegen"),
            ResolvedType::Hole(_) => panic!("hole type in codegen"),
        }
    }

    fn type_check_ref(&mut self, typ: &ResolvedType) {
        self.write("(ao,aa) -> ");
        self.check_type("ao", "aa", typ);
    }

    fn generate_constructor(&mut self, typ: &TypeDeclaration, fields: &[ResolvedField]) {
        self.write("function ");
        self.type_identifier(&typ.id);
        self.write("($__type_args,");
        for field in fields {
            self.write(&field.name);
            self.write(",");
        }
        self.write("){\n");
        self.indent();
        self.write_indent();
        self.write("this.$__type_args=$__type_args");
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
        self.write_indent();
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
            Statement::SaveLocal(save, expr) => {
                // special case for non-destructuring assignment
                if save.paths.len() == 1 && save.paths[0].0.is_empty() {
                    let target = &save.paths[0].1;
                    self.write("let ");
                    self.local(target);
                    self.write("=");
                    self.expr(expr);
                    self.write(";\n");
                } else {
                    todo!("generate statement save_local destructured")
                }
            },
            Statement::Discard(expr) => {
                self.write("(");
                self.expr(expr);
                self.write(");\n");
            },
            Statement::Return(expr) => {
                self.write("return ");
                self.expr(expr);
                self.write(";\n");
            }
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

    fn local(&mut self, LocalId(i): &LocalId) {
        self.write("var_");
        self.write(&i.to_string());
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
            ExprType::LoadLocal(id) => self.local(id),
            ExprType::LoadParam => {
                self.write("$args.pop()");
            },
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
                let (type_id, type_args) = if let ResolvedType::Id(id, args) = &expr.resolved_type { (id, args) } else { unreachable!() };
                self.write("new ");
                self.type_identifier(type_id);
                self.write("([");
                for arg in type_args {
                    self.type_check_ref(arg);
                    self.write(",");
                }
                self.write("],");
                for (_, expr) in field_inits {
                    self.expr(expr);
                    self.write(",");
                }
                self.write(")");
            },
            ExprType::Match { expr, arms } => {
                self.write("(function($e){\n");
                self.indent();
                self.write_indent();
                self.write("let $args=[$e];\n");
                self.write_indent();
                for (pat, stmts) in arms {
                    self.write("if(");
                    self.matches("$e", pat);
                    self.write("){\n");
                    self.indent();
                    for stmt in stmts {
                        self.statement(stmt);
                    }
                    self.unindent();
                    self.write_indent();
                    self.write("}else ");
                }
                self.write("{\n");
                self.indent();
                self.write_indent();
                self.write("throw \"Unmatched match expression\";\n");
                self.unindent();
                self.write_indent();
                self.write("};\n");
                self.unindent();
                self.write_indent();
                self.write("})(");
                self.expr(expr);
                self.write(")");
            },
        }
    }

    // returns boolean expression of whether ident matches patter
    fn matches(&mut self, ident: &str, pattern: &ResolvedPattern) {
        match &pattern.pattern_type {
            PatternType::Discard => self.check_type(ident, "[]", &pattern.resolved_type),
            PatternType::Name(_) => self.check_type(ident, "[]", &pattern.resolved_type),
            PatternType::Constant(c) => {
                self.write(ident);
                self.write("===");
                self.constant(c);
            }
            PatternType::Destructuring(_) => todo!("matches destructuring")
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
