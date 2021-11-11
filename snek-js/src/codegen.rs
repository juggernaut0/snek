use std::borrow::Cow;
use std::fmt::Write;
use std::ops::Range;
use snek::resolver::{BuiltinTypeNames, FunctionDeclaration, FunctionId, get_builtin_id, GlobalId, LocalId, PatternType, ResolvedField, ResolvedPattern, ResolvedType, TypeDeclaration, TypeDefinition, TypeId};
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

        for typ in irt.decls.types() {
            self.runtime_type_constructor(typ);
            if let TypeDefinition::Record(fields) = &typ.definition {
                self.constructor(typ, fields);
            }
        }
        for func in irt.decls.functions() {
            self.function(func);
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
        self.write("={call:(args)=>{console.log(...args);}};\n");

        self.write(include_str!("runtime_type.js"));
    }

    fn runtime_type_name(&mut self, id: &TypeId) {
        self.type_identifier(id);
        self.write("_RuntimeType");
    }

    fn runtime_type_constructor(&mut self, type_decl: &TypeDeclaration) {
        self.write("function ");
        self.runtime_type_name(&type_decl.id);
        self.write("(args){\n");
        self.indent();
        self.write_indent();
        self.write("RuntimeType.call(this);\n");
        self.write_indent();
        self.write("this.args=args;\n");
        self.unindent();
        self.write_indent();
        self.write("}\n");

        self.write_indent();
        self.runtime_type_name(&type_decl.id);
        self.write(".prototype=Object.create(RuntimeType.prototype);\n");

        self.write_indent();
        self.runtime_type_name(&type_decl.id);
        self.write(".prototype.equals=function(other){\n");
        self.indent();
        self.write_indent();
        self.write("let exactMatch = this.__proto__===other.__proto__");
        for i in 0..type_decl.num_type_params {
            write!(self, "&&this.args[{}]===other.args[{}]", i, i).unwrap();
        }
        self.write(";\n");
        self.write_indent();
        match &type_decl.definition {
            TypeDefinition::Record(_) => {
                self.write("return exactMatch;\n")
            }
            TypeDefinition::Union(cases) => {
                self.write("return exactMatch");
                for case in cases {
                    self.write("||");
                    self.runtime_type(case, "this.args");
                    self.write(".equals(other)");
                }
                self.write(";\n");
            }
            TypeDefinition::Primitive => unreachable!("user types cannot be primitive")
        }
        self.unindent();
        self.write_indent();
        self.write("}\n");
    }

    fn constructor(&mut self, typ: &TypeDeclaration, fields: &[ResolvedField]) {
        self.write("function ");
        self.type_identifier(&typ.id);
        self.write("($type_args,");
        for field in fields {
            self.write(&field.name);
            self.write(",");
        }
        self.write("){\n");
        self.indent();
        self.write_indent();
        self.write("this.$type=new ");
        self.runtime_type_name(&typ.id);
        self.write("($type_args);\n");
        for field in fields {
            self.write_indent();
            let name = &field.name;
            writeln!(self, "this.{}={};", name, name).unwrap();
        }
        self.unindent();
        self.write_indent();
        self.write("}\n");
    }

    fn runtime_type(&mut self, typ: &ResolvedType, type_args: &str) {
        match typ {
            ResolvedType::Id(id, args) => {
                if id == &BuiltinTypeNames::number_id() {
                    self.write("Number_RuntimeType");
                } else if id == &BuiltinTypeNames::string_id() {
                    self.write("String_RuntimeType");
                } else if id == &BuiltinTypeNames::boolean_id() {
                    self.write("Boolean_RuntimeType");
                } else {
                    self.write("new ");
                    self.runtime_type_name(id);
                    self.write("([");
                    for arg in args {
                        self.runtime_type(arg, type_args);
                        self.write(",");
                    }
                    self.write("])");
                }
            }
            ResolvedType::TypeParam(i) => {
                write!(self, "{}[{}]", type_args, i).unwrap();
            },
            ResolvedType::Func { params, return_type } => {
                self.write("new Function_RuntimeType([");
                for param_type in params {
                    self.runtime_type(param_type, type_args);
                    self.write(",");
                }
                self.write("],");
                self.runtime_type(return_type, type_args);
                self.write(")");
            },
            ResolvedType::Callable(_) => panic!("callable type in codegen"),
            ResolvedType::Unit => {
                self.write("Unit_RuntimeType");
            }
            ResolvedType::Any => {
                self.write("Any_RuntimeType");
            }
            ResolvedType::Nothing => {
                self.write("Nothing_RuntimeType");
            }
            ResolvedType::Inferred => panic!("inferred type in codegen"),
            ResolvedType::Error => panic!("error type in codegen"),
            ResolvedType::Hole(_) => panic!("hole type in codegen"),
        }
    }

    fn check_type(&mut self, typ: &ResolvedType, object: &str, type_args: &str) {
        self.runtime_type(typ, type_args);
        write!(self, ".instanceOf({})", object).unwrap();
    }

    fn function(&mut self, func: &FunctionDeclaration) {
        self.write("function ");
        self.function_name(&func.id());
        self.write("($captures){\n");
        self.indent();
        self.write_indent();
        self.write("Object.assign(this, $captures);\n");
        self.unindent();
        self.write_indent();
        self.write("}\n");

        self.write_indent();
        self.function_name(&func.id());
        self.write(".prototype.$type=");
        self.runtime_type(func.resolved_type(), "[]");
        self.write(";\n");

        self.write_indent();
        self.function_name(&func.id());
        self.write(".prototype.call=function($args){\n");
        self.indent();
        for stmt in func.statements() {
            self.statement(stmt);
        }
        self.unindent();
        self.write_indent();
        self.write("};\n");
    }

    fn function_name(&mut self, func_id: &FunctionId) {
        let module = func_id.module();
        if module != BuiltinTypeNames::mod_name().as_str() {
            write!(self, "$F${}_", module).unwrap();
        }
        write!(self, "func{}", func_id.id()).unwrap();
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

    fn capture(&mut self, id: &LocalId) {
        self.write("this.");
        self.local(id);
    }

    fn type_identifier(&mut self, type_id: &TypeId) {
        let module = type_id.module();
        if module != BuiltinTypeNames::mod_name().as_str() {
            write!(self, "$T${}_", module).unwrap();
        }
        for (i, part) in type_id.fqn().as_slice().iter().enumerate() {
            if i != 0 {
                self.write("_");
            }
            self.write(part);
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match &expr.expr_type {
            ExprType::Error => panic!("error expr in codegen"),
            ExprType::LoadConstant(c) => self.constant(c),
            ExprType::LoadGlobal(id) => self.global_identifier(id),
            ExprType::LoadLocal(id) => self.local(id),
            ExprType::LoadCapture(id) => self.capture(id),
            ExprType::LoadParam => {
                self.write("$args.pop()");
            },
            ExprType::Call { callee, args } => {
                self.expr(callee);
                self.write(".call([");
                for arg in args {
                    self.expr(arg);
                    self.write(",");
                }
                self.write("])");
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
            ExprType::Func { id, captures } => {
                self.write("new ");
                self.function_name(id);
                self.write("({");
                for capture in captures {
                    match capture {
                        FuncCapture::Local(id) => {
                            self.local(id);
                            self.write(":");
                            self.local(id);
                        }
                        FuncCapture::Capture(id) => {
                            self.local(id);
                            self.write(":");
                            self.capture(id);
                        }
                    }
                    self.write(",");
                }
                self.write("})");
            },
            ExprType::New { field_inits } => {
                let (type_id, type_args) = if let ResolvedType::Id(id, args) = &expr.resolved_type { (id, args) } else { unreachable!() };
                self.write("new ");
                self.type_identifier(type_id);
                self.write("([");
                for arg in type_args {
                    self.runtime_type(arg,"[]");
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
            PatternType::Discard => self.check_type(&pattern.resolved_type, ident, "[]"),
            PatternType::Name(_) => self.check_type(&pattern.resolved_type, ident, "[]"),
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

impl Write for JsGenerator {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.output.write_str(s)
    }

    fn write_char(&mut self, c: char) -> std::fmt::Result {
        self.output.write_char(c)
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
