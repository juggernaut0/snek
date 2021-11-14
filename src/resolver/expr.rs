use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::iter::repeat;

use crate::ast::{BinaryOp, CallExpr, Expr, ExprType, FieldInit, LambdaExpr, Literal, LiteralType, NamedType, Pattern, QName};
use crate::resolver::{BuiltinTypeNames, DefineGlobalResult, Error, FieldPath, FunctionId, GlobalId, LocalId, LocalScope, ResolvedPattern, ResolvedType, TypeDeclaration, TypeDefinition, TypeId, TypeStore};
use crate::resolver::globals::GlobalDeclaration;
use crate::resolver::irt::{BinaryOp as IrtBinaryOp, Constant, Expr as IrtExpr, ExprType as IrtExprType, FuncCapture, Save, Statement};
use crate::resolver::patterns::ExhaustivenessChecker;
use crate::resolver::types::{Hole};
use crate::resolver::unifier::{merge_types, Unifier};

pub(super) trait ExprResolverContext {
    fn push_error(&mut self, error: Error);
    fn get_type_decl(&self, id: &TypeId) -> &TypeDeclaration;
    fn get_global(&self, qn: &QName) -> Option<(GlobalId, bool)>;
    fn get_global_decl(&self, id: &GlobalId) -> Option<&GlobalDeclaration>;
    fn resolve_named_type(&mut self, named_type: &NamedType, type_params: &[String], line: u32, col: u32) -> ResolvedType;
    fn resolve_pattern(&mut self, pattern: &Pattern, expr_type: Option<ResolvedType>) -> ResolvedPattern;
    fn navigate_type_fields(&mut self, path: &FieldPath, base_type: ResolvedType, line: u32, col: u32) -> ResolvedType;
    fn is_visible(&self, visibility: &[String]) -> bool;
    fn define_function(&mut self, resolved_type: ResolvedType, statements: Vec<Statement>, captures: Vec<LocalId>) -> FunctionId;
    fn literal_to_constant(&mut self, lit: &Literal, line: u32, col: u32) -> Constant;
}

pub struct ExprResolver<'ctx> {
    context: &'ctx mut dyn ExprResolverContext,
    captures: HashSet<LocalId>,
    locals: HashSet<LocalId>,
    parameters: Option<Vec<ResolvedType>>,
    expected_return_type: Option<ResolvedType>,
}

impl ExprResolver<'_> {
    pub(super) fn new(context: &mut dyn ExprResolverContext) -> ExprResolver {
        ExprResolver {
            context,
            captures: HashSet::new(),
            locals: HashSet::new(),
            parameters: None,
            expected_return_type: None,
        }
    }

    fn new_sub_resolver(&mut self, expected_return_type: ResolvedType) -> ExprResolver {
        ExprResolver {
            context: self.context,
            captures: HashSet::new(),
            locals: HashSet::new(),
            parameters: None,
            expected_return_type: Some(expected_return_type),
        }
    }

    pub(super) fn resolve_top_expr(&mut self, expected_type: ResolvedType, expr: &Expr) -> DefineGlobalResult {
        self.resolve_expr(expected_type, expr, &LocalScope::new(None, false), None, false)
    }

    fn resolve_expr(&mut self, expected_type: ResolvedType, expr: &Expr, scope: &LocalScope, holes: Option<&mut [Hole]>, tail: bool) -> DefineGlobalResult {
        let (actual_type, irt_expr_type) = match &expr.expr_type {
            ExprType::QName(qn) => {
                if let Some(local) = scope.get_from_qname(qn) {
                    if local.level == scope.level() {
                        (local.typ.clone(), IrtExprType::LoadLocal(local.id))
                    } else {
                        self.captures.insert(local.id);
                        (local.typ.clone(), IrtExprType::LoadCapture(local.id))
                    }
                } else if let Some((global, visible)) = self.context.get_global(qn) {
                    if !visible {
                        self.context.push_error(Error {
                            message: format!("{} is private and cannot be accessed here.", global.fqn()),
                            line: expr.line,
                            col: expr.col,
                        })
                    }

                    let actual_type = if let Some(g_decl) = self.context.get_global_decl(&global) {
                        g_decl.resolved_type.clone()
                    } else {
                        return DefineGlobalResult::NeedsType(global)
                    };

                    (actual_type, IrtExprType::LoadGlobal(global))
                } else {
                    self.context.push_error(Error {
                        message: format!("Unknown name {}", qn.parts.join(".")),
                        line: expr.line,
                        col: expr.col,
                    });
                    return DefineGlobalResult::error_expr()
                }
            },
            ExprType::Constant(lit) => {
                let actual_type = match lit.lit_type {
                    LiteralType::Number => BuiltinTypeNames::number(),
                    LiteralType::String => BuiltinTypeNames::string(),
                    LiteralType::Bool => BuiltinTypeNames::boolean(),
                    LiteralType::Unit => ResolvedType::Unit,
                };
                let cons = self.context.literal_to_constant(lit, expr.line, expr.col);
                (actual_type, IrtExprType::LoadConstant(cons))
            },
            ExprType::Unary(_, _) => todo!(),
            ExprType::Binary(op, left, right) => {
                let res_left = match self.resolve_expr(ResolvedType::Inferred, left, scope, None, false) {
                    DefineGlobalResult::Success(it) => it,
                    DefineGlobalResult::NeedsType(id) => return DefineGlobalResult::NeedsType(id),
                };
                let res_right = match self.resolve_expr(ResolvedType::Inferred, right, scope, None, false) {
                    DefineGlobalResult::Success(it) => it,
                    DefineGlobalResult::NeedsType(id) => return DefineGlobalResult::NeedsType(id),
                };
                let (rt, op) = match (op, &res_left.resolved_type, &res_right.resolved_type) {
                    (BinaryOp::Equal, _, _) => (BuiltinTypeNames::boolean(), IrtBinaryOp::Eq),
                    (BinaryOp::NotEqual, _, _) => (BuiltinTypeNames::boolean(), IrtBinaryOp::Neq),
                    (BinaryOp::LessThan, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::LessThan),
                    (BinaryOp::GreaterThan, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::GreaterThan),
                    (BinaryOp::LessEqual, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::LessEq),
                    (BinaryOp::GreaterEqual, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::boolean(), IrtBinaryOp::GreaterEq),
                    (BinaryOp::Plus, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberAdd),
                    (BinaryOp::Minus, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberSub),
                    (BinaryOp::Times, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberMul),
                    (BinaryOp::Div, l, r) if l == &BuiltinTypeNames::number() && r == &BuiltinTypeNames::number() => (BuiltinTypeNames::number(), IrtBinaryOp::NumberDiv),
                    (BinaryOp::Plus, l, r) if l == &BuiltinTypeNames::string() && r == &BuiltinTypeNames::string() => (BuiltinTypeNames::string(), IrtBinaryOp::StringConcat),
                    (op, left, right) => {
                        let message = if left.is_inferred() {
                            "Unable to infer type of left-hand operand".to_string()
                        } else if right.is_inferred() {
                            "Unable to infer type of right-hand operand".to_string()
                        } else {
                            format!("Incompatible operand types for binary operator {:?}: {} & {}", op, left, right)
                        };
                        self.context.push_error(Error {
                            message,
                            line: expr.line,
                            col: expr.col,
                        });
                        (ResolvedType::Error, IrtBinaryOp::Error)
                    },
                };
                (rt, IrtExprType::Binary { op, left: Box::new(res_left), right: Box::new(res_right) })
            },
            ExprType::Call(call_expr) => {
                match self.resolve_call_expr(expected_type.clone(), expr, call_expr, scope, tail) {
                    Ok(it) => it,
                    Err(dfg) => return dfg
                }
            },
            ExprType::Lambda(lambda_expr) => {
                match self.resolve_function(expected_type.clone(), expr, lambda_expr, scope) {
                    Ok(it) => it,
                    Err(dfg) => return dfg
                }
            }
            ExprType::List(_) => todo!(),
            ExprType::New(nt, inits) => {
                match self.resolve_new_expr(expected_type.clone(), expr, nt.as_ref(), inits, scope) {
                    Ok(it) => it,
                    Err(dfg) => return dfg
                }
            },
            ExprType::Dot => unreachable!("dot handled in resolve_call_expr"),
            ExprType::Match => unreachable!("match handled in resolve_call_expr")
        };
        let resolved_type = self.unify(expected_type, actual_type, holes, expr.line, expr.col);
        DefineGlobalResult::Success(IrtExpr {
            resolved_type,
            expr_type: irt_expr_type,
        })
    }

    fn resolve_call_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        call_expr: &CallExpr,
        scope: &LocalScope,
        tail: bool,
    ) -> Result<(ResolvedType, IrtExprType), DefineGlobalResult> {
        let (params, return_type, callee) = if let ExprType::Match = &call_expr.callee.expr_type {
            return self.resolve_match_expr(expected_type, expr, &call_expr.args, scope, tail);
        } else if let ExprType::Dot = &call_expr.callee.expr_type {
            let params = if let Some(params) = &self.parameters {
                params.clone()
            } else {
                self.context.push_error(Error {
                    message: "Recursive call cannot be used here".to_string(),
                    line: call_expr.callee.line,
                    col: call_expr.callee.col,
                });
                return Ok((ResolvedType::Error, IrtExprType::Error));
            };

            let return_type = self.expected_return_type.clone()
                .filter(|it| it != &ResolvedType::Inferred)
                .unwrap_or_else(|| if tail { ResolvedType::Nothing } else {ResolvedType::Inferred });
            if return_type.is_inferred() {
                self.context.push_error(Error {
                    message: "Cannot infer type of recursive call".to_string(),
                    line: expr.line,
                    col: expr.col,
                })
            }

            (params, return_type, None)
        } else {
            let callee_result = self.resolve_expr(ResolvedType::Callable(Box::new(expected_type)), &call_expr.callee, scope, None, false);
            let callee = match callee_result {
                DefineGlobalResult::NeedsType(_) => return Err(callee_result),
                DefineGlobalResult::Success(callee_expr) => callee_expr,
            };
            match callee.resolved_type.clone() {
                ResolvedType::Func { params, return_type } => (params, *return_type, Some(callee)),
                ResolvedType::Callable(et) => {
                    self.context.push_error(Error {
                        message: "Unable to infer callee type".into(),
                        line: expr.line,
                        col: expr.col,
                    });
                    return Ok((*et, IrtExprType::Error))
                },
                ResolvedType::Error => return Err(DefineGlobalResult::error_expr()),
                _ => unreachable!() // Callable does not unify with anything except Func or Callable
            }
        };

        match params.len().cmp(&call_expr.args.len()) {
            Ordering::Less => {
                self.context.push_error(Error {
                    message: "Too many arguments provided for function".to_string(),
                    line: expr.line,
                    col: expr.col,
                })
            }
            Ordering::Greater => {
                // TODO currying
                self.context.push_error(Error {
                    message: "Partial application not yet supported".to_string(),
                    line: expr.line,
                    col: expr.col,
                })
            }
            Ordering::Equal => {}
        }

        let arg_results = params
            .iter()
            .cloned()
            .chain(repeat(ResolvedType::Inferred))
            .zip(&call_expr.args)
            .map(|(exp_arg_type, arg_expr)| {
                self.resolve_expr(exp_arg_type, arg_expr, scope, None, false)
            });

        let mut args = Vec::new();
        for arg_result in arg_results {
            match arg_result {
                DefineGlobalResult::NeedsType(_) => return Err(arg_result),
                DefineGlobalResult::Success(expr) => args.push(expr),
            }
        }
        let et = if let Some(callee) = callee {
            IrtExprType::Call {
                callee: Box::new(callee),
                args,
            }
        } else {
            IrtExprType::RecCall { args }
        };
        Ok((
            return_type,
            et,
        ))
    }

    fn resolve_match_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        args: &[Expr],
        scope: &LocalScope,
        tail: bool,
    ) -> Result<(ResolvedType, IrtExprType), DefineGlobalResult> {
        if args.is_empty() {
            self.context.push_error(Error {
                message: "Expression to match missing".to_string(),
                line: expr.line,
                col: expr.col,
            });
        }

        let dgr = self.resolve_expr(ResolvedType::Inferred, &args[0], scope, None, false);
        let matched_expr = match dgr {
            DefineGlobalResult::Success(e) => e,
            DefineGlobalResult::NeedsType(_) => return Err(dgr),
        };

        let mut patterns: Vec<_> = Vec::new();
        let mut arm_exprs: Vec<_> = Vec::new();
        let mut actual_type = ResolvedType::Inferred;
        let expected_arm_type = ResolvedType::Func { params: vec![matched_expr.resolved_type.clone()], return_type: Box::new(expected_type) };
        for arg in &args[1..] {
            let lambda = if let ExprType::Lambda(lambda) = &arg.expr_type {
                lambda
            } else {
                self.context.push_error(Error {
                    message: "Every arm in a match expression must be a lambda".to_string(),
                    line: arg.line,
                    col: arg.col,
                });
                continue
            };

            if lambda.params.len() != 1 {
                self.context.push_error(Error {
                    message: "Every arm in a match expression must have exactly one pattern".to_string(),
                    line: arg.line,
                    col: arg.col,
                });
                continue
            }

            let pattern = self.context.resolve_pattern(&lambda.params[0], Some(matched_expr.resolved_type.clone()));
            patterns.push(pattern);

            let (rrt, statements) = self.resolve_match_arm(expected_arm_type.clone(), arg, lambda, scope, tail)?;
            actual_type = merge_types(actual_type, rrt);
            arm_exprs.push(statements);
        }

        let patterns_ref: Vec<_> = patterns.iter().collect();
        if !self.exhaustive(&patterns_ref, matched_expr.resolved_type.clone()) {
            self.context.push_error(Error {
                message: "Match expression arms must be exhaustive".to_string(),
                line: expr.line,
                col: expr.col,
            });
        }

        let arms = patterns.into_iter().zip(arm_exprs).collect();
        Ok((actual_type, IrtExprType::Match { expr: Box::new(matched_expr), arms }))
    }

    fn resolve_match_arm(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        lambda_expr: &LambdaExpr,
        scope: &LocalScope,
        tail: bool,
    ) -> Result<(ResolvedType, Vec<Statement>), DefineGlobalResult> {
        let ResolvedLambdaExpr {
            return_type,
            statements,
        } = self.resolve_lambda_expr(expected_type, expr, lambda_expr, scope, false, tail)?;
        Ok((return_type, statements))
    }

    fn resolve_function(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        lambda_expr: &LambdaExpr,
        scope: &LocalScope,
    ) -> Result<(ResolvedType, IrtExprType), DefineGlobalResult> {
        let expected_return_type = match &expected_type {
            ResolvedType::Func { return_type, .. } => return_type.as_ref().clone(),
            ResolvedType::Callable(return_type) => return_type.as_ref().clone(),
            _ => ResolvedType::Inferred,
        };
        let mut sub_resolver = self.new_sub_resolver(expected_return_type);
        let ResolvedLambdaExpr {
            return_type,
            statements,
        } = sub_resolver.resolve_lambda_expr(expected_type, expr, lambda_expr, scope, true, true)?;
        let mut my_captures = sub_resolver.captures;
        let my_locals = sub_resolver.locals;
        my_captures.retain(|it| !my_locals.contains(it));
        let params = sub_resolver.parameters.expect("lambda did not set parameters");

        self.captures.extend(my_captures.iter().copied());
        let mut captures: Vec<_> = my_captures.into_iter().collect();
        captures.sort_by_key(|it| it.0);
        let func_captures = captures.iter()
            .copied()
            .map(|it| if self.locals.contains(&it) { FuncCapture::Local(it) } else { FuncCapture::Capture(it) } )
            .collect();

        let rt = ResolvedType::Func { params, return_type: Box::new(return_type) };
        let id = self.context.define_function(rt.clone(), statements, captures);
        let irt_expr_type = IrtExprType::Func { id, captures: func_captures };
        Ok((rt, irt_expr_type))
    }

    fn resolve_lambda_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        lambda_expr: &LambdaExpr,
        parent_scope: &LocalScope,
        capturing: bool,
        tail: bool,
    ) -> Result<ResolvedLambdaExpr, DefineGlobalResult> {
        let (expected_params, expected_return_type) = match expected_type {
            ResolvedType::Func { params, return_type } => {
                if params.len() != lambda_expr.params.len() {
                    self.context.push_error(Error {
                        message: format!("Parameter count mismatch. Expected {} Actual {}", params.len(), lambda_expr.params.len()),
                        line: expr.line,
                        col: expr.col,
                    });
                }
                (params, *return_type)
            }
            ResolvedType::Callable(return_type) => (Vec::new(), *return_type),
            ResolvedType::Inferred => (Vec::new(), ResolvedType::Inferred),
            e => {
                self.context.push_error(Error {
                    message: format!("Type mismatch, expected: {}", e),
                    line: expr.line,
                    col: expr.col,
                });
                (Vec::new(), ResolvedType::Inferred)
            }
        };

        let mut statements = Vec::new();
        let mut param_types = Vec::new();
        let mut scope = LocalScope::new(Some(parent_scope), capturing);

        let params = lambda_expr.params.iter()
            .zip(expected_params.into_iter().chain(repeat(ResolvedType::Inferred)));
        for (param, expected_param_type) in params {
            let resolved_pattern = self.context.resolve_pattern(param, Some(expected_param_type));
            // TODO check pattern exhaustiveness when NOT an arm of a match expr
            let resolved_type = resolved_pattern.resolved_type.clone();
            let names = resolved_pattern.extract_names();
            if resolved_type.is_inferred() {
                self.context.push_error(Error {
                    message: "Unable to infer parameter type, specify the type explicitly".to_string(),
                    line: param.line,
                    col: param.col,
                });
            }
            let irt_expr = IrtExpr { resolved_type: resolved_type.clone(), expr_type: IrtExprType::LoadParam };
            param_types.push(resolved_type);
            let paths: Vec<_> = names.into_iter()
                .map(|(name, declared_type, path)| {
                    let resolved_type = if path.is_empty() {
                        declared_type
                    } else {
                        let actual_type = self.context.navigate_type_fields(&path, irt_expr.resolved_type.clone(), param.line, param.col);
                        self.unify(declared_type, actual_type, None, param.line, param.col)
                    };
                    let id = scope.insert(name, resolved_type);
                    self.locals.insert(id);
                    (path, id)
                })
                .collect();
            let statement = if paths.is_empty() {
                Statement::Discard(irt_expr)
            } else {
                Statement::SaveLocal(Save { paths }, irt_expr)
            };
            statements.push(statement);
        }
        if capturing {
            self.parameters = Some(param_types);
        }

        for binding in &lambda_expr.bindings {
            #[cfg(debug_assertions)] {
                if let Expr { expr_type: ExprType::QName(qn), .. } = &binding.expr {
                    if qn.parts.len() == 1 && &qn.parts[0] == "diag" {
                        /*for item in &scope.declarations {
                            println!("local {} is {}: {}", item.id.0, item.name, item.typ)
                        }*/
                        continue
                    }
                }
            }

            let pattern = &binding.pattern;
            let resolved_pattern = self.context.resolve_pattern(pattern, None);
            let expected_type = resolved_pattern.resolved_type.clone();
            let names = resolved_pattern.extract_names();
            let dgr = self.resolve_expr(expected_type,  &binding.expr, &scope, None, false);
            // TODO can I accumulate these across all bindings and return them all at once?
            let irt_expr = match dgr {
                DefineGlobalResult::NeedsType(_) => return Err(dgr),
                DefineGlobalResult::Success(irt_expr) => irt_expr,
            };

            if irt_expr.resolved_type.is_inferred() {
                self.context.push_error(Error {
                    message: "Unable to infer type of local binding".into(),
                    line: pattern.line,
                    col: pattern.col,
                });
            }
            let resolved_pattern = self.context.resolve_pattern(pattern, Some(irt_expr.resolved_type.clone()));
            if !self.exhaustive(std::slice::from_ref(&&resolved_pattern), irt_expr.resolved_type.clone()) {
                self.context.push_error(Error {
                    message: "Binding pattern must be exhaustive".into(),
                    line: pattern.line,
                    col: pattern.col,
                });
            }

            let paths: Vec<_> = names.into_iter()
                .map(|(name, declared_type, path)| {
                    let actual_type = self.context.navigate_type_fields(&path, irt_expr.resolved_type.clone(), pattern.line, pattern.col);
                    let resolved_type = self.unify(declared_type, actual_type, None, pattern.line, pattern.col);
                    let id = scope.insert(name, resolved_type);
                    self.locals.insert(id);
                    (path, id)
                })
                .collect();
            let statement = if paths.is_empty() {
                Statement::Discard(irt_expr)
            } else {
                Statement::SaveLocal(Save { paths }, irt_expr)
            };
            statements.push(statement);
        }

        let dfg = self.resolve_expr(expected_return_type, &lambda_expr.expr, &scope, None, tail);
        let irt_expr = match dfg {
            DefineGlobalResult::NeedsType(_) => return Err(dfg),
            DefineGlobalResult::Success(irt_expr) => irt_expr,
        };
        let return_type = irt_expr.resolved_type.clone();

        statements.push(Statement::Return(irt_expr));

        Ok(ResolvedLambdaExpr { return_type, statements })
    }

    fn resolve_new_expr(
        &mut self,
        expected_type: ResolvedType,
        expr: &Expr,
        named_type: Option<&NamedType>,
        inits: &[FieldInit],
        scope: &LocalScope,
    ) -> Result<(ResolvedType, IrtExprType), DefineGlobalResult> {
        let actual_type = named_type
            .map(|nt| self.context.resolve_named_type(nt, &[], expr.line, expr.col))
            .unwrap_or(expected_type);

        let mut holes = Vec::new();
        let fields = match actual_type.clone() {
            ResolvedType::Id(id, args) => {
                holes = args.iter()
                    .map(|arg| {
                        match arg {
                            ResolvedType::Inferred => Hole::Empty,
                            ResolvedType::Hole(_) => Hole::Empty,
                            _ => Hole::Fixed
                        }
                    })
                    .collect();
                let typ = self.context.get_type_decl(&id);
                let holey_args: Vec<_> = args.into_iter()
                    .enumerate()
                    .map(|(i, arg)| {
                        match arg {
                            ResolvedType::Inferred => ResolvedType::Hole(i),
                            _ => arg
                        }
                    })
                    .collect();
                let def = typ.definition.clone().instantiate_into(&holey_args);

                if let TypeDefinition::Record(fields) = def {
                    Some((fields, typ))
                } else {
                    self.context.push_error(Error {
                        message: "Only record types can be created using new".to_string(),
                        line: expr.line,
                        col: expr.col,
                    });
                    None
                }
            }
            ResolvedType::TypeParam(_) => {
                self.context.push_error(Error {
                    message: "Can not create instance of type parameters using new".to_string(),
                    line: expr.line,
                    col: expr.col,
                });
                None
            },
            ResolvedType::Inferred => {
                self.context.push_error(Error {
                    message: "Cannot infer new expression type".to_string(),
                    line: expr.line,
                    col: expr.col,
                });
                None
            }
            _ => None
        };

        let mut init_to_field = HashMap::new();
        let mut field_iter_order = Vec::new();
        for init in inits {
            init_to_field.insert(&init.field_name, (Some(&init.expr), None));
        }
        if let Some((fields, type_decl)) = &fields {
            let fqn = type_decl.id.fqn().clone();
            let fields_are_visible = self.context.is_visible(fqn.namespace());
            for field in fields {
                if !field.public && !fields_are_visible {
                    self.context.push_error(Error {
                        message: format!("Cannot construct type {} with private field {}", fqn, &field.name),
                        line: expr.line, // TODO get more specific line&col
                        col: expr.col,
                    });
                }

                if let Some((_, typ)) = init_to_field.get_mut(&field.name) {
                    *typ = Some(field.resolved_type.clone());
                } else {
                    init_to_field.insert(&field.name, (None, Some(field.resolved_type.clone())));
                }

                field_iter_order.push(&field.name);
            }
        } else {
            for (name, (_, typ)) in init_to_field.iter_mut() {
                *typ = Some(ResolvedType::Inferred);
                field_iter_order.push(name);
            }
        }

        let mut field_inits = Vec::new();
        for field_name in field_iter_order {
            let v = init_to_field.remove(field_name).expect("field in order but not map");
            match v {
                (Some(init_expr), Some(expected_field_type)) => {
                    let dgr = self.resolve_expr(expected_field_type, init_expr, scope, Some(&mut holes), false);
                    let e = match dgr {
                        DefineGlobalResult::Success(e) => e,
                        _ => return Err(dgr)
                    };
                    field_inits.push((field_name.clone(), e));
                }
                (None, Some(_)) => {
                    self.context.push_error(Error {
                        message: format!("Field {} must be initialized", field_name),
                        line: expr.line,
                        col: expr.col,
                    });
                }
                (Some(init_expr), None) => {
                    self.context.push_error(Error {
                        message: format!("Unknown field {}", field_name),
                        line: expr.line,
                        col: expr.col,
                    });
                    let dgr = self.resolve_expr(ResolvedType::Inferred, init_expr, scope, Some(&mut holes), false);
                    match dgr {
                        DefineGlobalResult::Success(_) => {} // discard
                        _ => return Err(dgr)
                    };
                }
                _ => unreachable!()
            }
        }

        let resolved_type = match actual_type {
            ResolvedType::Id(id, args) => {
                let filled_args = args.into_iter()
                    .zip(holes)
                    .map(|(arg, hole)| {
                        match hole {
                            Hole::Empty => panic!("unfilled hole"),
                            Hole::Fixed => arg,
                            Hole::Filled(t) => t,
                        }
                    })
                    .collect();
                ResolvedType::Id(id, filled_args)
            }
            _ => actual_type
        };

        Ok((resolved_type, IrtExprType::New { field_inits }))
    }

    fn unify(&mut self, expected: ResolvedType, actual: ResolvedType, holes: Option<&mut [Hole]>, line: u32, col: u32) -> ResolvedType {
        let (rt, errors) = Unifier::new(&self.context, holes, line, col).unify(expected, actual);
        for error in errors {
            self.context.push_error(error);
        }
        rt
    }

    fn exhaustive<'a>(&self, patterns: &[&'a ResolvedPattern], typ: ResolvedType) -> bool {
        ExhaustivenessChecker::new(&self.context).exhaustive(patterns, typ)
    }
}

impl TypeStore for &mut dyn ExprResolverContext {
    fn get_type(&self, id: &TypeId) -> &TypeDeclaration {
        self.get_type_decl(id)
    }
}

struct ResolvedLambdaExpr {
    return_type: ResolvedType,
    statements: Vec<Statement>,
}
