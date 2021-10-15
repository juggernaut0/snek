use crate::resolver::{Error, Resolver};
use crate::resolver::types::{ResolvedType, TypeDeclaration, TypeDefinition, TypeId};

pub struct Unifier<'r, 'ast> {
    resolver: &'r mut Resolver<'ast>,
    line: u32,
    col: u32,
}

impl Unifier<'_, '_> {
    pub fn new<'r, 'ast>(resolver: &'r mut Resolver<'ast>, line: u32, col: u32,) -> Unifier<'r, 'ast> {
        Unifier { resolver, line, col }
    }

    pub fn unify(&mut self, expected: ResolvedType, actual: ResolvedType) -> ResolvedType {
        let mut errors = Vec::new();
        let res = self.unify_impl(&mut errors, expected, actual);
        for error in errors {
            self.resolver.errors.push(error);
        }
        res
    }

    fn unify_impl(&self, errors: &mut Vec<Error>, expected: ResolvedType, actual: ResolvedType) -> ResolvedType {
        match (expected, actual) {
            (ResolvedType::Inferred, actual) => actual,
            (ResolvedType::Any, _) => ResolvedType::Any,
            (expected, ResolvedType::Nothing) => expected,
            (expected, ResolvedType::Inferred) => expected,
            (expected, ResolvedType::Error) => expected,
            (ResolvedType::Error, _) => ResolvedType::Error,
            (expected, actual) if expected == actual => expected,
            (ResolvedType::Id(e_id, e_type_args), actual) => {
                self.unify_id(errors, e_id, e_type_args, actual)
            },
            (ResolvedType::TypeParam(e_i), ResolvedType::TypeParam(a_i)) => {
                if e_i != a_i {
                    self.add_unify_error(errors, ResolvedType::TypeParam(e_i), ResolvedType::TypeParam(a_i))
                } else {
                    ResolvedType::TypeParam(e_i)
                }
            }
            (ResolvedType::Callable(expected_return_type), ResolvedType::Func { params, mut return_type }) => {
                // Reuse the same Box for the return value
                let actual_return_type = std::mem::replace(return_type.as_mut(), ResolvedType::Error);
                *return_type = self.unify_impl(errors, *expected_return_type, actual_return_type);
                ResolvedType::Func { params, return_type }
            }
            (expected, actual) => {
                self.add_unify_error(errors, expected, actual)
            }
        }
    }

    fn unify_id(&self, errors: &mut Vec<Error>, e_id: TypeId, e_type_args: Vec<ResolvedType>, actual: ResolvedType) -> ResolvedType {
        let def = self.resolver.types.get(&e_id).expect("missing type").definition.clone();

        match def {
            TypeDefinition::Record(_) | TypeDefinition::Primitive => {
                if let ResolvedType::Id(a_id, a_type_args) = actual {
                    if e_id != a_id {
                        self.add_unify_error(errors, ResolvedType::Id(e_id, e_type_args), ResolvedType::Id(a_id, a_type_args))
                    } else {
                        let unified_args = e_type_args
                            .into_iter()
                            .zip(a_type_args)
                            .map(|(e, a)| self.unify_impl(errors, e, a))
                            .collect();
                        ResolvedType::Id(e_id, unified_args)
                    }
                } else {
                    self.add_unify_error(errors, ResolvedType::Id(e_id, e_type_args), actual)
                }
            }
            TypeDefinition::Union(mut cases) => {
                for mut case in cases {
                    case.instantiate(&e_type_args);
                    let mut errors = Vec::new();
                    let _ = self.unify_impl(&mut errors, case, actual.clone());
                    if errors.is_empty() {
                        return ResolvedType::Id(e_id, e_type_args)
                    }
                }
                self.add_unify_error(errors, ResolvedType::Id(e_id, e_type_args), actual)
            }
        }
    }

    fn add_unify_error(&self, errors: &mut Vec<Error>, expected: ResolvedType, actual: ResolvedType) -> ResolvedType {
        errors.push(Error {
            message: format!("Type mismatch. Expected: {} Actual: {}", expected, actual),
            line: self.line,
            col: self.col,
        });
        expected
    }
}
