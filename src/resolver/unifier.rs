use crate::resolver::{Error, TypeStore};
use crate::resolver::types::{Hole, ResolvedType, TypeDefinition, TypeId};

pub struct Unifier<'r, 'h> {
    type_store: &'r dyn TypeStore,
    holes: Option<&'h mut [Hole]>,
    line: u32,
    col: u32,
}

impl Unifier<'_, '_> {
    pub(super) fn new<'r, 'h>(type_store: &'r dyn TypeStore, holes: Option<&'h mut [Hole]>, line: u32, col: u32,) -> Unifier<'r, 'h> {
        Unifier { type_store, holes, line, col }
    }

    pub fn unify(&mut self, expected: ResolvedType, actual: ResolvedType) -> (ResolvedType, Vec<Error>) {
        let mut errors = Vec::new();
        let res = self.unify_impl(&mut errors, expected, actual, true);
        (res, errors)
    }

    pub fn unifies(&mut self, expected: ResolvedType, actual: ResolvedType) -> bool {
        let mut errors = Vec::new();
        let _ = self.unify_impl(&mut errors, expected, actual, true);
        errors.is_empty()
    }

    fn unify_impl(&mut self, errors: &mut Vec<Error>, expected: ResolvedType, actual: ResolvedType, allow_any: bool) -> ResolvedType {
        match (expected, actual) {
            (ResolvedType::Inferred, actual) => actual,
            (ResolvedType::Hole(i), actual) => {
                if let Some(holes) = &mut self.holes {
                    match &mut holes[i] {
                        hole @ Hole::Empty => *hole = Hole::Filled(actual.clone()),
                        Hole::Filled(ex) => merge(ex, actual.clone()),
                        Hole::Fixed => panic!("tried to fill a fixed hole"),
                    }
                }
                actual
            },
            (ResolvedType::Any, _) if allow_any => ResolvedType::Any,
            (expected, ResolvedType::Nothing) => expected,
            (expected, ResolvedType::Inferred) => expected,
            (expected, ResolvedType::Error) => expected,
            (ResolvedType::Error, _) => ResolvedType::Error,
            (expected, actual) if expected == actual => expected,
            (ResolvedType::Id(e_id, e_type_args), actual) => {
                self.unify_id(errors, e_id, e_type_args, actual, allow_any)
            },
            (ResolvedType::TypeParam(_), ResolvedType::TypeParam(_)) => {
                panic!("raw type params should never be compared"); // TODO is this true in a generic function?
            }
            (ResolvedType::Callable(expected_return_type), ResolvedType::Func { params, mut return_type }) => {
                // Reuse the same Box for the return value
                let actual_return_type = std::mem::replace(return_type.as_mut(), ResolvedType::Error);
                *return_type = self.unify_impl(errors, *expected_return_type, actual_return_type, true);
                ResolvedType::Func { params, return_type }
            }
            // TODO unify func - Return type can unify as normal, but params must not allow "generalizing" to Any
            (
                ResolvedType::Func { params: e_params, return_type: e_rt },
                ResolvedType::Func { params: a_params, return_type: a_rt }
            ) => {
                let params = e_params.into_iter()
                    .zip(a_params)
                    .map(|(ep, ap)| self.unify_impl(errors, ep, ap, false))
                    .collect();
                let return_type = Box::new(self.unify_impl(errors, *e_rt, *a_rt, true));
                ResolvedType::Func { params, return_type }
            }
            (expected, actual) => {
                self.add_unify_error(errors, expected, actual)
            }
        }
    }

    fn unify_id(&mut self, errors: &mut Vec<Error>, e_id: TypeId, e_type_args: Vec<ResolvedType>, actual: ResolvedType, allow_any: bool) -> ResolvedType {
        let def = self.type_store.get_type(&e_id).definition.clone().instantiate_into(&e_type_args);

        match def {
            TypeDefinition::Record(_) | TypeDefinition::Primitive => {
                if let ResolvedType::Id(a_id, a_type_args) = actual {
                    if e_id != a_id {
                        self.add_unify_error(errors, ResolvedType::Id(e_id, e_type_args), ResolvedType::Id(a_id, a_type_args))
                    } else {
                        let unified_args = e_type_args
                            .into_iter()
                            .zip(a_type_args)
                            .map(|(e, a)| self.unify_impl(errors, e, a, allow_any))
                            .collect();
                        ResolvedType::Id(e_id, unified_args)
                    }
                } else {
                    self.add_unify_error(errors, ResolvedType::Id(e_id, e_type_args), actual)
                }
            }
            TypeDefinition::Union(cases) => {
                for case in cases {
                    if self.unifies(case, actual.clone()) {
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

// Find most specific common supertype of a and b
pub fn merge_types(mut a: ResolvedType, b: ResolvedType) -> ResolvedType {
    merge(&mut a, b);
    a
}

// Find most specific common supertype of a and b and store into a
fn merge(a: &mut ResolvedType, b: ResolvedType) {
    if a == &b {
        return
    }
    match (a, b) {
        (ResolvedType::Id(a_id, a_args), ResolvedType::Id(b_id, b_args)) if a_id == &b_id => {
            a_args.iter_mut().zip(b_args).for_each(|(aa, ba)| merge(aa, ba));
        }
        (a, b) if a == &ResolvedType::Inferred => *a = b,
        (a, _) => *a = ResolvedType::Any,
    }
}
