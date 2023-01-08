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

    fn require_holes(&mut self) -> &mut [Hole] {
        self.holes.as_mut().expect("tried to get a hole but there were none")
    }

    // TODO generalize "allow_any" to also disallow unions
    fn unify_impl(&mut self, errors: &mut Vec<Error>, expected: ResolvedType, actual: ResolvedType, allow_any: bool) -> ResolvedType {
        #[cfg(debug_assertions)]
        {
            println!("unify exp: {:?} actual: {:?}", expected, actual);
        }
        if let ResolvedType::Hole(_) = actual {
            panic!("hole in actual")
        }
        match (expected, actual) {
            (ResolvedType::Inferred, actual) => actual,
            (ResolvedType::Hole(i), actual) => {
                let holes = self.require_holes();
                let ex =
                    match &mut holes[i] {
                        hole @ Hole::Empty => {
                            *hole = Hole::Filled(actual.clone());
                            None
                        },
                        Hole::Filled(ex) => {
                            merge(ex, actual.clone());
                            None
                        },
                        Hole::Fixed(ex) => Some(ex.clone()),
                    };
                if let Some(ex) = ex {
                    let rt = self.unify_impl(errors, ex, actual.clone(), allow_any);
                    self.require_holes()[i] = Hole::Fixed(rt.clone());
                    rt
                } else {
                    actual
                }
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
            (ResolvedType::Callable(expected_return_type), ResolvedType::Func { num_type_params, params, mut return_type }) => {
                // Reuse the same Box for the return value
                let actual_return_type = std::mem::replace(return_type.as_mut(), ResolvedType::Error);
                *return_type = self.unify_impl(errors, *expected_return_type, actual_return_type, true);
                ResolvedType::Func { num_type_params, params, return_type }
            }
            (
                ResolvedType::Func { num_type_params: e_num_type_params, params: e_params, return_type: e_rt },
                ResolvedType::Func { num_type_params: a_num_type_params, params: a_params, return_type: a_rt }
            ) => {
                if e_num_type_params != a_num_type_params {
                    errors.push(Error {
                        message: format!("Generic type parameter mismatch. Expected {} type parameters, found {}", e_num_type_params, a_num_type_params),
                        line: self.line,
                        col: self.col,
                    })
                }

                if e_params.len() != a_params.len() {
                    errors.push(Error {
                        message: format!("Parameter count mismatch. Expected {} parameters, found {}", e_params.len(), a_params.len()),
                        line: self.line,
                        col: self.col,
                    })
                }

                let params = e_params.into_iter()
                    .zip(a_params)
                    .map(|(ep, ap)| self.unify_impl(errors, ep, ap, false))
                    .collect();
                let return_type = Box::new(self.unify_impl(errors, *e_rt, *a_rt, true));
                ResolvedType::Func { num_type_params: e_num_type_params, params, return_type }
            }
            (expected, actual) => {
                self.add_unify_error(errors, expected, actual)
            }
        }
    }

    fn unify_id(&mut self, errors: &mut Vec<Error>, e_id: TypeId, e_type_args: Vec<ResolvedType>, actual: ResolvedType, allow_any: bool) -> ResolvedType {
        let def = self.type_store.get_type(&e_id).definition.clone().instantiate_into(&e_type_args);

        if let ResolvedType::Id(a_id, a_type_args) = actual.clone() {
            if e_id == a_id {
                let unified_args = e_type_args
                    .into_iter()
                    .zip(a_type_args)
                    .map(|(e, a)| self.unify_impl(errors, e, a, allow_any))
                    .collect();
                return ResolvedType::Id(e_id, unified_args)
            }
        }

        if let TypeDefinition::Union(cases) = def {
            for case in cases {
                if self.unifies(case, actual.clone()) {
                    return ResolvedType::Id(e_id, e_type_args)
                }
            }
        }

        self.add_unify_error(errors, ResolvedType::Id(e_id, e_type_args), actual)
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
