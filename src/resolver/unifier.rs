use crate::resolver::{Error, TypeStore};
use crate::resolver::types::{Hole, ResolvedType, TypeDefinition, TypeId};

pub struct Unifier<'r, 'h> {
    type_store: &'r dyn TypeStore,
    holes: Option<&'h mut [Hole]>,
    line: u32,
    col: u32,
}

impl Unifier<'_, '_> {
    pub fn new<'r, 'h>(type_store: &'r dyn TypeStore, holes: Option<&'h mut [Hole]>, line: u32, col: u32,) -> Unifier<'r, 'h> {
        Unifier { type_store, holes, line, col }
    }

    pub fn unify(&mut self, expected: ResolvedType, actual: ResolvedType) -> (ResolvedType, Vec<Error>) {
        let mut errors = Vec::new();
        let res = self.unify_impl(&mut errors, expected, actual);
        (res, errors)
    }

    pub fn unifies(&mut self, expected: ResolvedType, actual: ResolvedType) -> bool {
        let mut errors = Vec::new();
        let _ = self.unify_impl(&mut errors, expected, actual);
        errors.is_empty()
    }

    fn unify_impl(&mut self, errors: &mut Vec<Error>, expected: ResolvedType, actual: ResolvedType) -> ResolvedType {
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
            (ResolvedType::Any, _) => ResolvedType::Any,
            (expected, ResolvedType::Nothing) => expected,
            (expected, ResolvedType::Inferred) => expected,
            (expected, ResolvedType::Error) => expected,
            (ResolvedType::Error, _) => ResolvedType::Error,
            (expected, actual) if expected == actual => expected,
            (ResolvedType::Id(e_id, e_type_args), actual) => {
                self.unify_id(errors, e_id, e_type_args, actual)
            },
            (ResolvedType::TypeParam(_), ResolvedType::TypeParam(_)) => {
                panic!("raw type params should never be compared");
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

    fn unify_id(&mut self, errors: &mut Vec<Error>, e_id: TypeId, e_type_args: Vec<ResolvedType>, actual: ResolvedType) -> ResolvedType {
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
                            .map(|(e, a)| self.unify_impl(errors, e, a))
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

// Find most specific common supertype of a and b and store into a
fn merge(a: &mut ResolvedType, b: ResolvedType) {
    todo!("merge holes")
}

/*fn unify_holes(holey: ResolvedType, actual: ResolvedType, holes: &mut [ResolvedType]) {
    match (holey, actual) {
        (ResolvedType::Hole(i), any) => holes[i] = any,
        (ResolvedType::Id(hid, h_args), ResolvedType::Id(aid, a_args)) if hid == aid => {
            for (h_arg, a_arg) in h_args.into_iter().zip(a_args) {
                unify_holes(h_arg, a_arg, holes);
            }
        }
        (ResolvedType::Func { params: h_params, return_type: h_rt }, ResolvedType::Func { params: a_params, return_type: a_rt }) => {
            for (hp, ap) in
        }
    }
}*/
