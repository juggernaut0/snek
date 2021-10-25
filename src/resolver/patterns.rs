use crate::resolver::{BUILTIN_TYPE_NAMES, TypeStore};
use crate::resolver::irt::Constant;
use crate::resolver::types::{ResolvedField, ResolvedType, TypeDefinition};
use crate::resolver::unifier::Unifier;

#[derive(PartialEq)]
pub struct ResolvedPattern {
    pub resolved_type: ResolvedType,
    pub pattern_type: PatternType,
}

#[derive(PartialEq)]
pub enum PatternType {
    Discard,
    Name(String),
    Constant(Constant),
    Destructuring(Vec<(String, ResolvedPattern)>),
}

pub struct ExhaustivenessChecker<'r> {
    type_store: &'r dyn TypeStore,
}

impl ExhaustivenessChecker<'_> {
    pub fn new(type_store: &dyn TypeStore) -> ExhaustivenessChecker {
        ExhaustivenessChecker { type_store }
    }

    // Do the set of given patterns cover all possible values of typ?
    pub fn exhaustive<'a>(&self, patterns: &[&'a ResolvedPattern], typ: ResolvedType) -> bool {
        if patterns.is_empty() {
            return false;
        }

        if patterns.iter().copied().any(|pattern| self.pattern_is_catchall(pattern, &typ)) {
            return true;
        }

        match typ.clone() {
            ResolvedType::Id(id, args) => {
                let type_def = self.type_store.get_type(&id).definition.clone().instantiate_into(&args);
                if let TypeDefinition::Union(cases) = type_def {
                    cases.into_iter().all(|case| {
                        self.exhaustive(patterns, case)
                    })
                } else if let TypeDefinition::Record(fields) = type_def {
                    self.exh_for_fields(&fields, patterns, &typ)
                } else if id == BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone()) {
                    let mut has_true = false;
                    let mut has_false = false;
                    for pattern in patterns {
                        if let PatternType::Constant(Constant::Boolean(value)) = &pattern.pattern_type {
                            if *value {
                                has_true = true;
                            } else {
                                has_false = true;
                            }
                        }
                    }
                    has_true && has_false
                } else {
                    false
                }
            }
            ResolvedType::Unit => {
                patterns.iter().any(|pattern| {
                    matches!(&pattern.pattern_type, PatternType::Constant(Constant::Unit))
                })
            }
            ResolvedType::TypeParam(_) | ResolvedType::Func { .. } | ResolvedType::Callable(_) | ResolvedType::Any => false,
            ResolvedType::Nothing => true,
            ResolvedType::Inferred => false,
            ResolvedType::Hole(_) => false,
            ResolvedType::Error => false,
        }
    }

    fn exh_for_fields<'a>(&self, fields: &[ResolvedField], patterns: &[&'a ResolvedPattern], typ: &ResolvedType) -> bool {
        if fields.is_empty() {
            return true
        }
        let field = &fields[0];

        // find destructuring patterns of the right type
        let my_patterns: Vec<_> = patterns.iter().copied()
            .filter_map(|pattern| {
                if &pattern.resolved_type != typ {
                    None
                } else if let PatternType::Destructuring(fields) = &pattern.pattern_type {
                    Some((pattern, fields))
                } else {
                    None
                }
            })
            .collect();
        // collect all the patterns for the first field
        let relevant_patterns: Vec<_> = my_patterns.iter()
            .copied()
            .map(|(_, fields)| {
                fields.iter()
                    .find(|(name, _)| name == &field.name)
                    .map(|(_, p)| p)
                    .unwrap_or(&ResolvedPattern { resolved_type: ResolvedType::Inferred, pattern_type: PatternType::Discard })
            })
            .collect();

        if !self.exhaustive(&relevant_patterns, field.resolved_type.clone()) {
            return false
        }

        // TODO this can be optimized to not check the same rel_pattern multiple times
        relevant_patterns.iter().copied().all(|rel_pattern| {
            let new_patterns: Vec<_> = my_patterns.iter().copied()
                .filter_map(|(pattern, fields)| {
                    let it = fields.iter()
                        .find(|(name, _)| name == &field.name)
                        .map(|(_, p)| p)
                        .unwrap_or(&ResolvedPattern { resolved_type: ResolvedType::Inferred, pattern_type: PatternType::Discard });
                    if it == rel_pattern || self.pattern_is_catchall(it, &field.resolved_type) {
                        Some(pattern)
                    } else {
                        None
                    }
                })
                .collect();
            self.exh_for_fields(&fields[1..], &new_patterns, typ)
        })
    }

    fn pattern_is_catchall(&self, pattern: &ResolvedPattern, typ: &ResolvedType) -> bool {
        match &pattern.pattern_type {
            PatternType::Discard => {}
            PatternType::Name(_) => {}
            _ => return false
        }

        self.unifies(pattern.resolved_type.clone(), typ.clone())
    }

    fn unifies(&self, expected: ResolvedType, actual: ResolvedType) -> bool {
        Unifier::new(self.type_store, None, 0, 0).unifies(expected, actual)
    }
}

#[cfg(test)]
mod test {
    use crate::resolver::{BuiltinTypeNames, make_primitive_type};
    use super::*;
    use std::rc::Rc;
    use crate::resolver::types::{TypeDeclaration, TypeId};

    #[test]
    fn exh_wildcard_inferred() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let patterns = vec![
            &ResolvedPattern { resolved_type: ResolvedType::Inferred, pattern_type: PatternType::Discard }
        ];
        let typ = ResolvedType::Any;
        assert!(ec.exhaustive(&patterns, typ));
    }

    #[test]
    fn exh_wildcard_typed() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let patterns = vec![
            ResolvedPattern { resolved_type: ResolvedType::Any, pattern_type: PatternType::Name("x".into()) }
        ];
        let typ = ResolvedType::Any;
        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), typ));
    }

    #[test]
    fn exh_wildcard_typed_mismatch() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let patterns = vec![
            ResolvedPattern { resolved_type: ResolvedType::Unit, pattern_type: PatternType::Name("x".into()) }
        ];
        let typ = ResolvedType::Any;
        assert!(!ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), typ));
    }

    #[test]
    fn exh_named() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let patterns = vec![
            ResolvedPattern { resolved_type: ResolvedType::Inferred, pattern_type: PatternType::Name("x".into()) }
        ];
        let typ = ResolvedType::Any;
        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), typ));
    }

    #[test]
    fn exh_bools() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let patterns = vec![
            ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) },
            ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(false)) },
        ];
        let typ = BuiltinTypeNames::boolean();
        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), typ));
    }

    #[test]
    fn exh_bool() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let patterns = vec![
            ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) },
        ];
        let typ = BuiltinTypeNames::boolean();
        assert!(!ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), typ));
    }

    #[test]
    fn exh_destruct_with_catchall() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("R"), Vec::new());

        let b_pattern = ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(false)) };
        let patterns = vec![
            ResolvedPattern { resolved_type: r_typ.clone(), pattern_type: PatternType::Destructuring(vec![("b".into(), b_pattern)]) },
            ResolvedPattern { resolved_type: ResolvedType::Inferred, pattern_type: PatternType::Discard },
        ];

        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_destruct_exh_field() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("R"), Vec::new());

        let patterns = vec![
            // { let true = b }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) })
                ])
            },
            // { let false = b }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(false)) })
                ])
            },
        ];

        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_destruct_nonexh_field() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("R"), Vec::new());

        let patterns = vec![
            // { let true = b }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) })
                ])
            },
        ];

        assert!(!ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_destruct_exh_subfield() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("R"), Vec::new());

        let patterns = vec![
            // { let true = b, let 0 = n }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) }),
                    ("n".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::number(),  pattern_type: PatternType::Constant(Constant::Number(0.0)) }),
                ])
            },
            // { let true = b }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) }),
                ])
            },
            // { let false = b }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(false)) }),
                ])
            },
        ];

        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_destruct_nonexh_subfield() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("R"), Vec::new());

        let patterns = vec![
            // { let true = b, let 0 = n }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) }),
                    ("n".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::number(),  pattern_type: PatternType::Constant(Constant::Number(0.0)) }),
                ])
            },
            // { let false = b }
            ResolvedPattern {
                resolved_type: r_typ.clone(),
                pattern_type: PatternType::Destructuring(vec![
                    ("b".into(), ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(false)) }),
                ])
            },
        ];

        assert!(!ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_union_with_catchall() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("U"), Vec::new());

        let patterns = vec![
            // n: Number
            ResolvedPattern { resolved_type: BuiltinTypeNames::number(), pattern_type: PatternType::Name("n".into()) },
            // _
            ResolvedPattern { resolved_type: ResolvedType::Inferred, pattern_type: PatternType::Discard },
        ];

        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_union_incomplete() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("U"), Vec::new());

        let patterns = vec![
            // n: Number
            ResolvedPattern { resolved_type: BuiltinTypeNames::number(), pattern_type: PatternType::Name("n".into()) },
        ];

        assert!(!ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_union_complete() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("U"), Vec::new());

        let patterns = vec![
            // n: Number
            ResolvedPattern { resolved_type: BuiltinTypeNames::number(), pattern_type: PatternType::Name("n".into()) },
            // b: Boolean
            ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Name("b".into()) },
        ];

        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_union_incomplete_match() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("U"), Vec::new());

        let patterns = vec![
            // n: Number
            ResolvedPattern { resolved_type: BuiltinTypeNames::number(), pattern_type: PatternType::Name("n".into()) },
            // true
            ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) },
        ];

        assert!(!ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    #[test]
    fn exh_union_complete_match() {
        let ec = ExhaustivenessChecker { type_store: &MockTypeStore::new() };

        let r_typ = ResolvedType::Id(make_id("U"), Vec::new());

        let patterns = vec![
            // n: Number
            ResolvedPattern { resolved_type: BuiltinTypeNames::number(), pattern_type: PatternType::Name("n".into()) },
            // true
            ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(true)) },
            // false
            ResolvedPattern { resolved_type: BuiltinTypeNames::boolean(), pattern_type: PatternType::Constant(Constant::Boolean(false)) },
        ];

        assert!(ec.exhaustive(&patterns.iter().collect::<Vec<_>>(), r_typ));
    }

    fn make_id(name: &str) -> TypeId {
        TypeId::new(Default::default(), vec![name].into())
    }

    fn make_record_type() -> Rc<TypeDeclaration> {
        Rc::new(TypeDeclaration {
            id: make_id("R"),
            num_type_params: 0,
            definition: TypeDefinition::Record(vec![
                ResolvedField {
                    name: "n".to_string(),
                    public: false,
                    resolved_type: BuiltinTypeNames::number(),
                },
                ResolvedField {
                    name: "b".to_string(),
                    public: false,
                    resolved_type: BuiltinTypeNames::boolean(),
                },
            ]),
            visibility: vec![],
            export: false,
        })
    }

    fn make_union_type() -> Rc<TypeDeclaration> {
        Rc::new(TypeDeclaration {
            id: make_id("U"),
            num_type_params: 0,
            definition: TypeDefinition::Union(vec![
                BuiltinTypeNames::number(),
                BuiltinTypeNames::boolean(),
            ]),
            visibility: vec![],
            export: false,
        })
    }

    struct MockTypeStore {
        types: Vec<Rc<TypeDeclaration>>,
    }

    impl MockTypeStore {
        fn new() -> MockTypeStore {
            MockTypeStore {
                types: vec![
                    make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone())),
                    make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.string.clone())),
                    make_primitive_type(BUILTIN_TYPE_NAMES.with(|btn| btn.boolean.clone())),
                    make_record_type(),
                    make_union_type(),
                ]
            }
        }
    }

    impl TypeStore for MockTypeStore {
        fn get_type(&self, id: &TypeId) -> &TypeDeclaration {
            self.types.iter().find(|t| &t.id == id).unwrap()
        }
    }
}
