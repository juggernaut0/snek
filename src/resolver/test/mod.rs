use std::rc::Rc;

use super::*;

#[test]
fn qname_list_iter() {
    let a = vec![String::from("a"), String::from("b")];
    let b = vec![String::from("c")];
    let c = vec![String::from("d"), String::from("e")];

    let l1 = QNameList::Empty;
    let l2 = QNameList::List(&l1, &a);
    let l3 = QNameList::List(&l2, &b);
    let l4 = QNameList::List(&l3, &c);

    assert_eq!(5, l4.len());
    let full: Vec<_> = l4.iter().collect();
    assert_eq!(vec!["a", "b", "c", "d", "e"], full);
}

#[test]
fn qname_list_matches() {
    let type_id = TypeId::new(
        Rc::new(String::new()),
        vec!["a", "b", "c"].into(),
    );

    let a = vec![String::from("a"), String::from("b")];
    let b = vec![String::from("c")];

    let l1 = QNameList::Empty;
    let l2 = QNameList::List(&l1, &a);
    let l3 = QNameList::List(&l2, &b);

    assert!(l3.matches(type_id.fqn()))
}

#[test]
fn type_decl_visibility() {
    let src = include_str!("type_decl_visibility.snek");
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mut resolver = Resolver::new(Rc::new(String::new()), &[]);
    let type_declarations = resolver.resolve_1(&ast);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    println!("{:?}", resolver.types.keys());
    let assert_vis = |name: Vec<&str>, expected_vis: Vec<&str>| {
        println!("{:?}", name);
        let t = type_declarations.iter().find(|it| it.id.fqn().as_slice() == name.as_slice()).unwrap();
        assert_eq!(expected_vis, t.visibility, "type: {:?}", name)
    };

    assert_vis(vec!["A", "B", "A"], vec!["A"]);
    assert_vis(vec!["A", "B", "B"], vec!["A", "B"]);
    assert_vis(vec!["A", "B", "C"], vec![]);
    assert_vis(vec!["A", "B", "D"], vec!["A", "B"]);
    assert_vis(vec!["A", "E"], vec!["A"]);
    assert_vis(vec!["F"], vec![]);
}

#[test]
fn type_definition() {
    let src = include_str!("type_definition.snek");
    let mod_name = Rc::new(String::new());
    let resolver = define_types(src);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    let check_type_def = |name: Vec<&str>, expected_fields: Vec<(&str, Vec<&str>)>| {
        let id = TypeId::new(Rc::clone(&mod_name), name.into());
        let t = resolver.types.get(&id).unwrap();

        if let TypeDefinition::Record(fields) = &t.definition {
            assert_eq!(expected_fields.len(), fields.len(), "In type {:?}", id);
            for (rf, (exp_name, exp_type)) in fields.iter().zip(&expected_fields) {
                assert_eq!(exp_name, &rf.name, "In type {:?}", id);
                assert_eq!(&ResolvedType::Id(TypeId::new(Rc::clone(&mod_name), exp_type.as_slice().into()), Vec::new()), &rf.resolved_type, "In type {:?}", id)
            }
        } else {
            panic!("expected a record for {}", id.fqn())
        }
    };

    check_type_def(vec!["Foo"], vec![("a", vec!["Foo", "Bar", "A"])]);
    check_type_def(vec!["Foo", "Bar", "A"], vec![("b", vec!["Foo", "B"])]);
    check_type_def(vec!["Foo", "Bar", "X"], vec![("b", vec!["Foo", "B"])]);
    check_type_def(vec!["Foo", "B"], vec![("c", vec!["C"])]);
    check_type_def(vec!["Foo", "Y"], vec![("a", vec!["Foo", "Bar", "A"])]);
    check_type_def(vec!["C"], vec![]);
}

#[test]
fn unknown_type() {
    let src = "type Test { x: What }";
    let resolver = define_types(src);
    assert!(!resolver.errors.is_empty());
}

#[test]
fn visibility_error() {
    let src = "
namespace Foo {
    type Hidden
}
type A { f: Foo.Hidden }
";
    let resolver = define_types(src);
    assert!(!resolver.errors.is_empty());
}

#[test]
fn union() {
    let src = "type Option<T> = Some { t: T } | None { }";
    let resolver = define_types(src);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(3, resolver.types.iter().filter(|(t, _)| t.module() != "__builtin").count());
    {
        let option = get_type(&resolver, "Option");
        assert_eq!(1, option.num_type_params);
        if let TypeDefinition::Union(cases) = &option.definition {
            assert_eq!(2, cases.len());
        } else {
            panic!()
        }
    }
    {
        let some = get_type(&resolver, "Some");
        assert_eq!(1, some.num_type_params);
        if let TypeDefinition::Record(fields) = &some.definition {
            assert_eq!(1, fields.len());
            assert_eq!("t", &fields[0].name)
        } else {
            panic!()
        }
    }
    {
        let none = get_type(&resolver, "None");
        assert_eq!(1, none.num_type_params);
        if let TypeDefinition::Record(fields) = &none.definition {
            assert_eq!(0, fields.len());
        } else {
            panic!()
        }
    }
}

#[test]
fn func_field() {
    let src = "type BiConsumer<T> { consume: { T T -> () } }";
    let resolver = define_types(&src);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    let t = resolver.types
        .iter()
        .find_map(|(id, d)| if id.fqn().as_slice() == &["BiConsumer"] { Some(d) } else { None })
        .unwrap();
    let fields = if let TypeDefinition::Record(fields) = &t.definition { fields } else { panic!() };
    let consume = fields.first().unwrap();
    assert_eq!("consume", consume.name);
    let (params, return_type) = if let ResolvedType::Func { params, return_type } = &consume.resolved_type { (params, return_type) } else { panic!() };
    assert_eq!(ResolvedType::TypeParam(0), params[0]);
    assert_eq!(ResolvedType::TypeParam(0), params[1]);
    assert_eq!(&ResolvedType::Unit, return_type.as_ref());
}

#[test]
fn named_globals() {
    let src = include_str!("named_globals.snek");
    let resolver = define_globals(&src);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);
    let globals: Vec<_> = resolver.globals.values().filter(|it| it.id.module() != "__builtin").collect();

    assert_eq!(5, globals.len());
    globals.iter().find(|it| it.fqn().as_slice() == ["a"]).expect("Expected a");
    globals.iter().find(|it| it.fqn().as_slice() == ["A", "a"]).expect("Expected A.a");
    globals.iter().find(|it| it.fqn().as_slice() == ["B", "a"]).expect("Expected B.a");
    globals.iter().find(|it| it.fqn().as_slice() == ["A", "B", "C", "a"]).expect("Expected A.B.C.a");
    globals.iter().find(|it| it.fqn().as_slice() == ["A", "B", "C", "D", "a"]).expect("Expected A.B.C.D.a");
}

#[test]
fn destructured_simple() {
    let src = include_str!("destructured_simple.snek");
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let (undefined_globals, _) = resolver.resolve_3(&ast);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(4, undefined_globals.len());

    // Does this need to be order-independent?
    assert_eq!(["One", "x"], undefined_globals[0].decls[0].fqn.as_slice());
    assert_eq!(ResolvedType::Inferred, undefined_globals[0].decls[0].declared_type);

    assert_eq!(["Two", "x"], undefined_globals[1].decls[0].fqn.as_slice());
    assert_eq!(ResolvedType::Unit, undefined_globals[1].decls[0].declared_type);

    assert_eq!(["Three", "x"], undefined_globals[2].decls[0].fqn.as_slice());
    assert_eq!(ResolvedType::Unit, undefined_globals[2].decls[0].declared_type);

    assert_eq!(["Four", "x"], undefined_globals[3].decls[0].fqn.as_slice());
    assert_eq!(ResolvedType::Unit, undefined_globals[3].decls[0].declared_type);
}

#[test]
fn destructured_generic() {
    let src = "\
type A<T> { x: T }
let { x }: A<()> = (TODO)
";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let (undefined_globals, _) = resolver.resolve_3(&ast);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(1, undefined_globals.len());

    let global = &undefined_globals[0].decls[0];
    assert_eq!(["x"], global.fqn.as_slice());
    assert_eq!(ResolvedType::Unit, global.declared_type);
}

#[test]
fn destructured_named_type() {
    let src = "\
type A
type B { x: A }
let { x: A }: B = (TODO)
";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let (undefined_globals, _) = resolver.resolve_3(&ast);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(1, undefined_globals.len());

    let global = &undefined_globals[0].decls[0];
    assert_eq!(["x"], global.fqn.as_slice());
    let type_id = match &global.declared_type {
        ResolvedType::Id(id, _) => id,
        _ => panic!()
    };
    assert_eq!(["A"], type_id.fqn().as_slice());
}

#[test]
#[ignore] // TODO detecting field visibility will happen at a later stage
fn destructured_private_field() {
    let src = include_str!("destructured_private_field.snek");
    let resolver = find_globals(src);

    // should error
    assert!(!resolver.errors.is_empty());
}

#[test]
fn destructured_private_field_same_ns() {
    let src = "\
namespace A {
    public type A { x: () }
    let { x }: A = (TODO)
}
";
    let resolver = find_globals(src);

    assert!(resolver.errors.is_empty());
}

#[test]
fn destructured_public_field() {
    let src = "\
namespace A {
    public type A { public x: () }
}
let { x }: A.A = (TODO)
";
    let resolver = find_globals(src);

    assert!(resolver.errors.is_empty());
}

#[test]
fn loopy() {
    let src = "let a = a";
    let resolver = resolve_from_src(src);
    assert!(resolver.is_err());
}

#[test]
fn no_loopy() {
    let src = "let a: () = a";
    assert_no_errs(resolve_from_src(src));
}

#[test]
fn inference() {
    let src = "\
let x = 5
let y = x
    ";
    let (decls, _) = assert_no_errs(resolve_from_src(src));

    let number_type = ResolvedType::Id(BUILTIN_TYPE_NAMES.with(|btn| btn.number.clone()), Vec::new());

    {
        let global = get_global(&decls, "x");
        assert_eq!(number_type.clone(), global.resolved_type);
    }
    {
        let global = get_global(&decls, "y");
        assert_eq!(number_type.clone(), global.resolved_type);
    }
}

#[test]
fn type_conflict() {
    let src = "let x: String = 5";

    let result = resolve_from_src(src);
    assert!(result.is_err());
}

#[test]
fn explicit_types() {
    let src = "\
let n: Number = 5
let s: String = 'hello'
let u: () = ()
let b: Boolean = true
";

    assert_no_errs(resolve_from_src(src));
}

#[test]
fn wildcard_discard() {
    let src = "let _ = 5";

    let (decls, irt) = assert_no_errs(resolve_from_src(src));
    assert!(decls.globals.is_empty());
    assert_eq!(irt.statements.len(), 1);
    assert!(matches!(irt.statements[0], Statement::Discard(irt::Expr { expr_type: irt::ExprType::LoadConstant(irt::Constant::Number(5.0)), .. })));
}

#[test]
#[ignore] // TODO needs resolve_expr new
fn inferred_return_type() {
    let src = include_str!("inferred_return_type.snek");
    assert_no_errs(resolve_from_src(src));
}

#[test]
fn hello() {
    assert_no_errs(resolve_from_src(include_str!("../../../tests/hello.snek")));
}

#[test]
fn quick_maffs() {
    assert_no_errs(resolve_from_src(include_str!("../../../tests/math.snek")));
}

#[test]
fn union_case_unification() {
    assert_no_errs(resolve_from_src(include_str!("union_case_unification.snek")));
}

#[test]
fn union_case_mismatch() {
    assert!(resolve_from_src(include_str!("union_case_mismatch.snek")).is_err());
}

#[test]
fn generic_union() {
    assert_no_errs(resolve_from_src(include_str!("generic_union.snek")));
}

#[test]
fn basic_functions() {
    assert_no_errs(resolve_from_src(include_str!("../../../tests/func.snek")));
}

#[test]
fn nested_functions() {
    assert_no_errs(resolve_from_src(include_str!("nested_functions.snek")));
}

fn define_types(src: &str) -> Resolver {
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    resolver.resolve_2(&ast);
    resolver
}

fn find_globals(src: &str) -> Resolver {
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    resolver.resolve_3(&ast);
    resolver
}

fn define_globals(src: &str) -> Resolver {
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty(), "parser had errors: {:?}", errs);
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    resolver.resolve_4(&ast);
    resolver
}

fn resolve_from_src(src: &str) -> ResolveResult {
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty(), "parser had errors: {:?}", errs);
    let mod_name = Rc::new(String::new());
    resolve(mod_name, &[], &ast)
}

fn get_type<'a>(resolver: &'a Resolver, name: &str) -> &'a TypeDeclaration {
    resolver.types.values().find(|it| it.id.fqn().as_slice().last().unwrap() == name).unwrap()
}

fn get_global<'a>(decls: &'a ModuleDecls, name: &str) -> &'a GlobalDeclaration {
    decls.globals.iter().find(|it| {
        println!("{}", it.fqn());
        it.fqn().as_slice().last().unwrap() == name
    }).unwrap()
}

fn assert_no_errs(result: ResolveResult) -> (ModuleDecls, IrTree) {
    match result {
        Err(errors) => panic!("resolver had errors: {:?}", errors),
        Ok(stuff) => stuff,
    }
}

#[test]
fn test_unescape() {
    assert_eq!(unescape("'\\'\\\"\\n\\\\'"), Ok("'\"\n\\".into()));
}
