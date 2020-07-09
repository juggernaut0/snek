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
    let src = "
namespace A {
    namespace B {
        public type A # visibility: A
        type B # visibility: A.B
    }
    public namespace B {
        public type C # visibility: <root>
        type D # visibility: A.B
    }
    type E # visibility: A
}
type F # visibility: <root>
        ";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let resolver = Resolver::new(Rc::new(String::new()), &[]);
    let mut type_declarations = Vec::new();
    resolver.find_types(&mut type_declarations, &ast.root_namespace, QNameList::Empty, QNameList::Empty);

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
    let src = "
type Foo { a: Foo.Bar.A }
namespace Foo {
    public namespace Bar {
        public type A { b: Foo.B }
        type X { b: B }
    }
    type B { c: C }
    type Y { a: Bar.A }
}
type C
        ";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    assert!(resolver.errors.is_empty());
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
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
fn visibility_error() {
    let src = "
namespace Foo {
    type Hidden
}
type A { f: Foo.Hidden }
";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    assert!(resolver.errors.is_empty());
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
    assert!(!resolver.errors.is_empty());
}

#[test]
fn union() {
    let src = "type Option<T> = Some { t: T } | None { }";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(3, resolver.types.len());
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
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    let t = resolver.types.values().next().unwrap();
    let fields = if let TypeDefinition::Record(fields) = &t.definition { fields } else { panic!() };
    let consume = fields.first().unwrap();
    assert_eq!("consume", consume.name);
    let rft = if let ResolvedType::Func(rft) = &consume.resolved_type { rft } else { panic!() };
    assert_eq!(ResolvedType::TypeParam(0), rft.params[0]);
    assert_eq!(ResolvedType::TypeParam(0), rft.params[1]);
    assert_eq!(&ResolvedType::Unit, rft.return_type.as_ref());
}

#[test]
fn named_globals() {
    let src = "\
let a = 5
namespace A { let a = 1 }
namespace B { let a = 2 }
namespace A.B.C { let a = 3 }
namespace A.B {
    namespace C.D {
        let a = 4
    }
}";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let mut undefined_globals = Vec::new();
    let lookup = TypeDeclLookup::new(&[]);
    resolver.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, lookup.root_scope());
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(5, undefined_globals.len());
    undefined_globals.iter().find(|it| it.fqn.as_slice() == ["a"]).expect("Expected a");
    undefined_globals.iter().find(|it| it.fqn.as_slice() == ["A", "a"]).expect("Expected A.a");
    undefined_globals.iter().find(|it| it.fqn.as_slice() == ["B", "a"]).expect("Expected B.a");
    undefined_globals.iter().find(|it| it.fqn.as_slice() == ["A", "B", "C", "a"]).expect("Expected A.B.C.a");
    undefined_globals.iter().find(|it| it.fqn.as_slice() == ["A", "B", "C", "D", "a"]).expect("Expected A.B.C.D.a");
}

#[test]
fn destructured_simple() {
    let src = "\
type A { x: () }
namespace One {
    let { x } = ()
}
namespace Two {
    let { x }: A = ()
}
namespace Three {
    let { x: () } = ()
}
namespace Four {
    let { x: () }: A = ()
}
";
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
    let mut undefined_globals = Vec::new();
    resolver.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, lookup.root_scope());
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(4, undefined_globals.len());

    // Does this need to be order-independent?
    assert_eq!(["One", "x"], undefined_globals[0].fqn.as_slice());
    assert_eq!(ResolvedType::Inferred, undefined_globals[0].declared_type);

    assert_eq!(["Two", "x"], undefined_globals[1].fqn.as_slice());
    assert_eq!(ResolvedType::Unit, undefined_globals[1].declared_type);

    assert_eq!(["Three", "x"], undefined_globals[2].fqn.as_slice());
    assert_eq!(ResolvedType::Unit, undefined_globals[2].declared_type);

    assert_eq!(["Four", "x"], undefined_globals[3].fqn.as_slice());
    assert_eq!(ResolvedType::Unit, undefined_globals[3].declared_type);
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
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
    let mut undefined_globals = Vec::new();
    resolver.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, lookup.root_scope());
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(1, undefined_globals.len());

    let global = &undefined_globals[0];
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
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
    let mut undefined_globals = Vec::new();
    resolver.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, lookup.root_scope());
    assert!(resolver.errors.is_empty(), "{:?}", resolver.errors);

    assert_eq!(1, undefined_globals.len());

    let global = &undefined_globals[0];
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
    let src = "\
namespace A {
    public type A { x: () }
}
let { x }: A.A = (TODO)
";
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

fn find_globals(src: &str) -> Resolver {
    let (ast, errs) = crate::parser::parse(src);
    assert!(errs.is_empty());
    let mod_name = Rc::new(String::new());
    let mut resolver = Resolver::new(Rc::clone(&mod_name), &[]);
    let mut undefined_types = Vec::new();
    resolver.find_types(&mut undefined_types, &ast.root_namespace, QNameList::Empty, QNameList::Empty);
    let lookup = TypeDeclLookup::new(&undefined_types);
    resolver.define_types(undefined_types, lookup.root_scope());
    let mut undefined_globals = Vec::new();
    resolver.find_globals(&mut undefined_globals, &ast.root_namespace, QNameList::Empty, QNameList::Empty, lookup.root_scope());
    resolver
}

fn get_type<'a>(resolver: &'a Resolver, name: &str) -> &'a TypeDeclaration {
    resolver.types.values().find(|it| it.id.fqn().as_slice().last().unwrap() == name).unwrap()
}