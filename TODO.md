# Data flow
1. Scanning: Source -> Scanner -> Token stream
1. Parsing: Tokens -> Parser -> Ast
1. Resolve imports: Ast -> Importer -> ModuleDag
    * Build dag from imports: Error on recursive import
1. Resolve declarations & Usages, and types: Ast + Deps' ModuleDecls -> Resolver -> Irt + ModuleDecls
    * Resolver tracks all declarations, private and public, and their types
    * Need to resolve an Ast's dependencies' declarations before itself
1. CodeGen: TypedAst -> CodeGenerator -> Code blocks
1. Can do two things with Code
    1. Interpret: Code -> Interpreter
    1. Serialize: Code -> BytecodeSerializer -> File
        * Can serialize to many formats (direct to JVM?)

# TODO
* Performance improvements
    * Consider alternatives to `Rc` for sharing values
        * Implement tracing GC for values?
        * Arena allocation?
    * Simplify opcode decoding
    * Faster implementation of locals and captured bindings

## Gc impls

GC ON HOLD

* Wrap environments & values in a `Mark<T>` struct that can be marked.
  * Pros: easy to implement mark logic, no additional dynamic allocations
  * Cons: an additional RefCell<bool> for every value, an additional
    layer of unwrapping to do to access a value & when giving a reference to 
    interpreter
* Keep a Map of marked objects in GcRoot that can be reused between GC 
  cycles.
  * Pros: Marking logic can be entirely within the GcRoot impl
  * Cons: Dynamic allocations might happen as the Map expands.

## Resolver
1. Find all type declarations (including imported). Store (TypeId, visibility) in a lookup.
    * Visibility is a qname from where the name is accessible. This name or any child ns can 
    access.
1. Resolve record type fields. Make type declarations 
1. Starting from top-level scope, recursively extract all name declarations and record their 
declared type. Give each a unique ID and store name -> decl in a map.
1. Starting from top-level scope, resolve usages.
    * For each value decl (binding), recurse through initializer expr tree
    * When encountering names, look them up in scope and determine what declaration they are
    * Recursively resolve value decl type
        * When encountering a name, if it has a declared type, or an already resolved type, use 
          that type directly.
        * Otherwise, put the current expr on hold, and recurse to resolve that names type.
        * To avoid loops, keep track of the root decl that you are working on. If encountered, 
          emit an error.
    * Record resolved type in decl

### Resolver Example

a.snek
```
public type Foo { x: String }
public let make_foo = { -> new Foo { x: "hello" } }
public let break_foo = { { x }: Foo -> x }
```

1. Types
    - builtin:String - <root> (implicitly imported)
    - a.snek:Foo - <root>
2. resolved record type defs
    - a.snek:Foo { x: builtin:String }
3. binding decls and declared types
    - make_foo: Inferred -> a.snek:0
    - break_foo: Inferred -> a.snek:1
4. resolve exprs
    1. Line 2: Function expr: expected type is "Inferred"
        - create new scope
        - add params to scope
        - resolve body
            - new expr: expected type is "Inferred"
                - resolve Foo to a.snek:Foo
                - Unify Inferred & a.snek:Foo -> a.snek:Foo
                - expr type is a.snek:Foo
            - function body return type resolves is a.snek:Foo
        - unify Inferred & { -> a.snek:Foo } -> { -> a.snek:Foo }
        - expr type is { -> a.snek:Foo }
        - record type of a.snek:0 as { -> a.snek:Foo }
    2. Line 3: Function expr: expected type is "Inferred"
        - create new scope
        - add params to scope
            - decon pattern: Foo resolves to a.snek:Foo
                - new declaration x: builtin:String -> a.snek:2
        - resolve body
            - qname expr: Expected type is "Inferred"
                - resolve x to a.snek:2
                - unify Inferred & builtin:String -> builtin:String
                - expr type is builtin:String
            - function body return type is builtin:String
        - Unify Inferred & { a.snek:Foo -> builtin:String } -> { a.snek:Foo -> builtin:String }
        - expr type is { a.snek:Foo -> builtin:String }
        - record type of a.snek:1 as { a.snek:Foo -> builtin:String }
5. Exports
    - Add Foo -> a.snek:Foo to exported types
    - Add make_foo -> a.snek:0: { -> a.snek:Foo } to exported values
    - Add break_foo -> a.snek:1: { a.snek:Foo -> builtin:String } to exported values

b.snek
```
import make_foo, break_foo from "a.snek"
let foo = (make_foo)
(println (break_foo foo))
```

Steps:
1. Types
2. resolve record type defs: none
3. binding decls and declared types
    - println: { Any -> builtin:Unit }: b.snek:0
    - make_foo: { -> a.snek:Foo } -> b.snek:1
    - break_foo: { a.snek:Foo -> builtin:String } -> b.snek:2
    - foo: Inferred -> b.snek:3
4. Resolve exprs
    1. Line 2: call_expr: expected type "Inferred"
        - resolve callee
            - qname expr: Expected type is { ? -> Inferred }
                - resolve make_foo to b.snek:1
                - Unify { ? -> Inferred } & { -> a.snek:Foo } -> { -> a.snek:Foo }
                - expr type is { -> a.snek:Foo }
        - verify param count: 0 == 0
        - resolve params
        - unify "Inferred" & a.snek:Foo -> a.snek:Foo
        - expr type is a.snek:Foo
        - record type of b.snek:3 as a.snek:Foo
    2. Line 3: call expr: expected type "Any"
        - resolve callee
            - qname expr: Expected type is { ? -> Any }
                - resolve println to b.snek:0
                - Unify { ? -> Any } & { Any -> builtin:Unit } -> { Any -> Any }
                - expr type is { Any -> Any }
        - verify param count: 1 == 1
        - resolve params
            - call expr: expected type is "Any"
                - resolve callee
                    - qname expr: Expected type is { ? -> Any }
                        - resolve break_foo to b.snek:2
                        - Unify { ? -> Any } & { a.snek:Foo -> builtin:String } -> { a.snek:Foo -> Any }
                        - expr type is { a.snek:Foo -> Any }
                - verify param count: 1 == 1
                - resolve params
                    - qname expr: expected type is a.snek:Foo
                        - resolve foo to b.snek:3
                        - unify a.snek:Foo & a.snek:Foo -> a.snek:Foo
                        - expr type is a.snek:Foo
                - unify Any & Any -> Any
                - expr type is Any
        - unify Any & Any -> Any
        - expr type is Any
5. Exports: none

```
let b: Foo.B = new { a: () }
namespace Foo {
    let b: B = new { a: () }
    let b2: Bar.B = new { b: b }
    public type B { public a: A }
}
namespace Bar {
    public type B { b: Foo.B }
}
type A = ()
type B = Bar.B
```

1. Types
    - Foo.B - <root>
    - Bar.B - <root>
    - A - <root>
    - B - <root>

```
type Foo<T> { t: T }
let unfoo: { <T> Foo<T> -> T } = { { t } -> t }
(println (unfoo new { t: "Hello" }))
```
1. Types
    - :Foo - \<root\>
2. Resolve record field types
    - :Foo { t: ttp.0 }
3. Binding decls and declared types
    - builtin:println: { * -> () }
    - :unfoo: { <ftp.0.0> :Foo<ftp.0.0> -> ftp.0.0 }
4. Resolve exprs
    1. Line 2: Function expr: expected type is "{ <ftp.0.0> :Foo<ftp.0.0> -> ftp.0.0 }"
        - create new scope
        - add params to scope
            - decon pattern, expected type is ":Foo<ftp.0.0>"
                - t: ttp.0 = ftp.0.0 -> local.0
        - resolve body
            - qname expr: expected type is "ftp.0.0"
                - resolve to local.0
                - Unify ftp.0.0 & tp.0 -> ftp.0.0
                - expr type is ftp.0
            - function body return type is tp.0
        - unify { <tp.0> :Foo<tp.0> -> tp.0 } & { <tp.0> :Foo<tp.0> -> tp.0 } -> { <tp.0> :Foo<tp.0> -> tp.0 }
    1. Line 3: call expr: expected type "_"
        - resolve callee
            - qname expr: Expected type is { ? -> _ }
                - resolve println to builtin:println
                - Unify { ? -> _ } & { Any -> builtin:Unit } -> { Any -> builtin:Unit }
                - expr type is { Any -> builtin:Unit }
        - verify param count: 1 == 1
        - resolve params
            - call expr: expected type is "Any"
                - resolve callee
                    - qname expr: Expected type is { ? -> Any }
                        - resolve unfoo to :unfoo
                        - Unify { ? -> Any } & { <tp.0> :Foo<tp.0> -> tp.0 } -> { <tp.0> :Foo<tp.0> -> Any }
                        - expr type is { <tp.0> :Foo<tp.0> -> Any }
                - verify param count: 1 == 1
                - resolve params
                    - new expr: expected type is :Foo<tp.0>
                        - infer type as :Foo
                        - field init:
                            - t: string literal: expected type is tp.0
                                - unify tp.0 & String -> String
                                    - resolve tp.0 to String
                                - expr type is String
                        - Unify :Foo<tp.0> & :Foo<String> -> :Foo<String>
                        - expr type is :Foo<String>
                - Verify all type params resolved
                - Instantiate callee { <tp.0> :Foo<tp.0> -> Any } as { :Foo<String> -> Any }
                - unify Any & Any -> Any
                - expr type is Any
        - unify _ & builtin:Unit -> builtin:Unit
        - expr type is builtin:Unit

### Generic funcs ideas

```
type Pair<A B> { a: A, b: B }

let make_pair: { <A> A -> { <B> B -> Pair<A B> } } = { a -> { b -> new Pair { a: a, b: b } } }
let make_hello_pair: { <B> B -> Pair<String B> } = (make_pair "hello")
let hello_2_pair: Pair<String Number> = (make_hello_pair 2)
```

`(make_pair "hello")` instantiates `make_pair` to become `{ String -> { <B> B -> Pair<String B> } }`.

In fact, function types are special because they can exist *unspecializd*. `{ <A B> A -> B }` is merely the 
unspecialized form of `{ A -> B }` for any A and B. A generic function must be fully specialized in order to be 
callable.

#### Function type normalization

The types `{ <A B> A -> B }` and `{ <A B> B -> A }` are identical. Names and declaration order of generic parameters do 
not matter, only usage. Both of these examples would get resolved to something like 
`Func { num_params: 2, params: vec![TypeParam(0)], return_type: TypeParam(1) }`

Type parameters are stripped of their names and the ordering is normalized by usage order, not declaration order.

Another example: `{ <A> A -> { <B> B -> Pair<A B> } }` becomes

```
Func { 
    num_params: 1,
    params: vec![FTypeParam(0, 0)], 
    return_type: Func { 
        num_params: 1, 
        params: vec![FTypeParam(0, 1)],
        return_type: Id(Pair, vec![FTypeParam(0, 0), FTypeParam(0, 1)]),
    },
}
```

`{ <A> { <B> B -> A } -> A }` becomes

```
Func { 
    num_params: 1, 
    params: [
        Func { 
            num_params: 1, 
            params: [FTypeParam(0, 1)],
            return_type: FTypeParam(0, 0),
        }
    ],
    return_type: FTypeParam(0, 0),
}
```

When attempting to call the above like `(f { <B> _: B -> "hello" })`:

expected: `Func { num_params: 1, params: [FTypeParam(0, 0)], return_type: Hole(0) }`

Note that the level 0 FTypeParam became a Hole and all the rest decremented a level. This happens at callee resolving 
time.

actual: `{ <T> T -> String }` = `Func { num_params: 1, params: [FTypeParam(0, 0)], return_type: Id(String) }`

#### Automatic partial specialization

Given f...

```
let f: { <A B> A B -> Pair<A B> } = (TODO)
```

...all of the following automatic partial specializations should be valid.

```
let g: { <B> String B -> Pair<String B> } = f
let g2: { <A> A Number -> Pair<A Number> } = f
let h1: { String Number -> Pair<String Number> } = f
let h2: { String Number -> Pair<String Number> } = g
```

### Mutability

A binding name that can be assigned to

1. accept `mut <type_name>` in a type context
2. Add `ResolvedType::Mut(Box<ResolvedType>)` as a resolved type
   * When unifying two muts, this must be invariant on the inner type
   * expected Option<T>, actual mut T, fine
   * expected mut Option<T>, actual mut T, error
   * expected T, actual mut T, fine
   * expected mut T, actual T, error
   * expected mut Box<Option<T>>, actual mut Box<T>, error
3. Mut is mutable by reference. A mut passed into a function may be modified in the function body, and the changes will
   be visbile in the original mut binding.
4. special assignment operator or syntax to assign a value to a mut binding