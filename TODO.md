# Data flow
1. Scanning: Source -> Scanner -> Token stream
1. Parsing: Tokens -> Parser -> Ast
1. Resolve imports: Ast -> Importer -> ModuleDag
    * Build dag from imports: Error on recursive import
1. Resolve declarations & Usages, and types: Ast + Deps' decls -> Resolver -> TypedAst + decls
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
public let make_foo = { -> new Foo { "hello" } }
public let break_foo = { foo: { x }: Foo -> x }
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
