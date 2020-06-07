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
  * Cons: Dynamic allocations might be made as the Map expands.
