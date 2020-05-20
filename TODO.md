# TODO
* Performance improvements
    * Consider alternatives to `Rc` for sharing values
        * Implement tracing GC for values?
        * Arena allocation?
    * Simplify opcode decoding
    * Faster implementation of locals and captured bindings
* Methods and dynamic dispatch
    * `method` and `impl` declarations


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