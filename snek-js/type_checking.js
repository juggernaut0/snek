interface RuntimeType {
    fun equals(other: RuntimeType): Boolean

    fun isInstance(o: Any): Boolean {
        equals(runtimeTypeOf(o))
    }
}

function runtimeTypeOf(o) {
    let t = typeof o
    if (o === null) {
        return Unit_RuntimeType
    } else if (t === "object") {
        return t.$type
    } else if (t === "number") {
        Number_RuntimeType
    } else if ...
}

// type Box<T> { t: T }
function Box_RuntimeType(args) {
  this.args = args
  this.equals = (other) -> this.__proto__===other.__proto__ && this.args[0].equals(other.args[0]) && ...
}
function Box($ta, t) {
    this.$type = new Box_RuntimeType($ta)
    this.t = t
}
// let _ = new Box { t: "Hello" }
new Box([String_RuntimeType], "Hello")
// let _ = new Box { t: new Box { t: "" } }
new Box([new Box_RuntimeType([String_RuntimeType])], new Box([String_RuntimeType], "Hello"))

// type Box2<T> { b: Box<T> }
function Box2_RuntimeType(args) {
  this.args = args
  this.equals = (other) => this.__proto__===other.__proto__ && this.args[0].equals(other.args[0]) && ...
}
function Box2($ta, b) {
    this.$type = new Box2_RuntimeType($ta)
    this.b = b
}
// let _ = new Box2 { b: new { t: 0 } }
new Box2([Number_RuntimeType], new Box([Number_RuntimeType], 0))

//type None
//type Option<T> = T | None
function Option_RuntimeType(args) {
    this.args = args
    let exact_equals = (other) => this.__proto__===other.__proto__ && this.args[0].equals(other.args[0])
    this.equals = (other) => exact_equals(other) || this.args[0].equals(other) || new None_RuntimeType([]).equals(other)
}
// Something interesting falls out of this: a Box<Option<Number>> IS NOT a Box<Number>, but a Box<Number> IS a Box<Option<Number>>
// That is, new Box_RuntimeType([Number_RuntimeType]).equals(new Box_RuntimeType([new Option_RuntimeType([Number_RuntimeType])])) is false
// But,     new Box_RuntimeType([new Option_RuntimeType([Number_RuntimeType])]).equals(new Box_RuntimeType([Number_RuntimeType])) is true

//(match 2 { x: Option<Number> -> () })
let e = 2
if (new Option_RuntimeType([Number_RuntimeType]).isInstance(e)) {
    return null
} else {
    throw "unmatched expression"
}

// type BoxUnion<T> = Box<T> | None
function BoxUnion_RuntimeType(args) {
    this.equals = (other) => new Box_RuntimeType([args[0]]).equals(other) || new None_RuntimeType([]).equals(other)
}
