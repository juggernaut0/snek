function RuntimeType() {}
RuntimeType.prototype.equals = function(other) { return this===other; };
RuntimeType.prototype.instanceOf = function(o) { return this.equals(runtimeTypeOf(o)); }

Unit_RuntimeType = new RuntimeType();
Number_RuntimeType = new RuntimeType();
String_RuntimeType = new RuntimeType();
Boolean_RuntimeType = new RuntimeType();
Any_RuntimeType = new RuntimeType();
Any_RuntimeType.equals = function(other) { return true; };
Nothing_RuntimeType = new RuntimeType();
Nothing_RuntimeType.equals = function(other) { return false; };

function runtimeTypeOf(o) {
    let t = typeof o;
    if (o === null) {
        return Unit_RuntimeType;
    } else if (t === 'object') {
        return t.$type;
    } else if (t === 'number') {
        return Number_RuntimeType;
    } else if (t === 'string') {
        return String_RuntimeType;
    } else if (t == 'boolean') {
        return Boolean_RuntimeType;
    } else {
        throw 'Unknown runtime runtime type of ' + o;
    }
}
