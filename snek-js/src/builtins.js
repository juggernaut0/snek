function RuntimeType() {}
RuntimeType.prototype.equals = function(other) { return this===other; };
RuntimeType.prototype.instanceOf = function(o) { return this.equals(runtimeTypeOf(o)); }

function Function_RuntimeType(params, ret) {
    this.params = params;
    this.ret = ret;
}
Function_RuntimeType.prototype = Object.create(RuntimeType.prototype);
Function_RuntimeType.prototype.equals = function(other) {
    return Object.getPrototypeOf(this)===Object.getPrototypeOf(other) && this.params.map((e,i)=>[e,other.params[i]]).every(([t,o])=>t.equals(o)) && this.ret.equals(other.ret);
};

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
        return o.$type;
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

function TailRecursion(args) {
    this.args = args;
}
function tailRecurse(args) {
    throw new TailRecursion(args);
}
