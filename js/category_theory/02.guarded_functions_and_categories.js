// A contract generator. Contracts, in category theory, are called
// 'objects'.
var typeOf = function(type) {
    if (typeof type !== "string") {
        throw new TypeError("Expected a string!");
    } 

    // returns a contract (object) of type 'type'
    return function(a) {
        if (typeof a !== type) {
            throw new TypeError("Expected a " + type + "!");
        } else {
            return a;
        }
    };
};

var str = typeOf("string");
var bool = typeOf("boolean");
var obj = typeOf("object");
var num = typeOf("number");
var fun = typeOf("function");
var undef = typeOf("undefined");

// Once we have contracts (above), we can write functions that use
// them:

// repeat :: str -> str
var repeat = function(s) {
    s = str(s);
    return str(s + s);
};

// int :: num -> num
var inc = function(x) {
    x = num(x);
    return num(x + 1);
};

// A function that has a contract on the input or the output
// a *guarded function*, which are called 'morphisms' in
// category theory.

// Consider the following contract:
var any = function(s) { return s; }

// Is is thus possible to think of all functions as guarded functions,
// guarded by the any contract.

// Contracts and the functions they guard form a *category*.
