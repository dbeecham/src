var type = function(type) {
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

var bool = type("boolean");
var obj = type("object");
var num = type("number");
var undef = type("undefined");


// guarded functions input and output needs to pass a contract



// Guarded function ("morphisms")
var repeat = function(s) {
    s = str(s);
    return s + s;
};


// Another guarded function (morphism)
var inc = function(x) {
    x = num(x);
    return num(x + 1);
};

// A contract and a guarded function form a category.
