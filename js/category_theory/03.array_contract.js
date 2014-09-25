// Contract (object)generator.
var typeOf = function(type) {
    if (typeof type !== "string") {
        throw new TypeError("Expected a string!");
    } 

    // returns a contract (object) of type 'type'
    return function(a) {
        // Needs to do special magic to arrays.
        if (type === "array") {
            if (Object.prototype.toString.call(a) !== "[object Array]") {
                throw new TypeError("Expected array!");
            } else {
                return a;
            }
        }
        
        if (typeof a !== type) {
            throw new TypeError("Expected " + type + "!");
        } else {
            return a;
        }
    };
};

var bool = typeOf("boolean");
var fun = typeOf("function");
var obj = typeOf("object");
var num = typeOf("number");
var undef = typeOf("undefined");
var arr = typeOf("array");
