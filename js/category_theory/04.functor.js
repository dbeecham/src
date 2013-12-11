var type = function(type) {
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

var bool = type("boolean");
var obj = type("object");
var num = type("number");
var undef = type("undefined");
var arr = type("array");


// functor can work on both contracts and guarded functions
var arrOf = function(c) {
    return function(a) {
        return arr(a).map(c);
    };
};

var arrOfNum = arrOf(num);
console.log(arrOfNum([1,2,"hello"]));
