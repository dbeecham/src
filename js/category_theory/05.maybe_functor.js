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

// maybe functor ('option' in scala, 'freepointed set' in category theory)
var maybe = function(c) {
   return function(m) {
       if (m instanceof None) {
           return m;
       }
       if (m instanceof Some) {
           return some(c(m.x));
       }
       else {
           throw new TypeError("Expected None or Some(value)!");
       }
   }
};

var None = function() {
    
};
None.prototype = Object.create(maybe.prototype);
None.toString = function() {
    return "None";
};
var none = new None();

var Some = function(x) {
    this.x = x;
};
Some.prototype = Object.create(maybe.prototype);
Some.toString = function() {
    return "Some(" + this.x + ")";
};

var some = function(x) {
    return new Some(x);
};
