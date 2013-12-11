// Contract
var str = function(s) { // s is ostensibly a string
    if (typeof s !== "string") {
        throw new TypeError("Expected a string!");
    } else {
        return s;
    }
};

var repeat0 = function(s) {
    return s + s;
};

repeat0(5); // returns 10. Not what we want.

var repeat1 = function(s) {
    s = str(s);
    return s + s;
}

repeat1(5); // TypeError
