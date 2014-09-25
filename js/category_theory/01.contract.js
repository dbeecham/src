// Contract! :)
var str = function(s) { // s is ostensibly a string
    if (typeof s !== "string") {
        throw new TypeError("Expected a string!");
    } else {
        return s;
    }
};

// What is a contract good for?
// Lets say I want to duplicate a string:

var repeat0 = function(s) {
    return s + s;
};
repeat0(5);

// This returns 10, not "55".
// We want to make sure that s is a string:

var repeat1 = function(s) {
    s = str(s);
    return s + s;
}
repeat1(5);

// This now throws exception TypeError

repeat1("5");

// This does what we expect.
