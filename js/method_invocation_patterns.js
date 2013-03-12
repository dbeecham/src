// Method invocation pattern, this points to the methods parent object.
var obj0 = {
	priv: 3,
	func: function() {
		console.log(this.priv);
	}
};
obj0.func();


// Function invocation pattern, this points (unfortunately) to the global object.
var func0 = function() {
	console.log(this.process.pid);
};
func0();

// 'Fixing' this in function invocation pattern by calling it that.
var func1 = function() {
	priv: 3;

	that = this;
	var changePriv = function() {
		that.priv = 2;
	};
	changePriv();

	console.log(this.priv);
};
func1();


// Constructor invocation pattern
// If a function is invoced with a new prefix, a new object will be created
// with a hidden prototype link to the functions prototype member.
// this will point to that new object.

var obj1 = {
	foo: function() { console.log("foo"); }
};

var func2 = function() {};
func2.prototype = obj1;
var obj2 = new func2();
obj2.bar = function() { console.log("bar"); };
obj2.foo();
obj2.bar();


// Apply invocation pattern
// apply is a method of every function that calls the function with
// a specified 'this' and arguments array.
var func3 = function() {
	this.x = 3;
	this.y = 4;
};
var obj3 = {
	x: 1,
	y: 0
};
func3.apply(obj3);
console.log(obj3.x, obj3.y);
