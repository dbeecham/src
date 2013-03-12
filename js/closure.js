var func = function() {
	var i = 0;
	return function() {
		i++;
		return i;
	}
}();

console.log(func());
console.log(func());
console.log(func());
console.log(func());
