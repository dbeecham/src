o = {};
o.toString = function() {
	return 'two';
};

a = {
	'two': 'two is here!'
};

console.log(a[o]);
