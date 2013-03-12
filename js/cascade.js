var obj = {
	pcolor: "black",
	pheight: 30,
	pwidth: 20,
	pfont: "Serif",

	color: function(c) {
		this.pcolor = c;
		return this;
	},
	height: function(i) {
		this.pheight = i;
		return this;
	},
	width: function(i) {
		this.pwidth = i;
		return this;
	},
	font: function(c) {
		this.pfont = c;
		return this;
	}

}

o = obj.color("red").height(10).width(100).font("monospace");
console.log(o)
