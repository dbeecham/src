#include <stdio.h>

// Basic struct.
struct position {
	public:
		int x;
		int y;
};

// Rectangle is inherited by position.
struct rectangle : public position {
	public:
		int height;
		int width;
};

// It's legal to inherit classes from structures.
class cposition : public position {
	public:
		int z;
};


// Dummy class for multiple inheritance.
class dposition : public position {
	public:
		int z;
};

// Multiple Inheritance, with duplicate variable names.
class box : public dposition, public cposition {
};


int main() {
	struct rectangle r;
	r.x = 4;
	printf("%d\n", r.x);

	cposition pos;
	pos.x = 0;
	pos.z = 2;
	printf("%d\n", pos.x + pos.z);

	box b;
	b.dposition::z = 0;
	b.dposition::x = 0;

	boxtwo bt;
	bt.x = 0;
}
