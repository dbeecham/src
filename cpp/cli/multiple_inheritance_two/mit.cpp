#include <stdio.h>

class x {
	public:
		int x;
};

class i : virtual public x {
	public:
		int i;
};

class j : virtual public x {
	public:
		int k;
};

class y : public i, public j {
	public:
		int a;
};

int main() {
	y cy;
	cy.x = 0;
}
