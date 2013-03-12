#include <stdio.h>

struct mage {
	int mana;
};

struct warrior {
	int rage;
};

struct hunter {
	int focus;
};

typedef union {
	struct hunter h;
	struct mage m;
	struct warrior w;
} c;

int main() {
	c ibien;
	ibien.m.mana = 100;
	printf("%d\n", ibien.w.rage);
}
