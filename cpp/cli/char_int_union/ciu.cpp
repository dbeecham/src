#include <stdio.h>

int main() {
	union {
		short int i;
		char c[2];
	} u;

	u.i = 3000;
	printf("%s\n", u.c);
}
