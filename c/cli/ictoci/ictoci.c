#include <stdio.h>

int main() {
	const float fac = 2.54;
	int in = 0;
	char ch = 0;

	scanf("%8d%2c", &in, &ch);
	if (ch == 'c') printf("\n%f inches\n", in/fac);
	if (ch == 'i') printf("\n%f cm\n", in*fac);
}
