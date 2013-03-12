#include <stdio.h>

int main() {
	// An array is really a pointer pointing to the first element in the array.
	int a[4] = {1,2,3,4};
	char c[] = "hello";
	int b[] = {1, 3};

	// Deprecated
	char *d;
	d = "hi";
	char *e = "hey";

	// Incrementing the pointer and dereferencing the incremented value.
	printf("%d\n", *(a+1));

	// Dereference pointer pointing to an array.
	printf("%c\n", *c);
}
