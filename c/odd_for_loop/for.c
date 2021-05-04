#include <stdio.h>

int main() {
	char c[] = "Hello, World";
	int i;
	for (i = 0; c[i]; i++) {
		printf("%c\n", c[i]);
	}
}
