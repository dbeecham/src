#include <stdio.h>
#include <stdint.h>

#define FLAG_ONE 1
#define FLAG_TWO 2
#define FLAG_THREE 4
#define FLAG_FOUR 8

int main() {
	uint8_t settings;
	char buf[8];

	scanf("%8s", buf);

	printf("%s\n", buf);
}
