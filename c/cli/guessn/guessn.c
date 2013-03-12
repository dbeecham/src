#include <stdio.h>
#include <time.h>
#include <stdlib.h>

int main() {
	int i, g;
	srand(time(NULL));
	i = rand()%100;

	printf("Guess the number, it's between 0 and 100.\n");
	while (1) {
		scanf("%i", &g);
		if (g == i) break;
		if (g > i) printf("No, it's lower.");
		if (g < i) printf("No, it's higher.");
	}
	printf("Congratulations!\nYou won!\n");
	return 0;
}
