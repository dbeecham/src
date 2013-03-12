#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include "entry.h"

int main() {
	
	// Variables.
	char input[128];
	class everything *e = new everything;
	pthread_t t;

	// Sentinel node.
	e->e = new entry;

	// Set up thread.
	pthread_create(&t, NULL, netupdate, (void *)&e);

	// Main loop.
	while (1) {
		fscanf(stdin, "%s", input);
		fflush(stdin);

		if (!strcmp(input, "list")) printf("listing.");
		if (!strcmp(input, "quit")) break;
	}
}
