#include <stdio.h>
#include <stdlib.h>

struct pathway {
	struct position *destination;
	int time;
	struct pathway *next;
};

struct position {
	char *name;
	struct pathway *paths;
	struct position *next;
};

int main() {
	// Set up our positions.
	struct position *pos = (struct position*)malloc(sizeof(struct position));
	pos->name = "abc";
	pos->paths = (struct pathway*)malloc(sizeof(struct pathway));
	pos->paths->destination = pos;
	pos->paths->time = 3;

	printf("%s\n", pos->paths->destination->paths->destination->name);
}
