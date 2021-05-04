#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct pathway {
	struct position *destination;
	int time;
	struct pathway *next;
};

struct position {
	char name[128];
	struct pathway *paths;
	struct position *next;
	float probability;
	int num_paths;
};

int addpathway(struct position *pos, char *a, char *b, int time) {
	struct position *ptr;
	struct position *pa, *pb;
	struct pathway *pptr;
	pa = pb = NULL;

	ptr = pos;

	// Get the positions.
	while (ptr->next != NULL) {
		ptr = ptr->next;
		if (!strcmp(ptr->name, a)) {
			pa = ptr;
		}
		if (!strcmp(ptr->name, b)) {
			pb = ptr;
		}
	}

	// If we failed to find a position, return.
	if (pa == NULL || pb == NULL) {
		return 1;
	}

	// Add a pathway at the end of the pathlist.
	if (pa->paths == NULL) {
		pa->paths = (struct pathway*)malloc(sizeof(struct pathway));
		pptr = pa->paths;
	} else {
		pptr = pa->paths;
		while (pptr->next != NULL) pptr = pptr->next;
		pptr->next = (struct pathway*)malloc(sizeof(struct pathway));
		pptr = pptr->next;
	}
	pptr->destination = pb;
	pptr->time = time;
	pa->num_paths++;

	// Add a pathway at the end of the pathlist.
	if (pb->paths == NULL) {
		pb->paths = (struct pathway*)malloc(sizeof(struct pathway));
		pptr = pb->paths;
	} else {
		pptr = pb->paths;
		while (pptr->next != NULL) pptr = pptr->next;
		pptr->next = (struct pathway*)malloc(sizeof(struct pathway));
		pptr = pptr->next;
	}
	pptr->destination = pa;
	pptr->time = time;
	pb->num_paths++;

	return 0;
}

int main() {
	// open files and set up sentinel node
	FILE *f = fopen("file", "r");
	struct position *pos = (struct position*)malloc(sizeof(struct position));
	struct position *ptr, *ptrb;
	ptr = pos;

	char stra[128];
	char strb[128];
	int i, j, x, y;
	float fl;

	// read positions
	if (fscanf(f, "%d", &i) == 1) {
		for (j = 0; j < i; j++) {
			fscanf(f, "%128s %f", stra, &fl);
			ptr->next = (struct position*)malloc(sizeof(struct position));
			ptr = ptr->next;
			strcpy(ptr->name, stra);
			ptr->probability = fl;
		}
	}

	// read links
	if (fscanf(f, "%d", &i) == 1) {
		for (j = 0; j < i; j++) {
			fscanf(f, "%s %s %d", stra, strb, &x);
			addpathway(pos, stra, strb, x);
		}
	}

	printf("Current setup is:\n");

	struct pathway *pptr;
	ptr = pos;
	while (ptr->next != NULL) {
		ptr = ptr->next;
		printf("\t%s\n", ptr->name);
		printf("\t%d pathways:\n", ptr->num_paths);

		pptr = ptr->paths;

		for (i = 0; i < ptr->num_paths; i++) {
			printf("\t\t%s\n", pptr->destination->name);
			pptr = pptr->next;
		}
	}

	ptr = pos->next;
	printf("\nWe are starting in %s\n", ptr->name);
	
	struct timeval t;
	int newpath;

	while (!0) {
		printf("Is she here?\n");
		
		// randomize
		gettimeofday(&t, NULL);
		srand(t.tv_usec * t.tv_sec);
		if (rand()%10 <= ptr->probability) {
			// Found
			printf("Found her!\n");
			return 0;
		} else {
			// Not found.
			printf("No, she wasn't.\n");
			newpath = rand()%ptr->num_paths;
			pptr = ptr->paths;

			for (i = 0; i < newpath; i++) {
				pptr = pptr->next;
			}
			ptr = pptr->destination;
			
			printf("We're now walking to %s.\n", ptr->name);
			//sleep(pptr->time);
		}
	}

	return 0;
}
