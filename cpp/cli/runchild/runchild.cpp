#include <pthread.h>
#include <stdio.h>
#include <unistd.h>

struct threads {
	pthread_t thread;
	char buffer[256];
	struct threads *next;
};

void *runapp(void *app) {
	char *a = (char *)app;

	sleep(5);

	fprintf(stdout, "Hello, %s\n", a);
	pthread_exit(NULL);
}

int main() {
	int e;
	struct threads *t, *tptr;
	t = new threads;
	tptr = t;

	while (true) {
		e = scanf("%128s", buffer);
		fflush(stdin);

		pthread_create(&thread, NULL, runapp, (void*)buffer);
	}

	return 0;
}
