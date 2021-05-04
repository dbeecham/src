#include <stdio.h>
#include <signal.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

void sigabrt_handler(int sig) {
    fprintf(stderr, "Aborted. :(\n");
    exit(1);
}

void sigalrm_handler(int sig) {
    fprintf(stderr, "Alarmed!\n");
}

int main(int argc, char *argv[]) {
    signal(SIGABRT, &sigabrt_handler);
    signal(SIGALRM, &sigalrm_handler);

    alarm(1);
    sleep(5);

    return 0;
}
