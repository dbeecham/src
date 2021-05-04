#include <stdlib.h>
#include <stdio.h>

void atexit_clean(void * data);

void clean() {
    atexit_clean(NULL);
}

void atexit_clean(void * data) {
    static void *x;

    if (NULL != data) {
        x = data;
        printf("will clean data at exit.\n");
        atexit(clean);
    } else {
        printf("cleaning x...\n");
        free(x);
    }
}

int main(int argc, char *argv[])
{
    void * x = malloc(10);
    printf("Setting up cleaning at exit...\n");
    atexit_clean(x);

    printf("Ending program.\n");
    return 0;
}
