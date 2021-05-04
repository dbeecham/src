#include <stdio.h> /* printf */
#include <stdlib.h> /* getenv */

int main(int argc, char ** argv) {
    char * path = getenv("PATH");
    printf("%s\n", path);

    return 0;
}
