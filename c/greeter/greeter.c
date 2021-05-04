#include <stdio.h>
#include <string.h>

int main(int argc, char * argv[]) {
    char name[64];

    scanf("%63s", name);

    printf("Hello, %s.\n", name);

}
