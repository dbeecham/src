#include <stdio.h>
#include <string.h>

int main(int argc, char * argv[]) {
    char name[64];

    scanf("%63s", name);

    if (strcmp(name, "Alice") == 0 || strcmp(name, "Bob") == 0) {
        printf("Hello, %s.", name);
        return 0;
    }

    printf("No!\n");
    return 1;
}
