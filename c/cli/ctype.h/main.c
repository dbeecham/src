#include <stdio.h>
#include <stdbool.h>
#include <ctype.h>

int main(int argc, char *argv[]) {

    char c = getchar();
    while (c != 'q') {
        if (c != '\n') {
            printf("%c: isalpha: %i\n", c, isalpha(c));
            printf("%i\n", isalpha(c) == true);
        }

        c = getchar();
    }

    return 0;
}
