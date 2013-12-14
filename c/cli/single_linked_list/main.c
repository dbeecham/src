#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct cell {
    int x;
    struct cell *next;
};

const char *byte_to_binary(int x)
{
    static char b[9];
    b[0] = '\0';

    int z;
    for (z = 128; z > 0; z >>= 1)
    {
        strcat(b, ((x & z) == z) ? "1" : "0");
    }

    return b;
}

int main() {


    struct cell *root;
    root = (struct cell *) malloc(sizeof(struct cell));
    root->next = (struct cell *) malloc(sizeof(struct cell));
    root->next->next = (struct cell *) malloc(sizeof(struct cell));
    root->next->next->next = 0;


    root->x = 72;
    root->next->x = 0x65;
    root->next->next->x = 0152;


    printf("variable root                       has address: %p\n", &root);
    printf("variable root                       has value: %p\n", root);
    printf("variable root.x                     has value: %i\n", root->x);
    printf("variable root.next                  has value: %p\n", root->next);
    printf("variable root.next.x                has value: %i\n", root->next->x);
    printf("variable root.next.next             has value: %p\n", root->next->next);
    printf("variable root.next.next.x           has value: %i\n", root->next->next->x);
    printf("variable root.next.next.next        has value: %p\n", root->next->next->next);


    printf("variable root.x has value (as 'integer'): %i\n", (int) root->x);
    printf("variable root.x has value (as 'hexadecimal'): %x\n", (int) root->x);
    printf("variable root.x has value (as 'octal'): %o\n", (int) root->x);
    printf("variable root.x has value (as 'binary'): %s\n", byte_to_binary(root->x));
    printf("variable root.x has value (as 'character'): %c\n", (char) root->x);
    printf("variable root.next.x has value (as 'character'): %c\n", root->next->x);
    printf("variable root.next.next.x has value (as 'character'): %c\n", root->next->next->x);
}
