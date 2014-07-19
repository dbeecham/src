#include <stdio.h>

void show_bytes(char * data, int len) {
    for (int i = 0; i < len; i++) {
        printf("%p\t0x%.2x\n", (data + i), *(data + i));
    }
}

void show_int(int x) {
    show_bytes((char *) &x, sizeof(int));
}
