#include <stdio.h>

int main(int argc, char * argv[]) {
    int i = 1;
    int j;

    for (; i < 10; i++) {
        for (j = 1; j < 10; j++) {
            printf("\t%i\t", i * j);
        }
        printf("\n");
    }
}
