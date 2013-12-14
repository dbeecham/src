#include <stdio.h>
#include <math.h>

int mod(int x, int y) {
    return x % y;
}

int is_prime(unsigned long x) {
    unsigned long i;
    for (i = 2; i <= sqrtl(x); i++) {
        if (mod(x, i) == 0) return 0;
    }

    return 1;
}

int main(int argc, const char *argv[]) {
    unsigned long cur = 0;
    for (;; cur++) {
        if (is_prime(cur)) printf("%li\n", cur);
    }

    return 0;
}
