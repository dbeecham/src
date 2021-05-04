#include <stdio.h>
#include <math.h>


long double sum(long k, long n, long double (*func)(long x)) {
    long double result = 0;

    for (; k <= n; k++) {
        result += (*func)(k);
    }

    return result;
}

long double identity(long x) {
    return x;
}

long double pi(long x) {
    return pow(-1, x + 1)/(2*x - 1);
}

int main(int argc, const char *argv[]) {
    printf("%Lf", 4 * sum(1, 1000000, &pi));
    return 0;
}
