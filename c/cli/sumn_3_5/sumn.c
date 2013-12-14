#include <stdio.h>


int sum(int n) {
    if (n < 3) return 0;
    if (n == 3 || n == 5) return n + sum(n - 1);
    return sum(n - 1);
}

int main(int argc, void * argv[]) {
    int n;
    scanf("%i", &n);
    printf("%i\n", sum(n));
}
