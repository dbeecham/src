#include <stdio.h>

int sum(int n) {
    if (n == 0) return 0;
    return n + sum(n - 1);
}

int main(int argc, char * argv[]) {
    int target;
    scanf("%i", &target);

    printf("%i\n", sum(target));
}
