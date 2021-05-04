#include <stdio.h>

int sum(int n) {
    if (0 >= n) return 0;
    return n + sum(n - 1);
}

int factorial(int n) {
    if (0 >= n) return 1;
    return n * factorial(n - 1);
}

int main(int argc, char * argv[]) {
    int n;
    printf("n: ");
    scanf("%i", &n);

    int is_sum;
    printf("sum? ");
    scanf("%i", &is_sum);

    if (is_sum) printf("%i\n", sum(n));
    else printf("%i\n", factorial(n));

}
