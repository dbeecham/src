#include "../../lib/ll/ll.h"
#include <stdio.h>

bool divisible_by_3_or_5(int x) {
    if (x % 3 == 0 || x % 5 == 0) return true;
    return false;
}

int fact(list ptr) {
    if (!ptr) return 1;
    return car(ptr) * fact(cdr(ptr));
}

int main(int argc, const char *argv[]) {
    
    list l = range(10);

    l = reverse(l);
    int whatisthis = sum(l);
    printf("%i\n", whatisthis);
    printf("%i\n", l->value);
    printf("%i\n", fsum(l, divisible_by_3_or_5));

    delete(l);

    char buf[8];
    long int x, product;

    printf("Give me a number:\n");
    fgets(buf, 8, stdin);
    x = strtol(buf, null, 10);

    printf("Would you like a sum(0) or a product(1)?\n");
    fgets(buf, 8, stdin);
    product = strtol(buf, null, 10);

    l = range(x);

    if (product) printf("Product: %i\n", fact(l));
    else printf("Sum: %i\n", sum(l));

    delete(l);

    return 0;
}
