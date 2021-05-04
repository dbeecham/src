#include "smart_algorithm.h"
#include <stdio.h>

void smart_algorithm(struct set a) {
    
    a.insert(a.T, 3);
    
    if ((*a.member)(a.T, 3)) {
        printf("Yes, 3 is member.\n");
    } else {
        printf("No, 3 is not a member.\n");
    }
}