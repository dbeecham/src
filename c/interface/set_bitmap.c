#include "stdlib.h" /* div_t, div */

#include "set_bitmap.h"

/* Using the mapping function f, insert x into the given bitmap. */
/* This function does not do any bounds checking and will segfault if you
 * try to insert a value that does not fit into the bitmap. */
uint_fast32_t bitmap_insert(void * T_in, uint_fast32_t x) {
    
    struct set_bitmap * T = T_in;
    
    div_t d = div(T->map(x), 8);
    T->BM[d.quot] |= 1 << d.rem;
}

/* Using the mapping function f, delete x from the given bitmap. */
/* This function does not do any bounds checking and will segfault if you
 * try to insert a value that does not fit into the bitmap. */
uint_fast32_t bitmap_delete(void * T_in, uint_fast32_t x) {
    
    struct set_bitmap * T = T_in;
    
    div_t d = div(T->map(x), 8);
    T->BM[d.quot] &= 0 << d.rem;
}

/* Is x a member of the given bitmap? */
/* This function does not do any bounds checking and will segfault if you
 * try to insert a value that does not fit into the bitmap. */
uint_fast32_t bitmap_member(void * T_in, uint_fast32_t x) {

    struct set_bitmap * T = T_in;
    
    div_t d = div(T->map(x), 8);
    return (T->BM[d.quot] >> d.rem) & 1;
}