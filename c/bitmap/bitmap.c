/* Some reasoning of the code choices:
 * First, I wanted to avoid using 'malloc' and 'free' for portability reasons.
 * One of the devices we use does not have a MMU, and we'd like to keep the
 * amount of malloc's to a minimum since they can be tough to handle.
 * The stack, on the other hand, is very easy to handle.
 * And so I've not placed f and the bitmap in a structure, since that would
 * necessitate a malloc and a 'bitmap_new'-function.
 * I've also avoided dragging this out into a separate library - it's so small,
 * if you need it, just copy-paste it. */


#include "stdlib.h" /* div_t, div */
#include "string.h" /* memset */

/* not needed in production */
/* #include "assert.h" /* assert */
/* #include "stdbool.h" /* true, false */


/* Bitmap will hold BITMAP_SIZE * 8 items. */
#define BITMAP_SIZE 5


/* Some example addressing functions. These map values to memory addresses.
 * For example, maybe you only want to store even numbers in the bitmap.
 * Then you would want the first bit to represent 0, the second bit to represent
 * 2, the third would represent 4, and so on. Your mapping function would then
 * be f(x) = x/2. */
 
/* Numbers above 28. (28 -> 0, 29 -> 1, 30 -> 2, ...) */
int my_f_1(int x) {
    return x - 28;
}

/* Even numbers above 42. (42 -> 0, 44 -> 1, 46 -> 2, ...) */
int my_f_2(int x) {
    return (x - 42) / 2;
}

/* Odd numbers above 43. (43 -> 0, 45 -> 1, 47 -> 2, ...) */
int my_f_3(int x) {
    return (x - 43) / 2;
}


/* Using the mapping function f, insert x into the given bitmap. */
/* This function does not do any bounds checking and will segfault if you
 * try to insert a value that does not fit into the bitmap. */
int bitmap_insert(int (*f)(int), int bitmap[], int x) {
    div_t d = div(f(x), 8);
    bitmap[d.quot] |= 1 << d.rem;
}

/* Using the mapping function f, delete x from the given bitmap. */
/* This function does not do any bounds checking and will segfault if you
 * try to insert a value that does not fit into the bitmap. */
int bitmap_delete(int (*f)(int), int bitmap[], int x) {
    div_t d = div(f(x), 8);
    bitmap[d.quot] &= 0 << d.rem;
}

/* Is x a member of the given bitmap? */
/* This function does not do any bounds checking and will segfault if you
 * try to insert a value that does not fit into the bitmap. */
int bitmap_member(int (*f)(int), int bitmap[], int x) {
    div_t d = div(f(x), 8);
    return (bitmap[d.quot] >> d.rem) & 1;
}

int main() {
    
    /* Allocate bitmap. */
    int bm[BITMAP_SIZE];
    
    /* Don't forget to memset! */
    memset(&bm, 0, BITMAP_SIZE * sizeof(int));
    

    bitmap_insert(my_f_1, bm, 28);
    
    /* assert(true == bitmap_member(my_f_1, bm, 28)); */
    
    bitmap_delete(my_f_1, bm, 28);
    
    /* assert(false == bitmap_member(my_f_1, bm, 28)); */
    
}
