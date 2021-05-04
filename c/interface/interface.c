/* Some reasoning of the code choises:
 * First, I wanted to avoid using 'malloc' and 'free' for portability reasons.
 * One of the devices we use does not have a MMU, and we'd like to keep the
 * amount of malloc's to a minimum since they can be tough to handle.
 * The stack, on the other hand, is very easy to handle.
 * And so I've not placed f and the bitmap in a structure, since that would
 * necessitate a malloc and a 'bitmap_new'-function.
 * I've also avoided dragging this out into a separate library - it's so small,
 * if you need it, just copy-paste it. */



#include "string.h" /* memset */
#include <stdint.h>

#include "smart_algorithm.h"
#include "set_bitmap.h"



/* Bitmap will hold BITMAP_SIZE * 8 items. */
#define BITMAP_SIZE 5



/* Some example addressing functions. These map values to memory addresses.
 * For example, maybe you only want to store even numbers in the bitmap.
 * Then you would want the first bit to represent 0, the second bit to represent
 * 2, the third would represent 4, and so on. Your mapping function would then
 * be f(x) = x/2. */
 
/* Numbers above 28. (28 -> 0, 29 -> 1, 30 -> 2, ...) */
uint_fast32_t my_f_1(uint_fast32_t x) {
    return x - 28;
}

/* Even numbers above 42. (42 -> 0, 44 -> 1, 46 -> 2, ...) */
uint_fast32_t my_f_2(uint_fast32_t x) {
    return (x - 42) / 2;
}

/* Odd numbers above 43. (43 -> 0, 45 -> 1, 47 -> 2, ...) */
uint_fast32_t my_f_3(uint_fast32_t x) {
    return (x - 43) / 2;
}



int main() {
    
    /* Allocate bitmap. */
    uint_fast32_t bm[BITMAP_SIZE];
    
    /* Don't forget to memset! */
    memset(&bm, 0, BITMAP_SIZE * sizeof(uint_fast32_t));
    
    smart_algorithm((struct set){
        .T = &(struct set_bitmap) {
            .map = my_f_1,
            .BM = bm
        },
        .delete = bitmap_delete,
        .insert = bitmap_insert,
        .member = bitmap_member
    });
    
}