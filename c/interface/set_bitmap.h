#pragma once

#include <stdint.h>

struct set_bitmap {
    uint_fast32_t (*map)(uint_fast32_t);
    uint_fast32_t * BM;
};

uint_fast32_t bitmap_insert(void * T_in, uint_fast32_t x);

uint_fast32_t bitmap_delete(void * T_in, uint_fast32_t x);

uint_fast32_t bitmap_member(void * T_in, uint_fast32_t x);