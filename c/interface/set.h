#pragma once

#include <stdint.h>

struct set {
    void * T;
    uint_fast32_t (*insert)(void * T, uint_fast32_t elem);
    uint_fast32_t (*member)(void * T, uint_fast32_t elem);
    uint_fast32_t (*delete)(void * T, uint_fast32_t elem);
};