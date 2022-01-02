#pragma once

#include <stdbool.h>
#include <stdint.h>

struct example_opts_s {
    bool verbose;
    uint8_t path_len;
    uint8_t path[64];
};

int example_args_parser_parse (
    const int argc,
    const char * const * const argv,
    struct example_opts_s * opts,
    int (*no_filename_cb)(void * user_data),
    void * user_data
);
