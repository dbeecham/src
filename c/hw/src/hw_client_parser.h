#pragma once

#include "hw.h"

int hw_client_parser_init (
    struct hw_client_s * client
);

int hw_client_parser_parse (
    struct hw_client_s * client,
    const char * const buf,
    const int buf_len
);
