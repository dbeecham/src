#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "murmurhash3.h"


int main(int argc, const char *argv[])
{

    int ret = 0;

    char buf[] = "hello world";

    uint32_t h;
    MurmurHash3_x86_32(
            /* buffer = */ buf, 
            /* length = */ strlen(buf),
            /* seed   = */ 0,
            /* result = */ &h
    );

    printf("h: %u\n", h);

    return 0;
}
