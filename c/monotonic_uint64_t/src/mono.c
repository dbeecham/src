#define _DEFAULT_SOURCE

#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;
    struct timespec tp = {0};

    ret = clock_gettime(CLOCK_MONOTONIC, &tp);
    if (-1 == ret) {
        printf("%s:%d:%s: clock_gettime: %s\n", __FILE__, __LINE__, __func__, strerror(errno));
        exit(EXIT_FAILURE);
    }

    printf("%s:%d:%s: sec + nsec=%lx\n", 
            __FILE__, __LINE__, __func__, ((tp.tv_sec & 0x0000ffff) << 16) | ((tp.tv_nsec & 0xffff0000) >> 16));

    return 0;
    (void)argc;
    (void)argv;
}
