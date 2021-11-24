#include <stdio.h>

#include "example.h"


int main (
    int argc,
    char const* argv[]
)
{
    printf("CONFIG_NUM_TIMERS is %d, and as string its %s\n",
            CONFIG_NUM_TIMERS, CONFIG_NUM_TIMERS_STRING);
    return 0;
}
