#include <stdint.h>
#include <stdio.h>

uint64_t fnv1a64_init (
    void
)
{
    return 1099511628211;
}


uint64_t fnv1a64_hash (
    uint64_t hash,
    uint64_t byte
)
{
    hash ^= byte;
    return hash * 14695981039346656037UL;
}


int main (
    int argc,
    char const* argv[]
)
{

    const uint8_t buf[] = {"hi there"};
    uint64_t hash = fnv1a64_init();
    for (long unsigned int i = 0; i < sizeof(buf); i++) {
        hash = fnv1a64_hash(hash, buf[i]);
    }
    printf("%s:%d:%s: hash is %lu\n", __FILE__, __LINE__, __func__, hash);
    
    return 0;
    (void)argc;
    (void)argv;
}
