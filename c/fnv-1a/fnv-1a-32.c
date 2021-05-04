#include <stdint.h>
#include <stdio.h>

uint32_t fnv1a32_init (
    void
)
{
    return 2166136261;
}


uint32_t fnv1a32_hash (
    uint32_t hash,
    uint32_t byte
)
{
    hash ^= byte;
    return hash * 16777619;
}


int main (
    int argc,
    char const* argv[]
)
{

    const uint8_t buf[] = {"hi there"};
    uint32_t hash = fnv1a32_init();
    for (long unsigned int i = 0; i < sizeof(buf); i++) {
        hash = fnv1a32_hash(hash, buf[i]);
    }
    printf("%s:%d:%s: hash is %d\n", __FILE__, __LINE__, __func__, hash);
    
    return 0;
    (void)argc;
    (void)argv;
}
