// Don't use this code; it's for testing purposes only. It's an
// interpretation of Java's hashCode (it's string hashing function),
// but it's using an 8 bit hash size instead of the usual 32 bits.

#include <stdint.h>
#include <stdio.h>
#include <string.h>

uint8_t hashCode(uint_fast8_t * buf, size_t buflen)
{
    uint8_t hash = 0;

    for (int i = 0; i < buflen; i++) {
        hash = 31 * hash + buf[i];
    }

    return hash;
}

int main(int argc, const char *argv[])
{

    int ret = 0;

    char buf[] = "hello world";


    uint8_t hash = hashCode(buf, strlen(buf));

    printf("hash: %u\n", hash);

    return 0;
}
