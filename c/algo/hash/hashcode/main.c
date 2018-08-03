// Don't use this code; it's for testing purposes only. It's an
// interpretation of Java's hashCode (it's string hashing function),
// but it's using an 8 bit hash size instead of the usual 32 bits.

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <err.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>

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
    float hits[255] = {0};
    float total = 0;
    char buf[32];
    ssize_t bytes_read;
    uint8_t hash;

    int fd = open("/usr/share/dict/words", O_RDONLY);
    if (-1 == fd) {
        err(1, "open");
    }

    for (bytes_read = read(fd, buf, 32);
         0 < bytes_read;
         bytes_read = read(fd, buf, 32))
    {
        hash = hashCode(buf, strlen(buf));
        hits[hash] += 1.0;
        total += 1;
    }

    for (int i = 0; i < 255; i++) {
        printf("%f\n", hits[i]/total);
    }

    return 0;
}
