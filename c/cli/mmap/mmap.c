#include <sys/types.h> /* not sure, something for open */
#include <sys/stat.h> /* not sure, something for open */
#include <fcntl.h> /* not sure, something for open */
#include <unistd.h> /* close */
#include <err.h> /* err() */
#include <sys/mman.h> /* mmap, MAP_FAILED */
#include <stdio.h> /* printf */

int main(int argc, char *argv[]) {
    int fd = open("data.txt", O_RDWR);
    if (-1 == fd) {
        err(1, "open data.txt");
    }
    char * text = (char *) mmap(
                            NULL, /* map data to destination? */
                            4096, /* How much data to map? */
                            PROT_READ, /* Only allow prot (READ access) to memory */
                            MAP_PRIVATE, /* Which processes can read this file? */
                            fd, /* file */
                            0 /* offset */
    );

    if (MAP_FAILED == text) {
        close(fd);
        err(1, "mmap");
    }
    close(fd);


    printf("Here is the data: %s\n", text);

    return 0;
}
