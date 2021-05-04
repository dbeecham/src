#include <unistd.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    if (0 == fork()) {
        daemon(0, 0);
        char * const parm[] = {"/bin/sshtunnel", NULL};
        execv("/bin/sshtunnel", parm);
    }
    sleep(2);

    return 0;

}
