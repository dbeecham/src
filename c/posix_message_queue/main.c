#define _POSIX_C_SOURCE 201805L
#define _XOPEN_SOURCE 700
#define _XOPEN_SOURCE_EXTENDED 1

#include <fcntl.h>
#include <sys/stat.h>
#include <mqueue.h>
#include <err.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>

struct mq_msg {
    int fd;
    void * ptr;
};

int main(int argc, char *argv[])
{
    int ret;

    // SIGPIPE is annoying; we handle that when writing instead.
    sigaction(
        SIGPIPE,
        &(struct sigaction) {
            .sa_handler = SIG_IGN
        },
        NULL
    );


    // Open the message queue.
    mqd_t queue = mq_open(
            /* name = */ "/hello", 
            /* queue flags = */ O_CREAT | O_RDWR, 
            /* item_mode = */ 0666,
            /* attr = */ &((struct mq_attr) {
                .mq_maxmsg = 8,
                .mq_msgsize = sizeof(struct mq_msg)
            })
    );
    if (-1 == queue) {
        warn("mq_open");
        exit(EXIT_FAILURE);
    }

    // Unlink the queue immediately - it will be removed when all parties have
    // closed the queue.
    ret = mq_unlink("/hello");
    if (-1 == ret) {
        warn("mq_unlink");
    }

    ret = close(queue);
    if (-1 == ret) {
        warn("close");
    }

    
    // Lets spawn some threads.
    



    exit(EXIT_SUCCESS);
}
