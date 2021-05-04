// Try to not define _GNU_SOURCE or _DEFAULT_SOURCE, since those enable
// glibc-specific features. Being able to compile to e.g. musl or uclibc
// makes porting to embedded linux systems much easier (and generally
// pressures the programmer into stricter and better programming practices).
#define _POSIX_C_SOURCE 201805L

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <syslog.h>
#include <sys/stat.h>
#include <sys/epoll.h>

#include "hw.h"
#include "hw_nats_task.h"

void * hw_nats_task (
    void * arg
)
{
    struct hw_s * hw = arg;
    if (8090 != hw->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!", __FILE__, __LINE__, __func__);
        exit(EXIT_FAILURE);
    }

    // TODO:
    //  * Connect to NATS
    //  * Set up a watchdog timer for NATS
    //  * When connected to NATS, subscribe to some topic
    //  * epoll_wait
    //  * if disconnected from NATS, just retry the connection.
    //  * on PING from nats, PONG back
    //  * on MSG from nats, send it to the socketpair
    //  * on msg from socketpair, send it to nats

    return 0;
}
