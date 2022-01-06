#define _GNU_SOURCE

#include <stdio.h>
#include <syslog.h>
#include <errno.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/timerfd.h>
#include <unistd.h>
#include <fcntl.h>

// find free epoll_event slot, make a timerfd, attach it and set union data to be the job


#define EXAMPLE_NUM_EPOLL_FDS 32
#define EXAMPLE_EPOLLFD_TYPE_IDENTIFY_QUEUE_TIMERFD 1

#define EXAMPLE_SENTINEL 8090
#define EXAMPLE_EPOLLFD_SENTINEL 8091

struct example_epollfd_s {
    int sentinel;
    uint32_t type;
    int fd;
    union {
        struct {
            int job;
        } identify_queue;
    };
};

struct example_s {
    int sentinel;
    int epollfd;
    struct example_epollfd_s epollfds[32];
};


int example_epollfd_slot (
    struct example_s * example,
    struct example_epollfd_s ** example_epollfd
)
{
    for (int i = 0; i < EXAMPLE_NUM_EPOLL_FDS; i++) {
        if (0 == example->epollfds[i].sentinel) {
            *example_epollfd = &example->epollfds[i];
            return 0;
        }
    }

    return -1;
}


int example_init (
    struct example_s * example
)
{

    // Create the epoll instance
    example->epollfd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == example->epollfd) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


int example_epoll_event_identify_queue_timerfd (
    struct example_s * example,
    struct epoll_event * event,
    struct example_epollfd_s * example_epollfd
)
{

    int ret = 0;
    int bytes_read = 0;
    uint64_t expirations = 0;

    bytes_read = read(example_epollfd->fd, &expirations, sizeof(expirations));
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    if (0 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read 0 bytes", __FILE__, __LINE__, __func__);
        return -1;
    }

    // consume the job
    syslog(LOG_INFO, "%s:%d:%s: job: %d", __FILE__, __LINE__, __func__, example_epollfd->identify_queue.job);

    // we're done with the timerfd now
    // Remember to EPOLL_CTL_DEL *before* closing the file descriptor, see
    // https://idea.popcount.org/2017-03-20-epoll-is-fundamentally-broken-22/
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_DEL,
        example_epollfd->fd,
        NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    ret = close(example_epollfd->fd);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: close: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    *example_epollfd = (struct example_epollfd_s){0};

    return 0;
}


static int example_epoll_event_dispatch (
    struct example_s * example,
    struct epoll_event * event
)
{
    struct example_epollfd_s * example_epollfd = event->data.ptr;
    if (EXAMPLE_EPOLLFD_SENTINEL != example_epollfd->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong", __FILE__, __LINE__, __func__);
        return -1;
    }

    if (EXAMPLE_EPOLLFD_TYPE_IDENTIFY_QUEUE_TIMERFD == example_epollfd->type)
        return example_epoll_event_identify_queue_timerfd(example, event, example_epollfd);

    syslog(LOG_ERR, "%s:%d:%s: No match on epoll event.", __FILE__, __LINE__, __func__);
    return -1;
}


static int example_epoll_handle_events (
    struct example_s * example,
    struct epoll_event epoll_events[8],
    int epoll_events_len
)
{
    int ret = 0;
    for (int i = 0; i < epoll_events_len; i++) {
        ret = example_epoll_event_dispatch(example, &epoll_events[i]);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}


int example_loop (
    struct example_s * example
)
{

    int ret = 0;

    int epoll_events_len = 0;
    struct epoll_event epoll_events[8];
    while (1) {
        epoll_events_len = epoll_wait(
            /* epollfd = */ example->epollfd,
            /* &events = */ epoll_events,
            /* events_len = */ 8,
            /* timeout = */ -1
        );

        // got interrupted, just try again.
        if (-1 == epoll_events_len && EINTR == errno) {
            continue;
        }

        if (-1 == epoll_events_len) {
            syslog(LOG_ERR, "%s:%d:%s: epoll_wait: %s", __FILE__, __LINE__, __func__, strerror(errno));
            return -1;
        }

        if (0 == epoll_events_len) {
            syslog(LOG_ERR, "%s:%d:%s: epoll_wait returned 0 events", __FILE__, __LINE__, __func__);
            return -1;
        }

        // dispatch on event
        // (snippet: epev)
        ret = example_epoll_handle_events(example, epoll_events, epoll_events_len);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: example_epoll_handle_events returned -1", __FILE__, __LINE__, __func__);
            return -1;
        }
    }

    return 0;
}


int example_queue (
    struct example_s * example,
    const int sec,
    const int nsec,
    const int job
)
{
    int ret = 0;
    struct example_epollfd_s * example_epollfd = 0;
    int timerfd = 0;

    // find a free epollfd slot
    ret = example_epollfd_slot(example, &example_epollfd);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_epollfd_slot returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    // create timerfd
    timerfd = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (-1 == timerfd) {
        syslog(LOG_ERR, "%s:%d:%s: timerfd_create: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // fill in the details
    *example_epollfd = (struct example_epollfd_s) {
        .sentinel = EXAMPLE_EPOLLFD_SENTINEL,
        .type = EXAMPLE_EPOLLFD_TYPE_IDENTIFY_QUEUE_TIMERFD,
        .fd = timerfd,
        .identify_queue = {
            .job = job
        }
    };

    // add it on epoll
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_ADD,
        example_epollfd->fd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLONESHOT,
            .data = {
                .ptr = example_epollfd
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // arm timerfd
    ret = timerfd_settime(
        /* fd        = */ timerfd,
        /* opt       = */ 0,
        /* timerspec = */ &(struct itimerspec) {
            .it_interval = {0},
            .it_value = {
                .tv_sec  = sec,
                .tv_nsec = nsec
            }
        },
        /* old_ts    = */ NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: timerfd_settime: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    
    
    return 0;
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;
    struct example_s example = {0};

    ret = example_init(&example);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_init returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    // write a couple of events into the event queue
    ret = example_queue(&example, 1, 0, 1);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_queue returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_queue(&example, 1, 0, 2);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_queue returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_queue(&example, 2, 0, 3);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_queue returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = example_loop(&example);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_loop returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    return 0;
}
