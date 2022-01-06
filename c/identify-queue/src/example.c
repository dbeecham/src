#define _GNU_SOURCE

#include <stdio.h>
#include <syslog.h>
#include <errno.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/timerfd.h>
#include <unistd.h>
#include <fcntl.h>

struct example_identify_task_s {
    int job;
};

struct example_identify_queue_task_s {
    int sec;
    int nsec;
    struct example_identify_task_s task;
};

struct example_s {
    int epollfd;
    struct {
        int timerfd;
        int pipefd[2];
        struct example_identify_task_s task;
    } identify_queue;
};


int example_init (
    struct example_s * example
)
{

    int ret = 0;

    // Create the epoll instance
    example->epollfd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == example->epollfd) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // create queue timerfd
    example->identify_queue.timerfd = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (-1 == example->identify_queue.timerfd) {
        syslog(LOG_ERR, "%s:%d:%s: timerfd_create: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // add timerfd to epoll (no events just yet though)
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_ADD,
        example->identify_queue.timerfd,
        &(struct epoll_event){
            .events = EPOLLONESHOT,
            .data = {
                .fd = example->identify_queue.timerfd
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // create queue pipefd
    // example->pipe_fd[0] contains the read-end of the pipe, example->pipe_fd[1] contains the write-end.
    ret = pipe2(example->identify_queue.pipefd, O_CLOEXEC | O_NONBLOCK);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: pipe2: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // add pipefd to epoll
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_ADD,
        example->identify_queue.pipefd[0],
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLONESHOT,
            .data = {
                .fd = example->identify_queue.pipefd[0]
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


int example_epoll_event_identify_queue_timerfd (
    struct example_s * example,
    struct epoll_event * event
)
{

    int ret = 0;
    int bytes_read = 0;
    uint64_t expirations = 0;

    bytes_read = read(event->data.fd, &expirations, sizeof(expirations));
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    if (0 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read 0 bytes", __FILE__, __LINE__, __func__);
        return -1;
    }

    // consume the job
    syslog(LOG_INFO, "%s:%d:%s: job: %d", __FILE__, __LINE__, __func__, example->identify_queue.task.job);

    // re-arm the queue pipefd
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_MOD,
        example->identify_queue.pipefd[0],
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
            .data = {
                .fd = example->identify_queue.pipefd[0]
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


int example_epoll_event_identify_queue_pipefd (
    struct example_s * example,
    struct epoll_event * event
)
{

    int ret = 0;
    int bytes_read = 0;
    struct example_identify_queue_task_s queue_task = {0};

    bytes_read = read(event->data.fd, &queue_task, sizeof(struct example_identify_queue_task_s));
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    if (0 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read 0 bytes (pipe closed?)", __FILE__, __LINE__, __func__);
        return -1;
    }
    if (sizeof(struct example_identify_queue_task_s) != bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: partial read, expected to read %ld bytes, but read %d bytes", __FILE__, __LINE__, __func__, sizeof(struct example_identify_queue_task_s), bytes_read);
        return -1;
    }

    example->identify_queue.task = queue_task.task;

    // arm timerfd
    ret = timerfd_settime(
        /* fd        = */ example->identify_queue.timerfd,
        /* opt       = */ 0,
        /* timerspec = */ &(struct itimerspec) {
            .it_interval = {0},
            .it_value = {
                .tv_sec  = queue_task.sec,
                .tv_nsec = queue_task.nsec
            }
        },
        /* old_ts    = */ NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: timerfd_settime: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    
    // update timerfd interests on epoll
    ret = epoll_ctl(
        example->epollfd,
        EPOLL_CTL_MOD,
        example->identify_queue.timerfd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLONESHOT,
            .data = {
                .fd = example->identify_queue.timerfd
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


static int example_epoll_event_dispatch (
    struct example_s * example,
    struct epoll_event * event
)
{
    if (event->data.fd == example->identify_queue.timerfd)
        return example_epoll_event_identify_queue_timerfd(example, event);

    if (event->data.fd == example->identify_queue.pipefd[0])
        return example_epoll_event_identify_queue_pipefd(example, event);

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

    int bytes_written = 0;
    struct example_identify_queue_task_s queue_task = {
        .sec = sec,
        .nsec = nsec,
        .task = {
            .job = job
        }
    };

    bytes_written = write(example->identify_queue.pipefd[1], &queue_task, sizeof(struct example_identify_queue_task_s));
    if (-1 == bytes_written) {
        syslog(LOG_ERR, "%s:%d:%s: write: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    if (0 == bytes_written) {
        syslog(LOG_ERR, "%s:%d:%s: wrote 0 bytes (pipe closed?)", __FILE__, __LINE__, __func__);
        return -1;
    }
    if (sizeof(struct example_identify_queue_task_s) != bytes_written) {
        syslog(LOG_ERR, "%s:%d:%s: partial write, wrote %d bytes but expected to write %ld bytes", __FILE__, __LINE__, __func__, bytes_written, sizeof(struct example_identify_queue_task_s));
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
