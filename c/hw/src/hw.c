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
#include <sys/signalfd.h>
#include <signal.h>
#include <pthread.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#include "hw.h"
#include "hw_tcp_task.h"
#include "hw_nats_task.h"


#define EPOLL_NUM_EVENTS 8

int hw_init (
    struct hw_s * hw
)
{

    int ret = 0;

    // This application doesn't use stdin, stdout or stderr. Just close them.
    close(STDOUT_FILENO);
    close(STDIN_FILENO);
    close(STDERR_FILENO);

    // main thread needs an epoll
    hw->epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == hw->epoll_fd) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // tcp threads also needs an epoll
    hw->tcp_tasks_epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == hw->tcp_tasks_epoll_fd) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // And the nats task needs an epoll
    hw->nats_task_epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == hw->nats_task_epoll_fd ) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // nats task and tcp tasks communicate using a socketpair
    ret = socketpair(AF_UNIX, SOCK_STREAM | SOCK_NONBLOCK | SOCK_CLOEXEC, 0, hw->socketpair);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: socketpair: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // Main thread needs a filled sigset on a signalfd to react to signals
    sigset_t sigset = {0};
    ret = sigfillset(&sigset);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sigfillset: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // Create the signalfd
    hw->signal_fd = signalfd(
        /* fd = */ -1,
        /* &sigset = */ &sigset,
        /* flags = */ SFD_NONBLOCK | SFD_CLOEXEC
    );
    if (-1 == hw->signal_fd) {
        syslog(LOG_ERR, "%s:%d:%s: signalfd: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // Block the signals
    ret = sigprocmask(
            /* how = */ SIG_BLOCK,
            /* &sigset = */ &sigset,
            /* &oldset = */ NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sigprocmask: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // Add the signalfd to epoll
    ret = epoll_ctl(
        hw->epoll_fd,
        EPOLL_CTL_ADD,
        hw->signal_fd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
            .data = {
                .fd = hw->signal_fd
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


static int hw_epoll_event_signal_fd_sighup (
    struct hw_s * hw,
    struct epoll_event * event,
    struct signalfd_siginfo * siginfo
)
{
    int ret = 0;
    syslog(LOG_INFO, "%s:%d:%s: caught SIGHUP", __FILE__, __LINE__, __func__);

    // Do something useful here maybe.

    // Re-arm the fd in epoll
    // Re-arm EPOLLONESHOT file descriptor in epoll
    ret = epoll_ctl(
        hw->epoll_fd,
        EPOLL_CTL_MOD,
        event->data.fd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLOUT | EPOLLERR | EPOLLHUP | EPOLLET | EPOLLONESHOT,
            .data = {
                .fd = event->data.fd
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: epoll_ctl: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    // We're done.
    return 0;
    (void)siginfo;
}


static int hw_epoll_event_signal_fd_sigint (
    struct hw_s * hw,
    struct epoll_event * event,
    struct signalfd_siginfo * siginfo
)
{
    syslog(LOG_INFO, "%s:%d:%s: caught SIGINT - exiting!", __FILE__, __LINE__, __func__);
    exit(EXIT_SUCCESS);
    (void)hw;
    (void)event;
    (void)siginfo;
}


static int hw_epoll_event_signal_fd (
    struct hw_s * hw,
    struct epoll_event * event
)
{

    int bytes_read;
    struct signalfd_siginfo siginfo;

    bytes_read = read(event->data.fd, &siginfo, sizeof(struct signalfd_siginfo));
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        exit(EXIT_FAILURE);
    }
    if (0 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: signalfd fd was closed - which is unexpected!", __FILE__, __LINE__, __func__);
        exit(EXIT_FAILURE);
    }

    // Dispatch on signal number
    if (SIGHUP == siginfo.ssi_signo)
        return hw_epoll_event_signal_fd_sighup(hw, event, &siginfo);

    if (SIGINT == siginfo.ssi_signo)
        return hw_epoll_event_signal_fd_sigint(hw, event, &siginfo);

    syslog(LOG_ERR, "%s:%d:%s: caught unknown signal %d - exiting", __FILE__, __LINE__, __func__, siginfo.ssi_signo);
    exit(EXIT_FAILURE);
}


static int hw_epoll_event_dispatch (
    struct hw_s * hw,
    struct epoll_event * event
)
{
    if (event->data.fd == hw->signal_fd)
        return hw_epoll_event_signal_fd(hw, event);

    syslog(LOG_WARNING, "%s:%d:%s: No match on epoll event.", __FILE__, __LINE__, __func__);
    return 0;
}


static int hw_epoll_handle_events (
    struct hw_s * hw,
    struct epoll_event epoll_events[EPOLL_NUM_EVENTS],
    int ep_events_len
)
{
    int ret = 0;
    for (int i = 0; i < ep_events_len; i++) {
        ret = hw_epoll_event_dispatch(hw, &epoll_events[i]);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}


static int hw_tcp_server_start (
    struct hw_s * hw
)
{
    int ret = 0;

    struct addrinfo *servinfo, *p;
    ret = getaddrinfo(
        /* host = */ HOST,
        /* port = */ PORT, 
        /* hints = */ &(struct addrinfo) {
            .ai_family = AF_UNSPEC,
            .ai_socktype = SOCK_STREAM
        },
        /* servinfo = */ &servinfo
    );
    if (0 != ret) {
        syslog(LOG_ERR, "%s:%d:%s: getaddrinfo:: %s", __FILE__, __LINE__, __func__, gai_strerror(ret));
        return -1;
    }

    // Loop over the results from getaddrinfo and try to bind them up.
    for (p = servinfo; p != NULL; p = p->ai_next) {

        // Create a socket
        hw->tcp_fd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (-1 == hw->tcp_fd) {
            syslog(LOG_WARNING, "%s:%d:%s: socket: %s", __FILE__, __LINE__, __func__, strerror(errno));
            // let's try the next entry...
            continue;
        }

        // Set the socket REUSEADDR - this makes sure that we can start the
        // application after a restart even if the socket is still registered
        // in the kernel by the old application due to stale connections from
        // clients.
        ret = setsockopt(hw->tcp_fd, SOL_SOCKET, SO_REUSEADDR, &(int){1}, sizeof(int));
        if (-1 == ret) {
            syslog(LOG_WARNING, "%s:%d:%s: setsockopt: %s", __FILE__, __LINE__, __func__, strerror(errno));
            // We don't care if this doesn't work so much - we can run without REUSEADDR.
        }

        // Bind the socket to the port
        ret = bind(hw->tcp_fd, p->ai_addr, p->ai_addrlen);
        if (-1 == ret) {
            // Ok, we couldn't bind this socket - close this socket and try the
            // next hit from getaddrinfo.
            syslog(LOG_WARNING, "%s:%d:%s: bind: %s", __FILE__, __LINE__, __func__, strerror(errno));
            close(hw->tcp_fd);
            continue;
        }

        // If we get here, it means that we've successfully bound up a tcp
        // socket. We don't need to try any more results from getaddrinfo.
        // Break out of the loop.
        break;
    }
    // Remember to free up the servinfo data!
    freeaddrinfo(servinfo);

    // If p is NULL, it means that the above loop went through all of the
    // results from getaddrinfo and never broke out of the loop - so we have no
    // valid socket.
    if (NULL == p) {
        syslog(LOG_ERR, "%s:%d:%s: failed to bind to any address", __FILE__, __LINE__, __func__);
        return -1;
    }

    // At this point, we have successfully bound up a port. Now we just need to
    // listen for connection on the port.
    ret = listen(hw->tcp_fd, TCP_LISTEN_BACKLOG);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: listen: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // Add the tcp fd to tcp tasks epoll
    ret = epoll_ctl(
        hw->tcp_tasks_epoll_fd,
        EPOLL_CTL_ADD,
        hw->tcp_fd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
            .data = {
                .fd = hw->tcp_fd
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // We're done - we have a fd on the epoll that will trigger on incoming
    // connection.
    return 0;
}


int main (
    const int argc,
    const char *argv[]
)
{

    int ret = 0;

    openlog("hw", LOG_CONS | LOG_PID, LOG_USER);

    struct hw_s hw = {
        .sentinel = 8090
    };
    ret = hw_init(&hw);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: hw_init returned %d", __FILE__, __LINE__, __func__, ret);
        exit(EXIT_FAILURE);
    }


    // start listening for connections
    ret = hw_tcp_server_start(&hw);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: hw_tcp_server_start returned %d", __FILE__, __LINE__, __func__, ret);
        exit(EXIT_FAILURE);
    }

    // Create tcp listener threads
    for (int i = 0; i < NUM_THREADS; i++) {
        ret = pthread_create(&hw.tcp_task_threads[i], NULL, hw_tcp_task, &hw);
        if (0 != ret) {
            syslog(LOG_ERR, "%s:%d: pthread_create: %s", __func__, __LINE__, strerror(errno));
            return -1;
        }
    }


    // And the nats thread
    ret = pthread_create(&hw.nats_thread, NULL, hw_nats_task, &hw);
    if (0 != ret) {
        syslog(LOG_ERR, "%s:%d: pthread_create: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }


    // Time for the epoll_wait loop
    int ep_events_len = 0;
    struct epoll_event ep_events[EPOLL_NUM_EVENTS];
    for (ep_events_len = epoll_wait(hw.epoll_fd, ep_events, EPOLL_NUM_EVENTS, -1);
         ep_events_len > 0 || (-1 == ep_events_len && EINTR == errno);
         ep_events_len = epoll_wait(hw.epoll_fd, ep_events, EPOLL_NUM_EVENTS, -1))
    {
        ret = hw_epoll_handle_events(&hw, ep_events, ep_events_len);
        if (-1 == ret) {
            break;
        }
    }
    if (-1 == ep_events_len) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_wait: %s", __FILE__, __LINE__, __func__, strerror(errno));
        exit(EXIT_FAILURE);
    }


    exit(EXIT_SUCCESS);	
    (void)argc;
    (void)argv;
}
