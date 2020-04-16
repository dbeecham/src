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
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/timerfd.h>
#include <stddef.h>

#include "hw.h"
#include "hw_tcp_task.h"
#include "hw_client_parser.h"

#define EPOLL_NUM_EVENTS 8

// TODO:
// * on message from socketpair, send it to clients
// * on message from clients, send it to socketpair


static int hw_tcp_task_epoll_event_client_fd (
    struct hw_s * hw,
    struct epoll_event * event
)
{
    int ret = 0;
    int bytes_read = 0;
    char buf[TCP_READ_BUF_LEN];

    struct hw_client_s * client = event->data.ptr;
    if (18091 != client->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!", __FILE__, __LINE__, __func__);
        return -1;
    }

    bytes_read = read(client->fd, buf, TCP_READ_BUF_LEN);
    if (-1 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    if (0 == bytes_read) {
        // Client disconnected, clear out the clients stuff.

        // Remember to EPOLL_CTL_DEL *before* closing the file descriptor, see
        // https://idea.popcount.org/2017-03-20-epoll-is-fundamentally-broken-22/
        ret = epoll_ctl(
            hw->tcp_tasks_epoll_fd,
            EPOLL_CTL_DEL,
            client->fd,
            NULL
        );
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
            return -1;
        }

        close(client->fd);
        
        // Remember to EPOLL_CTL_DEL *before* closing the file descriptor, see
        // https://idea.popcount.org/2017-03-20-epoll-is-fundamentally-broken-22/
        ret = epoll_ctl(
            hw->tcp_tasks_epoll_fd,
            EPOLL_CTL_DEL,
            client->watchdog.timer_fd,
            NULL
        );
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
            return -1;
        }
        close(client->watchdog.timer_fd);

        *client = (struct hw_client_s){0};
        
        syslog(LOG_INFO, "%s:%d:%s: client disconnected", __FILE__, __LINE__, __func__);
        return 0;
    }

    syslog(LOG_INFO, "%s:%d:%s: read %.*s", __FILE__, __LINE__, __func__, bytes_read, buf);

    // We read some data, so so let's kick the clients watchdog timer
    // arm timerfd
    ret = timerfd_settime(
        /* fd        = */ client->watchdog.timer_fd,
        /* opt       = */ 0,
        /* timerspec = */ &(struct itimerspec) {
            .it_interval = {0},
            .it_value = {
                .tv_sec  = CLIENT_PING_TIMEOUT_S,
                .tv_nsec = 0
            }
        },
        /* old_ts    = */ NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: timerfd_settime: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    // Parse the data the user sent
    ret = hw_client_parser_parse(client, buf, bytes_read);
    if (-1 == ret) {
        syslog(LOG_WARNING, "%s:%d:%s: hw_client_parser_parse returned %d", __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    // Re-arm the fd on the epoll
    ret = epoll_ctl(
        hw->tcp_tasks_epoll_fd,
        EPOLL_CTL_MOD,
        client->fd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
            .data = event->data
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


static int hw_tcp_task_epoll_event_client_timer_fd (
    struct hw_s * hw,
    struct epoll_event * event
)
{
    // If this triggers, that means we've seen no data from the client for some
    // time, and it's time to kick the client out.
    //
    int ret = 0;

    struct hw_client_watchdog_s * watchdog = event->data.ptr;
    if (18092 != watchdog->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!", __FILE__, __LINE__, __func__);
        return -1;
    }

    struct hw_client_s * client = (struct hw_client_s*)(((char*)watchdog) - offsetof(struct hw_client_s, watchdog));
    if (18091 != client->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!", __FILE__, __LINE__, __func__);
        return -1;
    }

    // Remember to EPOLL_CTL_DEL *before* closing the file descriptor, see
    // https://idea.popcount.org/2017-03-20-epoll-is-fundamentally-broken-22/
    ret = epoll_ctl(
        hw->tcp_tasks_epoll_fd,
        EPOLL_CTL_DEL,
        client->fd,
        NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    close(client->fd);
    
    // Remember to EPOLL_CTL_DEL *before* closing the file descriptor, see
    // https://idea.popcount.org/2017-03-20-epoll-is-fundamentally-broken-22/
    ret = epoll_ctl(
        hw->tcp_tasks_epoll_fd,
        EPOLL_CTL_DEL,
        client->watchdog.timer_fd,
        NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
    close(client->watchdog.timer_fd);

    *client = (struct hw_client_s){0};
    
    syslog(LOG_INFO, "%s:%d:%s: client disconnected", __FILE__, __LINE__, __func__);
}


static int hw_tcp_task_epoll_event_tcp_fd (
    struct hw_s * hw,
    struct epoll_event * event
)
{
    // Accept the client into the hw religion/sect
    int ret;
    int client_fd = 0;
    struct sockaddr_storage their_addr = {0};
    socklen_t sin_size = sizeof(struct sockaddr_storage);
    
    client_fd = accept(hw->tcp_fd, (struct sockaddr*)&their_addr, &sin_size);
    if (-1 == client_fd) {
        syslog(LOG_ERR, "%s:%d:%s: accept: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // First off, we need to find a free spot for the client.
    for (int i = 0; i < MAX_CLIENTS; i++) {

        if (0 == hw->clients[i].sentinel) {

            // This spot is free! Assign it!
            hw->clients[i].sentinel = 18091;
            hw->clients[i].fd = client_fd;

            // Initialize the client parser
            ret = hw_client_parser_init(&hw->clients[i]);
            if (-1 == ret) {
                syslog(LOG_ERR, "%s:%d:%s: hw_client_parser_init returned %d", __FILE__, __LINE__, __func__, ret);
                
            }

            // Create a watchdog timerfd for this client
            hw->clients[i].watchdog.sentinel = 18092;
            hw->clients[i].watchdog.timer_fd = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
            if (-1 == hw->clients[i].watchdog.timer_fd) {
                syslog(LOG_ERR, "%s:%d:%s: timerfd_create: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            // Start the timer
            // arm timerfd
            ret = timerfd_settime(
                /* fd        = */ hw->clients[i].watchdog.timer_fd,
                /* opt       = */ 0,
                /* timerspec = */ &(struct itimerspec) {
                    .it_interval = {0},
                    .it_value = {
                        .tv_sec  = CLIENT_PING_TIMEOUT_S,
                        .tv_nsec = 0
                    }
                },
                /* old_ts    = */ NULL
            );
            if (-1 == ret) {
                syslog(LOG_ERR, "%s:%d:%s: timerfd_settime: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            // Add the client to epoll
            ret = epoll_ctl(
                hw->tcp_tasks_epoll_fd,
                EPOLL_CTL_ADD,
                client_fd,
                &(struct epoll_event){
                    .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
                    .data = {
                        .ptr = &hw->clients[i]
                    }
                }
            );
            if (-1 == ret) {
                syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            // And the clients watchdog timer
            ret = epoll_ctl(
                hw->tcp_tasks_epoll_fd,
                EPOLL_CTL_ADD,
                hw->clients[i].watchdog.timer_fd,
                &(struct epoll_event){
                    .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
                    .data = {
                        .ptr = &hw->clients[i].watchdog
                    }
                }
            );
            if (-1 == ret) {
                syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            // Before we go, let's re-arm the accept fd on epoll
            ret = epoll_ctl(
                hw->tcp_tasks_epoll_fd,
                EPOLL_CTL_MOD,
                event->data.fd,
                &(struct epoll_event){
                    .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
                    .data = event->data
                }
            );
            if (-1 == ret) {
                syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
                return -1;
            }

            // All done, let's pop back to epoll_wait.
            return 0;
        }
    }


    // If we reach this point, then we found on free spots for the new client.
    syslog(LOG_WARNING, "%s:%d:%s: no free spots for client", __FILE__, __LINE__, __func__);
    close(client_fd);
    return 0;
}


static int hw_tcp_task_epoll_event_dispatch (
    struct hw_s * hw,
    struct epoll_event * event
)
{
    if (event->data.fd == hw->tcp_fd)
        return hw_tcp_task_epoll_event_tcp_fd(hw, event);

    // If it's not the connect socket, it's either a client fd or a timer fd
    // associated with a client. For that, we need to dispatch on the sentinel
    // value.
    int event_sentinel = *(int*)event->data.ptr;
    if (18091 == event_sentinel)
        return hw_tcp_task_epoll_event_client_fd(hw, event);

    if (18092 == event_sentinel)
        return hw_tcp_task_epoll_event_client_timer_fd(hw, event);

    // otherwise, we've got no match on the epoll, just quit.
    syslog(LOG_ERR, "%s:%d:%s: event dispatch defaulted!", __FILE__, __LINE__, __func__);
    return -1;
}


static int hw_tcp_task_epoll_handle_events (
    struct hw_s * hw,
    struct epoll_event epoll_events[EPOLL_NUM_EVENTS],
    int ep_events_len
)
{
    int ret = 0;
    for (int i = 0; i < ep_events_len; i++) {
        ret = hw_tcp_task_epoll_event_dispatch(hw, &epoll_events[i]);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}


void * hw_tcp_task (
        void * arg
)
{
    int ret;
    struct hw_s * hw = arg;
    if (8090 != hw->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!", __FILE__, __LINE__, __func__);
        exit(EXIT_FAILURE);
    }

    int ep_events_len = 0;
    struct epoll_event ep_events[EPOLL_NUM_EVENTS];
    for (ep_events_len = epoll_wait(hw->tcp_tasks_epoll_fd, ep_events, EPOLL_NUM_EVENTS, -1);
         ep_events_len > 0 || (-1 == ep_events_len && EINTR == errno);
         ep_events_len = epoll_wait(hw->tcp_tasks_epoll_fd, ep_events, EPOLL_NUM_EVENTS, -1))
    {
        ret = hw_tcp_task_epoll_handle_events(hw, ep_events, ep_events_len);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: hw_tcp_task_epoll_handle_events returned %d", __FILE__, __LINE__, __func__, ret);
            exit(EXIT_FAILURE);
        }
    }
    if (-1 == ep_events_len) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_wait: %s", __FILE__, __LINE__, __func__, strerror(errno));
        exit(EXIT_FAILURE);
    }

    return 0;
}
