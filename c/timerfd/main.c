#define _POSIX_C_SOURCE 201805L

#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <err.h>
#include <time.h>
#include <sys/timerfd.h>
#include <sys/epoll.h>
#include <string.h>

#define EPOLL_MAX_EVENTS 8

#define STR_TIMER_EVENT "timer event!\n" 

struct app {
    int ep_fd;
    int timer_fd;
};

int app_epoll_handle_timerfd_event(struct app * app, struct epoll_event * event)
{

    int ret;

    (void)app;
    (void)event;
    write(STDOUT_FILENO, STR_TIMER_EVENT, strlen(STR_TIMER_EVENT));

    // rearm the timer
    ret = timerfd_settime(
        app->timer_fd, 
        0,
        &(struct itimerspec) {
            .it_interval = {0},
            .it_value = {
                .tv_sec = 1,
                .tv_nsec = 0
            }
        },
        NULL
    );
    if (-1 == ret) {
        warn("timerfd_settime");
        close(app->timer_fd);
        exit(EXIT_FAILURE);
    }

    ret = epoll_ctl(app->ep_fd, EPOLL_CTL_MOD, app->timer_fd, &(struct epoll_event) {
            .events = EPOLLIN | EPOLLHUP | EPOLLERR | EPOLLHUP | EPOLLET | EPOLLONESHOT,
            .data = {
                .fd = app->timer_fd
            }
    });
    if (-1 == ret) {
        warn("epoll_ctl");
        close(app->timer_fd);
        exit(EXIT_FAILURE);
    }

    return 0;
}

int app_epoll_handle_event(struct app * app, struct epoll_event * event)
{
    if (event->data.fd == app->timer_fd) {
        app_epoll_handle_timerfd_event(app, event);
    }
    return 0;
}

int app_epoll_handle_events(struct app * app, struct epoll_event epoll_events[EPOLL_MAX_EVENTS], int ep_num_events)
{
    // loop over all events
    for (int i = 0; i < ep_num_events; i++)
    {
        app_epoll_handle_event(app, &epoll_events[i]);
    }
    return 0;
}

int main(int argc, char *argv[])
{

    (void)argc;
    (void)argv;

    int ret = 0;

    // App structure - passed around callbacks
    struct app app = {0};
    
    // Epoll structures
    int ep_num_events = 0;
    struct epoll_event ep_events[EPOLL_MAX_EVENTS];

    app.timer_fd = timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK | TFD_CLOEXEC);
    if (-1 == app.timer_fd) {
        warn("timerf_create");
        exit(EXIT_FAILURE);
    }

    ret = timerfd_settime(
        app.timer_fd, 
        0,
        &(struct itimerspec) {
            .it_interval = {0},
            .it_value = {
                .tv_sec = 1,
                .tv_nsec = 0
            }
        },
        NULL
    );
    if (-1 == ret) {
        warn("timerfd_settime");
        close(app.timer_fd);
        exit(EXIT_FAILURE);
    }

    // create the polling instance
    app.ep_fd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == app.ep_fd) {
        warn("epoll_create1");
        exit(EXIT_FAILURE);
    }

    // add the timer 
    ret = epoll_ctl(app.ep_fd, EPOLL_CTL_ADD, app.timer_fd, &(struct epoll_event) {
            .events = EPOLLIN | EPOLLET | EPOLLONESHOT | EPOLLERR | EPOLLHUP,
            .data = {
                .fd = app.timer_fd
            }
    });
    if (-1 == ret) {
        warn("epoll_ctl");
        exit(EXIT_FAILURE);
    }

    for (ep_num_events = epoll_wait(app.ep_fd, ep_events, EPOLL_MAX_EVENTS, -1);
         ep_num_events > 0;
         ep_num_events = epoll_wait(app.ep_fd, ep_events, EPOLL_MAX_EVENTS, -1))
    {
        ret = app_epoll_handle_events(&app, ep_events, ep_num_events);
        if (-1 == ret) {
            break;
        }
    }
    if (-1 == ep_num_events) {
        warn("epoll_wait");
        exit(EXIT_FAILURE);
    }



    return 0;
}
