#define _DEFAULT_SOURCE

#include <sys/epoll.h>
#include <sys/signalfd.h>
#include <syslog.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "app.h"


// Create a sigset and signalfd and register the signalfd on the
// epoll. Called by `app_init`.
static int app_init_signalfd (
    struct app_s * app
)
{
    int ret = 0;

    if (APP_S_SENTINEL != app->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    // Create a sigset.
    ret = sigemptyset(&app->sigset);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sigemptyset: %s",
                __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    ret = sigaddset(&app->sigset, SIGINT);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sigaddset: %s",
                __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // Create the signalfd
    app->signalfd = signalfd(
        /* old fd = */ -1,
        /* sigset = */ &app->sigset,
        /* flags = */ SFD_NONBLOCK | SFD_CLOEXEC
    );
    if (app->signalfd < 0) {
        syslog(LOG_ERR, "%s:%d:%s: signalfd: %s",
               __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // Register the signalfd on epoll
    ret = epoll_ctl(
        app->epoll_fd,
        EPOLL_CTL_ADD,
        app->signalfd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
            .data = {
                .fd = app->signalfd
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", 
               __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // Finally, block the sigset so it's handled by signalfd, not signal
    // handlers.
    ret = sigprocmask(
            /* how = */ SIG_BLOCK,
            /* set = */ &app->sigset,
            /* old = */ NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: sigprocmask: %s",
                __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;

}


int app_init (
    struct app_s * app
)
{
    int ret = 0;

    syslog(LOG_DEBUG, "%s:%d:%s: hi!",
           __FILE__, __LINE__, __func__);

    app->sentinel = APP_S_SENTINEL;

    // Create the epoll instance
    app->epoll_fd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == app->epoll_fd) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", 
               __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    // Initialize signalfd
    ret = app_init_signalfd(app);
    if (ret < 0) {
        syslog(LOG_ERR, "%s:%d:%s: gab_init_signalfd returned %d",
               __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    
    
    return 0;
}


// In this example, this function is never called since
// SIGPIPE is not sigblocked.
static int app_epoll_event_signalfd_sigpipe (
    struct app_s * app,
    struct epoll_event * event,
    struct signalfd_siginfo * siginfo
)
{
    int ret = 0;

    syslog(LOG_DEBUG, "%s:%d:%s: hi!",
           __FILE__, __LINE__, __func__);

    if (APP_S_SENTINEL != app->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    exit(EXIT_FAILURE);

}


// In this example, this function is never called since
// SIGTERM is not sigblocked.
static int app_epoll_event_signalfd_sigterm (
    struct app_s * app,
    struct epoll_event * event,
    struct signalfd_siginfo * siginfo
)
{
    int ret = 0;

    syslog(LOG_DEBUG, "%s:%d:%s: hi!",
           __FILE__, __LINE__, __func__);

    if (APP_S_SENTINEL != app->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    exit(EXIT_FAILURE);

}


static int app_epoll_event_signalfd_sigint (
    struct app_s * app,
    struct epoll_event * event,
    struct signalfd_siginfo * siginfo
)
{
    int ret = 0;

    syslog(LOG_DEBUG, "%s:%d:%s: hi!",
           __FILE__, __LINE__, __func__);

    if (APP_S_SENTINEL != app->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    // To set an example, the first time we receive a sigint, just
    // increment a counter. The second time, we quit. This is some
    // state-keeping we need to push into the app structure.
    if (0 == app->num_sigint_received) {
        app->num_sigint_received += 1;

        // We're done with all handling of the signalfd; re-arm it in
        // epoll and return.
        // Re-arm EPOLLONESHOT file descriptor in epoll
        ret = epoll_ctl(
            app->epoll_fd,
            EPOLL_CTL_MOD,
            event->data.fd,
            &(struct epoll_event){
                .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLET | EPOLLONESHOT,
                .data = {
                    .fd = event->data.fd
                }
            }
        );
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d: epoll_ctl: %s", __func__, __LINE__, strerror(errno));
            return -1;
        }
        return 0;
    }

    // We've received a previous sigint, here we could
    // * `close(2)` file descriptors
    // * `pthread_cancel(3)` and `pthread_join(3)` threads
    // * send signals to children to quit, then join the children
    //     (but maybe make sure another SIGINT, or another signal,
    //     would immediately quit)
    //
    // Right now, we just quit. We're done with everything.
    exit(EXIT_SUCCESS);
}


static int app_epoll_event_signalfd (
    struct app_s * app,
    struct epoll_event * event
)
{
    int ret = 0;
    int bytes_read = 0;

    syslog(LOG_DEBUG, "%s:%d:%s: hi!",
           __FILE__, __LINE__, __func__);

    if (APP_S_SENTINEL != app->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    // We know we've received a signal through the signalfd, we now
    // need to figure out which signal we've received. To do that,
    // we read a signalfd_siginfo struct from the fd:
    struct signalfd_siginfo siginfo = {0};
    bytes_read = read(app->signalfd, &siginfo, sizeof(struct signalfd_siginfo));
    if (0 == bytes_read) {
        syslog(LOG_ERR, "%s:%d:%s: read 0 bytes!",
               __FILE__, __LINE__, __func__);
        return -1;
    }
    if (bytes_read < 0) {
        syslog(LOG_ERR, "%s:%d:%s: read: %s",
               __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // Now we can dispatch on the signal number.
    switch (siginfo.ssi_signo) {
        case SIGINT:
            return app_epoll_event_signalfd_sigint(app, event, &siginfo);

        case SIGPIPE:
            return app_epoll_event_signalfd_sigpipe(app, event, &siginfo);

        case SIGTERM:
            return app_epoll_event_signalfd_sigterm(app, event, &siginfo);

        default: {
            syslog(LOG_ERR, "%s:%d:%s: received unhandled signal %d",
                   __FILE__, __LINE__, __func__, siginfo.ssi_signo);
            return -1;
        }
    }
}


static int app_epoll_event_dispatch (
    struct app_s * app,
    struct epoll_event * event
)
{
    if (event->data.fd == app->signalfd)
        return app_epoll_event_signalfd(app, event);

    syslog(LOG_WARNING, "%s:%d:%s: No match on epoll event.",
           __FILE__, __LINE__, __func__);
    return -1;
}


static int app_epoll_handle_events (
    struct app_s * app,
    struct epoll_event epoll_events[EPOLL_NUM_EVENTS],
    int ep_num_events
)
{
    int ret = 0;
    for (int i = 0; i < ep_num_events; i++) {
        ret = app_epoll_event_dispatch(app, &epoll_events[i]);
        if (0 != ret) {
            syslog(LOG_ERR, "%s:%d:%s: app_epoll_event_dispatch returned %d",
                   __FILE__, __LINE__, __func__, ret);
            return ret;
        }
    }
}


int app_epoll_loop (
    struct app_s * app
)
{
    int ret = 0;

    syslog(LOG_DEBUG, "%s:%d:%s: hi!",
           __FILE__, __LINE__, __func__);

    if (APP_S_SENTINEL != app->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong!",
               __FILE__, __LINE__, __func__);
        return -1;
    }

    
    int ep_num_events = 0;
    struct epoll_event events[EPOLL_NUM_EVENTS];
    for (ep_num_events = epoll_wait(app->epoll_fd, events, EPOLL_NUM_EVENTS, -1);
         ep_num_events > 0 || (-1 == ep_num_events && EINTR == errno);
         ep_num_events = epoll_wait(app->epoll_fd, events, EPOLL_NUM_EVENTS, -1))
    {
        // (snippet: epev)
        ret = app_epoll_handle_events(app, events, ep_num_events);
        if (ret < 0) {
            syslog(LOG_ERR, "%s:%d:%s: app_epoll_handle_events returned %d",
                    __FILE__, __LINE__, __func__, ret);
            return -1;
        }
    }
    if (-1 == ep_num_events) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_wait: %s", 
               __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    return 0;
}


int main (
    int argc,
    char * argv[]
)
{

    int ret = 0;
    struct app_s app = {0};

    openlog("app", LOG_CONS | LOG_PID, LOG_USER);
    syslog(LOG_INFO, "%s:%d:%s: hi!",
           __FILE__, __LINE__, __func__);


    ret = app_init(&app);
    if (ret < 0) {
        syslog(LOG_ERR, "%s:%d:%s: app_init returned %d",
               __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    ret = app_epoll_loop(&app);
    if (ret < 0) {
        syslog(LOG_ERR, "%s:%d:%s: app_epoll_loop returned %d",
               __FILE__, __LINE__, __func__, ret);
        return -1;
    }

    return 0;
}
