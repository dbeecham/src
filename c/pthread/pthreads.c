#define _POSIX_C_SOURCE 201805L

#include <pthread.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/signal.h>
#include <sys/epoll.h>
#include <syslog.h>
#include <errno.h>
#include <string.h>


struct app {
    sigset_t sigset;
    pthread_t can_thread;
    pthread_t nats_thread;
    int pipe_fds[2];
};


// This could be in some application-specific file.
int app_signals_block(struct app * app)
{
    int ret;

    ret = sigemptyset(&app->sigset);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: sigemptyset: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    ret = sigaddset(&app->sigset, SIGPIPE);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: sigaddset: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    ret = sigaddset(&app->sigset, SIGTERM);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: sigaddset: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    ret = sigaddset(&app->sigset, SIGINT);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: sigaddset: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    ret = sigprocmask(SIG_BLOCK, &app->sigset, NULL);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: sigprocmask: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    return 0;
}


// Initialize application
int app_init(struct app * app)
{

    int ret = 0;

    // open syslog
    openlog("app", LOG_CONS | LOG_PID, LOG_USER);

    // create epoll instance
    ret = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: epoll_create1: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }


    // Set up a pipe for communication between pthreads
    ret = pipe(app->pipe_fds);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d: pipe: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }

    return 0;
}


// This could be specified in some other file.
void * socketcan_task(void * arg)
{
    int ret = 0;

    struct app * app = arg;

    // ret = open();
    // ret = epoll_wait();
    // ret = write(arg->pipe_fds[1], "hello\n", 6);

}


// This could be specified in some other file.
void * nats_task(void * arg)
{
    int ret = 0;

    struct app * app = arg;

    // ret = open();
    // ret = epoll_wait();
    // ret = write(arg->pipe_fds[1], "hello\n", 6);

}


// Start CANbus task. Separated out into it's own method to force callee to put
// thread-specific information into the app structure.
int app_can_task_start(struct app * app)
{
    int ret = pthread_create(&app->can_thread, NULL, socketcan_task, &app->pipe_fds[1]);
    if (0 != ret) {
        syslog(LOG_ERR, "%s:%d: pthread_create: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }
    return 0;
}


// Start NATS task. Separated out into it's own method to force callee to put
// thread-specific information into the app structure.
int app_nats_task_start(struct app * app)
{
    int ret = pthread_create(&app->nats_thread, NULL, nats_task, &app->pipe_fds[0]);
    if (0 != ret) {
        syslog(LOG_ERR, "%s:%d: pthread_create: %s", __func__, __LINE__, strerror(errno));
        return -1;
    }
    
}


int main(int argc, char * argv[])
{

    int ret = 0;
    int pipe_fds[2];
    struct app app = {0};

    // Initialize application
    ret = app_init(&app);
    if (-1 == ret) {
        exit(EXIT_FAILURE);
    }

    // Block signals; we'll deal with them later.
    ret = app_signals_block(&app);
    if (-1 == ret) {
        exit(EXIT_FAILURE);
    }

    // Start threads
    ret = app_can_task_start(&app);
    if (-1 == ret) {
        exit(EXIT_FAILURE);
    }
    ret = app_nats_task_start(&app);
    if (-1 == ret) {
        exit(EXIT_FAILURE);
    }


    // signal handler loop
    siginfo_t siginfo;
    for (;;) {
        ret = sigwaitinfo(&app.sigset, &siginfo);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d: sigwaitinfo: %s", __func__, __LINE__, strerror(errno));
            exit(EXIT_FAILURE);
        }

        write(STDERR_FILENO, "signal!\n", 8);

        // dispatch on signal number
        pthread_cancel(app.can_thread);
        pthread_cancel(app.nats_thread);
        pthread_join(app.can_thread, NULL);
        pthread_join(app.nats_thread, NULL);

        exit(EXIT_FAILURE);

    }

    pthread_join(app.can_thread, NULL);
    pthread_join(app.nats_thread, NULL);

    exit(EXIT_SUCCESS);

}
