#pragma once

#define APP_S_SENTINEL 7070
#define EPOLL_NUM_EVENTS 8

#include <signal.h>

struct app_s {
    int sentinel;
    int epoll_fd;
    int signalfd;
    int num_sigint_received;
    sigset_t sigset;
};
