#pragma once

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
#include <pthread.h>

#ifndef NUM_THREADS
#define NUM_THREADS 4
#endif

#ifndef TCP_LISTEN_BACKLOG
#define TCP_LISTEN_BACKLOG 8
#endif

#ifndef HOST
#define HOST "0.0.0.0"
#endif 

#ifndef PORT
#define PORT "10001"
#endif

#ifndef MAX_CLIENTS
#define MAX_CLIENTS 64
#endif

#ifndef TCP_READ_BUF_LEN
#define TCP_READ_BUF_LEN 512
#endif

#ifndef CLIENT_PING_TIMEOUT_S
#define CLIENT_PING_TIMEOUT_S 8
#endif

struct hw_client_watchdog_s {
    int sentinel;
    int timer_fd;
};

struct hw_client_s {
    int sentinel;
    int cs;
    int fd;
    struct hw_client_watchdog_s watchdog; 
};

struct hw_s {
    int sentinel;
    int epoll_fd;
    int tcp_fd;
    int tcp_tasks_epoll_fd;
    int nats_task_epoll_fd;
    pthread_t tcp_task_threads[NUM_THREADS];
    pthread_t nats_thread;
    int signal_fd;
    int socketpair[2];
    struct hw_client_s clients[MAX_CLIENTS];
};
