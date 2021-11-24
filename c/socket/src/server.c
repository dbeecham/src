#define _DEFAULT_SOURCE

#include <sys/types.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <err.h>
#include <syslog.h>
#include <string.h>
#include <errno.h>
#include <sys/epoll.h>

#define CONFIG_HOST "localhost"
#define CONFIG_PORT "7794"
#define CONFIG_PORT2 "7795"
#define CONFIG_LISTEN_BACKLOG 64
#define CONFIG_CLIENT_READ_BUF_LEN 128


#define SERVER_SENTINEL 8090
#define SERVER_EPOLLFD_SENTINEL 8091


struct server_epollfd_s {
    int sentinel;
    enum {
        SERVER_EPOLLFD_TYPE_LISTEN
    } type;
    int fd;
};

struct server_s {
    int sentinel;
    int epollfd;
    struct server_epollfd_s epollfds[32];
};


int server_getaddrinfo (
    struct server_s * server
)
{
    int ret = 0;
//    struct sockaddr_storage their_addr;
//    socklen_t sin_size;
    struct addrinfo hints = {
        .ai_family = AF_UNSPEC,
        .ai_socktype = SOCK_STREAM
    };
    struct addrinfo *servinfo, *p;
    int sockfd;
 //   int client_fd;
 //   char buf[CONFIG_CLIENT_READ_BUF_LEN];
 //   ssize_t bytes_read;


    if (SERVER_SENTINEL != server->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong", __FILE__, __LINE__, __func__);
        return -1;
    }


    /* Get server information. */
    ret = getaddrinfo(CONFIG_HOST, CONFIG_PORT2, &hints, &servinfo);
    if (0 != ret) {
        syslog(LOG_ERR, "%s:%d:%s: getaddrinfo: %s", __FILE__, __LINE__, __func__, gai_strerror(ret));
        return -1;
    }
    if (NULL == servinfo) {
        syslog(LOG_ERR, "%s:%d:%s: getaddrinfo returned no results", __FILE__, __LINE__, __func__);
        return -1;
    }

    int i = 0;
    p = servinfo;
    while (1) {

        sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (-1 == sockfd) {
			syslog(LOG_WARNING, "%s:%d:%s: socket: %s", __FILE__, __LINE__, __func__, strerror(errno));

            // try the next one
            p = p->ai_next;
            if (NULL == p) {
                break;
            }
            continue;
        }

        ret = setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &(int){1}, sizeof(int));
        if (-1 == ret) {
            syslog(LOG_WARNING, "%s:%d:%s: setsockopt: %s", __FILE__, __LINE__, __func__, strerror(errno));
        }


        ret = bind(sockfd, p->ai_addr, p->ai_addrlen);
        if (-1 == ret) {
            syslog(LOG_WARNING, "%s:%d:%s: bind: %s", __FILE__, __LINE__, __func__, strerror(errno));

            // close the bad sockfd
            close(sockfd);

            // and try the next one instead
            p = p->ai_next;
            if (NULL == p) {
                break;
            }
            continue;
        }


        // ok we've created a socket and we've bound a src address to it. let's
        // listen for connections on it.
        ret = listen(sockfd, CONFIG_LISTEN_BACKLOG);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: listen: %s", __FILE__, __LINE__, __func__, strerror(errno));
            return -1;
        }

        // add it to the server fds list
        server->epollfds[sockfd] = (struct server_epollfd_s) {
            .sentinel = SERVER_EPOLLFD_SENTINEL,
            .type = SERVER_EPOLLFD_TYPE_LISTEN,
            .fd = sockfd
        };

        // and finally to epoll
        ret = epoll_ctl(
            server->epollfd,
            EPOLL_CTL_ADD,
            sockfd,
            &(struct epoll_event){
                .events = EPOLLIN | EPOLLERR | EPOLLHUP | EPOLLONESHOT,
                .data = {
                    .ptr = &server->epollfds[sockfd]
                }
            }
        );
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
            return -1;
        }
        
        // on to the next result

        if (4096 < ++i) {
            syslog(LOG_ERR, "%s:%d:%s: infinite loop", __FILE__, __LINE__, __func__);
            return -1;
        }
    }
    freeaddrinfo(servinfo);


//    for (;;) {
//        sin_size = sizeof(their_addr);
//        client_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
//        if (-1 == client_fd) {
//            warn("accept");
//            continue;
//        }
//
//        bytes_read = read(client_fd, buf, BUFLEN);
//        while (0 < bytes_read) {
//            write(STDOUT_FILENO, buf, bytes_read);
//            bytes_read = read(client_fd, buf, BUFLEN);
//        }
//
//    }

    return 0;
}


int server_noaddrinfo (
    struct server_s * server
)
{

    int ret = 0;
    int sockfd = 0;


    // Open a socket
    sockfd = socket(
        /* domain = */ AF_INET,
        /* type = */ SOCK_STREAM | SOCK_NONBLOCK | SOCK_CLOEXEC,
        /* protocol = */ 0
    );
    if (-1 == sockfd) {
        syslog(LOG_WARNING, "%s:%d:%s: socket: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // set the socket SO_REUSEADDR
    ret = setsockopt(
        /* sockfd = */ sockfd,
        /* level = */ SOL_SOCKET,
        /* option = */ SO_REUSEADDR,
        /* value = */ &(int){1},
        /* value_len = */ sizeof(int)
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: setsockopt: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }
     

    // assign an empty src address to it
    struct sockaddr_in src = {
        .sin_family = AF_INET,
        .sin_port = htons(9001)
    };
    ret = bind(
        /* fd = */ sockfd,
        /* src_addr = */ (struct sockaddr*)&src,
        /* src_addr_len = */ sizeof(src)
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: bind: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // add it to server epollfd list
    server->epollfds[sockfd] = (struct server_epollfd_s) {
        .sentinel = SERVER_EPOLLFD_SENTINEL,
        .fd = sockfd,
        .type = SERVER_EPOLLFD_TYPE_LISTEN
    };


    // add it to epoll
    ret = epoll_ctl(
        server->epollfd,
        EPOLL_CTL_ADD,
        sockfd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLONESHOT,
            .data = {
                .ptr = &server->epollfds[sockfd]
            }
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


int server_init (
    struct server_s * server
)
{
    server->sentinel = SERVER_SENTINEL;

    // Create the epoll instance
    server->epollfd = epoll_create1(EPOLL_CLOEXEC);
    if (-1 == server->epollfd) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_create1: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


int server_epoll_event_listen (
    struct server_s * server,
    struct epoll_event * event,
    struct server_epollfd_s * server_epollfd
)
{

    int ret = 0;

    // accept() the client, add the client to epoll...

    int fd = accept(server_epollfd->fd, NULL, 0);
    if (-1 == fd) {
        syslog(LOG_ERR, "%s:%d:%s: accept: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    close(fd);

    // re-arm the accept fd on epoll
    ret = epoll_ctl(
        server->epollfd,
        EPOLL_CTL_MOD,
        server_epollfd->fd,
        &(struct epoll_event){
            .events = EPOLLIN | EPOLLONESHOT,
            .data = event->data
        }
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: epoll_ctl: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    return 0;
}


static int server_epoll_event_dispatch (
    struct server_s * server,
    struct epoll_event * event
)
{

    struct server_epollfd_s * server_epollfd = event->data.ptr;
    if (NULL == server_epollfd) {
        syslog(LOG_ERR, "%s:%d:%s: event data ptr is NULL", __FILE__, __LINE__, __func__);
        return -1;
    }
    if (SERVER_EPOLLFD_SENTINEL != server_epollfd->sentinel) {
        syslog(LOG_ERR, "%s:%d:%s: sentinel is wrong", __FILE__, __LINE__, __func__);
        return -1;
    }

    if (SERVER_EPOLLFD_TYPE_LISTEN == server_epollfd->type) {
        return server_epoll_event_listen(server, event, server_epollfd);
    }

    syslog(LOG_ERR, "%s:%d:%s: No match on epoll event.", __FILE__, __LINE__, __func__);
    return -1;
}


static int server_epoll_handle_events (
    struct server_s * server,
    struct epoll_event epoll_events[8],
    int epoll_events_len
)
{
    int ret = 0;
    for (int i = 0; i < epoll_events_len; i++) {
        // (snippet: epdisp)
        ret = server_epoll_event_dispatch(server, &epoll_events[i]);
        if (0 != ret) {
            return ret;
        }
    }
    return 0;
}


int server_loop (
    struct server_s * server
)
{

    int ret = 0;

    int epoll_events_len = 0;
    struct epoll_event epoll_events[8];
    while (1) {
        epoll_events_len = epoll_wait(
            /* epollfd = */ server->epollfd,
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
        ret = server_epoll_handle_events(server, epoll_events, epoll_events_len);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: server_epoll_handle_events returned -1", __FILE__, __LINE__, __func__);
            return -1;
        }
    }

    return 0;
}


int main (
    int argc,
    char **argv
)
{
    int ret = 0;
    struct server_s server = {0};

    openlog("server", LOG_CONS | LOG_PID, LOG_USER);

    syslog(LOG_DEBUG, "%s:%d:%s: hi!", __FILE__, __LINE__, __func__);

    ret = server_init(&server);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: server_init returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = server_noaddrinfo(&server);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: server_noaddrinfo returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = server_getaddrinfo(&server);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: server_getaddrinfo returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }


    ret = server_loop(&server);
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: server_loop returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }


    return 0;
    (void)argc;
    (void)argv;
}
