#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <err.h>

#define HOST "localhost"
#define PORT "7794"
#define BACKLOG 64
#define BUFLEN 128

#define STR_BIND_FAILED "bind failed"

int main(int argc, char **argv) {

    struct sockaddr_storage their_addr;
    socklen_t sin_size;
    struct addrinfo hints = {
        .ai_family = AF_UNSPEC,
        .ai_socktype = SOCK_STREAM
    };
    struct addrinfo *servinfo, *p;
    int sockfd;
    int client_fd;
    char buf[BUFLEN];
    ssize_t bytes_read;

    /* Get server information. */
    int ret = getaddrinfo(HOST, PORT, &hints, &servinfo);
    if (0 != ret) {
            /* Failed to get address information. Print an error message,
             * sleep for an hour and then try again. */
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(ret));
        exit(EXIT_FAILURE);
    }

    for (p = servinfo; p != NULL; p = p->ai_next) {
        sockfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
        if (-1 == sockfd) {
            warn("socket");
            continue;
        }

        int yes = 1;
        if (-1 == setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(int))) {
            err(EXIT_FAILURE, "setsockopt");
        }

        if (-1 == bind(sockfd, p->ai_addr, p->ai_addrlen)) {
            close(sockfd);
            warn("bind");
            continue;
        }

        break;

    }
    freeaddrinfo(servinfo);

    if (NULL == p) {
        write(STDERR_FILENO, STR_BIND_FAILED, strlen(STR_BIND_FAILED));
        exit(EXIT_FAILURE);
    }

    if (-1 == listen(sockfd, BACKLOG)) {
        err(EXIT_FAILURE, "listen");
    }

    for (;;) {
        sin_size = sizeof(their_addr);
        client_fd = accept(sockfd, (struct sockaddr *)&their_addr, &sin_size);
        if (-1 == client_fd) {
            warn("accept");
            continue;
        }

        bytes_read = read(client_fd, buf, BUFLEN);
        while (0 < bytes_read) {
            write(STDOUT_FILENO, buf, bytes_read);
            bytes_read = read(client_fd, buf, BUFLEN);
        }

    }

    exit(EXIT_SUCCESS);

}
