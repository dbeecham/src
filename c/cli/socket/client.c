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

#define STR_CONNECT_FAILED "connect failed"

int main(int argc, char *argv[])
{
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

        if (-1 == connect(sockfd, p->ai_addr, p->ai_addrlen)) {
            close(sockfd);
            warn("connect");
            continue;
        }
    
        break;
    }
    freeaddrinfo(servinfo);

    if (NULL == p) {
        write(STDERR_FILENO, STR_CONNECT_FAILED, strlen(STR_CONNECT_FAILED));
        exit(EXIT_FAILURE);
    }

    if (-1 == write(sockfd, "hello\n", 6)) {
        close(sockfd);
        err(EXIT_FAILURE, "write");
    }
    close(sockfd);

    exit(EXIT_SUCCESS);
}
