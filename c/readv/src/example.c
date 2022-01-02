#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <syslog.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <sys/uio.h>

#include "example.h"
#include "example_args_parser.h"


int example_usage (
    void * user_data
)
{
    fprintf(stderr, "USAGE: example [-v] path\n");
    return -1;
    (void)user_data;
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;
    struct example_opts_s opts = {0};
    char pathz[65] = {0};
    int pathz_len = 0;

    openlog("example", LOG_CONS | LOG_PID, LOG_USER);
    syslog(LOG_DEBUG, "%s:%d:%s: hi!", __FILE__, __LINE__, __func__);

    ret = example_args_parser_parse(
        /* argc = */ argc,
        /* argv = */ argv,
        /* &opts = */ &opts,
        /* no_filename_cb = */ example_usage,
        /* user_data = */ NULL
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: example_args_parser_parse returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    if (true == opts.verbose) {
        syslog(LOG_INFO, "%s:%d:%s: verbose mode is enabled", __FILE__, __LINE__, __func__);
    }

    // make sure path is null-terminated
    pathz_len = snprintf(pathz, 65, "%.*s", opts.path_len, opts.path);
    if (-1 == pathz_len) {
        syslog(LOG_ERR, "%s:%d:%s: snprintf returned -1", __FILE__, __LINE__, __func__);
        return -1;
    }

    // open the file
    int fd = open(pathz, O_RDONLY);
    if (-1 == fd) {
        syslog(LOG_ERR, "%s:%d:%s: open: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }

    uint8_t base0[64] = {0};
    uint8_t base1[64] = {0};

    ret = readv(
        /* fd = */ fd,
        /* iovecs = */ (struct iovec[]) {
            {
                .iov_base = base0,
                .iov_len = 8
            },
            {
                .iov_base = base1,
                .iov_len = 64
            }
        },
        /* iovecs_len = */ 2
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: readv: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    // now write them to stdout
    ret = writev(
        /* fd = */ STDOUT_FILENO,
        /* iovecs = */ (struct iovec[]) {
            {
                .iov_base = base0,
                .iov_len = 8
            },
            {
                .iov_base = base1,
                .iov_len = 64
            }
        },
        /* iovecs_len = */ 2
    );
    if (-1 == ret) {
        syslog(LOG_ERR, "%s:%d:%s: writev: %s", __FILE__, __LINE__, __func__, strerror(errno));
        return -1;
    }


    return 0;
}
