#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdint.h>
#include <assert.h>
#include <syslog.h>
#include <string.h>

#include "example_args_parser.h"

static int cb_called;

int no_filename_cb(void * user_data) {
    cb_called += 1;
    return 0;
    (void)user_data;
}

int test_driver_it_does_not_accept_empty_argc (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;


    ret = example_args_parser_parse(
        /* argc = */ 0,
        /* argv = */ NULL,
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (-1 != ret) {
        printf("%s:%d:%s: ret=%d, should be -1\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (0 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 0\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_does_not_accept_missing_path (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 1,
        /* argv = */ (const char*[]){ (char[]){"./example"} },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (-1 != ret) {
        printf("%s:%d:%s: ret=%d, should be -1\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (1 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 1\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_does_not_accept_missing_path_with_verbose (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 1,
        /* argv = */ (const char*[]){ 
            (char[]){"./example"},
            (char[]){"-v"} 
        },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (-1 != ret) {
        printf("%s:%d:%s: ret=%d, should be -1\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (1 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 1\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_accepts_basic_path (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 2,
        /* argv = */ (const char*[]){ 
            (char[]){"./example"},
            (char[]){"path"} 
        },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (0 != ret) {
        printf("%s:%d:%s: ret=%d, should be 0\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (0 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 0\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }
    if (4 != opts.path_len) {
        printf("%s:%d:%s: path_len=%d, should be 4\n", __FILE__, __LINE__, __func__, opts.path_len);
        return -1;
    }
    if (0 != memcmp(opts.path, "path", 4)) {
        printf("%s:%d:%s: opts.path=%.*s, should be \"path\"\n", __FILE__, __LINE__, __func__, opts.path_len, opts.path);
        return -1;
    }
    if (0 != opts.verbose) {
        printf("%s:%d:%s: opts.verbose=%d, should be 0\n", __FILE__, __LINE__, __func__, opts.verbose);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_accepts_basic_path_and_verbose (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 3,
        /* argv = */ (const char*[]){ 
            (char[]){"./example"},
            (char[]){"-v"},
            (char[]){"path"} 
        },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (0 != ret) {
        printf("%s:%d:%s: ret=%d, should be 0\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (0 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 0\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }
    if (4 != opts.path_len) {
        printf("%s:%d:%s: path_len=%d, should be 4\n", __FILE__, __LINE__, __func__, opts.path_len);
        return -1;
    }
    if (0 != memcmp(opts.path, "path", 4)) {
        printf("%s:%d:%s: opts.path=%.*s, should be \"path\"\n", __FILE__, __LINE__, __func__, opts.path_len, opts.path);
        return -1;
    }
    if (1 != opts.verbose) {
        printf("%s:%d:%s: opts.verbose=%d, should be 1\n", __FILE__, __LINE__, __func__, opts.verbose);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_accepts_basic_verbose_and_path (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 3,
        /* argv = */ (const char*[]){ 
            (char[]){"./example"},
            (char[]){"path"},
            (char[]){"-v"}
        },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (0 != ret) {
        printf("%s:%d:%s: ret=%d, should be 0\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (0 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 0\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }
    if (4 != opts.path_len) {
        printf("%s:%d:%s: path_len=%d, should be 4\n", __FILE__, __LINE__, __func__, opts.path_len);
        return -1;
    }
    if (0 != memcmp(opts.path, "path", 4)) {
        printf("%s:%d:%s: opts.path=%.*s, should be \"path\"\n", __FILE__, __LINE__, __func__, opts.path_len, opts.path);
        return -1;
    }
    if (1 != opts.verbose) {
        printf("%s:%d:%s: opts.verbose=%d, should be 1\n", __FILE__, __LINE__, __func__, opts.verbose);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_accepts_verbose_and_escaped_path (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 4,
        /* argv = */ (const char*[]){ 
            (char[]){"./example"},
            (char[]){"-v"},
            (char[]){"--"},
            (char[]){"-v"}
        },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (0 != ret) {
        printf("%s:%d:%s: ret=%d, should be 0\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (0 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 0\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }
    if (2 != opts.path_len) {
        printf("%s:%d:%s: path_len=%d, should be 2\n", __FILE__, __LINE__, __func__, opts.path_len);
        return -1;
    }
    if (0 != memcmp(opts.path, "-v", 2)) {
        printf("%s:%d:%s: opts.path=%.*s, should be \"-v\"\n", __FILE__, __LINE__, __func__, opts.path_len, opts.path);
        return -1;
    }
    if (1 != opts.verbose) {
        printf("%s:%d:%s: opts.verbose=%d, should be 1\n", __FILE__, __LINE__, __func__, opts.verbose);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_accepts_64_character_paths (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 2,
        /* argv = */ (const char*[]){ 
            (char[]){"./example"},
            (char[]){"0123456789012345678901234567890123456789012345678901234567890123"}
        },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (0 != ret) {
        printf("%s:%d:%s: ret=%d, should be 0\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (0 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 0\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }
    if (64 != opts.path_len) {
        printf("%s:%d:%s: path_len=%d, should be 64\n", __FILE__, __LINE__, __func__, opts.path_len);
        return -1;
    }
    if (0 != memcmp(opts.path, "0123456789012345678901234567890123456789012345678901234567890123", 2)) {
        printf("%s:%d:%s: opts.path=%.*s, should be \"0123456789012345678901234567890123456789012345678901234567890123\"\n", __FILE__, __LINE__, __func__, opts.path_len, opts.path);
        return -1;
    }
    if (0 != opts.verbose) {
        printf("%s:%d:%s: opts.verbose=%d, should be 0\n", __FILE__, __LINE__, __func__, opts.verbose);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int test_driver_it_does_not_accept_65_character_paths (
    void
)
{

    int ret = 0;
    struct example_opts_s opts = {0};
    cb_called = 0;

    ret = example_args_parser_parse(
        /* argc = */ 2,
        /* argv = */ (const char*[]){ 
            (char[]){"./example"},
            (char[]){"01234567890123456789012345678901234567890123456789012345678901234"}
        },
        /* &opts = */ &opts,
        /* no_filename_cb = */ no_filename_cb,
        /* user_data = */ NULL
    );

    if (-1 != ret) {
        printf("%s:%d:%s: ret=%d, should be -1\n", __FILE__, __LINE__, __func__, ret);
        return -1;
    }
    if (0 != cb_called) {
        printf("%s:%d:%s: cb_called=%d, should be 0\n", __FILE__, __LINE__, __func__, cb_called);
        return -1;
    }
    if (64 != opts.path_len) {
        printf("%s:%d:%s: path_len=%d, should be 64\n", __FILE__, __LINE__, __func__, opts.path_len);
        return -1;
    }
    if (0 != memcmp(opts.path, "01234567890123456789012345678901234567890123456789012345678901234", 2)) {
        printf("%s:%d:%s: opts.path=%.*s, should be \"01234567890123456789012345678901234567890123456789012345678901234\"\n", __FILE__, __LINE__, __func__, opts.path_len, opts.path);
        return -1;
    }
    if (0 != opts.verbose) {
        printf("%s:%d:%s: opts.verbose=%d, should be 0\n", __FILE__, __LINE__, __func__, opts.verbose);
        return -1;
    }

    printf("[ok] %s\n", __func__);

    return 0;
}


int main (
    int argc,
    char const* argv[]
)
{
    int ret = 0;

    openlog("test_driver", LOG_CONS | LOG_PID, LOG_USER);

    ret = test_driver_it_does_not_accept_empty_argc();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_does_not_accept_empty_argc returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_does_not_accept_missing_path();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_does_not_accept_missing_path returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_does_not_accept_missing_path_with_verbose();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_does_not_accept_missing_path returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_accepts_basic_path();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_accepts_basic_path returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_accepts_basic_path_and_verbose();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_accepts_basic_path_and_verbose returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_accepts_basic_verbose_and_path();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_accepts_basic_verbose_and_path returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_accepts_verbose_and_escaped_path();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_accepts_verbose_and_escaped_path returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_accepts_64_character_paths();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_accepts_64_character_paths returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    ret = test_driver_it_does_not_accept_65_character_paths();
    if (-1 == ret) {
        printf("%s:%d:%s: test_driver_it_does_not_accept_65_character_paths returned -1\n", __FILE__, __LINE__, __func__);
        return -1;
    }

    printf("[ok] %s\n", __func__);
    return 0;
    (void)argc;
    (void)argv;
}
