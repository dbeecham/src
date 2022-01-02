#include <stdio.h>
#include <syslog.h>

#include "example_args_parser.h"

%%{

    machine args_parser;

    verbose_1 = 
        (
            ('-v' 0) | ('--verbose' 0)
        ) @{
            // got a verbose flag, set it and continue on to next argument
            opts->verbose = true;
            fnext arg_1;
            fbreak;
        };


    arg_1 := (
        verbose_1
    )?
    $err{
        fprintf(stderr, "not a valid argument: %s\n", argv[i]);
        return -1;
    };


    verbose_0 = 
        (
            ('-v' 0) | ('--verbose' 0)
        ) @{
            // got a verbose flag, set it and continue on to next argument
            opts->verbose = true;
            fnext arg_0;
            fbreak;
        };

    action file_path_copy {
        opts->path[opts->path_len++] = fc;
    }

    file_path_0 =
        (any - '-' - 0) $file_path_copy
        (any - 0){0,63} $file_path_copy
        0 @{ fnext arg_1; fbreak; };

    action missing_file_path {
        ret = no_filename_cb(user_data);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: no_filename_cb returned -1", __FILE__, __LINE__, __func__);
            return -1;
        }
    }

    only_file_name := (
        (any - 0){0,64} $file_path_copy
        0 @{ fbreak; }
    ) $err{
        syslog(LOG_ERR, "%s:%d:%s: parse error", __FILE__, __LINE__, __func__);
        return -1;
    };

    no_more_flags_marker_0 = 
        '--'
        0 @{ fnext only_file_name; fbreak; };

    # first positional argument
    arg_0 = 
        ( verbose_0
        | file_path_0
        | no_more_flags_marker_0
        | 0 @missing_file_path
        );

    main := 
        arg_0
        @err{ 
            syslog(LOG_ERR, "%s:%d:%s: failed to parse \"%s\"", __FILE__, __LINE__, __func__, argv[i]);
            return -1; 
        };

    write data;

}%%


int example_args_parser_parse (
    const int argc,
    const char * const * const argv,
    struct example_opts_s * opts,
    int (*no_filename_cb)(void * user_data),
    void * user_data
)
{
    int ret = 0;

    if (argc <= 0) {
        syslog(LOG_ERR, "%s:%d:%s: argc is non-positive", __FILE__, __LINE__, __func__);
        return -1;
    }
    if (1 == argc) {
        ret = no_filename_cb(user_data);
        if (-1 == ret) {
            syslog(LOG_ERR, "%s:%d:%s: no_filename_cb returned -1", __FILE__, __LINE__, __func__);
            return -1;
        }
        return -1;
    }

    int cs;
    
    %% write init;

    // We dont really care about parsing the first string in the arguments (the
    // 'self' string); so we just skip it and set i = 1 directly.
    int i = 1;
    const char * p = argv[i];
    const char * eof = 0;

    while (true) {

        // parse this argument
        p = argv[i];

        %% write exec noend;

        i += 1;

        // are there more arguments to parse?
        if (argc <= i) {
            break;
        }
    }

    if (%%{ write error; }%% == cs) {
        syslog(LOG_ERR, "%s:%d:%s: argument parser error", __FILE__, __LINE__, __func__);
        return -1;
    }
    if (cs < %%{ write first_final; }%%) {
        syslog(LOG_ERR, "%s:%d:%s: did not reach first final (missing file path in arguments?)", __FILE__, __LINE__, __func__);
        return -1;
    }

    return 0;
}
