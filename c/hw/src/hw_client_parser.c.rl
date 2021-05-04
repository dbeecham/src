#define _POSIX_C_SOURCE 201805L

#include "hw.h"
#include "hw_client_parser.h"

%%{

    machine client;

    access client->;

    action dosomething {
        syslog(LOG_INFO, "hi there");
    }

    cmd = (
        'hi' @dosomething
    ) $err{ syslog(LOG_WARNING, "%s:%d:%s: failed to parse at %c\n", __FILE__, __LINE__, __func__, *p); fgoto main; };
        

    main := cmd*;

    write data;

}%%

int hw_client_parser_init (
    struct hw_client_s * client
)
{
    %% write init;
}

int hw_client_parser_parse (
    struct hw_client_s * client,
    const char * const buf,
    const int buf_len
)
{
    const char * p = buf;
    const char * pe = buf + buf_len;
    const char * eof = 0;

    %% write exec;

    return 0;

}
