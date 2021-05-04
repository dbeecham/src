#include <stdio.h>
#include <term.h>
#include <ncurses.h>
#include <stdlib.h>
#include <string.h>

#define BOLD_CYAN "\033[1;36m"
#define CYAN "\033[0;36m"
#define BOLD_MAGENTA "\033[1;35m"
#define MAGENTA "\033[0;35m"
#define BOLD_BLUE "\033[1;34m"
#define BLUE "\033[0;34m"
#define BOLD_YELLOW "\033[1;33m"
#define YELLOW "\033[0;33m"
#define BOLD_GREEN "\033[1;32m"
#define GREEN "\033[0;32m"
#define BOLD_RED "\033[1;31m"
#define RED "\033[0;31m"
#define BOLD_BLACK "\033[1;30m"
#define BLACK "\033[0;30m"
#define NORMAL "\033[0m"

int main(int argc, char *argv[])
{
    char * term = getenv("TERM");
    if (0 == strncmp(term, "", 1)) {
        fprintf(stderr, "no TERM env var\n");
        exit(1);
    }

    setupterm(term, 0, NULL);
    printf("colors: %i\n", max_colors);

    printf(BOLD_CYAN "cyan" NORMAL " normal\n");

    (void)argc;
    (void)argv;
    return 0;
}
