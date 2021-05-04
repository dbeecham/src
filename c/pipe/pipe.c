// Show an example of piping stdout of an application into stdin of another
// application, like a shell pipe ("ls | grep").
// Based on chapter 44 of The Linux Programming Interface by Michael Kerrisk.

#include <sys/types.h> // waitpid
#include <sys/wait.h> // waitpid
#include <unistd.h> // pipe, fork
#include <stdlib.h> // exit
#include <err.h> // err

int main(int argc, char *argv[])
{

    // A place to store the two file descriptors of the pipe. pfd[0] will
    // contain the read-end of the pipe, and pfd[1] will contain the write-end
    // of the pipe.
    int pfd[2];

    // A place to store the PIDs of the child processes.
    pid_t pid0;
    pid_t pid1;

    // create the pipe, store it in pfd.
    if (-1 == pipe(pfd)) {
        err(1, "pipe");
    }

    // fork. all file descriptors will be duplicated in the child process,
    // including both file descriptors of the pipe.
    switch (pid0 = fork()) {
        case -1:
            err(1, "fork");

        case 0:
            // Child process. Child writes to pipe; it does not read. Close the
            // reading end of the pipe.
            if (-1 == close(pfd[0])) {
                err(1, "close pfd[0]");
            }

            // Now we want to set the writing end of the pipe as stdout - but
            // we don't want to do it if the writing end of the pipe is already
            // stdout, since that would make dup2() a no-op, and we would
            // subsequently close the writing end of the pipe - so make sure
            // that pfd[1] is not already STDOUT_FILENO.
            if (STDOUT_FILENO != pfd[1]) {

                // close STDOUT_FILENO and move pfd[1] to STDOUT_FILENO.
                if (-1 == dup2(pfd[1], STDOUT_FILENO)) {
                    err(1, "dup2");
                }

                // dup2 duplicates the file descriptor pfd[1]; so now we have
                // two open file descriptors. We don't need pfd[1] any more,
                // so close it.
                if (-1 == close(pfd[1])) {
                    err(1, "close pfd[1]");
                }
            }

            // At this point, STDOUT_FILENO will refer to the write end of the
            // pipe. execlp will use STDOUT_FILENO.
            execlp("ls", "ls", (char*)NULL);
            
            // if we reach this point, then execlp failed :(
            err(1, "execlp ls");

        default:
            // parent falls through to create the next child.
            break;
    }

    // fork-exec again, this time to replace STDIN of a process with the
    // reading-end of the pipe pfd[0].
    switch (pid1 = fork()) {
        case -1:
            err(1, "fork");

        case 0:
            // The pipe-reading child process. This process does not use the
            // write-end of the process, so we close it.
            if (-1 == close(pfd[1])) {
                err(1, "close pdf[1]");
            }

            if (pfd[0] != STDIN_FILENO) {
                if (-1 == dup2(pfd[0], STDIN_FILENO)) {
                    err(1, "dup2(pfd[0], STDIN_FILENO)");
                }
                if (-1 == close(pfd[0])) {
                    err(1, "close(pfd[0])");
                }
            }

            // At this point, STDIN_FILENO will point to the read-end of the
            // pipe. execlp will use that.
            execlp("wc", "wc", "-l", (char*)NULL);

            // if we reach this point, then execlp failed. :(
            err(1, "execlp wc");

        // parent falls through
        default:
            break;
    }


    // Parent is no longer interested in the pipe file descriptors; let's close
    // them.
    if (-1 == close(pfd[0])) {
        err(1, "close(pfd[0])");
    }
    if (-1 == close(pfd[1])) {
        err(1, "close(pfd[1])");
    }

    // Wait for the children to exit.
    if (-1 == waitpid(pid0, NULL, 0)) {
        err(1, "waitpid");
    }
    if (-1 == waitpid(pid1, NULL, 0)) {
        err(1, "waitpid");
    }

    // We are DONE here.
    exit(EXIT_SUCCESS);
}
