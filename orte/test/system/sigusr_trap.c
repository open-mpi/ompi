/* -*- C -*-
 *
 * $HEADER$
 *
 * A test to trap user signals
 */

#include <stdio.h>
#include <signal.h>

#include "orte/runtime/runtime.h"

void sigusr_handler(int signum)
{
    switch (signum) {
        case SIGUSR1:
            fprintf(stderr, "Trapped SIGUSR1\n");
            break;

        case SIGUSR2:
            fprintf(stderr, "Trapped SIGUSR2\n");
            return;

        default:
            fprintf(stderr, "Undefined signal %d trapped\n", signum);
            return;
    }
}

void exit_handler(int signum)
{
    int rc;

    exit(0);
}


int main(int argc, char* argv[])
{

    int rc;
    int i;
    double pi;

    if (signal(SIGUSR1, sigusr_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGUSR1\n");
        exit(1);
    }

    if (signal(SIGUSR2, sigusr_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGUSR2\n");
        exit(1);
    }

    if (signal(SIGINT, exit_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGINT\n");
        exit(1);
    }

    if (signal(SIGHUP, exit_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGHUP\n");
        exit(1);
    }

    if (signal(SIGTERM, exit_handler) == SIG_IGN) {
        fprintf(stderr, "Could not setup signal trap for SIGTERM\n");
        exit(1);
    }

    i = 0;
    while (1) {
        i++;
        pi = i / 3.14159256;
        if (i > 100) i = 0;
    }

    return 0;
}
