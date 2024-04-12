/*
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <errno.h>

#include "debugger.h"
#include <pmix.h>

/* This program reads stdin until it closes, and then exits. It first
 * calls PMIx_Init so it can "hold" for a debugger, thereby allowing
 * the indirect and direct tests in this directory to function.
 */

static pmix_proc_t myproc;
char msg[8192];

int main(int argc, char **argv)
{
    pmix_status_t rc;
    int msgsize;
    pid_t pid;
    char hostname[1024];

    pid = getpid();
    gethostname(hostname, 1024);

    fprintf(stderr, "Proc %d on host %s running\n", (int)pid, hostname);

    rc = PMIx_Init(&myproc, NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "PMIx_Init failed: %s\n", PMIx_Error_string(rc));
        exit(1);
    }
    fprintf(stderr, "Proc %d on host %s RELEASED FROM INIT\n", (int)pid, hostname);

    if (0 == myproc.rank) {
        while (1) {
            msgsize = read(0, msg, 8192);
            if (msgsize < 0) {
                if (EAGAIN == errno || EINTR == errno) {
                    continue;
                }
                break;
            }
            if (0 == msgsize) {
                /* end of input */
                break;
            }
            msg[msgsize] = '\n';
            write(1, msg, msgsize);
        }
    }
    fprintf(stderr, "Proc %d on host %s finalizing\n", (int)pid, hostname);

    PMIx_Finalize(NULL, 0);
    return 0;
}
