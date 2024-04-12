/*
 * Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#define _GNU_SOURCE
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <errno.h>

#include <pmix.h>

static pmix_proc_t myproc;
static char buffer[1024];

int main(int argc, char **argv)
{
    pmix_status_t rc;
    pid_t pid;
    char hostname[1024];
    int numbytes;
    int n = 0;

    pid = getpid();
    gethostname(hostname, 1024);

    /* init us - note that the call to "init" includes the return of
     * any job-related info provided by the RM. This includes the
     * location of all procs in our job */
    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Init failed: %d\n",
                myproc.nspace, myproc.rank, rc);
        exit(1);
    }
    fprintf(stderr, "[%s:%d:%lu]: Running on node %s\n",
            myproc.nspace, myproc.rank,
            (unsigned long) pid, hostname);

    if (0 == myproc.rank) {
        /* we are going to read stdin and just throw it
         * away, taking a break between chunks */
        while (1) {
            memset(buffer, 0, 1024);
            numbytes = read(STDIN_FILENO, buffer, 1024);
            if (0 > numbytes) {
                if (EAGAIN == errno || EINTR == errno) {
                    /* retry */
                    continue;
                }
                /* unrecoverable error */
                fprintf(stderr, "[%s:%d:%lu]: Unrecoverable read error\n",
                        myproc.nspace, myproc.rank, (unsigned long) pid);
                break;
            } else if (0 == numbytes) {
                fprintf(stderr, "Read complete\n");
                break;
            }
            if (0 == n % 1000) {
                fprintf(stderr, "[%s:%d:%lu]: Read chunk %d with %d bytes\n",
                            myproc.nspace, myproc.rank, (unsigned long) pid, n, numbytes);
            }
            ++n;
        }
    } else {
        /* we are going to sleep for a time based
         * on our rank, and then finalize */
        sleep(2 * myproc.rank);
    }

    rc = PMIx_Finalize(NULL, 0);
    if (PMIX_SUCCESS != rc) {
        fprintf(stderr, "Client ns %s rank %d: PMIx_Finalize failed: %d\n",
                myproc.nspace, myproc.rank, rc);
    }
}
