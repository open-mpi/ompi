/* -*- C -*-
 *
 * Copyright (c) 2008 Los Alamos National Security, LLC.  All rights reserved.
 *
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2018      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include <mpi.h>

int main(int argc, char *argv[])
{
    int nppn;
    struct timeval tv;
    char *cmd;

    /* check for proper usage */
    if (2 < argc) {
        printf("usage: ziatest <#procs/node>\n");
        exit(1);
    }

    nppn = strtol(argv[1], NULL, 10);

    /* THIS BEGINS THE OFFICIAL TIMING POINT */

    /* get a starting time stamp */
    gettimeofday(&tv, NULL);

    /* form the command */
    opal_asprintf(&cmd, "mpirun -npernode %d ./ziaprobe %ld %d", nppn, (long) tv.tv_sec,
                  tv.tv_usec);

    /* execute it */
    system(cmd);

    /* done */
    free(cmd);
    return 0;
}
