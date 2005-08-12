/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>

#include "ompi/include/constants.h"
#include "opal/runtime/opal.h"
#include "opal/memory/memory.h"

#define iters 10000

static void* ptrs[iters];

int
main(int argc, char *argv[])
{
    int i;
    struct timeval start, end;
    long time;

    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        /* malloc out a random size from 1 to 256 bytes */
        ptrs[i] = malloc((rand() & 0xff) + 1);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("malloc: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);
    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        free(ptrs[i]);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("free: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);

    return 0;
}
