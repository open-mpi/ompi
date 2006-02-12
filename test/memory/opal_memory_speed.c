/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "ompi/constants.h"
#include "opal/runtime/opal.h"
#include "opal/memoryhooks/memory.h"

#define iters 100000
#define mask 0xfff

static void* ptrs[iters];

static void
callback(void *buf, size_t length, void *cbdata, bool extra)
{
}

int
main(int argc, char *argv[])
{
    int i, retval;
    struct timeval start, end;
    long time;

    opal_init();
    if (0 == ((OPAL_MEMORY_FREE_SUPPORT|OPAL_MEMORY_MALLOC_SUPPORT) &
              opal_mem_hooks_support_level())) {
        printf("no memory registration supported.  skipping\n");
        return 77;
    }

    printf("speed without a handler:\n");

    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        /* malloc out a random size */
        ptrs[i] = malloc((rand() & mask) + 1);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  malloc: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);
    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        free(ptrs[i]);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  free: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);

    printf("speed with empty handler:\n");
    retval = opal_mem_hooks_register_release(callback, NULL);
    if (retval != OMPI_SUCCESS) {
        printf("handler registration failed\n");
        return retval;
    }

    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        /* malloc out a random size */
        ptrs[i] = malloc((rand() & mask) + 1);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  malloc: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);
    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        free(ptrs[i]);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  free: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);
    opal_mem_hooks_unregister_release(callback);

    printf("speed without a handler:\n");

    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        /* malloc out a random size */
        ptrs[i] = malloc((rand() & mask) + 1);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  malloc: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);
    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        free(ptrs[i]);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  free: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);

    printf("speed with empty handler:\n");
    retval = opal_mem_hooks_register_release(callback, NULL);
    if (retval != OMPI_SUCCESS) {
        printf("handler registration failed\n");
        return retval;
    }

    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        /* malloc out a random size */
        ptrs[i] = malloc((rand() & mask) + 1);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  malloc: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);
    gettimeofday(&start, NULL);
    for (i = 0 ; i < iters ; ++i) {
        free(ptrs[i]);
    }
    gettimeofday(&end, NULL);
    time = ((end.tv_sec - start.tv_sec) * 1000 * 1000) +
        (end.tv_usec - start.tv_usec);
    printf("  free: %d calls in %ld microseconds.  %lf microseconds/call\n",
           iters, time, (double) time / iters);
    opal_mem_hooks_unregister_release(callback);

    opal_finalize();

    return 0;
}
