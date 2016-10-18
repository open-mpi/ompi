/*
 * Copyright (c) 2014-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * This sample allocates (shmalloc) symmetric memory (1 long integer),
 * and then frees it. Success of allocation is not checked.
 *
 * Produces no output.
 */

#include <shmem.h>

int main(void)
{
    long *x;

    shmem_init();

    x = (long *) shmem_malloc(sizeof(*x));

    shmem_free(x);

    shmem_finalize();
}

