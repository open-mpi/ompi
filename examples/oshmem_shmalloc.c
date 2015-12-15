/*
 * Copyright (c) 2014      Mellanox Technologies, Inc.
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

#warning This application uses deprecated API see http://www.open-mpi.org/

int main(void)
{
    long *x;

    start_pes(0);

    x = (long *) shmalloc(sizeof(*x));

    shfree(x);
}

