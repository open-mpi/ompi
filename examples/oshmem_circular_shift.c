/*
 * Copyright (c) 2014-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <stdio.h>
#include <shmem.h>

int main (void)
{
    static int aaa, bbb;
    int num_pes, my_pe, peer;

    shmem_init();

    num_pes = shmem_n_pes();
    my_pe = shmem_my_pe();

    peer = (my_pe + 1) % num_pes;

    printf("Process %d gets message from %d (%d processes in ring)\n", my_pe, peer, num_pes);
    shmem_int_get(&aaa, &bbb, 1, peer);

    shmem_barrier_all();
    printf("Process %d exiting\n", my_pe);
    shmem_finalize();

    return 0;
}

