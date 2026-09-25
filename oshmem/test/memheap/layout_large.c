/*
 * Copyright (c) 2026      NVIDIA Corporation.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <shmem.h>

static volatile int layout_storage[1024];

int main(void)
{
    int pe;

    shmem_init();
    pe = shmem_my_pe();
    layout_storage[0] = pe;
    shmem_barrier_all();
    shmem_finalize();

    return 0;
}
