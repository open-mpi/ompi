/*
 * Copyright (c) 2012      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/shmem/f77/bindings.h"
#include "oshmem/include/shmem.h"

OMPI_GENERATE_F77_BINDINGS (void,
        SHMEM_BARRIER_ALL,
        shmem_barrier_all_,
        shmem_barrier_all__,
        shmem_barrier_all_f,
        (), 
        () )

void shmem_barrier_all_f()
{
    shmem_barrier_all();
}

