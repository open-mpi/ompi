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
        SHMEM_FINALIZE,
        shmem_finalize_,
        shmem_finalize__,
        shmem_finalize_f,
        (), 
        () )

void shmem_finalize_f()
{
    shmem_finalize();
}

