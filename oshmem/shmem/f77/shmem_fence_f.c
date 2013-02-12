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
#include "oshmem/mca/spml/spml.h"

OMPI_GENERATE_F77_BINDINGS (void,
        SHMEM_FENCE,
        shmem_fence_,
        shmem_fence__,
        shmem_fence_f,
        (), 
        () )

void shmem_fence_f()
{
    MCA_SPML_CALL(fence());
}
