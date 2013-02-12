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
        SHMEM_QUIET,
        shmem_quiet_,
        shmem_quiet__,
        shmem_quiet_f,
        (), 
        () )

void shmem_quiet_f()
{
    MCA_SPML_CALL(fence());
}
