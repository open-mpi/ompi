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
        START_PES,
        start_pes_,
        start_pes__,
        start_pes_f,
        (MPI_Fint npes), 
        (npes) )

void start_pes_f(MPI_Fint npes)
{
    shmem_init();
}

