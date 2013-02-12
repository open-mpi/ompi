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

OMPI_GENERATE_F77_BINDINGS (MPI_Fint,
        NUM_PES,
        num_pes_,
        num_pes__,
        num_pes_f,
        (), 
        () )

MPI_Fint num_pes_f()
{
    MPI_Fint rc;
    rc = OMPI_INT_2_FINT(num_pes());
    return rc;
}

