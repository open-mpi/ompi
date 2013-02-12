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
        MY_PE,
        my_pe_,
        my_pe__,
        my_pe_f,
        (), 
        () )

MPI_Fint my_pe_f()
{
    MPI_Fint rc;
    rc = OMPI_INT_2_FINT(my_pe());
    return rc;
}

