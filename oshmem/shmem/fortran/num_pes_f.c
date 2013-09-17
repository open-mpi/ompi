/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "oshmem_config.h"
#include "oshmem/shmem/fortran/bindings.h"
#include "oshmem/include/shmem.h"

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (MPI_Fint,
        NUM_PES,
        num_pes_,
        num_pes__,
        num_pes_f,
        (void), 
        () )

MPI_Fint num_pes_f(void)
{
    MPI_Fint rc;
    rc = OMPI_INT_2_FINT(num_pes());
    return rc;
}

