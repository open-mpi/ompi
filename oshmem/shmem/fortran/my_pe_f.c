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
        MY_PE,
        my_pe_,
        my_pe__,
        my_pe_f,
        (void), 
        () )

MPI_Fint my_pe_f(void)
{
    MPI_Fint rc;
    rc = OMPI_INT_2_FINT(my_pe());
    return rc;
}

