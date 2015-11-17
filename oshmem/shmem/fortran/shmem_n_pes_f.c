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

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_N_PES, shmem_n_pes)
SHMEM_GENERATE_WEAK_BINDINGS(NUM_PES, num_pes)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (MPI_Fint,
        SHMEM_N_PES,
        shmem_n_pes_,
        shmem_n_pes__,
        shmem_n_pes_f,
        (void),
        () )

MPI_Fint shmem_n_pes_f(void)
{
    MPI_Fint rc;
    rc = OMPI_INT_2_FINT(shmem_n_pes());
    return rc;
}

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
