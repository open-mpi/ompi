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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INIT, shmem_init)
SHMEM_GENERATE_WEAK_BINDINGS(START_PES, start_pes)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INIT,
        shmem_init_,
        shmem_init__,
        shmem_init_f,
        (void),
        () )

void shmem_init_f(void)
{
    shmem_init();
}

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
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
