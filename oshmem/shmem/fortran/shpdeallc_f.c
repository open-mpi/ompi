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
#include "oshmem/shmem/shmem_api_logger.h"
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHPDEALLC, shpdeallc)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHPDEALLC,
        shpdeallc_,
        shpdeallc__,
        shpdeallc_f,
        (FORTRAN_POINTER_T *addr, MPI_Fint *errcode, MPI_Fint *abort), 
        (addr,errcode,abort) )


void shpdeallc_f(FORTRAN_POINTER_T *addr, MPI_Fint *errcode, MPI_Fint *abort)
{
    *errcode = 0;
    shfree((void *)((uintptr_t)*addr));
}

