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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_PTR, shmem_ptr)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (FORTRAN_POINTER_T *,
        SHMEM_PTR,
        shmem_ptr_,
        shmem_ptr__,
        shmem_ptr_f,
        (FORTRAN_POINTER_T target, MPI_Fint *pe), 
        (target,pe) )

FORTRAN_POINTER_T* shmem_ptr_f(FORTRAN_POINTER_T target, MPI_Fint *pe)
{
    return (FORTRAN_POINTER_T *)shmem_ptr(FPTR_2_VOID_PTR(target), OMPI_FINT_2_INT(*pe));
}

