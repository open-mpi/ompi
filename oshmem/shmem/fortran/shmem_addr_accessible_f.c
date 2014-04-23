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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_ADDR_ACCESSIBLE, shmem_addr_accessible)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (MPI_Fint,
        SHMEM_ADDR_ACCESSIBLE,
        shmem_addr_accessible_,
        shmem_addr_accessible__,
        shmem_addr_accessible_f,
        (FORTRAN_POINTER_T addr, MPI_Fint *pe), 
        (addr,pe) )

MPI_Fint shmem_addr_accessible_f(FORTRAN_POINTER_T addr, MPI_Fint *pe)
{
    return OMPI_INT_2_FINT(shmem_addr_accessible(FPTR_2_VOID_PTR(addr), OMPI_FINT_2_INT(*pe)));
}

