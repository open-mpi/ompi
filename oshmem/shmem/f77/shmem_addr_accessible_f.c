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
#include "oshmem/shmem/shmem_api_logger.h"
#include "stdio.h"

OMPI_GENERATE_F77_BINDINGS (MPI_Fint,
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

