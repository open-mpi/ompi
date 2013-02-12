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

OMPI_GENERATE_F77_BINDINGS (void,
        SHMEM_BARRIER,
        shmem_barrier_,
        shmem_barrier__,
        shmem_barrier_f,
        (MPI_Fint *PE_start, MPI_Fint *logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (PE_start,logPE_stride,PE_size,pSync))

void shmem_barrier_f(MPI_Fint *PE_start, MPI_Fint *logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync)
{
    //TODO: what if sizeof(long) != sizeof(Fortran integer)
    shmem_barrier(OMPI_FINT_2_INT(*PE_start), 
         OMPI_FINT_2_INT(*logPE_stride), 
         OMPI_FINT_2_INT(*PE_size), 
         (long *)FPTR_2_VOID_PTR(pSync));
}
 
