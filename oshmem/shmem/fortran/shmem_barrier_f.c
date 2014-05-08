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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_BARRIER, shmem_barrier)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_BARRIER,
        shmem_barrier_,
        shmem_barrier__,
        shmem_barrier_f,
        (MPI_Fint *PE_start, MPI_Fint *logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync), 
        (PE_start,logPE_stride,PE_size,pSync))

void shmem_barrier_f(MPI_Fint *PE_start, MPI_Fint *logPE_stride, MPI_Fint *PE_size, FORTRAN_POINTER_T pSync)
{
    shmem_barrier(OMPI_FINT_2_INT(*PE_start), 
         OMPI_FINT_2_INT(*logPE_stride), 
         OMPI_FINT_2_INT(*PE_size), 
         (long *)FPTR_2_VOID_PTR(pSync));
}
 
