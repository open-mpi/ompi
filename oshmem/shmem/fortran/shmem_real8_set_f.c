/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
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
#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/atomic/atomic.h"
#include "ompi/datatype/ompi_datatype.h"
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_REAL8_SET, shmem_real8_set)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_REAL8_SET,
        shmem_real8_set_,
        shmem_real8_set__,
        shmem_real8_set_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe),
        (target,value,pe) )

void shmem_real8_set_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe)
{
    ompi_fortran_real8_t out_value = 0;

    MCA_ATOMIC_CALL(cswap(FPTR_2_VOID_PTR(target),
        (void *)&out_value,
        NULL,
        FPTR_2_VOID_PTR(value),
        sizeof(out_value),
        OMPI_FINT_2_INT(*pe)));
}

