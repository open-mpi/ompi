/*
 * Copyright (c) 2018      Mellanox Technologies, Inc.
 *                         All rights reserved.
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
#include "oshmem/op/op.h"
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INT8_FXOR, shmem_int8_fxor)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (ompi_fortran_integer8_t,
        SHMEM_INT8_FXOR,
        shmem_int8_fxor_,
        shmem_int8_fxor__,
        shmem_int8_fxor_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe),
        (target,value,pe) )

ompi_fortran_integer8_t shmem_int8_fxor_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe)
{
    ompi_fortran_integer8_t out_value = 0;

    MCA_ATOMIC_CALL(fxor(FPTR_2_VOID_PTR(target),
        (void *)&out_value,
        FPTR_2_INT(value, sizeof(out_value)),
        sizeof(out_value),
        OMPI_FINT_2_INT(*pe)));

    return out_value;
}

