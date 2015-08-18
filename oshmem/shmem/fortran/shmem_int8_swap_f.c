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
#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/atomic/atomic.h"
#include "ompi/datatype/ompi_datatype.h"
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INT8_SWAP, shmem_int8_swap)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (ompi_fortran_integer8_t,
        SHMEM_INT8_SWAP,
        shmem_int8_swap_,
        shmem_int8_swap__,
        shmem_int8_swap_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe),
        (target,value,pe) )

ompi_fortran_integer8_t shmem_int8_swap_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe)
{
    ompi_fortran_integer8_t out_value = 0;

    MCA_ATOMIC_CALL(cswap(FPTR_2_VOID_PTR(target),
        (void *)&out_value,
        NULL,
        FPTR_2_VOID_PTR(value),
        sizeof(out_value),
        OMPI_FINT_2_INT(*pe)));

    return out_value;
}

