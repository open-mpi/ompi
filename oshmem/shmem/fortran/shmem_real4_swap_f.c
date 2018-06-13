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
#include "oshmem/op/op.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_REAL4_SWAP, shmem_real4_swap)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (ompi_fortran_real4_t,
        SHMEM_REAL4_SWAP,
        shmem_real4_swap_,
        shmem_real4_swap__,
        shmem_real4_swap_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe),
        (target,value,pe) )

ompi_fortran_real4_t shmem_real4_swap_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T value, MPI_Fint *pe)
{
    ompi_fortran_real4_t out_value = 0;
    oshmem_op_t* op = oshmem_op_swap_freal4;

    MCA_ATOMIC_CALL(swap(FPTR_2_VOID_PTR(target),
        (void *)&out_value,
        FPTR_2_VOID_PTR(value),
        sizeof(out_value),
        OMPI_FINT_2_INT(*pe),
        op));

    return out_value;
}

