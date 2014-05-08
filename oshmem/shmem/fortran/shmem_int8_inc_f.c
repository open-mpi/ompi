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
#include "oshmem/op/op.h"
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INT8_INC, shmem_int8_inc)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_INT8_INC,
        shmem_int8_inc_,
        shmem_int8_inc__,
        shmem_int8_inc_f,
        (FORTRAN_POINTER_T target, MPI_Fint *pe), 
        (target,pe))

void shmem_int8_inc_f(FORTRAN_POINTER_T target, MPI_Fint *pe)
{
    ompi_fortran_integer8_t out_value = 0;
    ompi_fortran_integer8_t value = 1;
    oshmem_op_t* op = oshmem_op_sum_fint8;

    MCA_ATOMIC_CALL(fadd(FPTR_2_VOID_PTR(target), 
        (void *)&out_value, 
        (const void*)&value, 
        sizeof(out_value), 
        OMPI_FINT_2_INT(*pe), 
        op));
}

