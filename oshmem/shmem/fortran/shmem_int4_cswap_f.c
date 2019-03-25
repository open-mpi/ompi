/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
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
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_INT4_CSWAP, shmem_int4_cswap)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_FUNCTION (ompi_fortran_integer4_t,
        SHMEM_INT4_CSWAP,
        shmem_int4_cswap_,
        shmem_int4_cswap__,
        shmem_int4_cswap_f,
        (FORTRAN_POINTER_T target, MPI_Fint *cond, FORTRAN_POINTER_T value, MPI_Fint *pe),
        (target,cond,value,pe) )

ompi_fortran_integer4_t shmem_int4_cswap_f(FORTRAN_POINTER_T target, MPI_Fint *cond, FORTRAN_POINTER_T value, MPI_Fint *pe)
{
    ompi_fortran_integer8_t out_value = 0;

    MCA_ATOMIC_CALL(cswap(oshmem_ctx_default, FPTR_2_VOID_PTR(target),
        (void *)&out_value,
        FPTR_2_INT(cond, sizeof(ompi_fortran_integer4_t)),
        FPTR_2_INT(value, sizeof(ompi_fortran_integer4_t)),
        sizeof(ompi_fortran_integer4_t),
        OMPI_FINT_2_INT(*pe)));

    return out_value;
}

