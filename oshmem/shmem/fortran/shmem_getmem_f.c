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
#include "oshmem/mca/spml/spml.h"
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHMEM_GETMEM, shmem_getmem)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_GETMEM,
        shmem_getmem_,
        shmem_getmem__,
        shmem_getmem_f,
        (FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe), 
        (target,source,len,pe) )

void shmem_getmem_f(FORTRAN_POINTER_T target, FORTRAN_POINTER_T source, MPI_Fint *len, MPI_Fint *pe)
{
    MCA_SPML_CALL(get(FPTR_2_VOID_PTR(source), 
        OMPI_FINT_2_INT(*len), 
        FPTR_2_VOID_PTR(target), 
        OMPI_FINT_2_INT(*pe)));
}

