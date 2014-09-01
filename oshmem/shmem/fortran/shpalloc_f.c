/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
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
#include "stdio.h"

#if OSHMEM_PROFILING
#include "oshmem/shmem/fortran/profile/pbindings.h"
SHMEM_GENERATE_WEAK_BINDINGS(SHPALLOC, shpalloc)
#include "oshmem/shmem/fortran/profile/defines.h"
#endif

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHPALLOC,
        shpalloc_,
        shpalloc__,
        shpalloc_f,
        (FORTRAN_POINTER_T *addr, MPI_Fint *length, MPI_Fint *errcode, MPI_Fint *abort), 
        (addr,length,errcode,abort) )

void shpalloc_f(FORTRAN_POINTER_T *addr, MPI_Fint *length, MPI_Fint *errcode, MPI_Fint *abort)
{
    *errcode = 0;
    /*current shmem spec implies that the length parameter to SHPALLOC function is the number of 32-bit words to allocate*/
    uint32_t *address;
    if (*length <= 0) {
        *errcode = -1;
    }
    address = shmalloc(*length*4);
    
    *addr = (FORTRAN_POINTER_T)(uintptr_t)address;
    if (!(*addr))
    {
        *errcode = -2;
        SHMEM_API_ERROR("could not allocate %i bytes in symmetric heap",*length*4);
        if (*abort)
        {
            SHMEM_API_ERROR("nonzero abort value, aborting..");
            oshmem_shmem_abort(-1);
        }
    }
}

