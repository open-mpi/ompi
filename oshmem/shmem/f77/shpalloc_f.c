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
#include "oshmem/shmem/shmem_api_logger.h"
#include "oshmem/runtime/runtime.h"
#include "stdio.h"

OMPI_GENERATE_F77_BINDINGS (void,
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
    address = shmalloc(*length*4);
    
    *addr = (FORTRAN_POINTER_T)(uintptr_t)address;
    if (!(*addr))
    {
        *errcode = -1;
        SHMEM_API_ERROR("could not allocate %i bytes in symmetric heap",*length*4);
        if (*abort)
        {
            SHMEM_API_ERROR("nonzero abort value, aborting..");
            oshmem_shmem_abort(-1);
        }
    }
}

