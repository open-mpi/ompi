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
#include "stdio.h"
OMPI_GENERATE_F77_BINDINGS (void,
        SHPDEALLC,
        shpdeallc_,
        shpdeallc__,
        shpdeallc_f,
        (FORTRAN_POINTER_T *addr, MPI_Fint *errcode, MPI_Fint *abort), 
        (addr,errcode,abort) )


void shpdeallc_f(FORTRAN_POINTER_T *addr, MPI_Fint *errcode, MPI_Fint *abort)
{
    *errcode = 0;
    shfree((void *)*addr);
}

