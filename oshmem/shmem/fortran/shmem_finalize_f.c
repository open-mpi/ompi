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

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_FINALIZE,
        shmem_finalize_,
        shmem_finalize__,
        shmem_finalize_f,
        (void), 
        () )

void shmem_finalize_f(void)
{
    shmem_finalize();
}

