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
#include "oshmem/mca/spml/spml.h"

SHMEM_GENERATE_FORTRAN_BINDINGS_SUB (void,
        SHMEM_FENCE,
        shmem_fence_,
        shmem_fence__,
        shmem_fence_f,
        (void), 
        () )

void shmem_fence_f(void)
{
    MCA_SPML_CALL(fence());
}
