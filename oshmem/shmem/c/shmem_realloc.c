/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#include "oshmem_config.h"
#include <stdio.h>
#include <stdlib.h>

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/shmem/shmem_api_logger.h"
#include "oshmem/mca/memheap/memheap.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shrealloc = pshrealloc
#include "oshmem/shmem/c/profile/defines.h"
#endif

void* shrealloc(void *ptr, size_t size)
{
    int rc;
    void* pBuff = NULL;

    RUNTIME_CHECK_INIT();

    rc = MCA_MEMHEAP_CALL(realloc(size, ptr, &pBuff));

    if (OSHMEM_SUCCESS != rc) {
        SHMEM_API_VERBOSE(1,
                          "Allocation with shrealloc(ptr=%p, size=%lu) failed.",
                          ptr, (unsigned long)size);
        return NULL ;
    }

#if OSHMEM_SPEC_COMPAT == 1
    shmem_barrier_all();
#endif

    return pBuff;
}
