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

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"

#include "oshmem/shmem/shmem_api_logger.h"

#include "oshmem/runtime/runtime.h"
#include "oshmem/mca/memheap/memheap.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmemalign = pshmemalign
#include "oshmem/shmem/c/profile/defines.h"
#endif

void* shmemalign(size_t align, size_t size)
{
    int rc;
    void* pBuff = NULL;

    RUNTIME_CHECK_INIT();

    rc = MCA_MEMHEAP_CALL(memalign(align, size, &pBuff));

    if (OSHMEM_SUCCESS != rc) {
        SHMEM_API_VERBOSE(1,
                          "Allocation with shmemalign(align=%lu, size=%lu) failed.",
                          (unsigned long)align, (unsigned long)size);
        return NULL ;
    }

#if OSHMEM_SPEC_COMPAT == 1
    shmem_barrier_all();
#endif
    return pBuff;
}

