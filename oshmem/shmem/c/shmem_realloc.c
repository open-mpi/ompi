/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
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
#include "oshmem/mca/memheap/base/base.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_realloc = pshmem_realloc
#pragma weak shrealloc = pshrealloc
#include "oshmem/shmem/c/profile/defines.h"
#endif

static inline void* _shrealloc(void *ptr, size_t size);

void* shmem_realloc(void *ptr, size_t size)
{
    return _shrealloc(ptr, size);
}

void* shrealloc(void *ptr, size_t size)
{
    return _shrealloc(ptr, size);
}

static inline void* _shrealloc(void *ptr, size_t size)
{
    int rc;
    void* pBuff = NULL;
    map_segment_t *s;

    RUNTIME_CHECK_INIT();

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);

    if (ptr) {
        s = memheap_find_va(ptr);
    } else {
        s = NULL;
    }

    if (s && s->allocator) {
        rc = s->allocator->realloc(s, size, ptr, &pBuff);
    } else {
        rc = MCA_MEMHEAP_CALL(realloc(size, ptr, &pBuff));
    }

    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

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
