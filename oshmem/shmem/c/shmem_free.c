/*
 * Copyright (c) 2013-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2018      IBM Corporation.  All rights reserved.
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
#include "oshmem/mca/memheap/base/base.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_free = pshmem_free
#pragma weak shfree = pshfree
#include "oshmem/shmem/c/profile/defines.h"
#endif

static inline void _shfree(void* ptr);

void shmem_free(void* ptr)
{
    _shfree(ptr);
}

void shfree(void* ptr)
{
    _shfree(ptr);
}

static inline void _shfree(void* ptr)
{
    int rc;
    map_segment_t *s;

    RUNTIME_CHECK_INIT();
    if (NULL == ptr) {
        return;
    }

    RUNTIME_CHECK_ADDR(ptr);

#if OSHMEM_SPEC_COMPAT == 1
    shmem_barrier_all();
#endif

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);

    if (ptr) {
        s = memheap_find_va(ptr);
    }

    if (s && s->allocator) {
        rc = s->allocator->free(s, ptr);
    } else {
        rc = MCA_MEMHEAP_CALL(free(ptr));
    }

    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    if (OSHMEM_SUCCESS != rc) {
        SHMEM_API_VERBOSE(10, "shfree failure.");
    }
}

