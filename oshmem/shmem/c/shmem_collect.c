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

#include "orte/mca/grpcomm/grpcomm.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/scoll/scoll.h"

#include "oshmem/proc/proc.h"
#include "oshmem/proc/proc_group_cache.h"

static void _shmem_collect(void *target,
                            const void *source,
                            size_t nbytes,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync,
                            bool nlong_type);

#define SHMEM_TYPE_COLLECT(name, element_size, nelems_type)     \
    void shmem##name( void *target,                             \
                      const void *source,                       \
                      size_t nelems,                            \
                      int PE_start,                             \
                      int logPE_stride,                         \
                      int PE_size,                              \
                      long *pSync)                              \
{                                                               \
    RUNTIME_CHECK_INIT();                                       \
    RUNTIME_CHECK_ADDR(target);                                 \
    RUNTIME_CHECK_ADDR(source);                                 \
                                                                \
    _shmem_collect( target, source, nelems * element_size,     \
                     PE_start, logPE_stride, PE_size,           \
                     pSync,                                     \
                     nelems_type);                              \
}

static void _shmem_collect(void *target,
                            const void *source,
                            size_t nbytes,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync,
                            bool array_type)
{
    int rc = OSHMEM_SUCCESS;
    oshmem_group_t* group = NULL;

    {
        /* Create group basing PE_start, logPE_stride and PE_size */
#if OSHMEM_GROUP_CACHE_ENABLED == 0
        group = oshmem_proc_group_create(PE_start, (1 << logPE_stride), PE_size);
        if (!group)
        rc = OSHMEM_ERROR;
#else
        group = find_group_in_cache(PE_start, logPE_stride, PE_size);
        if (!group) {
            group = oshmem_proc_group_create(PE_start,
                                             (1 << logPE_stride),
                                             PE_size);
            if (!group) {
                rc = OSHMEM_ERROR;
            } else {
                cache_group(group, PE_start, logPE_stride, PE_size);
            }
        }
#endif /* OSHMEM_GROUP_CACHE_ENABLED */

        /* Collective operation call */
        if (rc == OSHMEM_SUCCESS) {
            /* Call collective broadcast operation */
            rc = group->g_scoll.scoll_collect(group,
                                              target,
                                              source,
                                              nbytes,
                                              pSync,
                                              array_type,
                                              SCOLL_DEFAULT_ALG);
        }
#if OSHMEM_GROUP_CACHE_ENABLED == 0
        if ( rc == OSHMEM_SUCCESS )
        {
            oshmem_proc_group_destroy(group);
        }
#endif /* OSHMEM_GROUP_CACHE_ENABLED */
    }
}

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_collect32 = pshmem_collect32
#pragma weak shmem_collect64 = pshmem_collect64
#pragma weak shmem_fcollect32 = pshmem_fcollect32
#pragma weak shmem_fcollect64 = pshmem_fcollect64
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_COLLECT(_collect32, sizeof(uint32_t), false)
SHMEM_TYPE_COLLECT(_collect64, sizeof(uint64_t), false)
SHMEM_TYPE_COLLECT(_fcollect32, sizeof(uint32_t), true)
SHMEM_TYPE_COLLECT(_fcollect64, sizeof(uint64_t), true)
