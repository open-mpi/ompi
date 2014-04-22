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

static void _shmem_broadcast(void *target,
                              const void *source,
                              size_t nbytes,
                              int PE_root,
                              int PE_start,
                              int logPE_stride,
                              int PE_size,
                              long *pSync);

#define SHMEM_TYPE_BROADCAST(name, element_size)     \
    void shmem##name( void *target,                                 \
                      const void *source,                           \
                      size_t nelems,                                \
                      int PE_root,                                  \
                      int PE_start,                                 \
                      int logPE_stride,                             \
                      int PE_size,                                  \
                      long *pSync)                                  \
{                                                                   \
    RUNTIME_CHECK_INIT();                                           \
    RUNTIME_CHECK_ADDR(target);                                     \
    RUNTIME_CHECK_ADDR(source);                                     \
                                                                    \
    _shmem_broadcast( target, source, nelems * element_size,       \
                       PE_root, PE_start, logPE_stride, PE_size,    \
                       pSync);                                      \
}

static void _shmem_broadcast(void *target,
                              const void *source,
                              size_t nbytes,
                              int PE_root,
                              int PE_start,
                              int logPE_stride,
                              int PE_size,
                              long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    oshmem_group_t* group = NULL;

    if ((0 <= PE_root) && (PE_root < PE_size)) {
        /* Create group basing PE_start, logPE_stride and PE_size */
#if OSHMEM_GROUP_CACHE_ENABLED == 0
        group = oshmem_proc_group_create(PE_start, (1 << logPE_stride), PE_size);
        if (!group || (PE_root >= group->proc_count))
        {
            rc = OSHMEM_ERROR;
        }
#else
        group = find_group_in_cache(PE_start, logPE_stride, PE_size);
        if (!group) {
            group = oshmem_proc_group_create(PE_start,
                                             (1 << logPE_stride),
                                             PE_size);
            if (!group || (PE_root >= group->proc_count)) {
                rc = OSHMEM_ERROR;
            } else {
                cache_group(group, PE_start, logPE_stride, PE_size);
            }
        }
#endif /* OSHMEM_GROUP_CACHE_ENABLED */

        /* Collective operation call */
        if (rc == OSHMEM_SUCCESS) {
            /* Define actual PE using relative in active set */
            PE_root = oshmem_proc_pe(group->proc_array[PE_root]);

            /* Call collective broadcast operation */
            rc = group->g_scoll.scoll_broadcast(group,
                                                PE_root,
                                                target,
                                                source,
                                                nbytes,
                                                pSync,
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
#pragma weak shmem_broadcast = pshmem_broadcast
#pragma weak shmem_broadcast32 = pshmem_broadcast32
#pragma weak shmem_broadcast64 = pshmem_broadcast64
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_BROADCAST(_broadcast32, sizeof(uint32_t))
SHMEM_TYPE_BROADCAST(_broadcast64, sizeof(uint64_t))
SHMEM_TYPE_BROADCAST(_broadcast, sizeof(uint64_t))
