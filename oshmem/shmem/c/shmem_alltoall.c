/*
 * Copyright (c) 2016      Mellanox Technologies, Inc.
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

static void _shmem_alltoall(void *target,
                            const void *source,
                            ptrdiff_t dst, ptrdiff_t sst,
                            size_t nelems,
                            size_t element_size,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync);

#define SHMEM_TYPE_ALLTOALL(name, element_size)     \
    void shmem##name(void *target,                                   \
                     const void *source,                             \
                     size_t nelems,                                  \
                     int PE_start,                                   \
                     int logPE_stride,                               \
                     int PE_size,                                    \
                     long *pSync)                                    \
{                                                                    \
    RUNTIME_CHECK_INIT();                                            \
    RUNTIME_CHECK_ADDR(target);                                      \
    RUNTIME_CHECK_ADDR(source);                                      \
                                                                     \
    _shmem_alltoall(target, source, 1, 1, nelems, element_size,      \
                       PE_start, logPE_stride, PE_size,              \
                       pSync);                                       \
}

#define SHMEM_TYPE_ALLTOALLS(name, element_size)     \
    void shmem##name(void *target,                                   \
                     const void *source,                             \
                     ptrdiff_t dst, ptrdiff_t sst,                   \
                     size_t nelems,                                  \
                     int PE_start,                                   \
                     int logPE_stride,                               \
                     int PE_size,                                    \
                     long *pSync)                                    \
{                                                                    \
    RUNTIME_CHECK_INIT();                                            \
    RUNTIME_CHECK_ADDR(target);                                      \
    RUNTIME_CHECK_ADDR(source);                                      \
                                                                     \
    _shmem_alltoall(target, source, dst, sst, nelems, element_size,  \
                       PE_start, logPE_stride, PE_size,              \
                       pSync);                                       \
}

static void _shmem_alltoall(void *target,
                            const void *source,
                            ptrdiff_t dst, ptrdiff_t sst,
                            size_t nelems,
                            size_t element_size,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    oshmem_group_t* group = NULL;

    if ((0 <= PE_start) && (0 <= logPE_stride)) {
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
            /* Call collective alltoall operation */
            rc = group->g_scoll.scoll_alltoall(group,
                                               target,
                                               source,
                                               dst,
                                               sst,
                                               nelems,
                                               element_size,
                                               pSync,
                                               SCOLL_DEFAULT_ALG);
        }
#if OSHMEM_GROUP_CACHE_ENABLED == 0
        if ( rc == OSHMEM_SUCCESS ) {
            oshmem_proc_group_destroy(group);
        }
#endif /* OSHMEM_GROUP_CACHE_ENABLED */
    }
}

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_alltoall32 = pshmem_alltoall32
#pragma weak shmem_alltoall64 = pshmem_alltoall64
#pragma weak shmem_alltoalls32 = pshmem_alltoalls32
#pragma weak shmem_alltoalls64 = pshmem_alltoalls64
#include "oshmem/shmem/c/profile/defines.h"
#endif

SHMEM_TYPE_ALLTOALL(_alltoall32, sizeof(uint32_t))
SHMEM_TYPE_ALLTOALL(_alltoall64, sizeof(uint64_t))
SHMEM_TYPE_ALLTOALLS(_alltoalls32, sizeof(uint32_t))
SHMEM_TYPE_ALLTOALLS(_alltoalls64, sizeof(uint64_t))
