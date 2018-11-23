/*
 * Copyright (c) 2016-2018 Mellanox Technologies, Inc.
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

static void _shmem_alltoall(void *target,
                            const void *source,
                            ptrdiff_t dst, ptrdiff_t sst,
                            size_t nelems,
                            size_t element_size,
                            int PE_start,
                            int logPE_stride,
                            int PE_size,
                            long *pSync);

#define SHMEM_TYPE_ALLTOALL(name, element_size)                      \
    void shmem##name(void *target,                                   \
                     const void *source,                             \
                     size_t nelems,                                  \
                     int PE_start,                                   \
                     int logPE_stride,                               \
                     int PE_size,                                    \
                     long *pSync)                                    \
{                                                                    \
    RUNTIME_CHECK_INIT();                                            \
    RUNTIME_CHECK_ADDR_SIZE(target, nelems);                         \
    RUNTIME_CHECK_ADDR_SIZE(source, nelems);                         \
                                                                     \
    _shmem_alltoall(target, source, 1, 1, nelems, element_size,      \
                       PE_start, logPE_stride, PE_size,              \
                       pSync);                                       \
}

#define SHMEM_TYPE_ALLTOALLS(name, element_size)                     \
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
    RUNTIME_CHECK_ADDR_SIZE(target, nelems);                         \
    RUNTIME_CHECK_ADDR_SIZE(source, nelems);                         \
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
    int rc;
    oshmem_group_t* group;

    /* Create group basing PE_start, logPE_stride and PE_size */
    group = oshmem_proc_group_create_nofail(PE_start, 1<<logPE_stride, PE_size);
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
    oshmem_proc_group_destroy(group);
    RUNTIME_CHECK_RC(rc);
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
