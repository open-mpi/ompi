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

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/scoll/scoll.h"
#include "oshmem/mca/scoll/base/base.h"

#include "oshmem/proc/proc.h"
#include "oshmem/proc/proc_group_cache.h"


#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_barrier = pshmem_barrier
#pragma weak shmem_barrier_all = pshmem_barrier_all
#include "oshmem/shmem/c/profile/defines.h"
#endif

void shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int rc = OSHMEM_SUCCESS;
    oshmem_group_t* group = NULL;

    RUNTIME_CHECK_INIT();

#if OSHMEM_SPEC_COMPAT == 1
    /* all outstanding puts must be completed */
    shmem_fence();
#endif

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
            /* Call barrier operation */
            rc = group->g_scoll.scoll_barrier(group, pSync, SCOLL_DEFAULT_ALG);
        }

#if OSHMEM_GROUP_CACHE_ENABLED == 0
        if ( rc == OSHMEM_SUCCESS )
        {
            oshmem_proc_group_destroy(group);
        }
#endif /* OSHMEM_GROUP_CACHE_ENABLED */
    }
    RUNTIME_CHECK_RC(rc);
}

void shmem_barrier_all(void)
{
    int rc = OSHMEM_SUCCESS;

#if OSHMEM_SPEC_COMPAT == 1
    /* all outstanding puts must be completed */
    shmem_fence();
#endif

    if (mca_scoll_sync_array) {
        rc = oshmem_group_all->g_scoll.scoll_barrier(oshmem_group_all,
                                                     mca_scoll_sync_array,
                                                     SCOLL_DEFAULT_ALG);
    }
    RUNTIME_CHECK_RC(rc);
}
