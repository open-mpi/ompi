/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
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


#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_sync = pshmem_sync
#pragma weak shmem_sync_all = pshmem_sync_all
#include "oshmem/shmem/c/profile/defines.h"
#endif

void shmem_sync(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int rc;
    oshmem_group_t* group;

    RUNTIME_CHECK_INIT();

#if OSHMEM_SPEC_COMPAT == 1
    /* all outstanding puts must be completed */
    shmem_quiet();
#endif

    /* Create group basing PE_start, logPE_stride and PE_size */
    group = oshmem_proc_group_create_nofail(PE_start, 1<<logPE_stride, PE_size);
    /* Call barrier operation */
    rc = group->g_scoll.scoll_barrier(group, pSync, SCOLL_DEFAULT_ALG);

    oshmem_proc_group_destroy(group);
    RUNTIME_CHECK_RC(rc);
}

void shmem_sync_all(void)
{
    int rc = OSHMEM_SUCCESS;

#if OSHMEM_SPEC_COMPAT == 1
    /* all outstanding puts must be completed */
    shmem_quiet();
#endif

    if (mca_scoll_sync_array) {
        rc = oshmem_group_all->g_scoll.scoll_barrier(oshmem_group_all,
                                                     mca_scoll_sync_array,
                                                     SCOLL_DEFAULT_ALG);
    }
    RUNTIME_CHECK_RC(rc);
}
