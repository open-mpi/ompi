/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC.
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

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_finalize = pshmem_finalize
#include "oshmem/shmem/c/profile/defines.h"
#endif

extern int oshmem_shmem_globalexit_status;

void shmem_finalize(void)
{
    if (oshmem_shmem_globalexit_status != 0)
    {
        return;
    }
    oshmem_shmem_finalize();
}

