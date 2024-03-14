/*
 * Copyright (c) 2023      NVIDIA Corporation.
 *                         All rights reserved.
 * Copyright (c) 2013-2016 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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
#include "oshmem/shmem/shmem_lock.h"
#include "oshmem/runtime/params.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_clear_lock = pshmem_clear_lock
#include "oshmem/shmem/c/profile-defines.h"
#endif

void shmem_clear_lock(volatile long *lock)
{
    if (oshmem_shmem_enable_mcs_locks) {
        SHMEM_API_VERBOSE(10, "Clear Lock with MCS Lock implementation");
        _shmem_mcs_clear_lock((long *)lock);
    } else {
        SHMEM_API_VERBOSE(10, "Clear Lock with Ticket Lock implementation");
        _shmem_clear_lock((void *)lock, sizeof(long));
    }
}
