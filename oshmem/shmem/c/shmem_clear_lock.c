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

#include "orte/util/show_help.h"

#include "oshmem/shmem/shmem_api_logger.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/shmem/shmem_lock.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_clear_lock = pshmem_clear_lock
#include "oshmem/shmem/c/profile/defines.h"
#endif

void shmem_clear_lock(long *lock)
{
    _shmem_clear_lock(lock, sizeof(long));
}
