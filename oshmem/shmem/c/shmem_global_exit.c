/*
 * Copyright (c) 2012-2015 Mellanox Technologies, Inc.
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

#include "oshmem/include/shmem.h"
#include "oshmem/runtime/runtime.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_global_exit = pshmem_global_exit
#include "oshmem/shmem/c/profile-defines.h"
#endif

extern int oshmem_shmem_inglobalexit;

static inline void _globalexit(int status);

void shmem_global_exit(int status)
{
    _globalexit(status);
}

void globalexit(int status)
{
    _globalexit(status);
}

static inline void _globalexit(int status)
{
    oshmem_shmem_inglobalexit++;

    ompi_rte_abort(status, NULL);

    oshmem_shmem_aborted = true;
    exit(status);
}
