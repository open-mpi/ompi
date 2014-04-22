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

#include "oshmem/proc/proc.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak _num_pes = p_num_pes
#pragma weak _my_pe = p_my_pe
#include "oshmem/shmem/c/profile/defines.h"
#endif

int _num_pes(void)
{
    RUNTIME_CHECK_INIT();
    return oshmem_num_procs();
}

int _my_pe(void)
{
    RUNTIME_CHECK_INIT();
    return oshmem_my_proc_id();
}

int shmem_n_pes(void)
{
    RUNTIME_CHECK_INIT();
    return oshmem_num_procs();
}

int num_pes(void)
{
    RUNTIME_CHECK_INIT();
    return oshmem_num_procs();
}

int shmem_my_pe(void)
{
    RUNTIME_CHECK_INIT();
    return oshmem_my_proc_id();
}

int my_pe(void)
{
    RUNTIME_CHECK_INIT();
    return oshmem_my_proc_id();
}
