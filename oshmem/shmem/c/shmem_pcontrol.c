/*
 * Copyright (c) 2021      NVIDIA Corporation.
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

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/scoll/scoll.h"

#include "oshmem/proc/proc.h"


#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"

#pragma weak shmem_pcontrol    = pshmem_pcontrol
#include "oshmem/shmem/c/profile-defines.h"
#endif




void shmem_pcontrol(int level, ...)
{
    return ;
}

