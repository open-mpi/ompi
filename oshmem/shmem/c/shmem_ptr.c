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

#include <stdlib.h>

#include "orte/util/show_help.h"

#include "oshmem/constants.h" 
#include "oshmem/include/shmem.h"
#include "oshmem/shmem/shmem_api_logger.h"

#include "oshmem/runtime/runtime.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ptr = pshmem_ptr
#include "oshmem/shmem/c/profile/defines.h"
#endif

void *shmem_ptr(void *ptr, int pe)
{
    SHMEM_API_VERBOSE(10,
                      "*************** WARNING!!! NOT SUPPORTED FUNCTION **********************\n"
                      "shmem_ptr() function is available only on systems where ordinary memory loads\n"
                      "and stores are used to implement OpenSHMEM put and get operations.");
    return 0;
}
