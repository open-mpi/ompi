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

#include "oshmem/mca/spml/spml.h"

#include "oshmem/shmem/shmem_api_logger.h"
#include "opal/sys/architecture.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_clear_cache_line_inv = pshmem_clear_cache_line_inv
#include "oshmem/shmem/c/profile/defines.h"
#endif

void shmem_clear_cache_line_inv(void *target)
{
#if (OPAL_ASSEMBLY_ARCH == OMPI_IA64) || (OPAL_ASSEMBLY_ARCH == OMPI_IA32) || (OPAL_ASSEMBLY_ARCH == OMPI_AMD64)
    do {SHMEM_API_VERBOSE(10,"shmem_clear_cache_line_inv is not supported by the current CPU architecture");}while (0);
#else
    /* another implementation */
#endif
}
