/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
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
#include "opal/sys/architecture.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_udcflush_line = pshmem_udcflush_line
#include "oshmem/shmem/c/profile/defines.h"
#endif

void shmem_udcflush_line(void *target)
{
#if (OPAL_ASSEMBLY_ARCH == OPAL_IA64) || (OPAL_ASSEMBLY_ARCH == OPAL_IA32) || (OPAL_ASSEMBLY_ARCH == OPAL_AMD64)
    do {SHMEM_API_VERBOSE(10,"shmem_udcflush_line is not supported by the current CPU architecture");}while (0);
#else
    /* another implementation */
#endif
}
