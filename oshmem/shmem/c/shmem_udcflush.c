/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Intel, Inc. All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
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
#include "opal/opal_portable_platform.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_udcflush = pshmem_udcflush
#include "oshmem/shmem/c/profile-defines.h"
#endif

void shmem_udcflush(void)
{
#if defined(PLATFORM_ARCH_X86) || defined(PLATFORM_ARCH_X86_64)
    do {SHMEM_API_VERBOSE(10,"shmem_udcflush is not supported by the current CPU architecture");}while (0);
#else
    /* another implementation */
#endif
}
