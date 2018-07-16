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

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_fence = pshmem_fence
#pragma weak shmem_ctx_fence = pshmem_ctx_fence
#include "oshmem/shmem/c/profile/defines.h"
#endif

void shmem_fence(void)
{

    MCA_SPML_CALL(fence(oshmem_ctx_default));
}

void shmem_ctx_fence(shmem_ctx_t ctx)
{

    MCA_SPML_CALL(fence(ctx));
}
