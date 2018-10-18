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
#pragma weak shmem_quiet = pshmem_quiet
#pragma weak shmem_ctx_quiet = pshmem_ctx_quiet
#include "oshmem/shmem/c/profile/defines.h"
#endif

void shmem_quiet(void)
{

    MCA_SPML_CALL(quiet(oshmem_ctx_default));
}

void shmem_ctx_quiet(shmem_ctx_t ctx)
{

    MCA_SPML_CALL(quiet(ctx));
}
