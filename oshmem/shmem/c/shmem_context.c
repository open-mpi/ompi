/*
 * Copyright (c) 2013-2018 Mellanox Technologies, Inc.
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

#include "opal/runtime/opal_cr.h"
#include "opal/util/output.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/runtime/params.h"
#include "oshmem/runtime/runtime.h"
#include "oshmem/shmem/shmem_api_logger.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
#pragma weak shmem_ctx_create = pshmem_ctx_create
#pragma weak shmem_ctx_destroy = pshmem_ctx_destroy
#include "oshmem/shmem/c/profile/defines.h"
#endif

int shmem_ctx_create(long options, shmem_ctx_t *ctx)
{
    return MCA_SPML_CALL(ctx_create(options, ctx));
}

void shmem_ctx_destroy(shmem_ctx_t ctx)
{
    MCA_SPML_CALL(ctx_destroy(ctx));
}
