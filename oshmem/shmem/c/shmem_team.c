/*
 * Copyright (c) 2021      NVIDIA Corporation.
 *                         All rights reserved.
 * Copyright (c) 2019      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "oshmem_config.h"

#include "oshmem/constants.h"
#include "oshmem/include/shmem.h"
#include "oshmem/include/shmemx.h"

#include "oshmem/runtime/runtime.h"

#include "oshmem/mca/spml/spml.h"

#if OSHMEM_PROFILING
#include "oshmem/include/pshmem.h"
/*
 * Team management routines
 */
#pragma weak shmem_team_sync             		= pshmem_team_sync
#pragma weak shmem_team_my_pe             		= pshmem_team_my_pe
#pragma weak shmem_team_n_pes             		= pshmem_team_n_pes
#pragma weak shmem_team_get_config        		= pshmem_team_get_config
#pragma weak shmem_team_translate_pe      		= pshmem_team_translate_pe
#pragma weak shmem_team_split_strided     		= pshmem_team_split_strided
#pragma weak shmem_team_split_2d          		= pshmem_team_split_2d
#pragma weak shmem_team_destroy           		= pshmem_team_destroy
#pragma weak shmem_ctx_get_team           		= pshmem_ctx_get_team
#pragma weak shmem_team_create_ctx                      = pshmem_team_create_ctx

#include "oshmem/shmem/c/profile-defines.h"
#endif

void shmem_team_sync(shmem_team_t team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_sync(team));
    RUNTIME_CHECK_IMPL_RC(rc);

    return ;
}

int shmem_team_my_pe(shmem_team_t team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_my_pe(team));
    RUNTIME_CHECK_IMPL_RC(rc);

    return rc;
}

int shmem_team_n_pes(shmem_team_t team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_n_pes(team));
    RUNTIME_CHECK_IMPL_RC(rc);

    return rc;
}
int shmem_team_get_config(shmem_team_t team, long config_mask, shmem_team_config_t *config)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_get_config(team, config_mask, config));
    RUNTIME_CHECK_RC(rc);

    return rc;
}
int shmem_team_translate_pe(shmem_team_t src_team, int src_pe, shmem_team_t dest_team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_translate_pe(src_team, src_pe, dest_team));
    RUNTIME_CHECK_IMPL_RC(rc);

    return rc;
}
int shmem_team_split_strided (shmem_team_t parent_team, int start, int stride,
        int size, const shmem_team_config_t *config, long config_mask,
        shmem_team_t *new_team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_split_strided(parent_team, start, stride, size,
                config, config_mask, new_team));
    RUNTIME_CHECK_RC(rc);

    return rc;
}

int shmem_team_split_2d (shmem_team_t parent_team, int xrange, const
        shmem_team_config_t *xaxis_config, long xaxis_mask, shmem_team_t
        *xaxis_team, const shmem_team_config_t *yaxis_config, long yaxis_mask,
        shmem_team_t *yaxis_team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_split_2d(parent_team, xrange, xaxis_config,
                xaxis_mask, xaxis_team, yaxis_config, yaxis_mask, yaxis_team));
    RUNTIME_CHECK_RC(rc);

    return rc;
}

void shmem_team_destroy(shmem_team_t team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_destroy(team));
    RUNTIME_CHECK_RC(rc);

    return ;
}

int shmem_ctx_get_team(shmem_ctx_t ctx, shmem_team_t *team)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_get(ctx, team));
    RUNTIME_CHECK_RC(rc);

    return rc;
}

int shmem_team_create_ctx(shmem_team_t team, long options, shmem_ctx_t *ctx)
{
    int rc = 0;

    RUNTIME_CHECK_INIT();

    rc = MCA_SPML_CALL(team_create_ctx(team, options, ctx));
    RUNTIME_CHECK_RC(rc);

    return rc;
}
