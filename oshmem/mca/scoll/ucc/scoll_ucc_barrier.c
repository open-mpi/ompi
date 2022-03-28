/**
  Copyright (c) 2021 Mellanox Technologies. All rights reserved.
  $COPYRIGHT$

  Additional copyrights may follow

  $HEADER$
 */
#include "scoll_ucc.h"
#include "scoll_ucc_dtypes.h"
#include "scoll_ucc_common.h"

#include <ucc/api/ucc.h>

static inline ucc_status_t mca_scoll_ucc_barrier_init(mca_scoll_ucc_module_t * ucc_module,
                                                      ucc_coll_req_h * req)
{
    ucc_coll_args_t coll = {
        .mask = 0,
        .coll_type = UCC_COLL_TYPE_BARRIER
    };
    if (NULL == ucc_module->ucc_team) {
        if (OSHMEM_ERROR == mca_scoll_ucc_team_create(ucc_module, ucc_module->group)) {
            return OSHMEM_ERROR;
        }
    }
    SCOLL_UCC_REQ_INIT(req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_scoll_ucc_barrier(struct oshmem_group_t *group, long *pSync, int alg)
{
    mca_scoll_ucc_module_t *ucc_module;
    ucc_coll_req_h req;

    UCC_VERBOSE(3, "running ucc barrier");
    ucc_module = (mca_scoll_ucc_module_t *) group->g_scoll.scoll_barrier_module;

    SCOLL_UCC_CHECK(mca_scoll_ucc_barrier_init(ucc_module, &req));
    SCOLL_UCC_CHECK(ucc_collective_post(req));
    SCOLL_UCC_CHECK(scoll_ucc_req_wait(req));
    return OSHMEM_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback barrier");
    return ucc_module->previous_barrier(group, pSync, alg);
}

