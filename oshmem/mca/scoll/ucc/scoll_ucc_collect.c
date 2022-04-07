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

static inline ucc_status_t mca_scoll_ucc_collect_init(const void * sbuf, void * rbuf,
                                                      int count, 
                                                      mca_scoll_ucc_module_t * ucc_module,
                                                      ucc_coll_req_h * req)
{
    ucc_coll_args_t coll = {
        .mask = 0,
        .coll_type = UCC_COLL_TYPE_ALLGATHER,
        .src.info = {
            .buffer = (void *) sbuf,
            .count = count,
            .datatype = UCC_DT_INT8,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        }, 
        .dst.info = {
            .buffer = rbuf,
            .count = count,
            .datatype = UCC_DT_INT8,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
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

int mca_scoll_ucc_collect(struct oshmem_group_t *group,
                          void *target,
                          const void *source,
                          size_t nlong,
                          long *pSync,
                          bool nlong_type,
                          int alg)
{
    mca_scoll_ucc_module_t *ucc_module;
    ucc_coll_req_h req;

    UCC_VERBOSE(3, "running ucc collect");
    ucc_module = (mca_scoll_ucc_module_t *) group->g_scoll.scoll_collect_module;

    if (OPAL_UNLIKELY(!nlong)) {
        return OSHMEM_SUCCESS;
    }

    SCOLL_UCC_CHECK(mca_scoll_ucc_collect_init(source, target, nlong, ucc_module, &req));
    SCOLL_UCC_CHECK(ucc_collective_post(req));
    SCOLL_UCC_CHECK(scoll_ucc_req_wait(req));
    return OSHMEM_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback collect");
    return ucc_module->previous_collect(group, target, source, nlong,
                                        pSync, nlong_type, alg);
}
