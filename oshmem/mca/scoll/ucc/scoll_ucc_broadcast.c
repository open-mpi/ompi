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

static inline ucc_status_t mca_scoll_ucc_broadcast_init(void * buf, int count,
                                                        int root, 
                                                        mca_scoll_ucc_module_t * ucc_module,
                                                        ucc_coll_req_h * req)
{
    ucc_coll_args_t coll = {
        .mask = 0,
        .coll_type = UCC_COLL_TYPE_BCAST,
        .root = root,
        .src.info = {
            .buffer = buf,
            .count = count,
            .datatype = UCC_DT_INT8,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        }
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


int mca_scoll_ucc_broadcast(struct oshmem_group_t *group,
                            int PE_root,
                            void *target,
                            const void *source,
                            size_t nlong,
                            long *pSync,
                            bool nlong_type,
                            int alg)
{
    mca_scoll_ucc_module_t * ucc_module;
    void * buf;
    ucc_coll_req_h req;

    UCC_VERBOSE(3, "running ucc bcast");
    ucc_module = (mca_scoll_ucc_module_t *) group->g_scoll.scoll_broadcast_module;
    if (group->my_pe == PE_root) {
        buf = (void *) source;
    } else {
        buf = target;
    }

    /* Do nothing on zero-length request */
    if (OPAL_UNLIKELY(!nlong)) {
        return OSHMEM_SUCCESS;
    }

    SCOLL_UCC_CHECK(mca_scoll_ucc_broadcast_init(buf, nlong, PE_root, ucc_module, &req));
    SCOLL_UCC_CHECK(ucc_collective_post(req));
    SCOLL_UCC_CHECK(scoll_ucc_req_wait(req));
    return OSHMEM_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback bcast");
    return ucc_module->previous_broadcast(group, PE_root, target, source, 
                                          nlong, pSync, nlong_type, alg);
}
