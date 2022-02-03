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

static inline ucc_status_t mca_scoll_ucc_reduce_init(const void *sbuf, void *rbuf,
                                                     int count, struct oshmem_op_t * op,
                                                     mca_scoll_ucc_module_t * ucc_module,
                                                     ucc_coll_req_h * req)
{
    ucc_datatype_t ucc_dt;
    ucc_reduction_op_t ucc_op;

    ucc_dt = shmem_op_to_ucc_dtype(op);
    ucc_op = shmem_op_to_ucc_op(op->op);

    if (OPAL_UNLIKELY((ucc_datatype_t) SCOLL_UCC_DT_UNSUPPORTED == ucc_dt)) {
        UCC_VERBOSE(5, "shmem datatype is not supported: dtype # = %d", 
            op->dt);
    }
    if (OPAL_UNLIKELY((ucc_reduction_op_t) SCOLL_UCC_OP_UNSUPPORTED == ucc_op)) {
        UCC_VERBOSE(5, "shmem reduction op is not supported: op # = %d", 
            op->op);
    }

    ucc_coll_args_t coll = {
        .mask = 0,
        .coll_type = UCC_COLL_TYPE_ALLREDUCE,
        .src.info = {
            .buffer = (void *)sbuf,
            .count = count,
            .datatype = ucc_dt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info = {
            .buffer = rbuf,
            .count = count,
            .datatype = ucc_dt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .op = ucc_op,
    };

    if (sbuf == rbuf) {
        coll.mask |= UCC_COLL_ARGS_FIELD_FLAGS;
        coll.flags = UCC_COLL_ARGS_FLAG_IN_PLACE;
    }
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

int mca_scoll_ucc_reduce(struct oshmem_group_t *group,
                         struct oshmem_op_t *op,
                         void *target,
                         const void *source,
                         size_t nlong,
                         long *pSync,
                         void *pWrk,
                         int alg)
{
    mca_scoll_ucc_module_t *ucc_module;
    size_t count;
    ucc_coll_req_h req;

    UCC_VERBOSE(3, "running ucc reduce");
    ucc_module = (mca_scoll_ucc_module_t *) group->g_scoll.scoll_reduce_module;
    count = nlong / op->dt_size;

    /* Do nothing on zero-length request */
    if (OPAL_UNLIKELY(!nlong)) {
        return OSHMEM_SUCCESS;
    }

    SCOLL_UCC_CHECK(mca_scoll_ucc_reduce_init(source, target, count, op, ucc_module, &req));
    SCOLL_UCC_CHECK(ucc_collective_post(req));
    SCOLL_UCC_CHECK(scoll_ucc_req_wait(req));
    return OSHMEM_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback reduction");
    return ucc_module->previous_reduce(group, op, target, source, nlong, pSync,
                                       pWrk, alg);
}
