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

static inline ucc_status_t mca_scoll_ucc_alltoall_init(const void *sbuf, void *rbuf, 
                                                       int count, size_t elem_size,
                                                       mca_scoll_ucc_module_t *ucc_module,
                                                       ucc_coll_req_h *req)
{
    ucc_datatype_t dt;

    if (elem_size == 8) {
        dt = UCC_DT_INT64;
    } else if (elem_size == 4) {
        dt = UCC_DT_INT32;
    } else {
        dt = UCC_DT_INT8;
    }

    ucc_coll_args_t coll = {
        .mask = UCC_COLL_ARGS_FIELD_FLAGS | UCC_COLL_ARGS_FIELD_GLOBAL_WORK_BUFFER,
        .coll_type = UCC_COLL_TYPE_ALLTOALL,
        .src.info = {
            .buffer = (void *)sbuf,
            .count = count * ucc_module->group->proc_count,
            .datatype = dt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info = {
            .buffer = rbuf,
            .count = count * ucc_module->group->proc_count,
            .datatype = dt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .flags = UCC_COLL_ARGS_FLAG_MEM_MAPPED_BUFFERS,
        .global_work_buffer = ucc_module->pSync,
    };

    if (NULL == mca_scoll_ucc_component.ucc_context) {
        if (OSHMEM_ERROR == mca_scoll_ucc_init_ctx(ucc_module->group)) {
            return OSHMEM_ERROR;
        }
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


int mca_scoll_ucc_alltoall(struct oshmem_group_t *group,
                           void *target,
                           const void *source,
                           ptrdiff_t dst, ptrdiff_t sst,
                           size_t nelems,
                           size_t element_size,
                           long *pSync,
                           int alg)
{
    mca_scoll_ucc_module_t *ucc_module;
    size_t count;
    ucc_coll_req_h req;
    int rc;

    UCC_VERBOSE(3, "running ucc alltoall");
    ucc_module = (mca_scoll_ucc_module_t *) group->g_scoll.scoll_alltoall_module;
    count = nelems;

    if ((sst != 1) || (dst != 1)) {
        goto fallback;
    }
    /* Do nothing on zero-length request */
    if (OPAL_UNLIKELY(!nelems)) {
        return OSHMEM_SUCCESS;
    }

    SCOLL_UCC_CHECK(mca_scoll_ucc_alltoall_init(source, target, count, 
                                                element_size, ucc_module, &req));
    SCOLL_UCC_CHECK(ucc_collective_post(req));
    SCOLL_UCC_CHECK(scoll_ucc_req_wait(req));
    return OSHMEM_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback alltoall");
    PREVIOUS_SCOLL_FN(ucc_module, alltoall, group, target, source,
                      dst, sst, nelems, element_size, pSync, alg);
    return rc;
}
