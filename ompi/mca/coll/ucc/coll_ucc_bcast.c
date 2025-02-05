/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 */

#include "coll_ucc_common.h"

static inline ucc_status_t mca_coll_ucc_bcast_init(void *buf, size_t count, struct ompi_datatype_t *dtype,
                                                   int root, mca_coll_ucc_module_t *ucc_module,
                                                   ucc_coll_req_h *req,
                                                   mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t         ucc_dt     = ompi_dtype_to_ucc_dtype(dtype);
    if (COLL_UCC_DT_UNSUPPORTED == ucc_dt) {
        UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s", dtype->super.name);
        goto fallback;
    }

    ucc_coll_args_t coll = {
        .mask      = 0,
        .flags     = 0,
        .coll_type = UCC_COLL_TYPE_BCAST,
        .root = root,
        .src.info = {
            .buffer   = buf,
            .count    = count,
            .datatype = ucc_dt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        }
    };
    COLL_UCC_REQ_INIT(coll_req, req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_coll_ucc_bcast(void *buf, int count, struct ompi_datatype_t *dtype,
                       int root, struct ompi_communicator_t *comm,
                       mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    UCC_VERBOSE(3, "running ucc bcast");
    COLL_UCC_CHECK(mca_coll_ucc_bcast_init(buf, count, dtype, root,
                                           ucc_module, &req, NULL));
    COLL_UCC_POST_AND_CHECK(req);
    COLL_UCC_CHECK(coll_ucc_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback bcast");
    return mca_coll_ucc_call_previous(bcast, ucc_module,
        buf, count, dtype, root, comm);

}

int mca_coll_ucc_ibcast(void *buf, int count, struct ompi_datatype_t *dtype,
                        int root, struct ompi_communicator_t *comm,
                        ompi_request_t** request,
                        mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc ibcast");
    COLL_UCC_GET_REQ(coll_req);
    COLL_UCC_CHECK(mca_coll_ucc_bcast_init(buf, count, dtype, root,
                                           ucc_module, &req, coll_req));
    COLL_UCC_POST_AND_CHECK(req);
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback ibcast");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **)&coll_req);
    }
    return mca_coll_ucc_call_previous(ibcast, ucc_module,
        buf, count, dtype, root, comm, request);
}
