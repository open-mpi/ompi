/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022 NVIDIA Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 */

#include "coll_ucc_common.h"

static inline
ucc_status_t mca_coll_ucc_reduce_scatter_block_init(const void *sbuf, void *rbuf,
                                                    int rcount,
                                                    struct ompi_datatype_t *dtype,
                                                    struct ompi_op_t *op,
                                                    mca_coll_ucc_module_t *ucc_module,
                                                    ucc_coll_req_h *req,
                                                    mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t ucc_dt;
    ucc_reduction_op_t ucc_op;
    int comm_size = ompi_comm_size(ucc_module->comm);

    if (MPI_IN_PLACE == sbuf) {
        /* TODO: UCC defines inplace differently:
           data in rbuf of rank R is shifted by R * rcount */
        UCC_VERBOSE(5, "inplace reduce_scatter_block is not supported");
        return UCC_ERR_NOT_SUPPORTED;
    }
    ucc_dt = ompi_dtype_to_ucc_dtype(dtype);
    ucc_op = ompi_op_to_ucc_op(op);
    if (OPAL_UNLIKELY(COLL_UCC_DT_UNSUPPORTED == ucc_dt)) {
        UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                    dtype->super.name);
        goto fallback;
    }
    if (OPAL_UNLIKELY(COLL_UCC_OP_UNSUPPORTED == ucc_op)) {
        UCC_VERBOSE(5, "ompi_op is not supported: op = %s",
                    op->o_name);
        goto fallback;
    }
    ucc_coll_args_t coll = {
        .mask      = 0,
        .coll_type = UCC_COLL_TYPE_REDUCE_SCATTER,
        .src.info = {
            .buffer   = (void*)sbuf,
            .count    = ((size_t)rcount) * comm_size,
            .datatype = ucc_dt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info = {
            .buffer   = rbuf,
            .count    = rcount,
            .datatype = ucc_dt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .op = ucc_op,
    };
    COLL_UCC_REQ_INIT(coll_req, req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_coll_ucc_reduce_scatter_block(const void *sbuf, void *rbuf, int rcount,
                                      struct ompi_datatype_t *dtype,
                                      struct ompi_op_t *op,
                                      struct ompi_communicator_t *comm,
                                      mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;

    UCC_VERBOSE(3, "running ucc reduce scatter block");
    COLL_UCC_CHECK(mca_coll_ucc_reduce_scatter_block_init(sbuf, rbuf, rcount,
                                                          dtype, op, ucc_module,
                                                          &req, NULL));
    COLL_UCC_POST_AND_CHECK(req);
    COLL_UCC_CHECK(coll_ucc_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback reduce_scatter_block");
    return ucc_module->previous_reduce_scatter_block(sbuf, rbuf, rcount, dtype,
                                                     op, comm,
                                                     ucc_module->previous_reduce_scatter_block_module);
}

int mca_coll_ucc_ireduce_scatter_block(const void *sbuf, void *rbuf, int rcount,
                                       struct ompi_datatype_t *dtype,
                                       struct ompi_op_t *op,
                                       struct ompi_communicator_t *comm,
                                       ompi_request_t** request,
                                       mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc ireduce_scatter_block");
    COLL_UCC_GET_REQ(coll_req);
    COLL_UCC_CHECK(mca_coll_ucc_reduce_scatter_block_init(sbuf, rbuf, rcount,
                                                          dtype, op, ucc_module,
                                                          &req, coll_req));
    COLL_UCC_POST_AND_CHECK(req);
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback ireduce_scatter_block");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **)&coll_req);
    }
    return ucc_module->previous_ireduce_scatter_block(sbuf, rbuf, rcount, dtype,
                                                      op, comm, request,
                                                      ucc_module->previous_ireduce_scatter_block_module);
}
