
/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 */

#include "coll_ucc_common.h"

static inline ucc_status_t mca_coll_ucc_alltoall_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                                      void* rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                                                      mca_coll_ucc_module_t *ucc_module,
                                                      ucc_coll_req_h *req,
                                                      mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t ucc_sdt = UCC_DT_INT8, ucc_rdt = UCC_DT_INT8;
    bool is_inplace = (MPI_IN_PLACE == sbuf);
    int comm_size = ompi_comm_size(ucc_module->comm);

    if (!(is_inplace || ompi_datatype_is_contiguous_memory_layout(sdtype, scount * comm_size)) ||
        !ompi_datatype_is_contiguous_memory_layout(rdtype, rcount * comm_size)) {
        goto fallback;
    }

    ucc_rdt = ompi_dtype_to_ucc_dtype(rdtype);
    if (!is_inplace) {
        ucc_sdt = ompi_dtype_to_ucc_dtype(sdtype);
    }

    if (COLL_UCC_DT_UNSUPPORTED == ucc_sdt ||
        COLL_UCC_DT_UNSUPPORTED == ucc_rdt) {
        UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                    (COLL_UCC_DT_UNSUPPORTED == ucc_sdt) ?
                    sdtype->super.name : rdtype->super.name);
        goto fallback;
    }

    ucc_coll_args_t coll = {
        .mask      = 0,
        .flags     = 0,
        .coll_type = UCC_COLL_TYPE_ALLTOALL,
        .src.info = {
            .buffer   = (void*)sbuf,
            .count    = scount * comm_size,
            .datatype = ucc_sdt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info = {
            .buffer   = (void*)rbuf,
            .count    = rcount * comm_size,
            .datatype = ucc_rdt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        }
    };

    if (is_inplace) {
        coll.mask  = UCC_COLL_ARGS_FIELD_FLAGS;
        coll.flags = UCC_COLL_ARGS_FLAG_IN_PLACE;
    }
    COLL_UCC_REQ_INIT(coll_req, req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_coll_ucc_alltoall(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                          void* rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                          struct ompi_communicator_t *comm,
                          mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;

    UCC_VERBOSE(3, "running ucc alltoall");
    COLL_UCC_CHECK(mca_coll_ucc_alltoall_init(sbuf, scount, sdtype,
                                              rbuf, rcount, rdtype,
                                              ucc_module, &req, NULL));
    COLL_UCC_POST_AND_CHECK(req);
    COLL_UCC_CHECK(coll_ucc_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback alltoall");
    return ucc_module->previous_alltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                          comm, ucc_module->previous_alltoall_module);
}

int mca_coll_ucc_ialltoall(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                           void* rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           ompi_request_t** request,
                           mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc ialltoall");
    COLL_UCC_GET_REQ(coll_req);
    COLL_UCC_CHECK(mca_coll_ucc_alltoall_init(sbuf, scount, sdtype,
                                              rbuf, rcount, rdtype,
                                              ucc_module, &req, coll_req));
    COLL_UCC_POST_AND_CHECK(req);
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback ialltoall");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **)&coll_req);
    }
    return ucc_module->previous_ialltoall(sbuf, scount, sdtype, rbuf, rcount, rdtype,
                                          comm, request, ucc_module->previous_ialltoall_module);
}
