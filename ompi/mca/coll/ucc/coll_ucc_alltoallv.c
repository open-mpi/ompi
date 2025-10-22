
/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2025      Fujitsu Limited. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 */

#include "coll_ucc_common.h"

static inline ucc_status_t mca_coll_ucc_alltoallv_init(const void *sbuf, const int *scounts,
                                                       const int *sdisps, struct ompi_datatype_t *sdtype,
                                                       void* rbuf, const int *rcounts, const int *rdisps,
                                                       struct ompi_datatype_t *rdtype,
                                                       mca_coll_ucc_module_t *ucc_module,
                                                       ucc_coll_req_h *req,
                                                       mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t ucc_sdt = UCC_DT_INT8, ucc_rdt = UCC_DT_INT8;
    bool is_inplace = (MPI_IN_PLACE == sbuf);

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
        .coll_type = UCC_COLL_TYPE_ALLTOALLV,
        .src.info_v = {
            .buffer        = (void*)sbuf,
            .counts        = (ucc_count_t*)scounts,
            .displacements = (ucc_aint_t*)sdisps,
            .datatype      = ucc_sdt,
            .mem_type      = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info_v = {
            .buffer        = (void*)rbuf,
            .counts        = (ucc_count_t*)rcounts,
            .displacements = (ucc_aint_t*)rdisps,
            .datatype      = ucc_rdt,
            .mem_type      = UCC_MEMORY_TYPE_UNKNOWN
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

int mca_coll_ucc_alltoallv(const void *sbuf, const int *scounts,
                           const int *sdisps, struct ompi_datatype_t *sdtype,
                           void* rbuf, const int *rcounts, const int *rdisps,
                           struct ompi_datatype_t *rdtype,
                           struct ompi_communicator_t *comm,
                           mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;

    UCC_VERBOSE(3, "running ucc alltoallv");

    COLL_UCC_CHECK(mca_coll_ucc_alltoallv_init(sbuf, scounts, sdisps, sdtype,
                                               rbuf, rcounts, rdisps, rdtype,
                                               ucc_module, &req, NULL));
    COLL_UCC_POST_AND_CHECK(req);
    COLL_UCC_CHECK(coll_ucc_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback alltoallv");
    return mca_coll_ucc_call_previous(alltoallv, ucc_module,
        sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm);
}

int mca_coll_ucc_ialltoallv(const void *sbuf, const int *scounts,
                            const int *sdisps, struct ompi_datatype_t *sdtype,
                            void* rbuf, const int *rcounts, const int *rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc ialltoallv");
    COLL_UCC_GET_REQ(coll_req, comm);
    COLL_UCC_CHECK(mca_coll_ucc_alltoallv_init(sbuf, scounts, sdisps, sdtype,
                                               rbuf, rcounts, rdisps, rdtype,
                                               ucc_module, &req, coll_req));
    COLL_UCC_POST_AND_CHECK(req);
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback ialltoallv");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **)&coll_req);
    }
    return mca_coll_ucc_call_previous(ialltoallv, ucc_module,
        sbuf, scounts, sdisps, sdtype, rbuf, rcounts, rdisps, rdtype, comm, request);
}
