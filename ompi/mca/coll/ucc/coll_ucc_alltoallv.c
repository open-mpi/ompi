
/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 */

#include "coll_ucc_common.h"

static inline ucc_status_t mca_coll_ucc_alltoallv_init(const void *sbuf, ompi_count_array_t scounts,
                                                       ompi_disp_array_t sdisps, struct ompi_datatype_t *sdtype,
                                                       void* rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                                                       struct ompi_datatype_t *rdtype,
                                                       mca_coll_ucc_module_t *ucc_module,
                                                       ucc_coll_req_h *req,
                                                       mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t         ucc_sdt, ucc_rdt;

    ucc_sdt = ompi_dtype_to_ucc_dtype(sdtype);
    ucc_rdt = ompi_dtype_to_ucc_dtype(rdtype);
    if (COLL_UCC_DT_UNSUPPORTED == ucc_sdt ||
        COLL_UCC_DT_UNSUPPORTED == ucc_rdt) {
        UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                    (COLL_UCC_DT_UNSUPPORTED == ucc_sdt) ?
                    sdtype->super.name : rdtype->super.name);
        goto fallback;
    }

    /* Assumes that send counts/displs and recv counts/displs are both 32-bit or both 64-bit */
    uint64_t flags = ompi_count_array_is_64bit(scounts) ? UCC_COLL_ARGS_FLAG_COUNT_64BIT : 0;
    flags |= ompi_disp_array_is_64bit(sdisps) ? UCC_COLL_ARGS_FLAG_DISPLACEMENTS_64BIT : 0;

    ucc_coll_args_t coll = {
        .flags     = flags,
        .mask      = 0,
        .flags     = 0,
        .coll_type = UCC_COLL_TYPE_ALLTOALLV,
        .src.info_v = {
            .buffer        = (void*)sbuf,
            .counts        = (ucc_count_t*)ompi_count_array_ptr(scounts),
            .displacements = (ucc_aint_t*)ompi_disp_array_ptr(sdisps),
            .datatype      = ucc_sdt,
            .mem_type      = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info_v = {
            .buffer        = (void*)rbuf,
            .counts        = (ucc_count_t*)ompi_count_array_ptr(rcounts),
            .displacements = (ucc_aint_t*)ompi_disp_array_ptr(rdisps),
            .datatype      = ucc_rdt,
            .mem_type      = UCC_MEMORY_TYPE_UNKNOWN
        }
    };

    if (MPI_IN_PLACE == sbuf) {
        coll.mask  = UCC_COLL_ARGS_FIELD_FLAGS;
        coll.flags |= UCC_COLL_ARGS_FLAG_IN_PLACE;
    }
    COLL_UCC_REQ_INIT(coll_req, req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_coll_ucc_alltoallv(const void *sbuf, ompi_count_array_t scounts,
                           ompi_disp_array_t sdisps, struct ompi_datatype_t *sdtype,
                           void* rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
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
    return ucc_module->previous_alltoallv(sbuf, scounts, sdisps, sdtype,
                                          rbuf, rcounts, rdisps, rdtype,
                                          comm, ucc_module->previous_alltoallv_module);
}

int mca_coll_ucc_ialltoallv(const void *sbuf, ompi_count_array_t scounts,
                            ompi_disp_array_t sdisps, struct ompi_datatype_t *sdtype,
                            void* rbuf, ompi_count_array_t rcounts, ompi_disp_array_t rdisps,
                            struct ompi_datatype_t *rdtype,
                            struct ompi_communicator_t *comm,
                            ompi_request_t** request,
                            mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc ialltoallv");
    COLL_UCC_GET_REQ(coll_req);
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
    return ucc_module->previous_ialltoallv(sbuf, scounts, sdisps, sdtype,
                                          rbuf, rcounts, rdisps, rdtype,
                                           comm, request, ucc_module->previous_ialltoallv_module);
}
