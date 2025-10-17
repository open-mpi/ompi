
/**
 * Copyright (c) 2021 Mellanox Technologies. All rights reserved.
 * Copyright (c) 2022 NVIDIA Corporation. All rights reserved.
 * Copyright (c) 2025      Fujitsu Limited. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 */

#include "coll_ucc_common.h"

static inline ucc_status_t
mca_coll_ucc_gatherv_init_common(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                 void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                                 struct ompi_datatype_t *rdtype, int root,
                                 bool persistent, mca_coll_ucc_module_t *ucc_module,
                                 ucc_coll_req_h *req,
                                 mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t ucc_sdt = UCC_DT_INT8, ucc_rdt = UCC_DT_INT8;
    bool is_inplace = (MPI_IN_PLACE == sbuf);
    int comm_rank = ompi_comm_rank(ucc_module->comm);
    uint64_t flags = 0;

    if (comm_rank == root) {
        ucc_rdt = ompi_dtype_to_ucc_dtype(rdtype);
        if (!is_inplace) {
            ucc_sdt = ompi_dtype_to_ucc_dtype(sdtype);
        }
        if ((COLL_UCC_DT_UNSUPPORTED == ucc_sdt) ||
            (COLL_UCC_DT_UNSUPPORTED == ucc_rdt)) {
            UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                        (COLL_UCC_DT_UNSUPPORTED == ucc_sdt) ?
                        sdtype->super.name : rdtype->super.name);
            goto fallback;
        }
    } else {
        ucc_sdt = ompi_dtype_to_ucc_dtype(sdtype);
        if (COLL_UCC_DT_UNSUPPORTED == ucc_sdt) {
            UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                        sdtype->super.name);
            goto fallback;
        }
    }

    flags = (ompi_count_array_is_64bit(rcounts) ? UCC_COLL_ARGS_FLAG_COUNT_64BIT : 0) |
            (ompi_disp_array_is_64bit(disps) ? UCC_COLL_ARGS_FLAG_DISPLACEMENTS_64BIT : 0) |
            (is_inplace ? UCC_COLL_ARGS_FLAG_IN_PLACE : 0) |
            (persistent ? UCC_COLL_ARGS_FLAG_PERSISTENT : 0);

    ucc_coll_args_t coll = {
        .mask      = flags ? UCC_COLL_ARGS_FIELD_FLAGS : 0,
        .flags     = flags,
        .coll_type = UCC_COLL_TYPE_GATHERV,
        .root      = root,
        .src.info = {
            .buffer   = (void*)sbuf,
            .count    = scount,
            .datatype = ucc_sdt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info_v = {
            .buffer        = (void*)rbuf,
            .counts        = (ucc_count_t*)ompi_count_array_ptr(rcounts),
            .displacements = (ucc_aint_t*)ompi_disp_array_ptr(disps),
            .datatype      = ucc_rdt,
            .mem_type      = UCC_MEMORY_TYPE_UNKNOWN
        },
    };

    COLL_UCC_REQ_INIT(coll_req, req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_coll_ucc_gatherv(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                         void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                         struct ompi_datatype_t *rdtype, int root,
                         struct ompi_communicator_t *comm,
                         mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;

    UCC_VERBOSE(3, "running ucc gatherv");
    COLL_UCC_CHECK(mca_coll_ucc_gatherv_init_common(sbuf, scount, sdtype, rbuf, rcounts,
                                                    disps, rdtype, root, false, ucc_module,
                                                    &req, NULL));
    COLL_UCC_POST_AND_CHECK(req);
    COLL_UCC_CHECK(coll_ucc_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback gatherv");
    return ucc_module->previous_gatherv(sbuf, scount, sdtype, rbuf, rcounts,
                                        disps, rdtype, root, comm,
                                        ucc_module->previous_gatherv_module);
}

int mca_coll_ucc_igatherv(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                          void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                          struct ompi_datatype_t *rdtype, int root,
                          struct ompi_communicator_t *comm,
                          ompi_request_t** request,
                          mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc igatherv");
    COLL_UCC_GET_REQ(coll_req, comm);
    COLL_UCC_CHECK(mca_coll_ucc_gatherv_init_common(sbuf, scount, sdtype, rbuf, rcounts,
                                                    disps, rdtype, root, false, ucc_module,
                                                    &req, coll_req));
    COLL_UCC_POST_AND_CHECK(req);
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback igatherv");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **)&coll_req);
    }
    return ucc_module->previous_igatherv(sbuf, scount, sdtype, rbuf, rcounts,
                                         disps, rdtype, root, comm, request,
                                         ucc_module->previous_igatherv_module);
}

int mca_coll_ucc_gatherv_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                              void *rbuf, ompi_count_array_t rcounts, ompi_disp_array_t disps,
                              struct ompi_datatype_t *rdtype, int root,
                              struct ompi_communicator_t *comm, struct ompi_info_t *info,
                              ompi_request_t **request, mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t *) module;
    ucc_coll_req_h req;
    mca_coll_ucc_req_t *coll_req = NULL;

    COLL_UCC_GET_REQ_PERSISTENT(coll_req, comm);
    UCC_VERBOSE(3, "gatherv_init init %p", coll_req);
    COLL_UCC_CHECK(mca_coll_ucc_gatherv_init_common(sbuf, scount, sdtype, rbuf, rcounts,
                                                    disps, rdtype, root, true, ucc_module,
                                                    &req, coll_req));
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback gatherv_init");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **) &coll_req);
    }
    return ucc_module->previous_gatherv_init(sbuf, scount, sdtype, rbuf, rcounts, disps, rdtype,
                                             root, comm, info, request,
                                             ucc_module->previous_gatherv_init_module);
}
