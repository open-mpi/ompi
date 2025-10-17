
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
mca_coll_ucc_gather_init_common(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                                int root, bool persistent, mca_coll_ucc_module_t *ucc_module,
                                ucc_coll_req_h *req,
                                mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t ucc_sdt = UCC_DT_INT8, ucc_rdt = UCC_DT_INT8;
    bool is_inplace = (MPI_IN_PLACE == sbuf);
    int comm_rank = ompi_comm_rank(ucc_module->comm);
    int comm_size = ompi_comm_size(ucc_module->comm);
    uint64_t flags = 0;

    if (comm_rank == root) {
        if (!(is_inplace || ompi_datatype_is_contiguous_memory_layout(sdtype, scount)) ||
            !ompi_datatype_is_contiguous_memory_layout(rdtype, rcount * comm_size)) {
            goto fallback;
        }

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
        if (!ompi_datatype_is_contiguous_memory_layout(sdtype, scount)) {
            goto fallback;
        }

        ucc_sdt = ompi_dtype_to_ucc_dtype(sdtype);
        if (COLL_UCC_DT_UNSUPPORTED == ucc_sdt) {
            UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                        sdtype->super.name);
            goto fallback;
        }
    }

    flags = (is_inplace ? UCC_COLL_ARGS_FLAG_IN_PLACE : 0) |
            (persistent ? UCC_COLL_ARGS_FLAG_PERSISTENT : 0);

    ucc_coll_args_t coll = {
        .mask      = flags ? UCC_COLL_ARGS_FIELD_FLAGS : 0,
        .flags     = flags,
        .coll_type = UCC_COLL_TYPE_GATHER,
        .root      = root,
        .src.info = {
            .buffer   = (void*)sbuf,
            .count    = scount,
            .datatype = ucc_sdt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
        .dst.info = {
            .buffer   = (void*)rbuf,
            .count    = rcount * comm_size,
            .datatype = ucc_rdt,
            .mem_type = UCC_MEMORY_TYPE_UNKNOWN
        },
    };

    COLL_UCC_REQ_INIT(coll_req, req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_coll_ucc_gather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                        void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;

    UCC_VERBOSE(3, "running ucc gather");
    COLL_UCC_CHECK(mca_coll_ucc_gather_init_common(sbuf, scount, sdtype, rbuf, rcount,
                                                   rdtype, root, false, ucc_module,
                                                   &req, NULL));
    COLL_UCC_POST_AND_CHECK(req);
    COLL_UCC_CHECK(coll_ucc_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback gather");
    return ucc_module->previous_gather(sbuf, scount, sdtype, rbuf, rcount,
                                       rdtype, root, comm,
                                       ucc_module->previous_gather_module);
}

int mca_coll_ucc_igather(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                         void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                         int root, struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc igather");
    COLL_UCC_GET_REQ(coll_req, comm);
    COLL_UCC_CHECK(mca_coll_ucc_gather_init_common(sbuf, scount, sdtype, rbuf, rcount,
                                                   rdtype, root, false, ucc_module,
                                                   &req, coll_req));
    COLL_UCC_POST_AND_CHECK(req);
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback igather");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **)&coll_req);
    }
    return ucc_module->previous_igather(sbuf, scount, sdtype, rbuf, rcount,
                                        rdtype, root, comm, request,
                                        ucc_module->previous_igather_module);
}

int mca_coll_ucc_gather_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                             void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype, int root,
                             struct ompi_communicator_t *comm, struct ompi_info_t *info,
                             ompi_request_t **request, mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t *) module;
    ucc_coll_req_h req;
    mca_coll_ucc_req_t *coll_req = NULL;

    COLL_UCC_GET_REQ_PERSISTENT(coll_req, comm);
    UCC_VERBOSE(3, "gather_init init %p", coll_req);
    COLL_UCC_CHECK(mca_coll_ucc_gather_init_common(sbuf, scount, sdtype, rbuf, rcount,
                                                   rdtype, root, true, ucc_module,
                                                   &req, coll_req));
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback gather_init");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **) &coll_req);
    }
    return ucc_module->previous_gather_init(sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm,
                                            info, request, ucc_module->previous_gather_init_module);
}
