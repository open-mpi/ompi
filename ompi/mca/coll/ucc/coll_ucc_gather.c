
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
ucc_status_t mca_coll_ucc_gather_init(const void *sbuf, size_t scount, struct ompi_datatype_t *sdtype,
                                      void *rbuf, size_t rcount, struct ompi_datatype_t *rdtype,
                                      int root, mca_coll_ucc_module_t *ucc_module,
                                      ucc_coll_req_h *req,
                                      mca_coll_ucc_req_t *coll_req)
{
    ucc_datatype_t ucc_sdt, ucc_rdt;
    int comm_rank = ompi_comm_rank(ucc_module->comm);
    int comm_size = ompi_comm_size(ucc_module->comm);

    if (!ompi_datatype_is_contiguous_memory_layout(sdtype, scount)) {
        goto fallback;
    }
    ucc_sdt = ompi_dtype_to_ucc_dtype(sdtype);
    if (comm_rank == root) {
        if (!ompi_datatype_is_contiguous_memory_layout(rdtype, rcount * comm_size)) {
            goto fallback;
        }
        ucc_rdt = ompi_dtype_to_ucc_dtype(rdtype);
        if ((COLL_UCC_DT_UNSUPPORTED == ucc_rdt) ||
            (MPI_IN_PLACE != sbuf && COLL_UCC_DT_UNSUPPORTED == ucc_sdt)) {
            UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                        (COLL_UCC_DT_UNSUPPORTED == ucc_rdt) ?
                        rdtype->super.name : sdtype->super.name);
            goto fallback;
        }
    } else {
        if (COLL_UCC_DT_UNSUPPORTED == ucc_sdt) {
            UCC_VERBOSE(5, "ompi_datatype is not supported: dtype = %s",
                        sdtype->super.name);
            goto fallback;
        }
    }

    ucc_coll_args_t coll = {
        .mask      = 0,
        .flags     = 0,
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

    if (MPI_IN_PLACE == sbuf) {
        coll.mask |= UCC_COLL_ARGS_FIELD_FLAGS;
        coll.flags = UCC_COLL_ARGS_FLAG_IN_PLACE;
    }
    COLL_UCC_REQ_INIT(coll_req, req, coll, ucc_module);
    return UCC_OK;
fallback:
    return UCC_ERR_NOT_SUPPORTED;
}

int mca_coll_ucc_gather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                        void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                        int root, struct ompi_communicator_t *comm,
                        mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;

    UCC_VERBOSE(3, "running ucc gather");
    COLL_UCC_CHECK(mca_coll_ucc_gather_init(sbuf, scount, sdtype, rbuf, rcount,
                                            rdtype, root, ucc_module,
                                            &req, NULL));
    COLL_UCC_POST_AND_CHECK(req);
    COLL_UCC_CHECK(coll_ucc_req_wait(req));
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback gather");
    return mca_coll_ucc_call_previous(gather, ucc_module,
        sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm);
}

int mca_coll_ucc_igather(const void *sbuf, int scount, struct ompi_datatype_t *sdtype,
                         void *rbuf, int rcount, struct ompi_datatype_t *rdtype,
                         int root, struct ompi_communicator_t *comm,
                         ompi_request_t** request,
                         mca_coll_base_module_t *module)
{
    mca_coll_ucc_module_t *ucc_module = (mca_coll_ucc_module_t*)module;
    ucc_coll_req_h         req;
    mca_coll_ucc_req_t    *coll_req = NULL;

    UCC_VERBOSE(3, "running ucc igather");
    COLL_UCC_GET_REQ(coll_req);
    COLL_UCC_CHECK(mca_coll_ucc_gather_init(sbuf, scount, sdtype, rbuf, rcount,
                                            rdtype, root, ucc_module,
                                            &req, coll_req));
    COLL_UCC_POST_AND_CHECK(req);
    *request = &coll_req->super;
    return OMPI_SUCCESS;
fallback:
    UCC_VERBOSE(3, "running fallback igather");
    if (coll_req) {
        mca_coll_ucc_req_free((ompi_request_t **)&coll_req);
    }
    return mca_coll_ucc_call_previous(igather, ucc_module,
        sbuf, scount, sdtype, rbuf, rcount, rdtype, root, comm, request);
}
