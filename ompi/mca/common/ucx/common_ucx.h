/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2020 Huawei Technologies Co., Ltd.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_COMMON_UCX_H
#define MCA_COMMON_UCX_H

#include "common_ucx_datatype.h"

#define MCA_COMMON_UCX_SPECIFIC_SOURCE_MASK 0x800000fffffffffful

/*
 * Below is the slack reserved after each UCX common persistent request. This
 * addresses the need for higher-level components, i.e. PML and COLL, to have
 * a longer persistent context while sharing this piece of code.
 */
#define MCA_COMMON_UCX_PERSISTENT_REQUEST_SLACK (64)

typedef struct mca_common_ucx_persistent_request mca_common_ucx_persistent_request_t;
typedef ompi_request_t* (*mca_common_ucx_persistent_start_cb_f)
                        (mca_common_ucx_persistent_request_t *preq);

struct mca_common_ucx_persistent_request {
    ompi_request_t                       ompi;
    mca_common_ucx_persistent_start_cb_f start_cb;
    ompi_request_t                      *tmp_req;
};

OBJ_CLASS_DECLARATION(mca_common_ucx_persistent_request_t);

__opal_attribute_always_inline__
static inline void mca_common_ucx_set_status(ompi_status_public_t* mpi_status,
                                             ucs_status_t status)
{
    if (OPAL_LIKELY(status == UCS_OK)) {
        mpi_status->MPI_ERROR  = MPI_SUCCESS;
        mpi_status->_cancelled = false;
    } else if (status == UCS_ERR_CANCELED) {
        mpi_status->_cancelled = true;
    } else {
        mpi_status->MPI_ERROR  = MPI_ERR_INTERN;
    }
}

static inline int
mca_common_ucx_persistent_request_init(ompi_request_type_t req_type,
                                       struct ompi_communicator_t* comm,
                                       mca_common_ucx_persistent_start_cb_f cb,
                                       mca_common_ucx_persistent_request_t **request)
{
    mca_common_ucx_persistent_request_t *req;

    req = (mca_common_ucx_persistent_request_t*)
            COMMON_UCX_FREELIST_GET(&ompi_common_ucx.requests);
    if (req == NULL) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    req->ompi.req_type            = req_type;
    req->ompi.req_state           = OMPI_REQUEST_INACTIVE;
    req->ompi.req_mpi_object.comm = comm;
    req->start_cb                 = cb;

    *request = req;
    return OMPI_SUCCESS;
}

static inline void mca_common_ucx_request_reset(ompi_request_t *req)
{
    req->req_complete = REQUEST_PENDING;
}

static inline int mca_common_ucx_request_free(ompi_request_t **rptr)
{
    ompi_request_t *req = *rptr;

    MCA_COMMON_UCX_VERBOSE(9, "free request *%p=%p", (void*)rptr, (void*)req);

    *rptr = MPI_REQUEST_NULL;
    mca_common_ucx_request_reset(req);
    ucp_request_free(req);
    return OMPI_SUCCESS;
}

static void
mca_common_ucx_persistent_request_detach(mca_common_ucx_persistent_request_t *preq,
                                         ompi_request_t *tmp_req)
{
    tmp_req->req_complete_cb_data = NULL;
    preq->tmp_req                 = NULL;
}

static inline void
mca_common_ucx_persistent_request_complete(mca_common_ucx_persistent_request_t *preq,
                                           ompi_request_t *tmp_req)
{
    preq->ompi.req_status = tmp_req->req_status;
    mca_common_ucx_request_reset(tmp_req);
    mca_common_ucx_persistent_request_detach(preq, tmp_req);
    ucp_request_free(tmp_req);
    ompi_request_complete(&preq->ompi, true);
}

static inline void mca_common_ucx_preq_completion(ompi_request_t *tmp_req)
{
    mca_common_ucx_persistent_request_t *preq;

    ompi_request_complete(tmp_req, false);
    preq = (mca_common_ucx_persistent_request_t*)tmp_req->req_complete_cb_data;
    if (preq != NULL) {
        MCA_COMMON_UCX_ASSERT(preq->tmp_req != NULL);
        mca_common_ucx_persistent_request_complete(preq, tmp_req);
    }
}

int mca_common_ucx_open(const char *prefix, size_t *request_size);

int mca_common_ucx_close(void);

void mca_common_ucx_enable(void);

int mca_common_ucx_init(const mca_base_component_t *version);

int mca_common_ucx_cleanup(void);

int mca_common_ucx_start(size_t count, ompi_request_t** requests);

int mca_common_ucx_dump(struct ompi_communicator_t* comm, int verbose);

#endif /* MCA_COMMON_UCX_H */
