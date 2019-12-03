/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2019      Huawei Technologies Co., Ltd. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <inttypes.h>
#include "coll_ucx_request.h"
#include "ompi/message/message.h"


static int mca_coll_ucx_request_free(ompi_request_t **rptr)
{
    ompi_request_t *req = *rptr;

    COLL_UCX_VERBOSE(9, "free request *%p=%p", (void*)rptr, (void*)req);

    *rptr = MPI_REQUEST_NULL;
    mca_coll_ucx_request_reset(req);
    ucg_request_free(req);
    return OMPI_SUCCESS;
}

static int mca_coll_ucx_request_cancel(ompi_request_t *req, int flag)
{
    ucg_request_cancel(mca_coll_ucx_component.ucg_worker, req);
    return OMPI_SUCCESS;
}

void mca_coll_ucx_coll_completion(void *request, ucs_status_t status)
{
    ompi_request_t *req = request;

    COLL_UCX_VERBOSE(8, "send request %p completed with status %s", (void*)req,
                    ucs_status_string(status));

    mca_coll_ucx_set_coll_status(&req->req_status, status);
    COLL_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    ompi_request_complete(req, true);
}

static void mca_coll_ucx_persistent_op_detach(mca_coll_ucx_persistent_op_t *preq,
                                                  ompi_request_t *tmp_req)
{
    tmp_req->req_complete_cb_data = NULL;
    preq->tmp_req                 = NULL;
}

inline void
mca_coll_ucx_persistent_op_complete(mca_coll_ucx_persistent_op_t *preq,
                                        ompi_request_t *tmp_req)
{
    preq->ompi.req_status = tmp_req->req_status;
    mca_coll_ucx_request_reset(tmp_req);
    mca_coll_ucx_persistent_op_detach(preq, tmp_req);
    ucg_request_free(tmp_req);
    ompi_request_complete(&preq->ompi, true);
}

static inline void mca_coll_ucx_preq_completion(ompi_request_t *tmp_req)
{
    mca_coll_ucx_persistent_op_t *preq=NULL;

    ompi_request_complete(tmp_req, false);
    preq = (mca_coll_ucx_persistent_op_t*)tmp_req->req_complete_cb_data;
    if (preq != NULL) {
        COLL_UCX_ASSERT(preq->tmp_req != NULL);
        mca_coll_ucx_persistent_op_complete(preq, tmp_req);
    }
}

void mca_coll_ucx_pcoll_completion(void *request, ucs_status_t status)
{
    ompi_request_t *tmp_req = request;

    COLL_UCX_VERBOSE(8, "persistent collective request %p completed with status %s",
                    (void*)tmp_req, ucs_status_string(status));

    mca_coll_ucx_set_coll_status(&tmp_req->req_status, status);
    mca_coll_ucx_preq_completion(tmp_req);
}

static void mca_coll_ucx_request_init_common(ompi_request_t* ompi_req,
                                            bool op_persistent,
                                            ompi_request_state_t state,
                                            ompi_request_free_fn_t req_free,
                                            ompi_request_cancel_fn_t req_cancel)
{
    OMPI_REQUEST_INIT(ompi_req, op_persistent);
    ompi_req->req_type             = OMPI_REQUEST_COLL;
    ompi_req->req_state            = state;
    ompi_req->req_start            = mca_coll_ucx_start;
    ompi_req->req_free             = req_free;
    ompi_req->req_cancel           = req_cancel;
    /* This field is used to attach persistant request to a temporary req.
     * Receive (ucg_tag_recv_nb) may call completion callback
     * before the field is set. If the field is not NULL then mca_coll_ucx_preq_completion()
     * will try to complete bogus persistant request.
     */
    ompi_req->req_complete_cb_data = NULL;
}

void mca_coll_ucx_request_init(void *request)
{
    ompi_request_t* ompi_req = request;
    OBJ_CONSTRUCT(ompi_req, ompi_request_t);
    mca_coll_ucx_request_init_common(ompi_req, false, OMPI_REQUEST_ACTIVE,
                                    mca_coll_ucx_request_free,
                                    mca_coll_ucx_request_cancel);
}

void mca_coll_ucx_request_cleanup(void *request)
{
    ompi_request_t* ompi_req = request;
    ompi_req->req_state = OMPI_REQUEST_INVALID;
    OMPI_REQUEST_FINI(ompi_req);
    OBJ_DESTRUCT(ompi_req);
}

static int mca_coll_ucx_persistent_op_free(ompi_request_t **rptr)
{
    mca_coll_ucx_persistent_op_t* preq = (mca_coll_ucx_persistent_op_t*)*rptr;
    ompi_request_t *tmp_req = preq->tmp_req;

    preq->ompi.req_state = OMPI_REQUEST_INVALID;
    if (tmp_req != NULL) {
        mca_coll_ucx_persistent_op_detach(preq, tmp_req);
        ucg_request_free(tmp_req);
    }

    COLL_UCX_FREELIST_RETURN(&mca_coll_ucx_component.persistent_ops, &preq->ompi.super);
    *rptr = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_coll_ucx_persistent_op_cancel(ompi_request_t *req, int flag)
{
    mca_coll_ucx_persistent_op_t* preq = (mca_coll_ucx_persistent_op_t*)req;

    if (preq->tmp_req != NULL) {
        ucg_request_cancel(preq->ucg_worker, preq->tmp_req);
    }
    return OMPI_SUCCESS;
}

static void mca_coll_ucx_persisternt_op_construct(mca_coll_ucx_persistent_op_t* req)
{
    mca_coll_ucx_request_init_common(&req->ompi, true, OMPI_REQUEST_INACTIVE,
                                    mca_coll_ucx_persistent_op_free,
                                    mca_coll_ucx_persistent_op_cancel);
    req->tmp_req = NULL;
}

static void mca_coll_ucx_persisternt_op_destruct(mca_coll_ucx_persistent_op_t* req)
{
    req->ompi.req_state = OMPI_REQUEST_INVALID;
    OMPI_REQUEST_FINI(&req->ompi);
}

OBJ_CLASS_INSTANCE(mca_coll_ucx_persistent_op_t,
                   ompi_request_t,
                   mca_coll_ucx_persisternt_op_construct,
                   mca_coll_ucx_persisternt_op_destruct);

static int mca_coll_completed_request_free(struct ompi_request_t** rptr)
{
    *rptr = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_coll_completed_request_cancel(struct ompi_request_t* ompi_req, int flag)
{
    return OMPI_SUCCESS;
}

void mca_coll_ucx_completed_request_init(ompi_request_t *ompi_req)
{
    mca_coll_ucx_request_init_common(ompi_req, false, OMPI_REQUEST_ACTIVE,
                                     mca_coll_completed_request_free,
                                     mca_coll_completed_request_cancel);
    ompi_request_complete(ompi_req, false);
}
