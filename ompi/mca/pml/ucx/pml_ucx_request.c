/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2016      The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_ucx_request.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/message/message.h"
#include <inttypes.h>


static int mca_pml_ucx_request_free(ompi_request_t **rptr)
{
    ompi_request_t *req = *rptr;

    PML_UCX_VERBOSE(9, "free request *%p=%p", (void*)rptr, (void*)req);

    *rptr = MPI_REQUEST_NULL;
    mca_pml_ucx_request_reset(req);
    ucp_request_free(req);
    return OMPI_SUCCESS;
}

static int mca_pml_ucx_request_cancel(ompi_request_t *req, int flag)
{
    ucp_request_cancel(ompi_pml_ucx.ucp_worker, req);
    return OMPI_SUCCESS;
}

void mca_pml_ucx_send_completion(void *request, ucs_status_t status)
{
    ompi_request_t *req = request;

    PML_UCX_VERBOSE(8, "send request %p completed with status %s", (void*)req,
                    ucs_status_string(status));

    mca_pml_ucx_set_send_status(&req->req_status, status);
    PML_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    ompi_request_complete(req, true);
}

void mca_pml_ucx_bsend_completion(void *request, ucs_status_t status)
{
    ompi_request_t *req = request;

    PML_UCX_VERBOSE(8, "bsend request %p buffer %p completed with status %s", (void*)req,
                    req->req_complete_cb_data, ucs_status_string(status));
    mca_pml_base_bsend_request_free(req->req_complete_cb_data);
    req->req_complete_cb_data = NULL;
    mca_pml_ucx_set_send_status(&req->req_status, status);
    PML_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    mca_pml_ucx_request_free(&req);
}

void mca_pml_ucx_recv_completion(void *request, ucs_status_t status,
                                 ucp_tag_recv_info_t *info)
{
    ompi_request_t *req = request;

    PML_UCX_VERBOSE(8, "receive request %p completed with status %s tag %"PRIx64" len %zu",
                    (void*)req, ucs_status_string(status), info->sender_tag,
                    info->length);

    mca_pml_ucx_set_recv_status(&req->req_status, status, info);
    PML_UCX_ASSERT( !(REQUEST_COMPLETE(req)));
    ompi_request_complete(req, true);
}

static void mca_pml_ucx_persistent_request_detach(mca_pml_ucx_persistent_request_t *preq,
                                                  ompi_request_t *tmp_req)
{
    tmp_req->req_complete_cb_data = NULL;
    preq->tmp_req                 = NULL;
}

inline void
mca_pml_ucx_persistent_request_complete(mca_pml_ucx_persistent_request_t *preq,
                                        ompi_request_t *tmp_req)
{
    preq->ompi.req_status = tmp_req->req_status;
    mca_pml_ucx_request_reset(tmp_req);
    mca_pml_ucx_persistent_request_detach(preq, tmp_req);
    ucp_request_free(tmp_req);
    ompi_request_complete(&preq->ompi, true);
}

static inline void mca_pml_ucx_preq_completion(ompi_request_t *tmp_req)
{
    mca_pml_ucx_persistent_request_t *preq;

    ompi_request_complete(tmp_req, false);
    preq = (mca_pml_ucx_persistent_request_t*)tmp_req->req_complete_cb_data;
    if (preq != NULL) {
        PML_UCX_ASSERT(preq->tmp_req != NULL);
        mca_pml_ucx_persistent_request_complete(preq, tmp_req);
    }
}

void mca_pml_ucx_psend_completion(void *request, ucs_status_t status)
{
    ompi_request_t *tmp_req = request;

    PML_UCX_VERBOSE(8, "persistent send request %p completed with status %s",
                    (void*)tmp_req, ucs_status_string(status));

    mca_pml_ucx_set_send_status(&tmp_req->req_status, status);
    mca_pml_ucx_preq_completion(tmp_req);
}

void mca_pml_ucx_precv_completion(void *request, ucs_status_t status,
                                  ucp_tag_recv_info_t *info)
{
    ompi_request_t *tmp_req = request;

    PML_UCX_VERBOSE(8, "persistent receive request %p completed with status %s tag %"PRIx64" len %zu",
                    (void*)tmp_req, ucs_status_string(status), info->sender_tag,
                    info->length);

    mca_pml_ucx_set_recv_status(&tmp_req->req_status, status, info);
    mca_pml_ucx_preq_completion(tmp_req);
}

static void mca_pml_ucx_request_init_common(ompi_request_t* ompi_req,
                                            bool req_persistent,
                                            ompi_request_state_t state,
                                            ompi_request_free_fn_t req_free,
                                            ompi_request_cancel_fn_t req_cancel)
{
    OMPI_REQUEST_INIT(ompi_req, req_persistent);
    ompi_req->req_type             = OMPI_REQUEST_PML;
    ompi_req->req_state            = state;
    ompi_req->req_start            = mca_pml_ucx_start;
    ompi_req->req_free             = req_free;
    ompi_req->req_cancel           = req_cancel;
    /* This field is used to attach persistant request to a temporary req.
     * Receive (ucp_tag_recv_nb) may call completion callback
     * before the field is set. If the field is not NULL then mca_pml_ucx_preq_completion() 
     * will try to complete bogus persistant request.
     */ 
    ompi_req->req_complete_cb_data = NULL;
}

void mca_pml_ucx_request_init(void *request)
{
    ompi_request_t* ompi_req = request;
    OBJ_CONSTRUCT(ompi_req, ompi_request_t);
    mca_pml_ucx_request_init_common(ompi_req, false, OMPI_REQUEST_ACTIVE,
                                    mca_pml_ucx_request_free,
                                    mca_pml_ucx_request_cancel);
}

void mca_pml_ucx_request_cleanup(void *request)
{
    ompi_request_t* ompi_req = request;
    ompi_req->req_state = OMPI_REQUEST_INVALID;
    OMPI_REQUEST_FINI(ompi_req);
    OBJ_DESTRUCT(ompi_req);
}

static int mca_pml_ucx_persistent_request_free(ompi_request_t **rptr)
{
    mca_pml_ucx_persistent_request_t* preq = (mca_pml_ucx_persistent_request_t*)*rptr;
    ompi_request_t *tmp_req = preq->tmp_req;

    preq->ompi.req_state = OMPI_REQUEST_INVALID;
    if (tmp_req != NULL) {
        mca_pml_ucx_persistent_request_detach(preq, tmp_req);
        ucp_request_free(tmp_req);
    }
    if ((preq->flags & MCA_PML_UCX_REQUEST_FLAG_SEND) &&
         (MCA_PML_BASE_SEND_BUFFERED == preq->send.mode)) {
        OBJ_RELEASE(preq->datatype.ompi_datatype);
    }
    PML_UCX_FREELIST_RETURN(&ompi_pml_ucx.persistent_reqs, &preq->ompi.super);
    *rptr = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_ucx_persistent_request_cancel(ompi_request_t *req, int flag)
{
    mca_pml_ucx_persistent_request_t* preq = (mca_pml_ucx_persistent_request_t*)req;

    if (preq->tmp_req != NULL) {
        ucp_request_cancel(ompi_pml_ucx.ucp_worker, preq->tmp_req);
    }
    return OMPI_SUCCESS;
}

static void mca_pml_ucx_persisternt_request_construct(mca_pml_ucx_persistent_request_t* req)
{
    mca_pml_ucx_request_init_common(&req->ompi, true, OMPI_REQUEST_INACTIVE,
                                    mca_pml_ucx_persistent_request_free,
                                    mca_pml_ucx_persistent_request_cancel);
    req->tmp_req = NULL;
}

static void mca_pml_ucx_persisternt_request_destruct(mca_pml_ucx_persistent_request_t* req)
{
    req->ompi.req_state = OMPI_REQUEST_INVALID;
    OMPI_REQUEST_FINI(&req->ompi);
}

OBJ_CLASS_INSTANCE(mca_pml_ucx_persistent_request_t,
                   ompi_request_t,
                   mca_pml_ucx_persisternt_request_construct,
                   mca_pml_ucx_persisternt_request_destruct);

static int mca_pml_completed_request_free(struct ompi_request_t** rptr)
{
    *rptr = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_completed_request_cancel(struct ompi_request_t* ompi_req, int flag)
{
    return OMPI_SUCCESS;
}

void mca_pml_ucx_completed_request_init(ompi_request_t *ompi_req)
{
    mca_pml_ucx_request_init_common(ompi_req, false, OMPI_REQUEST_ACTIVE,
                                    mca_pml_completed_request_free,
                                    mca_pml_completed_request_cancel);
    ompi_req->req_mpi_object.comm = &ompi_mpi_comm_world.comm;
    ompi_request_complete(ompi_req, false);
}

