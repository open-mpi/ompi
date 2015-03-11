/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (C) Mellanox Technologies Ltd. 2001-2011.  ALL RIGHTS RESERVED.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "pml_yalla_request.h"

#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/message/message.h"


static inline void mca_pml_yalla_request_release(mca_pml_yalla_base_request_t *req,
                                                 mca_pml_yalla_freelist_t *fl)
{
    if (req->convertor != NULL) {
        mca_pml_yalla_convertor_free(req->convertor);
        req->convertor = NULL;
    }
    OBJ_RELEASE(req->ompi.req_mpi_object.comm);

#if PML_YALLA_ENABLE_DEBUG
    req->ompi.req_state = OMPI_REQUEST_INVALID;
#endif
    PML_YALLA_FREELIST_RETURN(fl, &req->ompi.super);
}

static inline int
mca_pml_yalla_check_request_state(mca_pml_yalla_base_request_t *req)
{
    if (req->mxm_base->state != MXM_REQ_COMPLETED) {
         PML_YALLA_VERBOSE(8, "request %p free called before completed", (void *)req);
         req->flags |= MCA_PML_YALLA_REQUEST_FLAG_FREE_CALLED;
         return 0;
    }

    return 1;
}

static int mca_pml_yalla_send_request_free(ompi_request_t **request)
{
    mca_pml_yalla_base_request_t *req = (mca_pml_yalla_base_request_t*)(*request);

    PML_YALLA_VERBOSE(9, "free send request *%p=%p", (void *)request, (void *)*request);

    OPAL_THREAD_LOCK(&ompi_request_lock);
    if (mca_pml_yalla_check_request_state(req)) {
        mca_pml_yalla_request_release(req, &ompi_pml_yalla.send_reqs);
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_yalla_send_request_cancel(ompi_request_t *request, int flag)
{
    mca_pml_yalla_send_request_t *sreq = (mca_pml_yalla_send_request_t*)request;
    mxm_error_t error;

    if (request->req_complete) {
        /*
         * This might be a buffered send request which has completed anyway, so
         * we cannot cancel it anymore. Just hope for the best.
         */
        PML_YALLA_VERBOSE(7, "not canceling a completed send request %p", (void *)request);
        return OMPI_SUCCESS;
    }

    error = mxm_req_cancel_send(&sreq->mxm);
    if ((error != MXM_OK) && (error != MXM_ERR_NO_PROGRESS)) {
        PML_YALLA_ERROR("failed to cancel send request %p: %s", (void *)request,
                        mxm_error_string(error));
        return OMPI_ERROR;
    }

    PML_YALLA_VERBOSE(9, "canceled send request %p", (void *)request);
    return OMPI_SUCCESS;
}

static int mca_pml_yalla_recv_request_free(ompi_request_t **request)
{
    mca_pml_yalla_base_request_t *req = (mca_pml_yalla_base_request_t*)(*request);

    PML_YALLA_VERBOSE(9, "free receive request *%p=%p", (void *)request, (void *)*request);

    OPAL_THREAD_LOCK(&ompi_request_lock);
    if (mca_pml_yalla_check_request_state(req)) {
        mca_pml_yalla_request_release(req, &ompi_pml_yalla.recv_reqs);
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);

    *request = MPI_REQUEST_NULL;
    return OMPI_SUCCESS;
}

static int mca_pml_yalla_recv_request_cancel(ompi_request_t *request, int flag)
{
    mca_pml_yalla_recv_request_t *rreq = (mca_pml_yalla_recv_request_t*)request;
    mxm_error_t error;

    error = mxm_req_cancel_recv(&rreq->mxm);
    if ((error != MXM_OK) && (error != MXM_ERR_NO_PROGRESS)) {
        PML_YALLA_ERROR("failed to cancel receive request %p: %s", (void *)request,
                        mxm_error_string(error));
        return OMPI_ERROR;
    }

    PML_YALLA_VERBOSE(9, "canceled receive request %p", (void *)request);
    return OMPI_SUCCESS;
}

static void init_mxm_base_req(mxm_req_base_t *mxm_req_base)
{
    mxm_req_base->state              = MXM_REQ_NEW;
    mxm_req_base->mq                 = NULL;
    mxm_req_base->conn               = NULL;
    mxm_req_base->data_type          = MXM_REQ_DATA_BUFFER;
    mxm_req_base->data.buffer.ptr    = NULL;
    mxm_req_base->data.buffer.length = 0;
    mxm_req_base->data.buffer.memh   = 0;
    mxm_req_base->context            = NULL;
    mxm_req_base->completed_cb       = NULL;
}

static void init_mxm_send_req(mxm_send_req_t *mxm_sreq)
{
    init_mxm_base_req(&mxm_sreq->base);
    mxm_sreq->opcode           = MXM_REQ_OP_SEND;
    mxm_sreq->op.send.imm_data = 0;
    mxm_sreq->op.send.tag      = 0;
#if defined(MXM_REQ_SEND_FLAG_REENTRANT)
    mxm_sreq->flags            = MXM_REQ_SEND_FLAG_REENTRANT;
#else
    mxm_sreq->flags            = 0;
#endif
}

static void init_mxm_recv_req(mxm_recv_req_t *mxm_rreq)
{
    init_mxm_base_req(&mxm_rreq->base);
    mxm_rreq->tag             = 0;
    mxm_rreq->tag_mask        = 0x7fffffff;
}

static void init_base_req(mca_pml_yalla_base_request_t *req)
{
    OMPI_REQUEST_INIT(&req->ompi, false);
    req->ompi.req_type             = OMPI_REQUEST_PML;
    req->ompi.req_cancel           = NULL;
    req->ompi.req_complete_cb      = NULL;
    req->ompi.req_complete_cb_data = NULL;
    req->convertor                  = NULL;
}

static void mca_pml_yalla_send_completion_cb(void *context)
{
    mca_pml_yalla_send_request_t* sreq = context;

    switch (sreq->mxm.base.error) {
    case MXM_OK:
        sreq->super.ompi.req_status.MPI_ERROR  = OMPI_SUCCESS;
        break;
    case MXM_ERR_CANCELED:
        sreq->super.ompi.req_status._cancelled = true;
        break;
    default:
        sreq->super.ompi.req_status.MPI_ERROR  = MPI_ERR_INTERN;
        break;
    }

    PML_YALLA_VERBOSE(8, "send request %p completed with status %s", (void *)sreq,
                   mxm_error_string(sreq->mxm.base.error));

    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_complete(&sreq->super.ompi, true);
    if (sreq->super.flags & MCA_PML_YALLA_REQUEST_FLAG_FREE_CALLED) {
        PML_YALLA_VERBOSE(7, "release request %p because free was already called", (void *)sreq);
        mca_pml_yalla_request_release(&sreq->super, &ompi_pml_yalla.send_reqs);
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}

static void mca_pml_yalla_bsend_completion_cb(void *context)
{
    mca_pml_yalla_bsend_request_t *bsreq = context;

    PML_YALLA_VERBOSE(8, "bsend request %p completed with status %s", (void *)bsreq,
                      mxm_error_string(bsreq->mxm.base.error));

    mca_pml_base_bsend_request_free(bsreq->mxm.base.data.buffer.ptr);
    PML_YALLA_FREELIST_RETURN(&ompi_pml_yalla.bsend_reqs, &bsreq->super);
}

static void mca_pml_yalla_recv_completion_cb(void *context)
{
    mca_pml_yalla_recv_request_t* rreq = context;

    PML_YALLA_SET_RECV_STATUS(&rreq->mxm, rreq->mxm.completion.actual_len,
                              &rreq->super.ompi.req_status);

    PML_YALLA_VERBOSE(8, "receive request %p completed with status %s source %d rtag %d(%d/0x%x) len %zu",
                      (void *)rreq, mxm_error_string(rreq->mxm.base.error),
                      rreq->mxm.completion.sender_imm, rreq->mxm.completion.sender_tag,
                      rreq->mxm.tag, rreq->mxm.tag_mask,
                      rreq->mxm.completion.actual_len);

    OPAL_THREAD_LOCK(&ompi_request_lock);
    ompi_request_complete(&rreq->super.ompi, true);
    if (rreq->super.flags & MCA_PML_YALLA_REQUEST_FLAG_FREE_CALLED) {
        PML_YALLA_VERBOSE(7, "release request %p because free was already called", (void *)rreq);
        mca_pml_yalla_request_release(&rreq->super, &ompi_pml_yalla.recv_reqs);
    }
    OPAL_THREAD_UNLOCK(&ompi_request_lock);
}

static void mca_pml_yalla_send_request_construct(mca_pml_yalla_send_request_t* sreq)
{
    init_base_req(&sreq->super);
    init_mxm_send_req(&sreq->mxm);
    sreq->super.ompi.req_free   = mca_pml_yalla_send_request_free;
    sreq->super.ompi.req_cancel = mca_pml_yalla_send_request_cancel;
    sreq->mxm.base.context      = sreq;
    sreq->mxm.base.completed_cb = mca_pml_yalla_send_completion_cb;
}

static void mca_pml_yalla_send_request_destruct(mca_pml_yalla_send_request_t *sreq)
{
    OMPI_REQUEST_FINI(&sreq->super.ompi);
}

static void mca_pml_yalla_bsend_request_construct(mca_pml_yalla_bsend_request_t* bsreq)
{
    init_mxm_send_req(&bsreq->mxm);
    bsreq->mxm.base.context      = bsreq;
    bsreq->mxm.base.completed_cb = mca_pml_yalla_bsend_completion_cb;
}

static void mca_pml_yalla_recv_request_construct(mca_pml_yalla_recv_request_t* rreq)
{
    init_base_req(&rreq->super);
    init_mxm_recv_req(&rreq->mxm);
    rreq->super.ompi.req_free   = mca_pml_yalla_recv_request_free;
    rreq->super.ompi.req_cancel = mca_pml_yalla_recv_request_cancel;
    rreq->mxm.base.context      = rreq;
    rreq->mxm.base.completed_cb = mca_pml_yalla_recv_completion_cb;
}

static void mca_pml_yalla_recv_request_destruct(mca_pml_yalla_recv_request_t *rreq)
{
    OMPI_REQUEST_FINI(&rreq->super.ompi);
}

void mca_pml_yalla_init_reqs(void)
{
    PML_YALLA_FREELIST_INIT(&ompi_pml_yalla.send_reqs, mca_pml_yalla_send_request_t,
                            128, -1, 128);

    PML_YALLA_FREELIST_INIT(&ompi_pml_yalla.bsend_reqs, mca_pml_yalla_bsend_request_t,
                            128, -1, 128);

    PML_YALLA_FREELIST_INIT(&ompi_pml_yalla.recv_reqs, mca_pml_yalla_recv_request_t,
                            128, -1, 128);
}

OBJ_CLASS_INSTANCE(mca_pml_yalla_send_request_t,
                   ompi_request_t,
                   mca_pml_yalla_send_request_construct,
                   mca_pml_yalla_send_request_destruct);

OBJ_CLASS_INSTANCE(mca_pml_yalla_bsend_request_t,
                   opal_free_list_item_t,
                   mca_pml_yalla_bsend_request_construct,
                   NULL);

OBJ_CLASS_INSTANCE(mca_pml_yalla_recv_request_t,
                   ompi_request_t,
                   mca_pml_yalla_recv_request_construct,
                   mca_pml_yalla_recv_request_destruct);

