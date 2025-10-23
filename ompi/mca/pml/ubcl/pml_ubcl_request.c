/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2019-2025 Bull SAS.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/**
 * @file pml_ubcl_request.c
 *
 * UBCL PML Requests
 *
 * This file holds the MPI matching engine for the pml. It uses expected_list,
 * unexpected_list and matched_list from the mca_pml_ubcl_component
 * component. Messages come down from the pml interface (isend, irecv, iprobe)
 * and up from the communication modules through
 * mca_pml_ubcl_request_report_unexpected(). Matching is perform according to the
 * norm on the envelop (rank, tag, cid) and in posted order. Note that messages
 * on different communicators are still ordered though it is not required. It
 * would need additionnal developments.
 *
 * Function parameters and return values are defined in ompi/request/request.h.
 * Following functions are actually used but inside macros and through function
 * pointers and are not detected by cppcheck.
 */

#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"

OBJ_CLASS_INSTANCE(mca_pml_ubcl_request_t,
                   opal_free_list_item_t,
                   NULL,
                   NULL);

/**
 * Start a PML request. Find the mca_pml_ubcl_request with the given ompi_request,
 * reset it and start it.
 */
int mca_pml_ubcl_request_start(size_t count, struct ompi_request_t **requests)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "PML/UBCL REQUEST_START %zu\n", count));

    int ret = OMPI_SUCCESS;
    for (size_t i = 0; i < count; i++) {
        mca_pml_ubcl_request_t *req = container_of(requests[i], mca_pml_ubcl_request_t, ompi_req);

        /* Save callback fields if they are not ours */
        if(mca_pml_ubcl_request_complete_cb != req->ompi_req.req_complete_cb) {
            req->saved_complete_cb = req->ompi_req.req_complete_cb;
            req->saved_complete_cb_data = req->ompi_req.req_complete_cb_data;
        } else {
            /* Else reset fields in case of persistent request */
            req->saved_complete_cb = NULL;
            req->saved_complete_cb_data = NULL;
        }

        /* Reset fields if persistent request */
        OMPI_REQUEST_INIT(&req->ompi_req, req->ompi_req.req_persistent);
        req->ompi_req.req_complete_cb = mca_pml_ubcl_request_complete_cb;
        req->completed = 0;
        req->message = NULL;
        req->prematched_req = NULL;
        if (req->is_any_src) {
            req->rank = OMPI_ANY_SOURCE;
            req->proc = NULL;
            opal_convertor_cleanup(&req->convertor);
        } else {
            size_t offset = 0;
            opal_convertor_set_position(&req->convertor, &offset);
        }
        if (req->is_any_tag) {
            req->tag = OMPI_ANY_TAG;
        }

        /* Start request */
        if (MCA_PML_UBCL_REQUEST_SEND != req->type) {
            /* Recv request */
            mca_pml_ubcl_irecv_start(requests + i);
        } else {
            /* Send request */
            mca_pml_ubcl_isend_start(requests + i);
        }
    }

    return ret;
}

/**
 * Free a PML request. Find the mca_pml_bxi_request with the given ompi_request,
 * mark it as "to be freed" and finalize if already completed.
 */
int
// cppcheck-suppress unusedFunction
mca_pml_ubcl_request_free(struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "PML/UBCL REQUEST_FREE %p %p\n",
                         (void *) request, (void *) *request));

    /* Null check */
    if (MPI_REQUEST_NULL == *request) {
        return OMPI_SUCCESS;
    }

    mca_pml_ubcl_request_t *req = container_of((*request), mca_pml_ubcl_request_t, ompi_req);
    if (!REQUEST_COMPLETE(&(req)->ompi_req) || !(req)->completed) {
        /* Free called before complete : mark as "to free" */
        req->to_free = 1;
    } else {
        mca_pml_ubcl_request_finalize(req);
    }

    *request = MPI_REQUEST_NULL;

    return OMPI_SUCCESS;
}

/**
 * Cannot cancel pml requests
 */
int
// cppcheck-suppress unusedFunction
mca_pml_ubcl_request_cancel(struct ompi_request_t *request, int complete)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "PML/UBCL REQUEST_CANCEL\n"));

    mca_pml_ubcl_request_t *req = container_of(request, mca_pml_ubcl_request_t, ompi_req);
    bool success = false;
    ubcl_error_t err;

    /* This lock cannot be removed, even in thread single mode */
    opal_atomic_lock(&req->req_lock);
    switch (req->type) {
        case MCA_PML_UBCL_REQUEST_SEND:
            /* Cannot cancel send requests */
            break;
        case MCA_PML_UBCL_REQUEST_RECV:
            if (req->completed) {
                /* Cannot cancel completed requests */
                break;
            }
            if (NULL == req->ubcl_operation_handle) {
                /* We did not store operation handle, cannot cancel */
                break;
            }

            /* Try to cancel the request */
            err = ubcl_cancel(req->ubcl_operation_handle);
            if (UBCL_SUCCESS != err) {
                break;
            }

            req->completed = true;
            success = true;
            break;
    }
    opal_atomic_unlock(&req->req_lock);

    if (!success) {
        return OMPI_SUCCESS;
    }

    /* If the cancel was successfull, mark the request as cancelled and complete it */
    switch (req->type) {
        case MCA_PML_UBCL_REQUEST_SEND:
            break;
        case MCA_PML_UBCL_REQUEST_RECV:
            request->req_status._cancelled = true;
            ompi_request_complete(&(req->ompi_req), true);
            break;
    }

    return OMPI_SUCCESS;
}

int mca_pml_ubcl_request_complete(struct ompi_request_t *request)
{
    /* Null check */
    if (MPI_REQUEST_NULL == request) {
        return 0;
    }

    mca_pml_ubcl_request_t *req = container_of(request, mca_pml_ubcl_request_t, ompi_req);

    /* If we saved a callback, reset the ompi_request_t fields and call it */
    if (req->saved_complete_cb) {
        request->req_complete_cb = req->saved_complete_cb;
        request->req_complete_cb_data = req->saved_complete_cb_data;
        request->req_complete_cb(request);
    }

    if (req->to_free && req->completed) {
        OPAL_OUTPUT_VERBOSE(
            (50, mca_pml_ubcl_component.output, "PML/UBCL REQUEST_COMPLETE CALL FINALIZE"));
        mca_pml_ubcl_request_finalize(req);
        return 1;
    }

    return 0;
}

/**
 * Complete a PML request. Find the mca_pml_ubcl_request with the given
 * ompi_request, mark it as "completed" and finalize if already freed.
 */
int mca_pml_ubcl_request_complete_cb(struct ompi_request_t *request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                         "PML/UBCL REQUEST_COMPLETE CALLBACK CALLED with ompi_req=%p\n",
                         (void *) request));

    return mca_pml_ubcl_request_complete(request);
}

/* TODO: Get a pointer to status and not a cpy ? */
void ubcl_request_send_complete_cb(ubcl_status_t status, void *cb_data)
{
    if (UBCL_SUCCESS != status.status) {
        mca_pml_ubcl_error(OMPI_ERROR, "UBCL error at request completion");
    }

    ompi_request_t *request = (ompi_request_t *) cb_data;
    mca_pml_ubcl_request_t *req = container_of(request, mca_pml_ubcl_request_t, ompi_req);

    size_t dt_size;
    ompi_datatype_type_size(req->datatype, &dt_size);

    /* This lock cannot be removed, even in thread single mode */
    opal_atomic_lock(&req->req_lock);
    req->completed = 1;
    opal_atomic_unlock(&req->req_lock);
    if (req->is_buffered) {
        mca_pml_base_bsend_request_free((void*)req->buf);
        /* Bsend started completed, but could not be freed, now that UBCL is
         * done the transfer, if MPI_Wait is done, let free it */
        if (req->to_free) {
            /* MPI request has already been waited (Bsend) or freed, no one needs it anymore */
            mca_pml_ubcl_request_finalize(req);
        }
    } else {
        /* No need to set a MPI_Status on Send operations */
        /* No need to free the request: completion callbacks will do it */
        ompi_request_complete(&(req->ompi_req), true);
    }

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                         "PML/UBCL SEND_COMPLETE pml_req=%p mpi_tag=%x\n", req, req->tag));

    /** mca_pml_ubcl_request_complete((ompi_request_t *) cb_data); */
}

void ubcl_request_recv_complete_cb(ubcl_status_t status, void *cb_data)
{
    if (UBCL_SUCCESS != status.status) {
        if (UBCL_ERR_TRUNCATE == status.status) {
            if (mca_pml_ubcl_component.warn_on_truncate
                || mca_pml_ubcl_component.abort_on_truncate) {
                mca_pml_ubcl_warn(MPI_ERR_TRUNCATE, "Truncation error found during UBCL recv");
            }
            if (mca_pml_ubcl_component.abort_on_truncate) {
                ompi_mpi_abort(&ompi_mpi_comm_world.comm, MPI_ERR_TRUNCATE);
            }
        } else {
            mca_pml_ubcl_error(OMPI_ERROR, "UBCL error at request completion");
        }
    }

    ompi_request_t *request = (ompi_request_t *) cb_data;
    mca_pml_ubcl_request_t *req = container_of(request, mca_pml_ubcl_request_t, ompi_req);

    mca_common_ubcl_status_to_ompi(&request->req_status, status, req->comm, req->rank);
    if (MPI_STATUS_IGNORE != &request->req_status) {
        request->req_status.MPI_ERROR = ubcl_error_to_ompi(status.status);
    }

    /* This lock cannot be removed, even in thread single mode */
    opal_atomic_lock(&req->req_lock);
    req->completed = 1;
    opal_atomic_unlock(&req->req_lock);
    ompi_request_complete(&(req->ompi_req), true);

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                         "PML/UBCL RECV_COMPLETE pml_req=%p mpi_tag=%d\n", req, req->tag));

    /** mca_pml_ubcl_request_complete((ompi_request_t *) cb_data); */
}

/**
 * Really cleanup and free request after a call to request_free and
 * request_complete
 */
void mca_pml_ubcl_request_finalize(mca_pml_ubcl_request_t *req)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                         "PML/UBCL REQUEST_FINALIZE BEGIN pml_req=%p mpi_tag=%x\n", req, req->tag));

    opal_convertor_cleanup(&req->convertor);
    OBJ_DESTRUCT(&req->convertor);
    OMPI_REQUEST_FINI(&req->ompi_req);
    OBJ_RELEASE(req->comm);
    OMPI_DATATYPE_RELEASE(req->datatype);
    OBJ_DESTRUCT(&req->ompi_req);

    opal_free_list_return(&mca_pml_ubcl_component.pml_req_free_list, (opal_free_list_item_t *) req);

    OPAL_OUTPUT_VERBOSE(
        (50, mca_pml_ubcl_component.output, "PML/UBCL REQUEST_FINALIZED %p\n", req));
}


bool pml_ubcl_request_is_cuda_buf(mca_pml_ubcl_request_t *req) {
    if (!mca_pml_ubcl_component.accelerator_is_cuda) {
        return false;
    }

    return !!(opal_convertor_on_device(&req->convertor));
}

int mca_pml_ubcl_request_need_xpack(mca_pml_ubcl_request_t *req, ubcl_endpoint_type_t type)
{
    int need_buffer;
    int is_cuda_buffer;
    ubcl_endpoint_capabilities_t *capabilities;

    ompi_datatype_t *datatype = req->datatype;
    if (datatype->super.true_lb) {
        return 1;
    }

    need_buffer = opal_convertor_need_buffers(&req->convertor);
    is_cuda_buffer = pml_ubcl_request_is_cuda_buf(req);

    /* If cuda contiguous ptr are allowed, we don't need to pack */
    if (!need_buffer && is_cuda_buffer) {
        capabilities = &mca_pml_ubcl_component.endpoint_capabilities[type];
        /* Contiguous cuda buffer */
        if(!capabilities->allow_cuda_contig_ptr
                || mca_pml_ubcl_component.force_cuda_custom_dt) {
            /* Contiguous cuda ptr not allowed, forcing the use of pack/unpack */
            need_buffer = 1;
        }
    }

    return need_buffer;
}

void pml_ubcl_bufferize(mca_pml_ubcl_request_t *req)
{
    if (NULL == req || req->is_buffered) {
        return;
    }

    void *buffer = NULL;
    size_t dt_size, msg_size;
    ompi_datatype_type_size(req->datatype, &dt_size);
    msg_size = req->count * dt_size;

    /* TODO pack in a buffer on the same device as request buffer */
    buffer = mca_pml_base_bsend_request_alloc_buf(msg_size);
    if (NULL == buffer) {
        mca_pml_ubcl_error(OMPI_ERR_OUT_OF_RESOURCE,
                "Buffered mode but no more memory left in attached "
                "buffer\n");
        return;
    }

    struct iovec iov;
    uint32_t iov_count = 1;
    size_t max_data;
    iov.iov_len = msg_size;
    iov.iov_base = (char *) buffer;
    opal_convertor_pack(&req->convertor, &iov, &iov_count, &max_data);
    req->is_buffered = 1;
    req->count = msg_size;
    req->datatype = MPI_PACKED;
    req->buf = buffer;
    req->need_xpack = 0;

    /* Copy is done Bsend is completed. UBCL just have to do the job for real */
    /* No need to set a MPI_Status on Send operations */
    ompi_request_complete(&(req->ompi_req), true);
}
