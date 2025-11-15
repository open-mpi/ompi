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
 * @file pml_ubcl_irecv.c
 *
 * UBCL PML irecv related functions
 *
 * Functions parameters and return values defined in pml.h.
 */

#include "opal/mca/common/ubcl/common_ubcl.h"

#include "ompi/constants.h"
#include "ompi/mca/pml/pml_constants.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/message/message.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ubcl_api.h"

/**
 * Prepare a request for reception.
 */
void mca_pml_ubcl_irecv_prepare(void *buf, size_t count,
                                ompi_datatype_t *datatype, int src, int tag,
                                struct ompi_communicator_t *comm,
                                struct ompi_request_t **request,
                                bool persistent, bool probe,
                                struct ompi_message_t *message)
{
    ompi_proc_t *proc;
    mca_pml_ubcl_request_t *req;

#if defined(OPAL_ENABLE_DEBUG) && OPAL_ENABLE_DEBUG
    if (probe) {
        OPAL_OUTPUT_VERBOSE((75, mca_pml_ubcl_component.output,
                            "UBCL_MODULE_IRECV_PREPARE\n"));
    } else {
        OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                            "UBCL_MODULE_IRECV_PREPARE\n"));
    }
#endif /* OPAL_ENABLE_DEBUG */

    /* Get proc */
    if (OMPI_ANY_SOURCE != src) {
        proc = ompi_comm_peer_lookup(comm, src);
        if (OPAL_UNLIKELY(NULL == proc)) {
            mca_pml_ubcl_error(OMPI_ERROR, "Unknown proc");
        }
    } else {
        proc = NULL;
    }

    /* Allocate request and activate it */
    req = (mca_pml_ubcl_request_t *) opal_free_list_get(&mca_pml_ubcl_component.pml_req_free_list);
    if (OPAL_UNLIKELY(NULL == req)) {
        mca_pml_ubcl_error(OMPI_ERR_OUT_OF_RESOURCE,
                           "Not enough memory to allocate a recv request");
    }

    MCA_PML_UBCL_RECV_REQUEST_INIT(req, buf, count, datatype, src, tag, comm,
                                   proc, persistent, probe, message);

    /* Set user request */
    *request = &req->ompi_req;
}

/**
 * Actually start a recv request.
 */
void mca_pml_ubcl_irecv_start(struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE(
        (50, mca_pml_ubcl_component.output, "UBCL_MODULE_IRECV_START %p\n",
         (void *) *request));

    mca_pml_ubcl_request_t *req = container_of((*request),
                                              mca_pml_ubcl_request_t, ompi_req);
    void *output_buf = (void *) req->buf;

    ubcl_memory_descriptor_t rbuf_md;
    ubcl_error_t err = 0;
    size_t size;

    /* Init UBCL MD */
    err = ubcl_memory_descriptor_init(&rbuf_md);
    if (UBCL_SUCCESS != err) {
        mca_pml_ubcl_error(ubcl_error_to_ompi(err), "Failed to initialize ubcl MD");
    }
    if (pml_ubcl_request_is_cuda_buf(req)) {
        err = ubcl_memory_descriptor_set_properties(UBCL_BUF_IS_CUDA, &rbuf_md);
        if (UBCL_SUCCESS != err) {
            mca_pml_ubcl_error(ubcl_error_to_ompi(err),
                               "Failed to set MD properties, got error: %d", err);
        }
    }

    /* If we don't need to pack we can build a contiguous */
    if (! MCA_PML_UBCL_REQUEST_NEED_XPACK(req)) {
        ompi_datatype_type_size(req->datatype, &size);
        size *= req->count;

        err = ubcl_memory_descriptor_build_contiguous(output_buf, size, &rbuf_md);
        if (UBCL_SUCCESS != err) {
            mca_pml_ubcl_error(ubcl_error_to_ompi(err),
                               "Failed to build memory descriptor for output buffer");
        }
    }

    /* Always build a custom MD representation so that we have a fallback */
    err = ubcl_memory_descriptor_build_custom((void *) &req->convertor,
                                              pml_ubcl_datatype_pack,
                                              pml_ubcl_datatype_unpack,
                                              pml_ubcl_datatype_mem_size,
                                              pml_ubcl_datatype_finish,
                                              &rbuf_md);
    if (UBCL_SUCCESS != err) {
        mca_pml_ubcl_error(ubcl_error_to_ompi(err),
                           "Failed to build custom memory descriptor for input buffer");
    }

    /* Activate request */
    MCA_PML_UBCL_REQUEST_ACTIVATE(req);

    if (req->message != NULL) {
        err = ubcl_imrecv(rbuf_md, (ubcl_message_t **) &req->message,
                          (ubcl_completion_callback_fct) &ubcl_request_recv_complete_cb,
                          *request);
    } else {
        uint64_t rank;
        uint64_t cid;
        int32_t tag = req->tag;

        if (OMPI_ANY_SOURCE == req->rank) {
            rank = UBCL_ANY_SOURCE;
        } else {
            mca_common_ubcl_endpoint_t *endpoint = NULL;
            endpoint = (mca_common_ubcl_endpoint_t *) req->proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];
            rank = endpoint->rank;
        }

        cid = ompi_comm_get_local_cid(req->comm);
        ubcl_cid_t ubcl_cid = mca_pml_ubcl_compute_ubcl_cid(req->tag, cid);
        tag = req->tag;

        OPAL_OUTPUT_VERBOSE(
                (50, mca_pml_ubcl_component.output, "PML/UBCL IRECV: recv from rank=%zu\n", rank));
        err = ubcl_irecv(rbuf_md, tag, ubcl_cid, rank,
                         (ubcl_completion_callback_fct) &ubcl_request_recv_complete_cb,
                         *request, &req->ubcl_operation_handle);
    }

    if (UBCL_ERROR == err) {
        mca_pml_ubcl_error(ubcl_error_to_ompi(err), "Failed to start recv comm");
    }

    /* Optionnal call to progress */
    if (mca_pml_ubcl_component.can_progress) {
        opal_progress();
    }
}

int mca_pml_ubcl_irecv_init(void *buf, size_t count, ompi_datatype_t *datatype,
                            int src, int tag, struct ompi_communicator_t *comm,
                            struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_IRECV_INIT\n"));

    /* Create request */
    mca_pml_ubcl_irecv_prepare(buf, count, datatype, src, tag, comm, request,
                               true, false, NULL);

    return OMPI_SUCCESS;
}

/**
 * Non blocking receive primitive. Get endpoint, allocate a pml request and
 * forward to selected communication module
 */
int mca_pml_ubcl_irecv(void *buf, size_t count, ompi_datatype_t *datatype,
                       int src, int tag, struct ompi_communicator_t *comm,
                       struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_IRECV\n"));

    /* Create request and start communication */
    mca_pml_ubcl_irecv_prepare(buf, count, datatype, src, tag, comm, request,
                               false, false, NULL);
    mca_pml_ubcl_irecv_start(request);

    return OMPI_SUCCESS;
}

/**
 * Blocking receive primitive. Call non-blocking receive and wait for request
 * completion
 */
int mca_pml_ubcl_recv(void *buf, size_t count, ompi_datatype_t *datatype,
                      int src, int tag, struct ompi_communicator_t *comm,
                      ompi_status_public_t *status)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_RECV\n"));

    /* Create request and start communication */
    struct ompi_request_t *request = NULL;
    int rc = 0; /** TODO: fix return code */
    mca_pml_ubcl_irecv_prepare(buf, count, datatype, src, tag, comm, &request,
                               false, false, NULL);
    mca_pml_ubcl_irecv_start(&request);

    /* Wait for data to be received */
    ompi_request_wait_completion(request);

    mca_pml_ubcl_request_t *req = container_of(request, mca_pml_ubcl_request_t,
                                               ompi_req);
    rc = req->ompi_req.req_status.MPI_ERROR;

    if (MPI_STATUS_IGNORE != status) {
        OMPI_COPY_STATUS(status, req->ompi_req.req_status, false);
    }

    mca_pml_ubcl_request_finalize(req);

    return rc;
}

int mca_pml_ubcl_imrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                        struct ompi_message_t **message,
                        struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_IMRECV\n"));

    /* Create request and start communication */
    mca_pml_ubcl_irecv_prepare(buf, count, datatype, (*message)->peer,
                               OMPI_ANY_TAG, (*message)->comm, request,
                               false, true, (*message)->req_ptr);
    mca_pml_ubcl_irecv_start(request);
    ompi_message_return(*message);
    *message = MPI_MESSAGE_NULL;
    return OMPI_SUCCESS;
}

int mca_pml_ubcl_mrecv(void *buf, size_t count, ompi_datatype_t *datatype,
                       struct ompi_message_t **message,
                       ompi_status_public_t *status)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                        "UBCL_MODULE_MRECV\n"));

    struct ompi_request_t *request = NULL;
    int rc = 0;
    //we're matching any message tag
    mca_pml_ubcl_irecv_prepare(buf, count, datatype, (*message)->peer,
                               OMPI_ANY_TAG, (*message)->comm, &request,
                               false, true, (*message)->req_ptr);
    mca_pml_ubcl_irecv_start(&request);

    /* Wait for data to be received */
    ompi_request_wait_completion(request);

    mca_pml_ubcl_request_t *req = container_of(request, mca_pml_ubcl_request_t,
                                               ompi_req);
    rc = req->ompi_req.req_status.MPI_ERROR;

    if (MPI_STATUS_IGNORE != status) {
        OMPI_COPY_STATUS(status, req->ompi_req.req_status, false);
    }

    mca_pml_ubcl_request_finalize(req);
    ompi_message_return(*message);
    *message = MPI_MESSAGE_NULL;

    return rc;
}

