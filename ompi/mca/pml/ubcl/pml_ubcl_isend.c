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
 * @file pml_ubcl_isend.c
 *
 * PML/UBCL isend related functions
 *
 * Functions parameters and return values defined in ompi/mca/pml/pml.h.
 */

#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/pml/base/pml_base_bsend.h"
#include "ompi/mca/pml/ubcl/pml_ubcl.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_request.h"
#include "ompi/mca/pml/ubcl/pml_ubcl_utils.h"
#include "ompi/request/request.h"
#include "ompi/mca/common/ubcl/common_ubcl.h"
#include "opal/mca/common/ubcl/common_ubcl.h"
#include "ubcl_api.h"

static inline void get_ubcl_send_mode(mca_pml_base_send_mode_t mode, ubcl_send_mode_t *send_mode)
{
    switch(mode) {
        case MCA_PML_BASE_SEND_SYNCHRONOUS:
            *send_mode = UBCL_SEND_MODE_SYNCHRONOUS;
            break;
        case MCA_PML_BASE_SEND_READY:
            *send_mode = UBCL_SEND_MODE_READY;
            break;
        case MCA_PML_BASE_SEND_BUFFERED:
            *send_mode = UBCL_SEND_MODE_BUFFERED;
            break;
        /* Other modes not yet supported in UBCL */
        default:
            *send_mode = UBCL_SEND_MODE_STANDARD;
            break;
    }
}

/**
 * Prepare a request for sending and perform actions according to send mode.
 *
 * Send modes:
 *   - BUFFERED = Use a specific user-defined buffer to store buf and return.
 *     See buffer_attach/detach
 *   - READY = User tells us that matching receive has already been posted by peer
 *   - SYNCHRONOUS = Return only when peer has begun to receive
 *   - STANDARD = BUFFERED or SYNCHRONOUS (up to pml to decide)
 *
 * By default READY is equivalent to STANDARD, except if checks are enabled by
 * MCA: then receiver may print a warning or an error.
 * SYNCHRONOUS forces STANDARD rendezvous protocols.
 */
static inline void mca_pml_ubcl_isend_prepare(const void *buf, size_t count,
                                              ompi_datatype_t *datatype, int dst, int tag,
                                              mca_pml_base_send_mode_t mode,
                                              struct ompi_communicator_t *comm,
                                              struct ompi_request_t **request, bool persistent)
{
    ompi_proc_t *proc;
    mca_pml_ubcl_request_t *req;

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_ISEND_PREPARE\n"));

    /* Get proc */
    proc = ompi_comm_peer_lookup(comm, dst);
    if (OPAL_UNLIKELY(NULL == proc)) {
        mca_pml_ubcl_error(OMPI_ERROR, "Unknown proc");
    }

    /* Allocate request */
    req = (mca_pml_ubcl_request_t *) opal_free_list_get(&mca_pml_ubcl_component.pml_req_free_list);
    if (OPAL_UNLIKELY(NULL == req)) {
        mca_pml_ubcl_error(OMPI_ERR_OUT_OF_RESOURCE, "Not enough memory to allocate a PML request");
    }

    /* TODO: Find out what can be simplified in this macro and request structure */
    MCA_PML_UBCL_SEND_REQUEST_INIT(req, buf, count, datatype, dst, tag, mode, comm, proc,
                                   persistent);

    /* Set user request */
    *request = &req->ompi_req;
}

/**
 * Actually start a send request and perform actions according to send mode.
 */
void mca_pml_ubcl_isend_start(struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE(
        (50, mca_pml_ubcl_component.output, "UBCL_MODULE_ISEND_START %p\n", *request));

    mca_pml_ubcl_request_t *req = container_of((*request), mca_pml_ubcl_request_t, ompi_req);

    char *input_buf = NULL;
    mca_common_ubcl_endpoint_t *endpoint = NULL;
    ubcl_memory_descriptor_t sbuf_md;
    ubcl_error_t err = 0;
    ubcl_send_mode_t send_mode;
    uint64_t cid;
    int32_t tag = req->tag;
    ubcl_cid_t ubcl_cid;

    /* Activate request */
    MCA_PML_UBCL_REQUEST_ACTIVATE(req);

    if (MCA_PML_BASE_SEND_BUFFERED == req->mode) {
        pml_ubcl_bufferize(req);
    }
    get_ubcl_send_mode(req->mode, &send_mode);

    input_buf = (char*) req->buf;

    /* Retrieve endpoint and compute overall message size */
    endpoint = (mca_common_ubcl_endpoint_t *) req->proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_PML];

    /* Init UBCL MD */
    err = ubcl_memory_descriptor_init(&sbuf_md);
    if (UBCL_SUCCESS != err) {
        mca_pml_ubcl_error(ubcl_error_to_ompi(err), "Failed to initialize ubcl MD");
    }
    if (pml_ubcl_request_is_cuda_buf(req)) {
        err = ubcl_memory_descriptor_set_properties(UBCL_BUF_IS_CUDA, &sbuf_md);
        if (UBCL_SUCCESS != err) {
            mca_pml_ubcl_error(ubcl_error_to_ompi(err),
                               "Failed to set MD properties, got error: %d", err);
        }
    }

    /* If we don't need to pack we can build a contiguous */
    if (! MCA_PML_UBCL_REQUEST_NEED_XPACK(req)) {
        ptrdiff_t gap = 0;
        size_t span = opal_datatype_span(&req->datatype->super, req->count, &gap);
        err = ubcl_memory_descriptor_build_contiguous(input_buf+gap, span, &sbuf_md);
        if (UBCL_SUCCESS != err) {
            mca_pml_ubcl_error(ubcl_error_to_ompi(err),
                               "Failed to build contiguous memory descriptor for input buffer");
        }
    }

    /* Always build a custom MD representation so that we have a fallback */
    err = ubcl_memory_descriptor_build_custom((void *) &req->convertor,
                                              pml_ubcl_datatype_pack,
                                              pml_ubcl_datatype_unpack,
                                              pml_ubcl_datatype_mem_size,
                                              pml_ubcl_datatype_finish, &sbuf_md);
    if (UBCL_SUCCESS != err) {
        mca_pml_ubcl_error(ubcl_error_to_ompi(err),
                           "Failed to build custom memory descriptor for input buffer");
    }

    cid = ompi_comm_get_local_cid(req->comm);
    ubcl_cid = mca_pml_ubcl_compute_ubcl_cid(req->tag, cid);

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output,
                         "PML/UBCL ISEND: send mpi_tag=%x comm_id=%zu\n", tag, ubcl_cid.bits));
    OPAL_OUTPUT_VERBOSE(
        (50, mca_pml_ubcl_component.output, "PML/UBCL ISEND: ompi_req=%p\n", *request));
    OPAL_OUTPUT_VERBOSE(
        (50, mca_pml_ubcl_component.output, "PML/UBCL ISEND: sending to rank=%zu\n", endpoint->rank));

    err = ubcl_isend(sbuf_md, tag, ubcl_cid, endpoint->rank, send_mode,
                     (ubcl_completion_callback_fct) &ubcl_request_send_complete_cb,
                     *request, &req->ubcl_operation_handle);
    if (UBCL_ERROR == err) {
        mca_pml_ubcl_error(ubcl_error_to_ompi(err), "Failed to send data");
    }

    /* Optionnal call to progress */
    if (mca_pml_ubcl_component.can_progress) {
        opal_progress();
    }
}

/**
 * Initialize a permanent send request
 */
int mca_pml_ubcl_isend_init(const void *buf, size_t count, ompi_datatype_t *datatype, int dst,
                            int tag, mca_pml_base_send_mode_t mode,
                            struct ompi_communicator_t *comm, struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_ISEND_INIT\n"));

    /* Create request */
    mca_pml_ubcl_isend_prepare(buf, count, datatype, dst, tag, mode, comm, request, true);

    return OMPI_SUCCESS;
}

/**
 * Non-blocking send primitive. Return to user as soon as possible after the
 * communication is started.
 */
int mca_pml_ubcl_isend(const void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                       mca_pml_base_send_mode_t mode, struct ompi_communicator_t *comm,
                       struct ompi_request_t **request)
{
    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_ISEND\n"));

    /* Create request and start communication */
    mca_pml_ubcl_isend_prepare(buf, count, datatype, dst, tag, mode, comm, request, false);
    mca_pml_ubcl_isend_start(request);

    return OMPI_SUCCESS;
}

/**
 * Blocking send primitive. Return only when buffer can be reused by user
 * (i.e. either dest has received all or we buffered).
 */
int mca_pml_ubcl_send(const void *buf, size_t count, ompi_datatype_t *datatype, int dst, int tag,
                      mca_pml_base_send_mode_t mode, struct ompi_communicator_t *comm)
{
    int ret;
    mca_pml_ubcl_request_t *request = NULL;
    struct ompi_request_t *ompi_request;

    OPAL_OUTPUT_VERBOSE((50, mca_pml_ubcl_component.output, "UBCL_MODULE_SEND\n"));

    ret = mca_pml_ubcl_isend(buf, count, datatype, dst, tag, mode, comm, &ompi_request);
    if (OMPI_SUCCESS != ret || NULL == ompi_request) {
        return ret;
    }

    request = container_of(ompi_request, mca_pml_ubcl_request_t, ompi_req);

    if (MCA_PML_BASE_SEND_BUFFERED == mode) {
        /* MPI specification: Bsend is local, no information about the remote.
         * PML/BXI always buffers Bsend data. No need to wait request completion */
        request->to_free = 1;
    } else {
        ompi_request_wait_completion(ompi_request);
        mca_pml_ubcl_request_finalize(request);
    }

    return OMPI_SUCCESS;
}
