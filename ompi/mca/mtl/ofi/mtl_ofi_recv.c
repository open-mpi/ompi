/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/class/opal_list.h"
#include "ompi/communicator/communicator.h"
#include "ompi/datatype/ompi_datatype.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "ompi/message/message.h"

#include "mtl_ofi.h"
#include "mtl_ofi_types.h"
#include "mtl_ofi_endpoint.h"
#include "mtl_ofi_request.h"
#include "mtl_ofi_message.h"

/**
 * Called when a completion for SYNC ACK send is received.
 * This completes the synchronous recv operation. Thus, we
 * call the upper layer's completion function.
 */
static int
ompi_mtl_ofi_sync_recv_callback(struct fi_cq_tagged_entry *wc,
                                ompi_mtl_ofi_request_t *ofi_req)
{
    ofi_req->super.completion_callback(&ofi_req->super);

    return OMPI_SUCCESS;
}

/**
 * Called when a completion for a posted recv is received.
 */
static int
ompi_mtl_ofi_recv_callback(struct fi_cq_tagged_entry *wc,
                           ompi_mtl_ofi_request_t *ofi_req)
{
    int ret;
    ssize_t ret_length;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    int src;
    ompi_status_public_t *status = NULL;

    assert(ofi_req->super.ompi_req);
    status = &ofi_req->super.ompi_req->req_status;

    /**
     * Any event associated with a request starts it.
     * This prevents a started request from being cancelled.
     */
    ofi_req->req_started = true;

    status->MPI_SOURCE = MTL_OFI_GET_SOURCE(wc->tag);
    status->MPI_TAG = MTL_OFI_GET_TAG(wc->tag);
    status->_ucount = wc->len;

    if (OPAL_UNLIKELY(wc->len > ofi_req->length)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "truncate expected: %ld %ld",
                            wc->len, ofi_req->length);
        status->MPI_ERROR = MPI_ERR_TRUNCATE;
    }

    /**
     * Unpack data into recv buffer if necessary.
     */
    if (OPAL_UNLIKELY(ofi_req->buffer)) {
        ret = ompi_mtl_datatype_unpack(ofi_req->convertor,
                                       ofi_req->buffer,
                                       wc->len);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: ompi_mtl_datatype_unpack failed: %d",
                                __FILE__, __LINE__, ret);
            status->MPI_ERROR = ret;
        }
    }

    /**
     * We do not want any SYNC_SEND_ACK here!
     * See mtl_ofi_send.c for details.
     */
    assert(!MTL_OFI_IS_SYNC_SEND_ACK(wc->tag));

    /**
     * If this recv is part of an MPI_Ssend operation, then we send an
     * acknowledgment back to the sender. The fi_context can be
     * re-used safely because the previous operation has completed.
     * This recv request will complete once we get a completion for
     * this send. See ompi_mtl_ofi_sync_recv_callback().
     * Otherwise, this request is now complete.
     */
    if (OPAL_UNLIKELY(MTL_OFI_IS_SYNC_SEND(wc->tag))) {
        ofi_req->event_callback = ompi_mtl_ofi_sync_recv_callback;
        /**
         * If the recv request was posted for any source,
         * we need to extract the source's actual address.
         */
        if (!ofi_req->remote_addr) {
            src = MTL_OFI_GET_SOURCE(wc->tag);
            ompi_proc = ompi_comm_peer_lookup(ofi_req->comm, src );
            endpoint = ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
            ofi_req->remote_addr = endpoint->peer_fiaddr;
        }
	    ret_length = fi_tsend(ompi_mtl_ofi.ep,
                              NULL,
                              0,
                              ompi_mtl_ofi.mr,
                              ofi_req->remote_addr,
                              wc->tag | MTL_OFI_SYNC_SEND_ACK,
                              (void *) &ofi_req->ctx);

        if (OPAL_UNLIKELY(ret_length < 0)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: fi_tsend failed: %zd",
                                __FILE__, __LINE__, ret_length);
            status->MPI_ERROR = OMPI_ERROR;
        }
    } else {
        ofi_req->super.completion_callback(&ofi_req->super);
    }

    return OMPI_SUCCESS;
}

/**
 * Called when an error occured on a recv request.
 */
static int
ompi_mtl_ofi_recv_error_callback(struct fi_cq_err_entry *error,
                                 ompi_mtl_ofi_request_t *ofi_req)
{
    ompi_status_public_t *status;
    assert(ofi_req->super.ompi_req);
    status = &ofi_req->super.ompi_req->req_status;
    status->MPI_TAG = MTL_OFI_GET_TAG(ofi_req->match_bits);
    status->MPI_SOURCE = MTL_OFI_GET_SOURCE(ofi_req->match_bits);

    /* FIXME: This could be done on a single line... */
    switch (error->err) {
        case FI_EMSGSIZE:
            status->MPI_ERROR = MPI_ERR_TRUNCATE;
            break;
        default:
            status->MPI_ERROR = MPI_ERR_INTERN;
    }

    ofi_req->super.completion_callback(&ofi_req->super);
    return OMPI_SUCCESS;
}

int
ompi_mtl_ofi_irecv(struct mca_mtl_base_module_t *mtl,
                   struct ompi_communicator_t *comm,
                   int src,
                   int tag,
                   struct opal_convertor_t *convertor,
                   mca_mtl_request_t *mtl_request)
{
    int ret = OMPI_SUCCESS;
    ssize_t ret_length;
    uint64_t match_bits, mask_bits;
    fi_addr_t remote_addr;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;
    void *start;
    size_t length;
    bool free_after;

    if (MPI_ANY_SOURCE != src) {
        ompi_proc = ompi_comm_peer_lookup(comm, src);
        endpoint = ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
        remote_addr = endpoint->peer_fiaddr;
    } else {
        remote_addr = ompi_mtl_ofi.any_addr;
    }

    MTL_OFI_SET_RECV_BITS(match_bits, mask_bits, comm->c_contextid, src, tag);

    ret = ompi_mtl_datatype_recv_buf(convertor, &start, &length, &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ofi_req->type = OMPI_MTL_OFI_RECV;
    ofi_req->event_callback = ompi_mtl_ofi_recv_callback;
    ofi_req->error_callback = ompi_mtl_ofi_recv_error_callback;
    ofi_req->comm = comm;
    ofi_req->buffer = (free_after) ? start : NULL;
    ofi_req->length = length;
    ofi_req->convertor = convertor;
    ofi_req->req_started = false;
    ofi_req->status.MPI_ERROR = OMPI_SUCCESS;
    ofi_req->remote_addr = remote_addr;
    ofi_req->match_bits = match_bits;

    ret_length = fi_trecv(ompi_mtl_ofi.ep,
                          start,
                          length,
                          ompi_mtl_ofi.mr,
                          remote_addr,
                          match_bits,
                          mask_bits,
                          (void *)&ofi_req->ctx);

    if (OPAL_UNLIKELY(ret_length < 0)) {
        if (NULL != ofi_req->buffer) {
            free(ofi_req->buffer);
        }
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_trecv failed: %s(%zd)",
                            __FILE__, __LINE__, strerror(errno), ret_length);
        return ompi_mtl_ofi_get_error(ret);
    }

    return OMPI_SUCCESS;
}

int
ompi_mtl_ofi_imrecv(struct mca_mtl_base_module_t *mtl,
                    struct opal_convertor_t *convertor,
                    struct ompi_message_t **message,
                    struct mca_mtl_request_t *mtl_request)
{
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;
    void *start;
    size_t length;
    bool free_after;
    int ret;
    ompi_mtl_ofi_message_t *ofi_message =
        (ompi_mtl_ofi_message_t*) (*message)->req_ptr;

    ret = ompi_mtl_datatype_recv_buf(convertor, &start, &length, &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ofi_req->type = OMPI_MTL_OFI_RECV;
    ofi_req->event_callback = ompi_mtl_ofi_recv_callback;
    ofi_req->error_callback = ompi_mtl_ofi_recv_error_callback;
    ofi_req->buffer = (free_after) ? start : NULL;
    ofi_req->length = length;
    ofi_req->convertor = convertor;
    ofi_req->status.MPI_ERROR = OMPI_SUCCESS;

    (*message) = MPI_MESSAGE_NULL;

    return ompi_mtl_ofi_recv_callback(&(ofi_message->wc), ofi_req);
}
