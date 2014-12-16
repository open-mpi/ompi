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
#include "ompi/communicator/communicator.h"
#include "ompi/message/message.h"

#include "mtl_ofi.h"
#include "mtl_ofi_types.h"
#include "mtl_ofi_request.h"
#include "mtl_ofi_endpoint.h"
#include "mtl_ofi_message.h"

/**
 * Called when a probe request completes. Read fi_cq_tagged_entry's
 * data field to determine whether or not a matching message was found.
 */
static int
ompi_mtl_ofi_probe_callback(struct fi_cq_tagged_entry *wc,
                            ompi_mtl_ofi_request_t *ofi_req)
{
    if (wc->data > 0) {
        ofi_req->match_state = 1;
        ofi_req->status.MPI_SOURCE = MTL_OFI_GET_SOURCE(wc->tag);
        ofi_req->status.MPI_TAG = MTL_OFI_GET_TAG(wc->tag);
        ofi_req->status.MPI_ERROR = MPI_SUCCESS;
        ofi_req->status._ucount = wc->len;
    } else {
        ofi_req->match_state = 0;
    }
    ofi_req->completion_count--;

    return OMPI_SUCCESS;
}

/**
 * Called when a probe request encounters an error.
 */
static int
ompi_mtl_ofi_probe_error_callback(struct fi_cq_err_entry *error,
                                  ompi_mtl_ofi_request_t *ofi_req)
{
    ofi_req->super.ompi_req->req_status.MPI_ERROR = MPI_ERR_INTERN;
    ofi_req->completion_count--;
    return OMPI_SUCCESS;
}


int
ompi_mtl_ofi_iprobe(struct mca_mtl_base_module_t *mtl,
                    struct ompi_communicator_t *comm,
                    int src,
                    int tag,
                    int *flag,
                    struct ompi_status_public_t *status)
{
    struct ompi_mtl_ofi_request_t ofi_req;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    fi_addr_t remote_proc = 0;
    size_t length = 0;
    uint64_t match_bits, mask_bits;
    int ret;

    /**
     * If the source is known, use its peer_fiaddr.
     */
    if (MPI_ANY_SOURCE != src) {
        ompi_proc = ompi_comm_peer_lookup( comm, src );
        endpoint = ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
        remote_proc = endpoint->peer_fiaddr;
    }

    MTL_OFI_SET_RECV_BITS(match_bits, mask_bits, comm->c_contextid, src, tag);

    ofi_req.type = OMPI_MTL_OFI_PROBE;
    ofi_req.event_callback = ompi_mtl_ofi_probe_callback;
    ofi_req.error_callback = ompi_mtl_ofi_probe_error_callback;
    ofi_req.completion_count = 1;
    ofi_req.match_state = 0;

    ret = fi_tsearch(ompi_mtl_ofi.ep,
                     &match_bits,
                     mask_bits,
                     0,
                     &remote_proc,
                     &length,
                     (void *)&ofi_req.ctx);

    /**
     * Probe is a blocking operation. fi_tsearch() is non-blocking.
     * We inspect the return code and decide what to do.
     * The request can either:
     *     - be queued successfully,
     *     - return no matching message, or
     *     - return a matching message.
     */
    if (0 == ret) {
        /**
         * The search request was queued successfully. Wait until complete.
         */
        while (0 < ofi_req.completion_count) {
            opal_progress();
        }

        *flag = ofi_req.match_state;
        if (1 == *flag) {
            *status = ofi_req.status;
        }
    } else if (1 == ret) {
        /**
         * The search request completed and a matching message was found.
         */
        ofi_req.match_state = 1;
        ofi_req.status.MPI_SOURCE = MTL_OFI_GET_SOURCE(match_bits);
        ofi_req.status.MPI_TAG = MTL_OFI_GET_TAG(match_bits);
        ofi_req.status.MPI_ERROR = MPI_SUCCESS;
        ofi_req.status._ucount = length;
        *flag = 1;
        *status = ofi_req.status;
    } else if (ret < 0 && -FI_ENOMSG == ret) {
        /**
         * The search request completed but no matching message was found.
         */
        *flag = 0;
    } else if (ret < 0 && ret != -FI_ENOMSG) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_tsearch failed: %d (%s)",
                            __FILE__, __LINE__, ret, fi_strerror(ret));
        return ompi_mtl_ofi_get_error(-ret);
    } else {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: unexpected return code from fi_tsearch: %d",
                            __FILE__, __LINE__, ret);
        return ompi_mtl_ofi_get_error(-ret);
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_ofi_improbe(struct mca_mtl_base_module_t *mtl,
                     struct ompi_communicator_t *comm,
                     int src,
                     int tag,
                     int *matched,
                     struct ompi_message_t **message,
                     struct ompi_status_public_t *status)
{
    struct ompi_mtl_ofi_request_t ofi_req;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    fi_addr_t remote_proc = 0;
    size_t length = 0;
    uint64_t match_bits, mask_bits;
    int ret;

    /**
     * If the source is known, use its peer_fiaddr.
     */
    if (MPI_ANY_SOURCE != src) {
        ompi_proc = ompi_comm_peer_lookup( comm, src );
        endpoint = ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];
        remote_proc = endpoint->peer_fiaddr;
    }

    MTL_OFI_SET_RECV_BITS(match_bits, mask_bits, comm->c_contextid, src, tag);

    ofi_req.type = OMPI_MTL_OFI_PROBE;
    ofi_req.event_callback = ompi_mtl_ofi_probe_callback;
    ofi_req.error_callback = ompi_mtl_ofi_probe_error_callback;
    ofi_req.completion_count = 1;
    ofi_req.match_state = 0;

    ret = fi_tsearch(ompi_mtl_ofi.ep,
                     &match_bits,
                     mask_bits,
                     FI_CLAIM,
                     &remote_proc,
                     &length,
                     (void *)&ofi_req.ctx);

    /**
     * Probe is a blocking operation. fi_tsearch() is non-blocking.
     * We inspect the return code and decide what to do.
     * The request can either:
     *     - be queued successfully,
     *     - return no matching message, or
     *     - return a matching message.
     */
    if (ret == 0) {
        /**
         * The search request was queued successfully. Wait until complete.
         */
        while (0 < ofi_req.completion_count) {
            opal_progress();
        }
        *matched = ofi_req.match_state;
        if (1 == *matched) {
            *status = ofi_req.status;

            (*message) = ompi_message_alloc();
            if (NULL == (*message)) {
                return OMPI_ERR_OUT_OF_RESOURCE;
            }

            (*message)->comm = comm;
            (*message)->req_ptr = ofi_req.message;
            (*message)->peer = status->MPI_SOURCE;
            (*message)->count = status->_ucount;

            if (NULL == (*message)->req_ptr) {
                ompi_message_return(*message);
                *message = NULL;
                return OMPI_ERR_OUT_OF_RESOURCE;
            }
        } else {
            (*message) = MPI_MESSAGE_NULL;
        }
    } else if (1 == ret) {
        /**
         * The search request completed and a matching message was found.
         */
        *matched = 1;
        *status = ofi_req.status;
        ofi_req.match_state = 1;

        (*message) = ompi_message_alloc();
        if (NULL == (*message)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        (*message)->comm = comm;
        (*message)->req_ptr = ofi_req.message;
        (*message)->peer = status->MPI_SOURCE;
        (*message)->count = status->_ucount;

        if (NULL == (*message)->req_ptr) {
            ompi_message_return(*message);
            *message = NULL;
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
    } else if (ret < 0 && ret == -FI_ENOMSG) {
        /**
         * The search request completed but no matching message was found.
         */
        *matched = 0;
    } else if (ret < 0 && ret != -FI_ENOMSG) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_tsearch failed: %d (%s)",
                            __FILE__, __LINE__, ret, fi_strerror(ret));
        return ompi_mtl_ofi_get_error(-ret);
    } else {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: unexpected return code from fi_tsearch: %d",
                            __FILE__, __LINE__, ret);
        return ompi_mtl_ofi_get_error(-ret);
    }

    return OMPI_SUCCESS;
}
