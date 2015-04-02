/*
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_OFI_H_HAS_BEEN_INCLUDED
#define MTL_OFI_H_HAS_BEEN_INCLUDED

#include "ompi/mca/pml/pml.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "opal/datatype/opal_convertor.h"
#include "opal/mca/pmix/pmix.h"

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_tagged.h>

#include "ompi_config.h"
#include "ompi/proc/proc.h"
#include "ompi/mca/mtl/mtl.h"
#include "opal/class/opal_list.h"
#include "ompi/communicator/communicator.h"
#include "opal/datatype/opal_convertor.h"
#include "ompi/mca/mtl/base/base.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "ompi/message/message.h"

#include "mtl_ofi.h"
#include "mtl_ofi_types.h"
#include "mtl_ofi_request.h"
#include "mtl_ofi_endpoint.h"
#include "mtl_ofi_message.h"

BEGIN_C_DECLS

extern mca_mtl_ofi_module_t ompi_mtl_ofi;
extern mca_base_framework_t ompi_mtl_base_framework;

extern int ompi_mtl_ofi_add_procs(struct mca_mtl_base_module_t *mtl,
                                  size_t nprocs,
                                  struct ompi_proc_t **procs);

extern int ompi_mtl_ofi_del_procs(struct mca_mtl_base_module_t *mtl,
                                  size_t nprocs,
                                  struct ompi_proc_t **procs);

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_progress(void)
{
    int ret, count = 0;
    struct fi_cq_tagged_entry wc;
    struct fi_cq_err_entry error;
    ompi_mtl_ofi_request_t *ofi_req = NULL;

    /**
     * Read the work completions from the CQ.
     * From the completion's op_context, we get the associated OFI request.
     * Call the request's callback.
     */
    while (true) {
        memset(&wc, 0, sizeof(wc));
        ret = fi_cq_read(ompi_mtl_ofi.cq, (void *)&wc, 1);
        if (ret > 0) {
            count++;
            if (NULL != wc.op_context) {
                ofi_req = TO_OFI_REQ(wc.op_context);
                assert(ofi_req);
                ret = ofi_req->event_callback(&wc, ofi_req);
                if (OMPI_SUCCESS != ret) {
                    opal_output(ompi_mtl_base_framework.framework_output,
                                "Error returned by request event callback: %d",
                                ret);
                    abort();
                }
            }
        } else if (ret == -FI_EAVAIL) {
            /**
             * An error occured and is being reported via the CQ.
             * Read the error and forward it to the upper layer.
             */
            memset(&error, 0, sizeof(error));
            ret = fi_cq_readerr(ompi_mtl_ofi.cq,
                                &error,
                                0);
            if (ret) {
                opal_output(ompi_mtl_base_framework.framework_output,
                            "Error returned from fi_cq_readerr: %d", ret);
            }

            assert(error.op_context);
            ofi_req = TO_OFI_REQ(error.op_context);
            assert(ofi_req);
            ret = ofi_req->error_callback(&error, ofi_req);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_mtl_base_framework.framework_output,
                        "Error returned by request error callback: %d",
                        ret);
                abort();
            }
        } else {
            /**
             * The CQ is empty. Return.
             */
            break;
        }
    }
    return count;
}


/* MTL interface functions */
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_finalize(struct mca_mtl_base_module_t *mtl)
{
    opal_progress_unregister(ompi_mtl_ofi_progress);

    /**
     * Close all the OFI objects
     */
    if (fi_close((fid_t)ompi_mtl_ofi.ep)) {
        opal_output(ompi_mtl_base_framework.framework_output,
                "fi_close failed: %s", strerror(errno));
        abort();
    }
    if (fi_close((fid_t)ompi_mtl_ofi.mr)) {
        opal_output(ompi_mtl_base_framework.framework_output,
                "fi_close failed: %s", strerror(errno));
        abort();
    }
    if (fi_close((fid_t)ompi_mtl_ofi.cq)) {
        opal_output(ompi_mtl_base_framework.framework_output,
                "fi_close failed: %s", strerror(errno));
        abort();
    }
    if (fi_close((fid_t)ompi_mtl_ofi.av)) {
        opal_output(ompi_mtl_base_framework.framework_output,
                "fi_close failed: %s", strerror(errno));
        abort();
    }
    if (fi_close((fid_t)ompi_mtl_ofi.domain)) {
        opal_output(ompi_mtl_base_framework.framework_output,
                "fi_close failed: %s", strerror(errno));
        abort();
    }
    if (fi_close((fid_t)ompi_mtl_ofi.fabric)) {
        opal_output(ompi_mtl_base_framework.framework_output,
                "fi_close failed: %s", strerror(errno));
        abort();
    }

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_get_error(int error_num)
{
    int ret;

    switch (error_num) {
    case 0:
        ret = OMPI_SUCCESS;
        break;
    default:
        ret = OMPI_ERROR;
    }

    return ret;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_callback(struct fi_cq_tagged_entry *wc,
                           ompi_mtl_ofi_request_t *ofi_req)
{
    assert(ofi_req->completion_count > 0);
    ofi_req->completion_count--;
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_error_callback(struct fi_cq_err_entry *error,
                                 ompi_mtl_ofi_request_t *ofi_req)
{
    switch(error->err) {
        case FI_EMSGSIZE:
            ofi_req->status.MPI_ERROR = MPI_ERR_TRUNCATE;
            break;
        default:
            ofi_req->status.MPI_ERROR = MPI_ERR_INTERN;
    }
    return ofi_req->event_callback(NULL, ofi_req);
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_ack_callback(struct fi_cq_tagged_entry *wc,
                               ompi_mtl_ofi_request_t *ofi_req)
{
    ompi_mtl_ofi_request_t *parent_req = ofi_req->parent;

    free(ofi_req);

    parent_req->event_callback(NULL, parent_req);

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_ack_error_callback(struct fi_cq_err_entry *error,
                                     ompi_mtl_ofi_request_t *ofi_req)
{
    ompi_mtl_ofi_request_t *parent_req = ofi_req->parent;

    free(ofi_req);

    parent_req->status.MPI_ERROR = MPI_ERR_INTERN;

    return parent_req->error_callback(error, parent_req);
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_isend_callback(struct fi_cq_tagged_entry *wc,
                            ompi_mtl_ofi_request_t *ofi_req)
{
    assert(ofi_req->completion_count > 0);
    ofi_req->completion_count--;

    if (0 == ofi_req->completion_count) {
        /* Request completed */
        if (OPAL_UNLIKELY(NULL != ofi_req->buffer)) {
            free(ofi_req->buffer);
            ofi_req->buffer = NULL;
        }

        ofi_req->super.ompi_req->req_status.MPI_ERROR =
            ofi_req->status.MPI_ERROR;

        ofi_req->super.completion_callback(&ofi_req->super);
    }

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_start(struct mca_mtl_base_module_t *mtl,
                        struct ompi_communicator_t *comm,
                        int dest,
                        int tag,
                        struct opal_convertor_t *convertor,
                        mca_pml_base_send_mode_t mode,
                        ompi_mtl_ofi_request_t *ofi_req)
{
    int ret;
    void *start;
    size_t length;
    ssize_t ret_length;
    bool free_after;
    uint64_t match_bits;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    ompi_mtl_ofi_request_t *ack_req = NULL; /* For synchronous send */

    ompi_proc = ompi_comm_peer_lookup(comm, dest);
    endpoint = ompi_proc->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL];

    ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ret) return ret;

    ofi_req->buffer = (free_after) ? start : NULL;
    ofi_req->length = length;
    ofi_req->status.MPI_ERROR = OMPI_SUCCESS;

    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_SYNCHRONOUS == mode)) {
        ack_req = malloc(sizeof(ompi_mtl_ofi_request_t));
        assert(ack_req);
        ack_req->parent = ofi_req;
        ack_req->event_callback = ompi_mtl_ofi_send_ack_callback;
        ack_req->error_callback = ompi_mtl_ofi_send_ack_error_callback;

        ofi_req->completion_count = 2;
        MTL_OFI_SET_SEND_BITS(match_bits, comm->c_contextid,
                              comm->c_my_rank, tag, MTL_OFI_SYNC_SEND);
        ret_length = fi_trecv(ompi_mtl_ofi.ep,
                              NULL,
                              0,
                              ompi_mtl_ofi.mr,
                              endpoint->peer_fiaddr,
                              match_bits | MTL_OFI_SYNC_SEND_ACK,
                              0, /* Exact match, no ignore bits */
                              (void *) &ack_req->ctx);
        if (OPAL_UNLIKELY(ret_length < 0)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: fi_trecv failed: %s(%zd)",
                                __FILE__, __LINE__,
                                strerror(errno), ret_length);
            return ompi_mtl_ofi_get_error(ret);
        }
    } else {
        ofi_req->completion_count = 1;
        MTL_OFI_SET_SEND_BITS(match_bits, comm->c_contextid,
                              comm->c_my_rank, tag, 0);
    }

    ret_length = fi_tsend(ompi_mtl_ofi.ep,
                          start,
                          length,
                          ompi_mtl_ofi.mr,
                          endpoint->peer_fiaddr,
                          match_bits,
                          (void *) &ofi_req->ctx);

    if (OPAL_UNLIKELY(0 > ret_length)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_tsend failed: %zd",
                            __FILE__, __LINE__, ret_length);
        return ompi_mtl_ofi_get_error(ret);
    }

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode)
{
    int ret = OMPI_SUCCESS;
    ompi_mtl_ofi_request_t ofi_req;

    /**
     * Create a send request, start it and wait until it completes.
     */
    ofi_req.event_callback = ompi_mtl_ofi_send_callback;
    ofi_req.error_callback = ompi_mtl_ofi_send_error_callback;

    ret = ompi_mtl_ofi_send_start(mtl, comm, dest, tag,
                                  convertor, mode, &ofi_req);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        if (NULL != ofi_req.buffer) {
            free(ofi_req.buffer);
        }
        return ret;
    }

    /**
     * Wait until the request is completed.
     * ompi_mtl_ofi_send_callback() updates this variable.
     */
    while (0 < ofi_req.completion_count) {
        ompi_mtl_ofi_progress();
    }

    if (OPAL_UNLIKELY(NULL != ofi_req.buffer)) {
        free(ofi_req.buffer);
    }

    return ofi_req.status.MPI_ERROR;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_isend(struct mca_mtl_base_module_t *mtl,
                   struct ompi_communicator_t *comm,
                   int dest,
                   int tag,
                   struct opal_convertor_t *convertor,
                   mca_pml_base_send_mode_t mode,
                   bool blocking,
                   mca_mtl_request_t *mtl_request)
{
    int ret = OMPI_SUCCESS;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;

    ofi_req->event_callback = ompi_mtl_ofi_isend_callback;
    ofi_req->error_callback = ompi_mtl_ofi_send_error_callback;

    ret = ompi_mtl_ofi_send_start(mtl, comm, dest, tag,
                                  convertor, mode, ofi_req);

    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret && NULL != ofi_req->buffer)) {
        free(ofi_req->buffer);
    }

    return ret;
}

/**
 * Called when a completion for SYNC ACK send is received.
 * This completes the synchronous recv operation. Thus, we
 * call the upper layer's completion function.
 */
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_sync_recv_callback(struct fi_cq_tagged_entry *wc,
                                ompi_mtl_ofi_request_t *ofi_req)
{
    ofi_req->super.completion_callback(&ofi_req->super);

    return OMPI_SUCCESS;
}

/**
 * Called when a completion for a posted recv is received.
 */
__opal_attribute_always_inline__ static inline int
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
__opal_attribute_always_inline__ static inline int
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

__opal_attribute_always_inline__ static inline int
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


__opal_attribute_always_inline__ static inline int
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

/**
 * Called when a probe request completes. Read fi_cq_tagged_entry's
 * data field to determine whether or not a matching message was found.
 */
__opal_attribute_always_inline__ static inline int
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
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_probe_error_callback(struct fi_cq_err_entry *error,
                                  ompi_mtl_ofi_request_t *ofi_req)
{
    ofi_req->super.ompi_req->req_status.MPI_ERROR = MPI_ERR_INTERN;
    ofi_req->completion_count--;
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
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

__opal_attribute_always_inline__ static inline int
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

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_cancel(struct mca_mtl_base_module_t *mtl,
                    mca_mtl_request_t *mtl_request,
                    int flag)
{
    int ret;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;

    switch (ofi_req->type) {
        case OMPI_MTL_OFI_SEND:
            /**
             * Cannot cancel sends yet
             */
            break;

        case OMPI_MTL_OFI_RECV:
            /**
             * Cancel a receive request only if it hasn't been matched yet.
             * The event queue needs to be drained to make sure there isn't
             * any pending receive completion event.
             */
            ompi_mtl_ofi_progress();

            if (!ofi_req->req_started) {
                ret = fi_cancel((fid_t)ompi_mtl_ofi.ep, &ofi_req->ctx);
                if (0 == ret) {
                    /**
                     * The request was successfully cancelled.
                     */
                    ofi_req->super.ompi_req->req_status._cancelled = true;
                    ofi_req->super.completion_callback(&ofi_req->super);
                }
            }
            break;

        default:
            return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_add_comm(struct mca_mtl_base_module_t *mtl,
                      struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_del_comm(struct mca_mtl_base_module_t *mtl,
                      struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}


END_C_DECLS

#endif  /* MTL_OFI_H_HAS_BEEN_INCLUDED */
