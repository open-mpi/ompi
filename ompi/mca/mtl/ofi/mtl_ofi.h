/*
 * Copyright (c) 2013-2016 Intel, Inc. All rights reserved
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_OFI_H_HAS_BEEN_INCLUDED
#define MTL_OFI_H_HAS_BEEN_INCLUDED

#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "opal/datatype/opal_convertor.h"

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

#include "mtl_ofi_types.h"
#include "mtl_ofi_request.h"
#include "mtl_ofi_endpoint.h"
#include "mtl_ofi_compat.h"

#define MTL_OFI_RETRY_UNTIL_DONE(FUNC)         \
    do {                                       \
        do {                                   \
            ret = FUNC;                        \
            if(OPAL_LIKELY(0 == ret)) {break;} \
        } while(-FI_EAGAIN == ret);            \
    } while(0);

BEGIN_C_DECLS

extern mca_mtl_ofi_module_t ompi_mtl_ofi;
extern mca_base_framework_t ompi_mtl_base_framework;

extern int ompi_mtl_ofi_del_procs(struct mca_mtl_base_module_t *mtl,
                                  size_t nprocs,
                                  struct ompi_proc_t **procs);

int ompi_mtl_ofi_progress_no_inline(void);

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_progress(void)
{
    ssize_t ret;
    int count = 0;
    struct fi_cq_tagged_entry wc = { 0 };
    struct fi_cq_err_entry error = { 0 };
    ompi_mtl_ofi_request_t *ofi_req = NULL;

    /**
     * Read the work completions from the CQ.
     * From the completion's op_context, we get the associated OFI request.
     * Call the request's callback.
     */
    while (true) {
        ret = fi_cq_read(ompi_mtl_ofi.cq, (void *)&wc, 1);
        if (ret > 0) {
            count++;
            if (NULL != wc.op_context) {
                ofi_req = TO_OFI_REQ(wc.op_context);
                assert(ofi_req);
                ret = ofi_req->event_callback(&wc, ofi_req);
                if (OMPI_SUCCESS != ret) {
                    opal_output(ompi_mtl_base_framework.framework_output,
                                "Error returned by request event callback: %zd",
                                ret);
                    abort();
                }
            }
        } else if (ret == -FI_EAVAIL) {
            /**
             * An error occured and is being reported via the CQ.
             * Read the error and forward it to the upper layer.
             */
            ret = fi_cq_readerr(ompi_mtl_ofi.cq,
                                &error,
                                0);
            if (0 > ret) {
                opal_output(ompi_mtl_base_framework.framework_output,
                            "Error returned from fi_cq_readerr: %zd", ret);
                abort();
            }

            assert(error.op_context);
            ofi_req = TO_OFI_REQ(error.op_context);
            assert(ofi_req);
            ret = ofi_req->error_callback(&error, ofi_req);
            if (OMPI_SUCCESS != ret) {
                opal_output(ompi_mtl_base_framework.framework_output,
                        "Error returned by request error callback: %zd",
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
int ompi_mtl_ofi_finalize(struct mca_mtl_base_module_t *mtl);

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
        case FI_ETRUNC:
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
    int ompi_ret;
    void *start;
    size_t length;
    ssize_t ret;
    bool free_after;
    uint64_t match_bits;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    ompi_mtl_ofi_request_t *ack_req = NULL; /* For synchronous send */

    ompi_proc = ompi_comm_peer_lookup(comm, dest);
    endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);

    ompi_ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OMPI_SUCCESS != ompi_ret) return ompi_ret;

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
        MTL_OFI_RETRY_UNTIL_DONE(fi_trecv(ompi_mtl_ofi.ep,
                                          NULL,
                                          0,
                                          NULL,
                                          endpoint->peer_fiaddr,
                                          match_bits | MTL_OFI_SYNC_SEND_ACK,
                                          0, /* Exact match, no ignore bits */
                                          (void *) &ack_req->ctx));
        if (OPAL_UNLIKELY(0 > ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: fi_trecv failed: %s(%zd)",
                                __FILE__, __LINE__, fi_strerror(-ret), ret);
            free(ack_req);
            return ompi_mtl_ofi_get_error(ret);
        }
    } else {
        ofi_req->completion_count = 1;
        MTL_OFI_SET_SEND_BITS(match_bits, comm->c_contextid,
                              comm->c_my_rank, tag, 0);
    }

    if (ompi_mtl_ofi.max_inject_size >= length) {
        MTL_OFI_RETRY_UNTIL_DONE(fi_tinject(ompi_mtl_ofi.ep,
                                            start,
                                            length,
                                            endpoint->peer_fiaddr,
                                            match_bits));
        if (OPAL_UNLIKELY(0 > ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: fi_tinject failed: %s(%zd)",
                                __FILE__, __LINE__, fi_strerror(-ret), ret);
            if (ack_req) {
                fi_cancel((fid_t)ompi_mtl_ofi.ep, &ack_req->ctx);
                free(ack_req);
            }
            return ompi_mtl_ofi_get_error(ret);
        }

        ofi_req->event_callback(NULL,ofi_req);
    } else {
        MTL_OFI_RETRY_UNTIL_DONE(fi_tsend(ompi_mtl_ofi.ep,
                                          start,
                                          length,
                                          NULL,
                                          endpoint->peer_fiaddr,
                                          match_bits,
                                          (void *) &ofi_req->ctx));
        if (OPAL_UNLIKELY(0 > ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: fi_tsend failed: %s(%zd)",
                                __FILE__, __LINE__, fi_strerror(-ret), ret);
            return ompi_mtl_ofi_get_error(ret);
        }
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
    int ompi_ret;
    ssize_t ret;
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
        ompi_ret = ompi_mtl_datatype_unpack(ofi_req->convertor,
                                            ofi_req->buffer,
                                            wc->len);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ompi_ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: ompi_mtl_datatype_unpack failed: %d",
                                __FILE__, __LINE__, ompi_ret);
            status->MPI_ERROR = ompi_ret;
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
        if (ompi_mtl_ofi.any_addr == ofi_req->remote_addr) {
            src = MTL_OFI_GET_SOURCE(wc->tag);
            ompi_proc = ompi_comm_peer_lookup(ofi_req->comm, src);
            endpoint = ompi_mtl_ofi_get_endpoint(ofi_req->mtl, ompi_proc);
            ofi_req->remote_addr = endpoint->peer_fiaddr;
        }
	    MTL_OFI_RETRY_UNTIL_DONE(fi_tsend(ompi_mtl_ofi.ep,
                                          NULL,
                                          0,
                                          NULL,
                                          ofi_req->remote_addr,
                                          wc->tag | MTL_OFI_SYNC_SEND_ACK,
                                          (void *) &ofi_req->ctx));
        if (OPAL_UNLIKELY(0 > ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: fi_tsend failed: %s(%zd)",
                                __FILE__, __LINE__, fi_strerror(-ret), ret);
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

    switch (error->err) {
        case FI_ETRUNC:
            status->MPI_ERROR = MPI_ERR_TRUNCATE;
            break;
        case FI_ECANCELED:
            status->_cancelled = true;
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
    int ompi_ret = OMPI_SUCCESS;
    ssize_t ret;
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
        endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);
        remote_addr = endpoint->peer_fiaddr;
    } else {
        remote_addr = ompi_mtl_ofi.any_addr;
    }

    MTL_OFI_SET_RECV_BITS(match_bits, mask_bits, comm->c_contextid, src, tag);

    ompi_ret = ompi_mtl_datatype_recv_buf(convertor,
                                          &start,
                                          &length,
                                          &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ompi_ret)) {
        return ompi_ret;
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

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecv(ompi_mtl_ofi.ep,
                                      start,
                                      length,
                                      NULL,
                                      remote_addr,
                                      match_bits,
                                      mask_bits,
                                      (void *)&ofi_req->ctx));
    if (OPAL_UNLIKELY(0 > ret)) {
        if (NULL != ofi_req->buffer) {
            free(ofi_req->buffer);
        }
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_trecv failed: %s(%zd)",
                            __FILE__, __LINE__, fi_strerror(-ret), ret);
        return ompi_mtl_ofi_get_error(ret);
    }

    return OMPI_SUCCESS;
}

/**
 * Called when a mrecv request completes.
 */
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_mrecv_callback(struct fi_cq_tagged_entry *wc,
                            ompi_mtl_ofi_request_t *ofi_req)
{
    struct mca_mtl_request_t *mrecv_req = ofi_req->mrecv_req;
    ompi_status_public_t *status = &mrecv_req->ompi_req->req_status;
    status->MPI_SOURCE = MTL_OFI_GET_SOURCE(wc->tag);
    status->MPI_TAG = MTL_OFI_GET_TAG(wc->tag);
    status->MPI_ERROR = MPI_SUCCESS;
    status->_ucount = wc->len;

    free(ofi_req);

    mrecv_req->completion_callback(mrecv_req);

    return OMPI_SUCCESS;
}

/**
 * Called when an error occured on a mrecv request.
 */
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_mrecv_error_callback(struct fi_cq_err_entry *error,
                                  ompi_mtl_ofi_request_t *ofi_req)
{
    struct mca_mtl_request_t *mrecv_req = ofi_req->mrecv_req;
    ompi_status_public_t *status = &mrecv_req->ompi_req->req_status;
    status->MPI_TAG = MTL_OFI_GET_TAG(ofi_req->match_bits);
    status->MPI_SOURCE = MTL_OFI_GET_SOURCE(ofi_req->match_bits);

    switch (error->err) {
        case FI_ETRUNC:
            status->MPI_ERROR = MPI_ERR_TRUNCATE;
            break;
        case FI_ECANCELED:
            status->_cancelled = true;
            break;
        default:
            status->MPI_ERROR = MPI_ERR_INTERN;
    }

    free(ofi_req);

    mrecv_req->completion_callback(mrecv_req);

    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_imrecv(struct mca_mtl_base_module_t *mtl,
                    struct opal_convertor_t *convertor,
                    struct ompi_message_t **message,
                    struct mca_mtl_request_t *mtl_request)
{
    ompi_mtl_ofi_request_t *ofi_req =
        (ompi_mtl_ofi_request_t *)(*message)->req_ptr;
    void *start;
    size_t length;
    bool free_after;
    struct iovec iov;
    struct fi_msg_tagged msg;
    int ompi_ret;
    ssize_t ret;
    uint64_t msgflags = FI_CLAIM;

    ompi_ret = ompi_mtl_datatype_recv_buf(convertor,
                                          &start,
                                          &length,
                                          &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ompi_ret)) {
        return ompi_ret;
    }

    ofi_req->type = OMPI_MTL_OFI_RECV;
    ofi_req->event_callback = ompi_mtl_ofi_mrecv_callback;
    ofi_req->error_callback = ompi_mtl_ofi_mrecv_error_callback;
    ofi_req->buffer = (free_after) ? start : NULL;
    ofi_req->length = length;
    ofi_req->convertor = convertor;
    ofi_req->status.MPI_ERROR = OMPI_SUCCESS;
    ofi_req->mrecv_req = mtl_request;

    /**
     * fi_trecvmsg with FI_CLAIM
     */
    iov.iov_base = start;
    iov.iov_len = length;
    msg.msg_iov = &iov;
    msg.desc = NULL;
    msg.iov_count = 1;
    msg.addr = 0;
    msg.tag = 0;
    msg.ignore = 0;
    msg.context = (void *)&ofi_req->ctx;
    msg.data = 0;

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecvmsg(ompi_mtl_ofi.ep, &msg, msgflags));
    if (OPAL_UNLIKELY(0 > ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_trecvmsg failed: %s(%zd)",
                            __FILE__, __LINE__, fi_strerror(-ret), ret);
        return ompi_mtl_ofi_get_error(ret);
    }

    return OMPI_SUCCESS;
}

/**
 * Called when a probe request completes.
 */
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_probe_callback(struct fi_cq_tagged_entry *wc,
                            ompi_mtl_ofi_request_t *ofi_req)
{
    ofi_req->match_state = 1;
    ofi_req->match_bits = wc->tag;
    ofi_req->status.MPI_SOURCE = MTL_OFI_GET_SOURCE(wc->tag);
    ofi_req->status.MPI_TAG = MTL_OFI_GET_TAG(wc->tag);
    ofi_req->status.MPI_ERROR = MPI_SUCCESS;
    ofi_req->status._ucount = wc->len;
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
    ofi_req->status.MPI_ERROR = MPI_ERR_INTERN;
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
    uint64_t match_bits, mask_bits;
    ssize_t ret;
    struct fi_msg_tagged msg;
    uint64_t msgflags = FI_PEEK;

    /**
     * If the source is known, use its peer_fiaddr.
     */
    if (MPI_ANY_SOURCE != src) {
        ompi_proc = ompi_comm_peer_lookup( comm, src );
        endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);
        remote_proc = endpoint->peer_fiaddr;
    }

    MTL_OFI_SET_RECV_BITS(match_bits, mask_bits, comm->c_contextid, src, tag);

    /**
     * fi_trecvmsg with FI_PEEK:
     * Initiate a search for a match in the hardware or software queue.
     * The search can complete immediately with -ENOMSG.
     * If successful, libfabric will enqueue a context entry into the completion
     * queue to make the search nonblocking.  This code will poll until the
     * entry is enqueued.
     */
    msg.msg_iov = NULL;
    msg.desc = NULL;
    msg.iov_count = 0;
    msg.addr = remote_proc;
    msg.tag = match_bits;
    msg.ignore = mask_bits;
    msg.context = (void *)&ofi_req.ctx;
    msg.data = 0;

    ofi_req.type = OMPI_MTL_OFI_PROBE;
    ofi_req.event_callback = ompi_mtl_ofi_probe_callback;
    ofi_req.error_callback = ompi_mtl_ofi_probe_error_callback;
    ofi_req.completion_count = 1;
    ofi_req.match_state = 0;

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecvmsg(ompi_mtl_ofi.ep, &msg, msgflags));
    if (-FI_ENOMSG == ret) {
        /**
         * The search request completed but no matching message was found.
         */
        *flag = 0;
        return OMPI_SUCCESS;
    } else if (OPAL_UNLIKELY(0 > ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_trecvmsg failed: %s(%zd)",
                            __FILE__, __LINE__, fi_strerror(-ret), ret);
        return ompi_mtl_ofi_get_error(ret);
    }

    while (0 < ofi_req.completion_count) {
        opal_progress();
    }

    *flag = ofi_req.match_state;
    if (1 == *flag) {
        if (MPI_STATUS_IGNORE != status) {
            *status = ofi_req.status;
        }
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
    struct ompi_mtl_ofi_request_t *ofi_req;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    fi_addr_t remote_proc = 0;
    uint64_t match_bits, mask_bits;
    ssize_t ret;
    struct fi_msg_tagged msg;
    uint64_t msgflags = FI_PEEK | FI_CLAIM;

    ofi_req = malloc(sizeof *ofi_req);
    if (NULL == ofi_req) {
        return OMPI_ERROR;
    }

    /**
     * If the source is known, use its peer_fiaddr.
     */
    if (MPI_ANY_SOURCE != src) {
        ompi_proc = ompi_comm_peer_lookup( comm, src );
        endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);
        remote_proc = endpoint->peer_fiaddr;
    }

    MTL_OFI_SET_RECV_BITS(match_bits, mask_bits, comm->c_contextid, src, tag);

    /**
     * fi_trecvmsg with FI_PEEK and FI_CLAIM:
     * Initiate a search for a match in the hardware or software queue.
     * The search can complete immediately with -ENOMSG.
     * If successful, libfabric will enqueue a context entry into the completion
     * queue to make the search nonblocking.  This code will poll until the
     * entry is enqueued.
     */
    msg.msg_iov = NULL;
    msg.desc = NULL;
    msg.iov_count = 0;
    msg.addr = remote_proc;
    msg.tag = match_bits;
    msg.ignore = mask_bits;
    msg.context = (void *)&ofi_req->ctx;
    msg.data = 0;

    ofi_req->type = OMPI_MTL_OFI_PROBE;
    ofi_req->event_callback = ompi_mtl_ofi_probe_callback;
    ofi_req->error_callback = ompi_mtl_ofi_probe_error_callback;
    ofi_req->completion_count = 1;
    ofi_req->match_state = 0;

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecvmsg(ompi_mtl_ofi.ep, &msg, msgflags));
    if (-FI_ENOMSG == ret) {
        /**
         * The search request completed but no matching message was found.
         */
        *matched = 0;
        free(ofi_req);
        return OMPI_SUCCESS;
    } else if (OPAL_UNLIKELY(0 > ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_trecvmsg failed: %s(%zd)",
                            __FILE__, __LINE__, fi_strerror(-ret), ret);
        free(ofi_req);
        return ompi_mtl_ofi_get_error(ret);
    }

    while (0 < ofi_req->completion_count) {
        opal_progress();
    }

    *matched = ofi_req->match_state;
    if (1 == *matched) {
        if (MPI_STATUS_IGNORE != status) {
            *status = ofi_req->status;
        }

        (*message) = ompi_message_alloc();
        if (NULL == (*message)) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }

        (*message)->comm = comm;
        (*message)->req_ptr = ofi_req;
        (*message)->peer = ofi_req->status.MPI_SOURCE;
        (*message)->count = ofi_req->status._ucount;

    } else {
        (*message) = MPI_MESSAGE_NULL;
        free(ofi_req);
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
                     * Wait for the request to be cancelled.
                     */
                    while (!ofi_req->super.ompi_req->req_status._cancelled) {
                        opal_progress();
                    }
                } else {
                    /**
                     * Could not cancel the request.
                     */
                    ofi_req->super.ompi_req->req_status._cancelled = false;
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
