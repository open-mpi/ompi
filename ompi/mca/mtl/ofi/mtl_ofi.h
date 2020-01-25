/*
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2019      Triad National Security, LLC. All rights
 *                         reserved.
 *
 * Copyright (c) 2018      Amazon.com, Inc. or its affiliates.  All Rights reserved.
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
#include "opal/util/show_help.h"
#include "opal/util/printf.h"

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

#include "mtl_ofi_opt.h"
#include "mtl_ofi_types.h"
#include "mtl_ofi_request.h"
#include "mtl_ofi_endpoint.h"
#include "mtl_ofi_compat.h"

BEGIN_C_DECLS

extern mca_mtl_ofi_module_t ompi_mtl_ofi;
extern mca_base_framework_t ompi_mtl_base_framework;

extern int ompi_mtl_ofi_del_procs(struct mca_mtl_base_module_t *mtl,
                                  size_t nprocs,
                                  struct ompi_proc_t **procs);

int ompi_mtl_ofi_progress_no_inline(void);

#if OPAL_HAVE_THREAD_LOCAL
extern opal_thread_local int per_thread_ctx;
extern opal_thread_local struct fi_cq_tagged_entry wc[MTL_OFI_MAX_PROG_EVENT_COUNT];
#endif

/* Set OFI context for operations which generate completion events */
__opal_attribute_always_inline__ static inline void
set_thread_context(int ctxt)
{
#if OPAL_HAVE_THREAD_LOCAL
    per_thread_ctx = ctxt;
    return;
#endif
}

/* Retrieve OFI context to use for CQ poll */
__opal_attribute_always_inline__ static inline void
get_thread_context(int *ctxt)
{
#if OPAL_HAVE_THREAD_LOCAL
    *ctxt = per_thread_ctx;
#endif
    return;
}

#define MTL_OFI_CONTEXT_LOCK(ctxt_id) \
OPAL_LIKELY(!opal_mutex_atomic_trylock(&ompi_mtl_ofi.ofi_ctxt[ctxt_id].context_lock))

#define MTL_OFI_CONTEXT_UNLOCK(ctxt_id) \
opal_mutex_atomic_unlock(&ompi_mtl_ofi.ofi_ctxt[ctxt_id].context_lock)

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_context_progress(int ctxt_id)
{
    int count = 0, i, events_read;
    ompi_mtl_ofi_request_t *ofi_req = NULL;
    struct fi_cq_err_entry error = { 0 };
    ssize_t ret;
#if !OPAL_HAVE_THREAD_LOCAL
    struct fi_cq_tagged_entry wc[MTL_OFI_MAX_PROG_EVENT_COUNT];
#endif

    /**
     * Read the work completions from the CQ.
     * From the completion's op_context, we get the associated OFI request.
     * Call the request's callback.
     */
    while (true) {
        ret = fi_cq_read(ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq, (void *)&wc,
                         ompi_mtl_ofi.ofi_progress_event_count);
        if (ret > 0) {
            count+= ret;
            events_read = ret;
            for (i = 0; i < events_read; i++) {
                if (NULL != wc[i].op_context) {
                    ofi_req = TO_OFI_REQ(wc[i].op_context);
                    assert(ofi_req);
                    ret = ofi_req->event_callback(&wc[i], ofi_req);
                    if (OMPI_SUCCESS != ret) {
                        opal_output(0, "%s:%d: Error returned by request event callback: %zd.\n"
                                       "*** The Open MPI OFI MTL is aborting the MPI job (via exit(3)).\n",
                                       __FILE__, __LINE__, ret);
                        fflush(stderr);
                        exit(1);
                    }
                }
            }
        } else if (OPAL_UNLIKELY(ret == -FI_EAVAIL)) {
            /**
             * An error occured and is being reported via the CQ.
             * Read the error and forward it to the upper layer.
             */
            ret = fi_cq_readerr(ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq,
                                &error,
                                0);
            if (0 > ret) {
                opal_output(0, "%s:%d: Error returned from fi_cq_readerr: %s(%zd).\n"
                               "*** The Open MPI OFI MTL is aborting the MPI job (via exit(3)).\n",
                               __FILE__, __LINE__, fi_strerror(-ret), ret);
                fflush(stderr);
                exit(1);
            }

            assert(error.op_context);
            ofi_req = TO_OFI_REQ(error.op_context);
            assert(ofi_req);
            ret = ofi_req->error_callback(&error, ofi_req);
            if (OMPI_SUCCESS != ret) {
                    opal_output(0, "%s:%d: Error returned by request error callback: %zd.\n"
                                   "*** The Open MPI OFI MTL is aborting the MPI job (via exit(3)).\n",
                                   __FILE__, __LINE__, ret);
                fflush(stderr);
                exit(1);
            }
        } else {
            if (ret == -FI_EAGAIN || ret == -EINTR) {
                break;
            } else {
                opal_output(0, "%s:%d: Error returned from fi_cq_read: %s(%zd).\n"
                               "*** The Open MPI OFI MTL is aborting the MPI job (via exit(3)).\n",
                               __FILE__, __LINE__, fi_strerror(-ret), ret);
                fflush(stderr);
                exit(1);
            }
        }
    }

    return count;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_progress(void)
{
    int count = 0, ctxt_id = 0, i;
    static volatile uint32_t num_calls = 0;

    get_thread_context(&ctxt_id);

    if (ompi_mtl_ofi.mpi_thread_multiple) {
        if (MTL_OFI_CONTEXT_LOCK(ctxt_id)) {
            count += ompi_mtl_ofi_context_progress(ctxt_id);
            MTL_OFI_CONTEXT_UNLOCK(ctxt_id);
        }
    } else {
        count += ompi_mtl_ofi_context_progress(ctxt_id);
    }

#if OPAL_HAVE_THREAD_LOCAL
    /*
     * Try to progress other CQs in round-robin fashion.
     * Progress is only made if no events were read from the CQ
     * local to the calling thread past 16 times.
     */
    if (OPAL_UNLIKELY((count == 0) && ompi_mtl_ofi.mpi_thread_multiple &&
        (((num_calls++) & 0xF) == 0 ))) {
        for (i = 0; i < ompi_mtl_ofi.total_ctxts_used - 1; i++) {
            ctxt_id = (ctxt_id + 1) % ompi_mtl_ofi.total_ctxts_used;

            if (MTL_OFI_CONTEXT_LOCK(ctxt_id)) {
                count += ompi_mtl_ofi_context_progress(ctxt_id);
                MTL_OFI_CONTEXT_UNLOCK(ctxt_id);
            }

            /* Upon any work done, exit to let other threads take lock */
            if (OPAL_LIKELY(count > 0)) {
                break;
            }
        }
    }
#endif

    return count;
}

/**
 * When attempting to execute an OFI operation we need to handle
 * resource overrun cases. When a call to an OFI OP fails with -FI_EAGAIN
 * the OFI mtl will attempt to progress any pending Completion Queue
 * events that may prevent additional operations to be enqueued.
 * If the call to ofi progress is successful, then the function call
 * will be retried.
 */
#define MTL_OFI_RETRY_UNTIL_DONE(FUNC, RETURN)         \
    do {                                               \
        do {                                           \
            RETURN = FUNC;                             \
            if (OPAL_LIKELY(0 == RETURN)) {break;}     \
            if (OPAL_LIKELY(RETURN == -FI_EAGAIN)) {   \
                ompi_mtl_ofi_progress();               \
            }                                          \
        } while (OPAL_LIKELY(-FI_EAGAIN == RETURN));   \
    } while (0);

#define MTL_OFI_LOG_FI_ERR(err, string)                                     \
    do {                                                                    \
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,    \
                            "%s:%d:%s: %s\n",                               \
                            __FILE__, __LINE__, string, fi_strerror(-err)); \
    } while(0);

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

/* Return OFI context ID associated with the specific communicator */
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_map_comm_to_ctxt(uint32_t comm_id)
{
    /* For non-thread-grouping use case, only one context is used which is
     * associated to MPI_COMM_WORLD, so use that. */
    if (0 == ompi_mtl_ofi.thread_grouping) {
        comm_id = 0;
    }

    return ompi_mtl_ofi.comm_to_context[comm_id];
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_ssend_recv(ompi_mtl_ofi_request_t *ack_req,
                  struct ompi_communicator_t *comm,
                  fi_addr_t *src_addr,
                  ompi_mtl_ofi_request_t *ofi_req,
                  mca_mtl_ofi_endpoint_t *endpoint,
                  uint64_t *match_bits,
                  int tag)
{
    ssize_t ret = OMPI_SUCCESS;
    int ctxt_id = 0;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(comm->c_contextid);
    set_thread_context(ctxt_id);

    ack_req = malloc(sizeof(ompi_mtl_ofi_request_t));
    assert(ack_req);

    ack_req->parent = ofi_req;
    ack_req->event_callback = ompi_mtl_ofi_send_ack_callback;
    ack_req->error_callback = ompi_mtl_ofi_send_ack_error_callback;

    ofi_req->completion_count += 1;

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecv(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep,
                                      NULL,
                                      0,
                                      NULL,
                                      *src_addr,
                                      *match_bits | ompi_mtl_ofi.sync_send_ack,
                                      0, /* Exact match, no ignore bits */
                                      (void *) &ack_req->ctx), ret);
    if (OPAL_UNLIKELY(0 > ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: fi_trecv failed: %s(%zd)",
                            __FILE__, __LINE__, fi_strerror(-ret), ret);
        free(ack_req);
        return ompi_mtl_ofi_get_error(ret);
    }

     /* The SYNC_SEND tag bit is set for the send operation only.*/
    MTL_OFI_SET_SYNC_SEND(*match_bits);
    return OMPI_SUCCESS;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send_generic(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode,
                  bool ofi_cq_data)
{
    ssize_t ret = OMPI_SUCCESS;
    ompi_mtl_ofi_request_t ofi_req;
    int ompi_ret, ctxt_id = 0;
    void *start;
    bool free_after;
    size_t length;
    uint64_t match_bits;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    ompi_mtl_ofi_request_t *ack_req = NULL; /* For synchronous send */
    fi_addr_t src_addr = 0;
    fi_addr_t sep_peer_fiaddr = 0;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(comm->c_contextid);
    set_thread_context(ctxt_id);

    /**
     * Create a send request, start it and wait until it completes.
     */
    ofi_req.event_callback = ompi_mtl_ofi_send_callback;
    ofi_req.error_callback = ompi_mtl_ofi_send_error_callback;

    ompi_proc = ompi_comm_peer_lookup(comm, dest);
    endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);

    /* For Scalable Endpoints, gather target receive context */
    sep_peer_fiaddr = fi_rx_addr(endpoint->peer_fiaddr, ctxt_id, ompi_mtl_ofi.rx_ctx_bits);

    ompi_ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ompi_ret)) {
        return ompi_ret;
    }

    ofi_req.buffer = (free_after) ? start : NULL;
    ofi_req.length = length;
    ofi_req.status.MPI_ERROR = OMPI_SUCCESS;
    ofi_req.completion_count = 0;

    if (OPAL_UNLIKELY(length > endpoint->mtl_ofi_module->max_msg_size)) {
        opal_show_help("help-mtl-ofi.txt",
            "message too big", false,
            length, endpoint->mtl_ofi_module->max_msg_size);
        return OMPI_ERROR;
    }

    if (ofi_cq_data) {
        match_bits = mtl_ofi_create_send_tag_CQD(comm->c_contextid, tag);
        src_addr = sep_peer_fiaddr;
    } else {
        match_bits = mtl_ofi_create_send_tag(comm->c_contextid,
                                             comm->c_my_rank, tag);
        /* src_addr is ignored when FI_DIRECTED_RECV is not supported */
    }

    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_SYNCHRONOUS == mode)) {
        ofi_req.status.MPI_ERROR = ompi_mtl_ofi_ssend_recv(ack_req, comm, &src_addr,
                                                           &ofi_req, endpoint,
                                                           &match_bits, tag);
        if (OPAL_UNLIKELY(ofi_req.status.MPI_ERROR != OMPI_SUCCESS))
            goto free_request_buffer;
    }

    if (ompi_mtl_ofi.max_inject_size >= length) {
        if (ofi_cq_data) {
            MTL_OFI_RETRY_UNTIL_DONE(fi_tinjectdata(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep,
                                            start,
                                            length,
                                            comm->c_my_rank,
                                            sep_peer_fiaddr,
                                            match_bits), ret);
        } else {
            MTL_OFI_RETRY_UNTIL_DONE(fi_tinject(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep,
                                            start,
                                            length,
                                            sep_peer_fiaddr,
                                            match_bits), ret);
        }
        if (OPAL_UNLIKELY(0 > ret)) {
            MTL_OFI_LOG_FI_ERR(ret,
                               ofi_cq_data ? "fi_tinjectdata failed"
                               : "fi_tinject failed");
            if (ack_req) {
                fi_cancel((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep, &ack_req->ctx);
                free(ack_req);
            }

            ofi_req.status.MPI_ERROR = ompi_mtl_ofi_get_error(ret);
            goto free_request_buffer;
        }
    } else {
        ofi_req.completion_count += 1;
        if (ofi_cq_data) {
            MTL_OFI_RETRY_UNTIL_DONE(fi_tsenddata(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep,
                                          start,
                                          length,
                                          NULL,
                                          comm->c_my_rank,
                                          sep_peer_fiaddr,
                                          match_bits,
                                          (void *) &ofi_req.ctx), ret);
        } else {
            MTL_OFI_RETRY_UNTIL_DONE(fi_tsend(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep,
                                          start,
                                          length,
                                          NULL,
                                          sep_peer_fiaddr,
                                          match_bits,
                                          (void *) &ofi_req.ctx), ret);
        }
        if (OPAL_UNLIKELY(0 > ret)) {
            MTL_OFI_LOG_FI_ERR(ret,
                               ofi_cq_data ? "fi_tsenddata failed"
                               : "fi_tsend failed");
            ofi_req.status.MPI_ERROR = ompi_mtl_ofi_get_error(ret);
            goto free_request_buffer;
        }
    }

    /**
     * Wait until the request is completed.
     * ompi_mtl_ofi_send_callback() updates this variable.
     */
    while (0 < ofi_req.completion_count) {
        ompi_mtl_ofi_progress();
    }

free_request_buffer:
    if (OPAL_UNLIKELY(NULL != ofi_req.buffer)) {
        free(ofi_req.buffer);
    }

    return ofi_req.status.MPI_ERROR;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_isend_generic(struct mca_mtl_base_module_t *mtl,
                   struct ompi_communicator_t *comm,
                   int dest,
                   int tag,
                   struct opal_convertor_t *convertor,
                   mca_pml_base_send_mode_t mode,
                   bool blocking,
                   mca_mtl_request_t *mtl_request,
                   bool ofi_cq_data)
{
    ssize_t ret = OMPI_SUCCESS;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t *) mtl_request;
    int ompi_ret, ctxt_id = 0;
    void *start;
    size_t length;
    bool free_after;
    uint64_t match_bits;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    ompi_mtl_ofi_request_t *ack_req = NULL; /* For synchronous send */
    fi_addr_t sep_peer_fiaddr = 0;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(comm->c_contextid);
    set_thread_context(ctxt_id);

    ofi_req->event_callback = ompi_mtl_ofi_isend_callback;
    ofi_req->error_callback = ompi_mtl_ofi_send_error_callback;

    ompi_proc = ompi_comm_peer_lookup(comm, dest);
    endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);

    /* For Scalable Endpoints, gather target receive context */
    sep_peer_fiaddr = fi_rx_addr(endpoint->peer_fiaddr, ctxt_id, ompi_mtl_ofi.rx_ctx_bits);

    ompi_ret = ompi_mtl_datatype_pack(convertor, &start, &length, &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ompi_ret)) return ompi_ret;

    ofi_req->buffer = (free_after) ? start : NULL;
    ofi_req->length = length;
    ofi_req->status.MPI_ERROR = OMPI_SUCCESS;
    ofi_req->completion_count = 1;

    if (OPAL_UNLIKELY(length > endpoint->mtl_ofi_module->max_msg_size)) {
        opal_show_help("help-mtl-ofi.txt",
            "message too big", false,
            length, endpoint->mtl_ofi_module->max_msg_size);
        return OMPI_ERROR;
    }

    if (ofi_cq_data) {
        match_bits = mtl_ofi_create_send_tag_CQD(comm->c_contextid, tag);
    } else {
        match_bits = mtl_ofi_create_send_tag(comm->c_contextid,
                          comm->c_my_rank, tag);
        /* src_addr is ignored when FI_DIRECTED_RECV  is not supported */
    }

    if (OPAL_UNLIKELY(MCA_PML_BASE_SEND_SYNCHRONOUS == mode)) {
        ofi_req->status.MPI_ERROR = ompi_mtl_ofi_ssend_recv(ack_req, comm, &sep_peer_fiaddr,
                                                           ofi_req, endpoint,
                                                           &match_bits, tag);
        if (OPAL_UNLIKELY(ofi_req->status.MPI_ERROR != OMPI_SUCCESS))
            goto free_request_buffer;
    }

    if (ofi_cq_data) {
        MTL_OFI_RETRY_UNTIL_DONE(fi_tsenddata(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep,
                                      start,
                                      length,
                                      NULL,
                                      comm->c_my_rank,
                                      sep_peer_fiaddr,
                                      match_bits,
                                      (void *) &ofi_req->ctx), ret);
    } else {
        MTL_OFI_RETRY_UNTIL_DONE(fi_tsend(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep,
                                      start,
                                      length,
                                      NULL,
                                      sep_peer_fiaddr,
                                      match_bits,
                                      (void *) &ofi_req->ctx), ret);
    }
    if (OPAL_UNLIKELY(0 > ret)) {
        MTL_OFI_LOG_FI_ERR(ret,
                           ofi_cq_data ? "fi_tsenddata failed"
                           : "fi_tsend failed");
        ofi_req->status.MPI_ERROR = ompi_mtl_ofi_get_error(ret);
    }

free_request_buffer:
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ofi_req->status.MPI_ERROR
            && NULL != ofi_req->buffer)) {
        free(ofi_req->buffer);
    }

    return ofi_req->status.MPI_ERROR;
}

/**
 * Called when a completion for a posted recv is received.
 */
__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_recv_callback(struct fi_cq_tagged_entry *wc,
                           ompi_mtl_ofi_request_t *ofi_req)
{
    int ompi_ret, ctxt_id = 0;
    ssize_t ret;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    int src = mtl_ofi_get_source(wc);
    ompi_status_public_t *status = NULL;
    struct fi_msg_tagged tagged_msg;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(ofi_req->comm->c_contextid);

    assert(ofi_req->super.ompi_req);
    status = &ofi_req->super.ompi_req->req_status;

    /**
     * Any event associated with a request starts it.
     * This prevents a started request from being cancelled.
     */
    ofi_req->req_started = true;

    status->MPI_SOURCE = src;
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
    * We can only accept MTL_OFI_SYNC_SEND in the standard recv callback.
    * MTL_OFI_SYNC_SEND_ACK should only be received in the send_ack
    * callback.
    */
    assert(!MTL_OFI_IS_SYNC_SEND_ACK(wc->tag));

    /**
     * If this recv is part of an MPI_Ssend operation, then we send an
     * acknowledgment back to the sender.
     * The ack message is sent without generating a completion event in
     * the completion queue by not setting FI_COMPLETION in the flags to
     * fi_tsendmsg(FI_SELECTIVE_COMPLETION).
     * This is done since the 0 byte message requires no
     * notification on the send side for a successful completion.
     * If a failure occurs the provider will notify the error
     * in the cq_readerr during OFI progress. Once the message has been
     * successfully processed the request is marked as completed.
     */
    if (OPAL_UNLIKELY(MTL_OFI_IS_SYNC_SEND(wc->tag))) {
        /**
         * If the recv request was posted for any source,
         * we need to extract the source's actual address.
         */
        if (ompi_mtl_ofi.any_addr == ofi_req->remote_addr) {
            ompi_proc = ompi_comm_peer_lookup(ofi_req->comm, src);
            endpoint = ompi_mtl_ofi_get_endpoint(ofi_req->mtl, ompi_proc);
            ofi_req->remote_addr = fi_rx_addr(endpoint->peer_fiaddr, ctxt_id, ompi_mtl_ofi.rx_ctx_bits);
        }

        tagged_msg.msg_iov = NULL;
        tagged_msg.desc = NULL;
        tagged_msg.iov_count = 0;
        tagged_msg.addr = ofi_req->remote_addr;
        /**
        * We must continue to use the user's original tag but remove the
        * sync_send protocol tag bit and instead apply the sync_send_ack
        * tag bit to complete the initator's sync send receive.
        */
        tagged_msg.tag = (wc->tag | ompi_mtl_ofi.sync_send_ack) & ~ompi_mtl_ofi.sync_send;
        tagged_msg.context = NULL;
        tagged_msg.data = 0;

        MTL_OFI_RETRY_UNTIL_DONE(fi_tsendmsg(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep,
                                 &tagged_msg, 0), ret);
        if (OPAL_UNLIKELY(0 > ret)) {
            MTL_OFI_LOG_FI_ERR(ret, "fi_tsendmsg failed");
            status->MPI_ERROR = OMPI_ERROR;
        }
    }

    ofi_req->super.completion_callback(&ofi_req->super);

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
    status->MPI_SOURCE = mtl_ofi_get_source((struct fi_cq_tagged_entry *) error);

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
ompi_mtl_ofi_irecv_generic(struct mca_mtl_base_module_t *mtl,
                   struct ompi_communicator_t *comm,
                   int src,
                   int tag,
                   struct opal_convertor_t *convertor,
                   mca_mtl_request_t *mtl_request,
                   bool ofi_cq_data)
{
    int ompi_ret = OMPI_SUCCESS, ctxt_id = 0;
    ssize_t ret;
    uint64_t match_bits, mask_bits;
    fi_addr_t remote_addr = ompi_mtl_ofi.any_addr;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;
    void *start;
    size_t length;
    bool free_after;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(comm->c_contextid);
    set_thread_context(ctxt_id);

    if (ofi_cq_data) {
        if (MPI_ANY_SOURCE != src) {
            ompi_proc = ompi_comm_peer_lookup(comm, src);
            endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);
            remote_addr = fi_rx_addr(endpoint->peer_fiaddr, ctxt_id, ompi_mtl_ofi.rx_ctx_bits);
        }

        mtl_ofi_create_recv_tag_CQD(&match_bits, &mask_bits, comm->c_contextid,
                                    tag);
    } else {
        mtl_ofi_create_recv_tag(&match_bits, &mask_bits, comm->c_contextid, src,
                                tag);
        /* src_addr is ignored when FI_DIRECTED_RECV is not used */
    }

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

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecv(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep,
                                      start,
                                      length,
                                      NULL,
                                      remote_addr,
                                      match_bits,
                                      mask_bits,
                                      (void *)&ofi_req->ctx), ret);
    if (OPAL_UNLIKELY(0 > ret)) {
        if (NULL != ofi_req->buffer) {
            free(ofi_req->buffer);
        }
        MTL_OFI_LOG_FI_ERR(ret, "fi_trecv failed");
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
    status->MPI_SOURCE = mtl_ofi_get_source(wc);
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
    status->MPI_SOURCE = mtl_ofi_get_source((struct fi_cq_tagged_entry  *) error);

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
    int ompi_ret, ctxt_id = 0;
    ssize_t ret;
    uint64_t msgflags = FI_CLAIM | FI_COMPLETION;
    struct ompi_communicator_t *comm = (*message)->comm;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(comm->c_contextid);
    set_thread_context(ctxt_id);

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
    msg.tag = ofi_req->match_bits;
    msg.ignore = ofi_req->mask_bits;
    msg.context = (void *)&ofi_req->ctx;
    msg.data = 0;

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecvmsg(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep, &msg, msgflags), ret);
    if (OPAL_UNLIKELY(0 > ret)) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_trecvmsg failed");
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
    ofi_req->status.MPI_SOURCE = mtl_ofi_get_source(wc);
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
ompi_mtl_ofi_iprobe_generic(struct mca_mtl_base_module_t *mtl,
                    struct ompi_communicator_t *comm,
                    int src,
                    int tag,
                    int *flag,
                    struct ompi_status_public_t *status,
                    bool ofi_cq_data)
{
    struct ompi_mtl_ofi_request_t ofi_req;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    fi_addr_t remote_proc = ompi_mtl_ofi.any_addr;
    uint64_t match_bits, mask_bits;
    ssize_t ret;
    struct fi_msg_tagged msg;
    uint64_t msgflags = FI_PEEK | FI_COMPLETION;
    int ctxt_id = 0;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(comm->c_contextid);
    set_thread_context(ctxt_id);

    if (ofi_cq_data) {
     /* If the source is known, use its peer_fiaddr. */
        if (MPI_ANY_SOURCE != src) {
            ompi_proc = ompi_comm_peer_lookup( comm, src );
            endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);
            remote_proc = fi_rx_addr(endpoint->peer_fiaddr, ctxt_id, ompi_mtl_ofi.rx_ctx_bits);
        }

        mtl_ofi_create_recv_tag_CQD(&match_bits, &mask_bits, comm->c_contextid,
                                    tag);
    }
    else {
        mtl_ofi_create_recv_tag(&match_bits, &mask_bits, comm->c_contextid, src,
                                tag);
        /* src_addr is ignored when FI_DIRECTED_RECV is not used */
    }

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

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecvmsg(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep, &msg, msgflags), ret);
    if (-FI_ENOMSG == ret) {
        /**
         * The search request completed but no matching message was found.
         */
        *flag = 0;
        return OMPI_SUCCESS;
    } else if (OPAL_UNLIKELY(0 > ret)) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_trecvmsg failed");
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
ompi_mtl_ofi_improbe_generic(struct mca_mtl_base_module_t *mtl,
                     struct ompi_communicator_t *comm,
                     int src,
                     int tag,
                     int *matched,
                     struct ompi_message_t **message,
                     struct ompi_status_public_t *status,
                     bool ofi_cq_data)
{
    struct ompi_mtl_ofi_request_t *ofi_req;
    ompi_proc_t *ompi_proc = NULL;
    mca_mtl_ofi_endpoint_t *endpoint = NULL;
    fi_addr_t remote_proc = ompi_mtl_ofi.any_addr;
    uint64_t match_bits, mask_bits;
    ssize_t ret;
    struct fi_msg_tagged msg;
    uint64_t msgflags = FI_PEEK | FI_CLAIM | FI_COMPLETION;
    int ctxt_id = 0;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(comm->c_contextid);
    set_thread_context(ctxt_id);

    ofi_req = malloc(sizeof *ofi_req);
    if (NULL == ofi_req) {
        return OMPI_ERROR;
    }

    /**
     * If the source is known, use its peer_fiaddr.
     */

    if (ofi_cq_data) {
        if (MPI_ANY_SOURCE != src) {
            ompi_proc = ompi_comm_peer_lookup( comm, src );
            endpoint = ompi_mtl_ofi_get_endpoint(mtl, ompi_proc);
            remote_proc = fi_rx_addr(endpoint->peer_fiaddr, ctxt_id, ompi_mtl_ofi.rx_ctx_bits);
        }

        mtl_ofi_create_recv_tag_CQD(&match_bits, &mask_bits, comm->c_contextid,
                                    tag);
    }
    else {
        /* src_addr is ignored when FI_DIRECTED_RECV is not used */
        mtl_ofi_create_recv_tag(&match_bits, &mask_bits, comm->c_contextid, src,
                                tag);
    }

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
    ofi_req->mask_bits = mask_bits;

    MTL_OFI_RETRY_UNTIL_DONE(fi_trecvmsg(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep, &msg, msgflags), ret);
    if (-FI_ENOMSG == ret) {
        /**
         * The search request completed but no matching message was found.
         */
        *matched = 0;
        free(ofi_req);
        return OMPI_SUCCESS;
    } else if (OPAL_UNLIKELY(0 > ret)) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_trecvmsg failed");
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
    int ret, ctxt_id = 0;
    ompi_mtl_ofi_request_t *ofi_req = (ompi_mtl_ofi_request_t*) mtl_request;

    ctxt_id = ompi_mtl_ofi_map_comm_to_ctxt(ofi_req->comm->c_contextid);

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
                ret = fi_cancel((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep,
                               &ofi_req->ctx);
                if (0 == ret) {
                    if (ofi_req->req_started)
                        goto ofi_cancel_not_possible;
                } else {
ofi_cancel_not_possible:
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

static int ompi_mtl_ofi_init_contexts(struct mca_mtl_base_module_t *mtl,
                                      struct ompi_communicator_t *comm,
                                      mca_mtl_ofi_ep_type ep_type)
{
    int ret;
    int ctxt_id = ompi_mtl_ofi.total_ctxts_used;
    struct fi_cq_attr cq_attr = {0};
    cq_attr.format = FI_CQ_FORMAT_TAGGED;
    cq_attr.size = ompi_mtl_ofi.ofi_progress_event_count;

    if (OFI_REGULAR_EP == ep_type) {
        /*
         * For regular endpoints, just create the Lock object and register
         * progress function.
         */
        goto init_regular_ep;
    }

    /*
     * We only create upto Max number of contexts asked for by the user.
     * If user enables thread grouping feature and creates more number of
     * communicators than available contexts, then we set the threshold
     * context_id so that new communicators created beyond the threshold
     * will be assigned to contexts in a round-robin fashion.
     */
    if (ompi_mtl_ofi.num_ofi_contexts <= ompi_mtl_ofi.total_ctxts_used) {
        ompi_mtl_ofi.comm_to_context[comm->c_contextid] = comm->c_contextid %
                                                          ompi_mtl_ofi.total_ctxts_used;
        if (!ompi_mtl_ofi.threshold_comm_context_id) {
            ompi_mtl_ofi.threshold_comm_context_id = comm->c_contextid;

            opal_show_help("help-mtl-ofi.txt", "SEP thread grouping ctxt limit", true, ctxt_id,
                           ompi_process_info.nodename, __FILE__, __LINE__);
        }

        return OMPI_SUCCESS;
    }

    /* Init context info for Scalable EPs */
    ret = fi_tx_context(ompi_mtl_ofi.sep, ctxt_id, NULL, &ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep, NULL);
    if (ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_tx_context failed");
        goto init_error;
    }

    ret = fi_rx_context(ompi_mtl_ofi.sep, ctxt_id, NULL, &ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep, NULL);
    if (ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_rx_context failed");
        goto init_error;
    }

    ret = fi_cq_open(ompi_mtl_ofi.domain, &cq_attr, &ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq, NULL);
    if (ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_cq_open failed");
        goto init_error;
    }

    /* Bind CQ to TX/RX context object */
    ret = fi_ep_bind(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep, (fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq,
                     FI_TRANSMIT | FI_SELECTIVE_COMPLETION);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_bind CQ-EP (FI_TRANSMIT) failed");
        goto init_error;
    }

    ret = fi_ep_bind(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep, (fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq,
                     FI_RECV | FI_SELECTIVE_COMPLETION);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_bind CQ-EP (FI_RECV) failed");
        goto init_error;
    }

    /* Enable Endpoint for communication. This commits the bind operations */
    ret = fi_enable(ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_enable (send context) failed");
        goto init_error;
    }

    ret = fi_enable(ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep);
    if (0 != ret) {
        MTL_OFI_LOG_FI_ERR(ret, "fi_enable (recv context) failed");
        goto init_error;
    }

init_regular_ep:
    /* Initialize per-context lock */
    OBJ_CONSTRUCT(&ompi_mtl_ofi.ofi_ctxt[ctxt_id].context_lock, opal_mutex_t);

    if (MPI_COMM_WORLD == comm) {
        ret = opal_progress_register(ompi_mtl_ofi_progress_no_inline);
        if (OMPI_SUCCESS != ret) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: opal_progress_register failed: %d\n",
                                __FILE__, __LINE__, ret);
            goto init_error;
        }
    }

    ompi_mtl_ofi.comm_to_context[comm->c_contextid] = ompi_mtl_ofi.total_ctxts_used;
    ompi_mtl_ofi.total_ctxts_used++;

    return OMPI_SUCCESS;

init_error:
    if (ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep) {
        (void) fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep);
    }

    if (ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep) {
        (void) fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep);
    }

    if (ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq) {
        (void) fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq);
    }

    return ret;
}

static int ompi_mtl_ofi_finalize_contexts(struct mca_mtl_base_module_t *mtl,
                                          struct ompi_communicator_t *comm,
                                          mca_mtl_ofi_ep_type ep_type)
{
    int ret = OMPI_SUCCESS, ctxt_id = 0;

    if (OFI_REGULAR_EP == ep_type) {
        /* For regular EPs, simply destruct Lock object and exit */
        goto finalize_regular_ep;
    }

    if (ompi_mtl_ofi.thread_grouping &&
        ompi_mtl_ofi.threshold_comm_context_id &&
        ((uint32_t) ompi_mtl_ofi.threshold_comm_context_id <= comm->c_contextid)) {
        return OMPI_SUCCESS;
    }

    ctxt_id = ompi_mtl_ofi.thread_grouping ?
           ompi_mtl_ofi.comm_to_context[comm->c_contextid] : 0;

    /*
     * For regular EPs, TX/RX contexts are aliased to SEP object which is
     * closed in ompi_mtl_ofi_finalize(). So, skip handling those here.
     */
    if ((ret = fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].tx_ep))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].rx_ep))) {
        goto finalize_err;
    }

    if ((ret = fi_close((fid_t)ompi_mtl_ofi.ofi_ctxt[ctxt_id].cq))) {
        goto finalize_err;
    }

finalize_regular_ep:
    /* Destroy context lock */
    OBJ_DESTRUCT(&ompi_mtl_ofi.ofi_ctxt[ctxt_id].context_lock);

    return OMPI_SUCCESS;

finalize_err:
    opal_show_help("help-mtl-ofi.txt", "OFI call fail", true,
                   "fi_close",
                   ompi_process_info.nodename, __FILE__, __LINE__,
                   fi_strerror(-ret), ret);

    return OMPI_ERROR;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_add_comm(struct mca_mtl_base_module_t *mtl,
                      struct ompi_communicator_t *comm)
{
    int ret;
    mca_mtl_ofi_ep_type ep_type = (0 == ompi_mtl_ofi.enable_sep) ?
                                  OFI_REGULAR_EP : OFI_SCALABLE_EP;

    /*
     * If thread grouping enabled, add new OFI context for each communicator
     * other than MPI_COMM_SELF.
     */
    if ((ompi_mtl_ofi.thread_grouping && (MPI_COMM_SELF != comm)) ||
        /* If no thread grouping, add new OFI context only
         * for MPI_COMM_WORLD.
         */
        (!ompi_mtl_ofi.thread_grouping && (MPI_COMM_WORLD == comm))) {

        ret = ompi_mtl_ofi_init_contexts(mtl, comm, ep_type);

        if (OMPI_SUCCESS != ret) {
            goto error;
        }
    }

    return OMPI_SUCCESS;

error:
    return OMPI_ERROR;
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_del_comm(struct mca_mtl_base_module_t *mtl,
                      struct ompi_communicator_t *comm)
{
    int ret = OMPI_SUCCESS;
    mca_mtl_ofi_ep_type ep_type = (0 == ompi_mtl_ofi.enable_sep) ?
                                  OFI_REGULAR_EP : OFI_SCALABLE_EP;

    /*
     * Clean up OFI contexts information.
     */
    if ((ompi_mtl_ofi.thread_grouping && (MPI_COMM_SELF != comm)) ||
        (!ompi_mtl_ofi.thread_grouping && (MPI_COMM_WORLD == comm))) {

        ret = ompi_mtl_ofi_finalize_contexts(mtl, comm, ep_type);
    }

    return ret;
}

#ifdef MCA_ompi_mtl_DIRECT_CALL

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_send(struct mca_mtl_base_module_t *mtl,
                  struct ompi_communicator_t *comm,
                  int dest,
                  int tag,
                  struct opal_convertor_t *convertor,
                  mca_pml_base_send_mode_t mode)
{
    return ompi_mtl_ofi_send_generic(mtl, comm, dest, tag,
                                    convertor, mode,
                                    ompi_mtl_ofi.fi_cq_data);
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
    return ompi_mtl_ofi_isend_generic(mtl, comm, dest, tag,
                                    convertor, mode, blocking, mtl_request,
                                    ompi_mtl_ofi.fi_cq_data);
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_irecv(struct mca_mtl_base_module_t *mtl,
               struct ompi_communicator_t *comm,
               int src,
               int tag,
               struct opal_convertor_t *convertor,
               mca_mtl_request_t *mtl_request)
{
    return ompi_mtl_ofi_irecv_generic(mtl, comm, src, tag,
                                    convertor, mtl_request,
                                    ompi_mtl_ofi.fi_cq_data);
}

__opal_attribute_always_inline__ static inline int
ompi_mtl_ofi_iprobe(struct mca_mtl_base_module_t *mtl,
                struct ompi_communicator_t *comm,
                int src,
                int tag,
                int *flag,
                struct ompi_status_public_t *status)
{
    return ompi_mtl_ofi_iprobe_generic(mtl, comm, src, tag,
                                    flag, status,
                                    ompi_mtl_ofi.fi_cq_data);
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
    return ompi_mtl_ofi_improbe_generic(mtl, comm, src, tag,
                                    matched, message, status,
                                    ompi_mtl_ofi.fi_cq_data);
}
#endif

END_C_DECLS

#endif  /* MTL_OFI_H_HAS_BEEN_INCLUDED */
