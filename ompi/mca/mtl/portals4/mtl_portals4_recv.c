/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2012 Sandia National Laboratories.  All rights reserved.
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

#include "mtl_portals4.h"
#include "mtl_portals4_endpoint.h"
#include "mtl_portals4_request.h"
#include "mtl_portals4_recv_short.h"
#include "mtl_portals4_message.h"

static int
triggered_read_msg(void *start, ptl_size_t length, ptl_process_t target,
         ptl_match_bits_t match_bits, ptl_size_t remote_offset,
         ompi_mtl_portals4_recv_request_t *request)
{
    int ret;

    ret = PtlCTAlloc(ompi_mtl_portals4.ni_h, &request->ct_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlGet failed: %d",
                            __FILE__, __LINE__, ret);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    ret = PtlTriggeredGet(ompi_mtl_portals4.send_md_h,
            (ptl_size_t) start,
            length,
            target,
            ompi_mtl_portals4.read_idx,
            match_bits,
            remote_offset,
            request,
            request->ct_h, 1);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        PtlCTFree(request->ct_h);
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlTriggeredGet failed: %d",
                            __FILE__, __LINE__, ret);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    return OMPI_SUCCESS;
}

static int
read_msg(void *start, ptl_size_t length, ptl_process_t target,
         ptl_match_bits_t match_bits, ptl_size_t remote_offset,
         ompi_mtl_portals4_recv_request_t *request)
{
    int ret;

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
    while (OPAL_UNLIKELY(OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, -1) < 0)) {
        OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
        ompi_mtl_portals4_progress();
    }
#endif

    ret = PtlGet(ompi_mtl_portals4.send_md_h,
                 (ptl_size_t) start,
                 length,
                 target,
                 ompi_mtl_portals4.read_idx,
                 match_bits,
                 remote_offset,
                 request);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlGet failed: %d",
                            __FILE__, __LINE__, ret);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    return OMPI_SUCCESS;
}


/* called when a receive should be progressed */
static int
ompi_mtl_portals4_recv_progress(ptl_event_t *ev,
                                ompi_mtl_portals4_base_request_t* ptl_base_request)
{
    int ret;
    ompi_mtl_portals4_recv_request_t* ptl_request =
        (ompi_mtl_portals4_recv_request_t*) ptl_base_request;
    size_t msg_length = 0;

    /* as soon as we've seen any event associated with a request, it's
       started */
    ptl_request->req_started = true;

    switch (ev->type) {
    case PTL_EVENT_PUT:
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                             "Recv %lu (0x%lx) got put event",
                             ptl_request->opcount, ev->hdr_data));

        if (ev->ni_fail_type != PTL_NI_OK) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PTL_EVENT_PUT with ni_fail_type: %d",
                                __FILE__, __LINE__, ev->ni_fail_type);
            goto callback_error;
        }

        if (!ptl_request->is_triggered) {
            ptl_request->me_h = PTL_INVALID_HANDLE;

            msg_length = ev->mlength;
            ptl_request->super.super.ompi_req->req_status.MPI_SOURCE =
                    MTL_PORTALS4_GET_SOURCE(ev->match_bits);
            ptl_request->super.super.ompi_req->req_status.MPI_TAG =
                    MTL_PORTALS4_GET_TAG(ev->match_bits);
            if (OPAL_UNLIKELY(msg_length > ptl_request->delivery_len)) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "truncate expected: %ld %ld",
                        msg_length, ptl_request->delivery_len);
                ptl_request->super.super.ompi_req->req_status.MPI_ERROR = MPI_ERR_TRUNCATE;
            }

#if OPAL_ENABLE_DEBUG
            ptl_request->hdr_data = ev->hdr_data;
#endif

            if (!MTL_PORTALS4_IS_SHORT_MSG(ev->match_bits) && ompi_mtl_portals4.protocol == rndv) {
                /* If it's not a short message and we're doing rndv, we
               only have the first part of the message.  Issue the get
               to pull the second part of the message. */
                ret = read_msg((char*) ptl_request->delivery_ptr + ompi_mtl_portals4.eager_limit,
                        ((msg_length > ptl_request->delivery_len) ?
                                ptl_request->delivery_len : msg_length) - ompi_mtl_portals4.eager_limit,
                                ev->initiator,
                                ev->hdr_data,
                                ompi_mtl_portals4.eager_limit,
                                ptl_request);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
                    goto callback_error;
                }

            } else {
                /* If we're either using the eager protocol or were a
               short message, all data has been received, so complete
               the message. */
                ret = ompi_mtl_datatype_unpack(ptl_request->convertor,
                        ev->start,
                        ev->mlength);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: ompi_mtl_datatype_unpack failed: %d",
                            __FILE__, __LINE__, ret);
                    ptl_request->super.super.ompi_req->req_status.MPI_ERROR = ret;
                }
                ptl_request->super.super.ompi_req->req_status._ucount = ev->mlength;

                OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                        "Recv %lu (0x%lx) completed, expected",
                        ptl_request->opcount, ptl_request->hdr_data));
                ptl_request->super.super.completion_callback(&ptl_request->super.super);
            }
        }
        break;

    case PTL_EVENT_REPLY:
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                             "Recv %lu (0x%lx) got reply event",
                             ptl_request->opcount, ptl_request->hdr_data));

        if (OPAL_UNLIKELY(ev->ni_fail_type != PTL_NI_OK)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PTL_EVENT_REPLY with ni_fail_type: %d",
                                __FILE__, __LINE__, ev->ni_fail_type);
            goto callback_error;
        }

        if (ptl_request->is_triggered)
            PtlCTFree(ptl_request->ct_h);

        /* set the received length in the status, now that we know
           exactly how much data was sent. */
        ptl_request->super.super.ompi_req->req_status._ucount = ev->mlength;
        if (ompi_mtl_portals4.protocol == rndv) {
            ptl_request->super.super.ompi_req->req_status._ucount +=
                ompi_mtl_portals4.eager_limit;
        }

#if OMPI_MTL_PORTALS4_FLOW_CONTROL
        OPAL_THREAD_ADD32(&ompi_mtl_portals4.flowctl.send_slots, 1);
#endif

        /* make sure the data is in the right place.  Use _ucount for
           the total length because it will be set correctly for all
           three protocols. mlength is only correct for eager, and
           delivery_len is the length of the buffer, not the length of
           the send. */
        ret = ompi_mtl_datatype_unpack(ptl_request->convertor,
                                       ptl_request->delivery_ptr,
                                       ptl_request->super.super.ompi_req->req_status._ucount);
        if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: ompi_mtl_datatype_unpack failed: %d",
                                __FILE__, __LINE__, ret);
            ptl_request->super.super.ompi_req->req_status.MPI_ERROR = ret;
        }

        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                             "Recv %lu (0x%lx) completed, reply",
                             ptl_request->opcount, ptl_request->hdr_data));
        ptl_request->super.super.completion_callback(&ptl_request->super.super);
        break;

    case PTL_EVENT_PUT_OVERFLOW:
        OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                             "Recv %lu (0x%lx) got put_overflow event",
                             ptl_request->opcount, ev->hdr_data));

        if (OPAL_UNLIKELY(ev->ni_fail_type != PTL_NI_OK)) {
            opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PTL_EVENT_PUT_OVERFLOW with ni_fail_type: %d",
                                __FILE__, __LINE__, ev->ni_fail_type);
            goto callback_error;
        }

        if (!ptl_request->is_triggered) {
            ptl_request->me_h = PTL_INVALID_HANDLE;

            msg_length = ev->mlength;
            ptl_request->super.super.ompi_req->req_status.MPI_SOURCE =
                    MTL_PORTALS4_GET_SOURCE(ev->match_bits);
            ptl_request->super.super.ompi_req->req_status.MPI_TAG =
                    MTL_PORTALS4_GET_TAG(ev->match_bits);
            if (OPAL_UNLIKELY(msg_length > ptl_request->delivery_len)) {
                opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                        "truncate unexpected: %ld %ld %d",
                        msg_length, ptl_request->delivery_len,
                        MTL_PORTALS4_IS_SHORT_MSG(ev->match_bits));
                ptl_request->super.super.ompi_req->req_status.MPI_ERROR = MPI_ERR_TRUNCATE;
            }

#if OPAL_ENABLE_DEBUG
            ptl_request->hdr_data = ev->hdr_data;
#endif

            /* overflow case.  Short messages have the buffer stashed
           somewhere.  Long messages left in buffer at the source */
            if (MTL_PORTALS4_IS_SHORT_MSG(ev->match_bits)) {
                ptl_request->super.super.ompi_req->req_status._ucount = ev->mlength;
                if (ev->mlength > 0) {
                    struct iovec iov;
                    uint32_t iov_count = 1;
                    size_t max_data;
                    iov.iov_base = (char*) ev->start;
                    iov.iov_len = ev->mlength;
                    max_data = iov.iov_len;

                    ret = opal_convertor_unpack(ptl_request->convertor,
                            &iov, &iov_count,
                            &max_data );
                    if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
                    if (OPAL_UNLIKELY(ret < 0)) {
                        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: opal_convertor_unpack failed: %d",
                                __FILE__, __LINE__, ret);
                        goto callback_error;
                    }
                }
                /* if it's a sync, send the ack */
                if (MTL_PORTALS4_IS_SYNC_MSG(ev->hdr_data)) {
                    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                            "Recv %lu (0x%lx) sending sync ack",
                            ptl_request->opcount, ptl_request->hdr_data));
                    ret = PtlPut(ompi_mtl_portals4.zero_md_h,
                            0,
                            0,
                            PTL_NO_ACK_REQ,
                            ev->initiator,
                            ompi_mtl_portals4.read_idx,
                            ev->hdr_data,
                            0,
                            NULL,
                            0);
                    if (OPAL_UNLIKELY(PTL_OK != ret)) {
                        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                                "%s:%d: PtlPut failed: %d",
                                __FILE__, __LINE__, ret);
                        goto callback_error;
                    }
                }

                OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                        "Recv %lu (0x%lx) completed, unexpected short (0x%lx)",
                        ptl_request->opcount, ptl_request->hdr_data, (long) ev->start));
                ptl_request->super.super.completion_callback(&ptl_request->super.super);

            } else {
                if (ev->mlength > 0) {
                    /* if rndv or triggered, copy the eager part to the right place */
                    memcpy(ptl_request->delivery_ptr, ev->start, ev->mlength);
                }

                ret = read_msg((char*) ptl_request->delivery_ptr + ev->mlength,
                        ((msg_length > ptl_request->delivery_len) ?
                                ptl_request->delivery_len : msg_length) - ev->mlength,
                                ev->initiator,
                                ev->hdr_data,
                                ev->mlength,
                                ptl_request);
                if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
                    if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
                    goto callback_error;
                }
            }
        }
        break;

    case PTL_EVENT_LINK:
        break;

    default:
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "Unhandled receive callback with event type %d",
                            ev->type);
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;

 callback_error:
    ptl_request->super.super.ompi_req->req_status.MPI_ERROR =
        ompi_mtl_portals4_get_error(ret);
    ptl_request->super.super.completion_callback(&ptl_request->super.super);
    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_irecv(struct mca_mtl_base_module_t* mtl,
                        struct ompi_communicator_t *comm,
                        int src,
                        int tag,
                        struct opal_convertor_t *convertor,
                        mca_mtl_request_t *mtl_request)
{
    ptl_match_bits_t match_bits, ignore_bits;
    ptl_hdr_data_t hdr_data;
    int ret = OMPI_SUCCESS;
    ptl_process_t remote_proc;
    ompi_mtl_portals4_recv_request_t *ptl_request =
        (ompi_mtl_portals4_recv_request_t*) mtl_request;
    void *start;
    size_t length;
    bool free_after;
    ptl_me_t me;

    if  (MPI_ANY_SOURCE == src) {
        if (ompi_mtl_portals4.use_logical) {
            remote_proc.rank = PTL_RANK_ANY;
        } else {
            remote_proc.phys.nid = PTL_NID_ANY;
            remote_proc.phys.pid = PTL_PID_ANY;
        }
    } else if ((ompi_mtl_portals4.use_logical) && (MPI_COMM_WORLD == comm)) {
        remote_proc.rank = src;
    } else {
        ompi_proc_t* ompi_proc = ompi_comm_peer_lookup( comm, src );
        remote_proc = *((ptl_process_t*) ompi_mtl_portals4_get_endpoint (mtl, ompi_proc));
    }

    MTL_PORTALS4_SET_RECV_BITS(match_bits, ignore_bits, comm->c_contextid,
                               src, tag);

    MTL_PORTALS4_SET_HDR_DATA(hdr_data, tag, comm->c_contextid, 0);

    ret = ompi_mtl_datatype_recv_buf(convertor, &start, &length, &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

    ptl_request->is_triggered =
            ((ompi_mtl_portals4.protocol == eager) ||
            (ompi_mtl_portals4.eager_limit >= length) ||
            (MPI_ANY_SOURCE == src) ||
            (MPI_ANY_TAG == tag)) ? false : true;

    if (ptl_request->is_triggered) {
        ret = triggered_read_msg(ptl_request->delivery_ptr + ompi_mtl_portals4.eager_limit,
                ptl_request->delivery_len - ompi_mtl_portals4.eager_limit,
                remote_proc,
                hdr_data,
                ompi_mtl_portals4.eager_limit,
                ptl_request);
    }

    ptl_request->super.type = portals4_req_recv;
    ptl_request->super.event_callback = ompi_mtl_portals4_recv_progress;
#if OPAL_ENABLE_DEBUG
    ptl_request->opcount = OPAL_THREAD_ADD64((int64_t*) &ompi_mtl_portals4.recv_opcount, 1);
    ptl_request->hdr_data = 0;
#endif
    ptl_request->buffer_ptr = (free_after) ? start : NULL;
    ptl_request->convertor = convertor;
    ptl_request->delivery_ptr = start;
    ptl_request->delivery_len = length;
    ptl_request->req_started = false;
    ptl_request->super.super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "Recv %lu from %x,%x of length %ld (0x%lx, 0x%lx, 0x%lx, 0x%lx)\n",
                         ptl_request->opcount,
                         remote_proc.phys.nid, remote_proc.phys.pid,
                         (int64_t)length, match_bits, ignore_bits, hdr_data, (unsigned long) ptl_request));

    me.start = start;
    me.length = length;
    if (ptl_request->is_triggered)
        me.ct_handle = ptl_request->ct_h;
    else
        me.ct_handle = PTL_CT_NONE;
    me.min_free = 0;
    me.uid = ompi_mtl_portals4.uid;
    me.options =
        PTL_ME_OP_PUT |
        PTL_ME_USE_ONCE |
        PTL_ME_EVENT_UNLINK_DISABLE;
    if (ptl_request->is_triggered)
        me.options |= PTL_ME_EVENT_CT_COMM | PTL_ME_EVENT_CT_OVERFLOW;

    if (length <= ompi_mtl_portals4.eager_limit) {
        me.options |= PTL_ME_EVENT_LINK_DISABLE;
    }
    me.match_id = remote_proc;
    me.match_bits = match_bits;
    me.ignore_bits = ignore_bits;

    ret = PtlMEAppend(ompi_mtl_portals4.ni_h,
                      ompi_mtl_portals4.recv_idx,
                      &me,
                      PTL_PRIORITY_LIST,
                      ptl_request,
                      &ptl_request->me_h);
    if (OPAL_UNLIKELY(PTL_OK != ret)) {
        if (NULL != ptl_request->buffer_ptr) free(ptl_request->buffer_ptr);
        opal_output_verbose(1, ompi_mtl_base_framework.framework_output,
                            "%s:%d: PtlMEAppend failed: %d",
                            __FILE__, __LINE__, ret);
        return ompi_mtl_portals4_get_error(ret);
    }

    /* if a long message, spin until we either have a comm event or a
       link event, guaranteeing progress for long unexpected
       messages. */
    if (length > ompi_mtl_portals4.eager_limit) {
        while (true != ptl_request->req_started) {
            ompi_mtl_portals4_progress();
        }
    }

    return OMPI_SUCCESS;
}


int
ompi_mtl_portals4_imrecv(struct mca_mtl_base_module_t* mtl,
                         struct opal_convertor_t *convertor,
                         struct ompi_message_t **message,
                         struct mca_mtl_request_t *mtl_request)
{
    ompi_mtl_portals4_recv_request_t *ptl_request =
        (ompi_mtl_portals4_recv_request_t*) mtl_request;
    void *start;
    size_t length;
    bool free_after;
    int ret;
    ompi_mtl_portals4_message_t *ptl_message =
        (ompi_mtl_portals4_message_t*) (*message)->req_ptr;

    ret = ompi_mtl_datatype_recv_buf(convertor, &start, &length, &free_after);
    if (OPAL_UNLIKELY(OMPI_SUCCESS != ret)) {
        return ret;
    }

#if OPAL_ENABLE_DEBUG
    ptl_request->opcount = OPAL_THREAD_ADD64((int64_t*) &ompi_mtl_portals4.recv_opcount, 1);
    ptl_request->hdr_data = 0;
#endif
    ptl_request->super.type = portals4_req_recv;
    ptl_request->super.event_callback = ompi_mtl_portals4_recv_progress;
    ptl_request->buffer_ptr = (free_after) ? start : NULL;
    ptl_request->convertor = convertor;
    ptl_request->delivery_ptr = start;
    ptl_request->delivery_len = length;
    ptl_request->super.super.ompi_req->req_status.MPI_ERROR = OMPI_SUCCESS;

    OPAL_OUTPUT_VERBOSE((50, ompi_mtl_base_framework.framework_output,
                         "Mrecv %lu of length %ld (0x%lx)\n",
                         ptl_request->opcount,
                         (int64_t)length, (unsigned long) ptl_request));

    (*message) = MPI_MESSAGE_NULL;

    return ompi_mtl_portals4_recv_progress(&(ptl_message->ev), &ptl_request->super);
}
