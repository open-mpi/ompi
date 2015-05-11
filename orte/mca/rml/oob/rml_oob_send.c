/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "opal/types.h"

#include "opal/dss/dss.h"
#include "opal/util/output.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/oob/base/base.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "rml_oob.h"
#include "orte/mca/qos/base/base.h"
typedef struct {
    opal_object_t object;
    opal_event_t ev;
    orte_rml_tag_t tag;
    struct iovec* iov;
    int count;
    opal_buffer_t *buffer;
    union {
        orte_rml_callback_fn_t        iov;
        orte_rml_buffer_callback_fn_t buffer;
    } cbfunc;
    void *cbdata;
} orte_self_send_xfer_t;
static void xfer_cons(orte_self_send_xfer_t *xfer)
{
    xfer->iov = NULL;
    xfer->cbfunc.iov = NULL;
    xfer->buffer = NULL;
    xfer->cbfunc.buffer = NULL;
    xfer->cbdata = NULL;
}
OBJ_CLASS_INSTANCE(orte_self_send_xfer_t,
                   opal_object_t,
                   xfer_cons, NULL);

static void send_self_exe(int fd, short args, void* data)
{
    orte_self_send_xfer_t *xfer = (orte_self_send_xfer_t*)data;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send_to_self callback executing for tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), xfer->tag));

    /* execute the send callback function - note that
     * send-to-self always returns a SUCCESS status
     */
    if (NULL != xfer->iov) {
        if (NULL != xfer->cbfunc.iov) {
            /* non-blocking iovec send */
            xfer->cbfunc.iov(ORTE_SUCCESS, ORTE_PROC_MY_NAME, xfer->iov, xfer->count,
                             xfer->tag, xfer->cbdata);
        }
    } else if (NULL != xfer->buffer) {
        if (NULL != xfer->cbfunc.buffer) {
            /* non-blocking buffer send */
            xfer->cbfunc.buffer(ORTE_SUCCESS, ORTE_PROC_MY_NAME, xfer->buffer,
                                xfer->tag, xfer->cbdata);
        }
    } else {
        /* should never happen */
        abort();
    }

    /* cleanup the memory */
    OBJ_RELEASE(xfer);
}

static void send_msg(int fd, short args, void *cbdata)
{
    orte_rml_send_request_t *req = (orte_rml_send_request_t*)cbdata;
    orte_process_name_t *peer = &(req->post.send.dst);
    orte_rml_tag_t tag = req->post.send.tag;
    orte_rml_recv_t *rcv;
    orte_rml_send_t *snd;
    int bytes;
    orte_self_send_xfer_t *xfer;
    int i;
    char* ptr;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send_msg to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));
    OPAL_TIMING_EVENT((&tm_rml, "to %s", ORTE_NAME_PRINT(peer)));

    /* if this is a message to myself, then just post the message
     * for receipt - no need to dive into the oob
     */
    if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, peer, ORTE_PROC_MY_NAME)) {  /* local delivery */
        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                             "%s rml_send_iovec_to_self at tag %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), tag));
        /* send to self is a tad tricky - we really don't want
         * to track the send callback function throughout the recv
         * process and execute it upon receipt as this would provide
         * very different timing from a non-self message. Specifically,
         * if we just retain a pointer to the incoming data
         * and then execute the send callback prior to the receive,
         * then the caller will think we are done with the data and
         * can release it. So we have to copy the data in order to
         * execute the send callback prior to receiving the message.
         *
         * In truth, this really is a better mimic of the non-self
         * message behavior. If we actually pushed the message out
         * on the wire and had it loop back, then we would receive
         * a new block of data anyway.
         */

        /* setup the send callback */
        xfer = OBJ_NEW(orte_self_send_xfer_t);
        if (NULL != req->post.send.iov) {
            xfer->iov = req->post.send.iov;
            xfer->count = req->post.send.count;
            xfer->cbfunc.iov = req->post.send.cbfunc.iov;
        } else {
            xfer->buffer = req->post.send.buffer;
            xfer->cbfunc.buffer = req->post.send.cbfunc.buffer;
        }
        xfer->tag = tag;
        xfer->cbdata = req->post.send.cbdata;
        /* setup the event for the send callback */
        opal_event_set(orte_event_base, &xfer->ev, -1, OPAL_EV_WRITE, send_self_exe, xfer);
        opal_event_set_priority(&xfer->ev, ORTE_MSG_PRI);
        opal_event_active(&xfer->ev, OPAL_EV_WRITE, 1);

        /* copy the message for the recv */
        rcv = OBJ_NEW(orte_rml_recv_t);
        rcv->sender = *peer;
        rcv->tag = tag;
        if (NULL != req->post.send.iov) {
            /* get the total number of bytes in the iovec array */
            bytes = 0;
            for (i = 0 ; i < req->post.send.count ; ++i) {
                bytes += req->post.send.iov[i].iov_len;
            }
            /* get the required memory allocation */
            if (0 < bytes) {
                rcv->iov.iov_base = (IOVBASE_TYPE*)malloc(bytes);
                rcv->iov.iov_len = bytes;
                /* transfer the bytes */
                ptr =  (char*)rcv->iov.iov_base;
                for (i = 0 ; i < req->post.send.count ; ++i) {
                    memcpy(ptr, req->post.send.iov[i].iov_base, req->post.send.iov[i].iov_len);
                    ptr += req->post.send.iov[i].iov_len;
                }
            }
        } else if (0 < req->post.send.buffer->bytes_used) {
            rcv->iov.iov_base = (IOVBASE_TYPE*)malloc(req->post.send.buffer->bytes_used);
            memcpy(rcv->iov.iov_base, req->post.send.buffer->base_ptr, req->post.send.buffer->bytes_used);
            rcv->iov.iov_len = req->post.send.buffer->bytes_used;
        }
        /* post the message for receipt - since the send callback was posted
         * first and has the same priority, it will execute first
         */
        ORTE_RML_ACTIVATE_MESSAGE(rcv);
        OBJ_RELEASE(req);
        return;
    }

    snd = OBJ_NEW(orte_rml_send_t);
    snd->dst = *peer;
    snd->origin = *ORTE_PROC_MY_NAME;
    snd->tag = tag;
    if (NULL != req->post.send.iov) {
        snd->iov = req->post.send.iov;
        snd->count = req->post.send.count;
        snd->cbfunc.iov = req->post.send.cbfunc.iov;
    } else {
        snd->buffer = req->post.send.buffer;
        snd->cbfunc.buffer = req->post.send.cbfunc.buffer;
    }
    snd->cbdata = req->post.send.cbdata;
    snd->channel = req->post.send.channel;
    /* call send prep to prep the Qos channel for send */
    if (NULL != snd->channel)
    {
        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                             "%s send_msg sending on channel %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), snd->channel->channel_num));
        orte_rml_base_prep_send_channel (snd->channel, snd);
    }
    /* activate the OOB send state */
    ORTE_OOB_SEND(snd);

    OBJ_RELEASE(req);
}



int orte_rml_oob_send_nb(orte_process_name_t* peer,
                         struct iovec* iov,
                         int count,
                         orte_rml_tag_t tag,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata)
{
    orte_rml_send_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if( NULL == peer ||
            OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, peer) ) {
        /* cannot send to an invalid peer */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* get ourselves into an event to protect against
     * race conditions and threads
     */
    req = OBJ_NEW(orte_rml_send_request_t);
    req->post.send.dst = *peer;
    req->post.send.iov = iov;
    req->post.send.count = count;
    req->post.send.tag = tag;
    req->post.send.cbfunc.iov = cbfunc;
    req->post.send.cbdata = cbdata;
    /* setup the event for the send callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, send_msg, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);

    return ORTE_SUCCESS;
}


int orte_rml_oob_send_buffer_nb(orte_process_name_t* peer,
                                opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata)
{
    orte_rml_send_request_t *req;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send_buffer to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    if( NULL == peer ||
            OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, peer) ) {
        /* cannot send to an invalid peer */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* get ourselves into an event to protect against
     * race conditions and threads
     */
    req = OBJ_NEW(orte_rml_send_request_t);
    req->post.send.dst = *peer;
    req->post.send.buffer = buffer;
    req->post.send.tag = tag;
    req->post.send.cbfunc.buffer = cbfunc;
    req->post.send.cbdata = cbdata;
    /* setup the event for the send callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, send_msg, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);

    return ORTE_SUCCESS;
}

int orte_rml_oob_open_channel(orte_process_name_t * peer,
                              opal_list_t *qos_attributes,
                              orte_rml_channel_callback_fn_t cbfunc,
                              void *cbdata)
{
    orte_rml_send_request_t *req;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel to peer %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));
    if( NULL == peer ||
            OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_NAME_INVALID, peer) ) {
        /* cannot send to an invalid peer */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* process the request in an event to be safe */
    req = OBJ_NEW(orte_rml_send_request_t);
    req->post.open_channel.dst = *peer;
    req->post.open_channel.qos_attributes = qos_attributes;
    req->post.open_channel.cbfunc = cbfunc;
    req->post.open_channel.cbdata = cbdata;
    /* setup the event for the open callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, orte_rml_base_open_channel, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_open_channel to peer %s - set event done",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer)));
    return ORTE_SUCCESS;
}

int orte_rml_oob_send_channel_nb (orte_rml_channel_num_t channel_num,
                                  struct iovec* msg,
                                  int count,
                                  orte_rml_tag_t tag,
                                  orte_rml_send_channel_callback_fn_t cbfunc,
                                  void* cbdata)
{
    orte_rml_send_request_t *req;
    orte_rml_channel_t *channel;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send_buffer to channel %d at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         channel_num, tag));

    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    channel = (orte_rml_channel_t*) orte_rml_base_get_channel (channel_num);
    if (NULL == channel) {
        /* cannot send to a non existing or closed channel */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* get ourselves into an event to protect against
    * race conditions and threads
    */
    req = OBJ_NEW(orte_rml_send_request_t);
    req->post.send.dst = channel->peer;
    req->post.send.iov = msg;
    req->post.send.count = count;
    req->post.send.tag = tag;
    req->post.send.cbfunc.iov_chan = cbfunc;
    req->post.send.cbdata = cbdata;
    req->post.send.channel = channel;
    /* setup the event for the send callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, send_msg, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);
    return ORTE_SUCCESS;
}

int orte_rml_oob_send_buffer_channel_nb (orte_rml_channel_num_t channel_num,
        opal_buffer_t *buffer,
        orte_rml_tag_t tag,
        orte_rml_send_buffer_channel_callback_fn_t cbfunc,
        void* cbdata)
{
    orte_rml_send_request_t *req;
    orte_rml_channel_t *channel;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send_buffer to channel %d at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         channel_num, tag));

    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    channel = (orte_rml_channel_t*) orte_rml_base_get_channel (channel_num);
    if (NULL == channel) {
        /* cannot send to a non existing or closed channel */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }
    /* get ourselves into an event to protect against
    * race conditions and threads
    */
    req = OBJ_NEW(orte_rml_send_request_t);
    req->post.send.dst = channel->peer;
    req->post.send.buffer = buffer;
    req->post.send.tag = tag;
    req->post.send.cbfunc.buf_chan = cbfunc;
    req->post.send.cbdata = cbdata;
    req->post.send.channel = channel;
    /* setup the event for the send callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, send_msg, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);
    return ORTE_SUCCESS;
}

int orte_rml_oob_close_channel (orte_rml_channel_num_t channel_num,
                                orte_rml_channel_callback_fn_t cbfunc,
                                void* cbdata)
{
    orte_rml_channel_t *channel;
    orte_rml_send_request_t *req;
    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_close_channel channel num %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         channel_num));
    channel = orte_rml_base_get_channel (channel_num);
    if (NULL == channel)
        return ORTE_ERR_BAD_PARAM;
    /* process the request in an event to be safe */
    req = OBJ_NEW(orte_rml_send_request_t);
    req->post.close_channel.channel = channel;
    req->post.close_channel.cbfunc = cbfunc;
    req->post.close_channel.cbdata = cbdata;
    /* setup the event for the open callback */
    opal_event_set(orte_event_base, &req->ev, -1, OPAL_EV_WRITE, orte_rml_base_close_channel, req);
    opal_event_set_priority(&req->ev, ORTE_MSG_PRI);
    opal_event_active(&req->ev, OPAL_EV_WRITE, 1);
    return ORTE_SUCCESS;
}
