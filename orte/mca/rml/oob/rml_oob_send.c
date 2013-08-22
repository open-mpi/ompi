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

int orte_rml_oob_send_nb(orte_process_name_t* peer,
                         struct iovec* iov,
                         int count,
                         orte_rml_tag_t tag,
                         orte_rml_callback_fn_t cbfunc,
                         void* cbdata)
{
    orte_rml_recv_t *rcv;
    orte_rml_send_t *snd;
    int bytes;
    orte_self_send_xfer_t *xfer;
    int i;
    char* ptr;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

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
        xfer->iov = iov;
        xfer->count = count;
        xfer->tag = tag;
        xfer->cbfunc.iov = cbfunc;
        xfer->cbdata = cbdata;
        /* setup the event for the send callback */
        opal_event_set(orte_event_base, &xfer->ev, -1, OPAL_EV_WRITE, send_self_exe, xfer);
        opal_event_set_priority(&xfer->ev, ORTE_MSG_PRI);
        opal_event_active(&xfer->ev, OPAL_EV_WRITE, 1);

        /* copy the message for the recv */
        rcv = OBJ_NEW(orte_rml_recv_t);
        rcv->sender = *peer;
        rcv->tag = tag;
        /* get the total number of bytes in the iovec array */
        bytes = 0;
        for (i = 0 ; i < count ; ++i) {
            bytes += iov[i].iov_len;
        }
        /* get the required memory allocation */
        rcv->iov.iov_base = (IOVBASE_TYPE*)malloc(bytes);
        rcv->iov.iov_len = bytes;
        /* transfer the bytes */
        ptr =  (char*)rcv->iov.iov_base;
        for (i = 0 ; i < count ; ++i) {
            memcpy(ptr, iov[i].iov_base, iov[i].iov_len);
            ptr += iov[i].iov_len;
        }
        /* post the message for receipt - since the send callback was posted
         * first and has the same priority, it will execute first
         */
        ORTE_RML_ACTIVATE_MESSAGE(rcv);
        return ORTE_SUCCESS;
    }

    snd = OBJ_NEW(orte_rml_send_t);
    snd->peer = *peer;
    snd->tag = tag;
    snd->iov = iov;
    snd->count = count;
    snd->cbfunc.iov = cbfunc;
    snd->cbdata = cbdata;

    /* activate the OOB send state */
    ORTE_OOB_SEND(snd);

    return ORTE_SUCCESS;
}


int orte_rml_oob_send_buffer_nb(orte_process_name_t* peer,
                                opal_buffer_t* buffer,
                                orte_rml_tag_t tag,
                                orte_rml_buffer_callback_fn_t cbfunc,
                                void* cbdata)
{
    orte_rml_recv_t *rcv;
    orte_rml_send_t *snd;
    orte_self_send_xfer_t *xfer;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "%s rml_send_buffer to peer %s at tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    if (ORTE_RML_TAG_INVALID == tag) {
        /* cannot send to an invalid tag */
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        return ORTE_ERR_BAD_PARAM;
    }

    /* if this is a message to myself, then just post the message
     * for receipt - no need to dive into the oob
     */
    if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, peer, ORTE_PROC_MY_NAME)) {  /* local delivery */
        OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                             "%s rml_send_buffer_to_self at tag %d",
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
        xfer->buffer = buffer;
        xfer->tag = tag;
        xfer->cbfunc.buffer = cbfunc;
        xfer->cbdata = cbdata;
        /* setup the event for the send callback */
        opal_event_set(orte_event_base, &xfer->ev, -1, OPAL_EV_WRITE, send_self_exe, xfer);
        opal_event_set_priority(&xfer->ev, ORTE_MSG_PRI);
        opal_event_active(&xfer->ev, OPAL_EV_WRITE, 1);

        /* copy the message for the recv */
        rcv = OBJ_NEW(orte_rml_recv_t);
        rcv->sender = *peer;
        rcv->tag = tag;
        rcv->iov.iov_base = (IOVBASE_TYPE*)malloc(buffer->bytes_used);
        memcpy(rcv->iov.iov_base, buffer->base_ptr, buffer->bytes_used);
        rcv->iov.iov_len = buffer->bytes_used;
        /* post the message for receipt - since the send callback was posted
         * first and has the same priority, it will execute first
         */
        ORTE_RML_ACTIVATE_MESSAGE(rcv);
        return ORTE_SUCCESS;
    }

    /* setup the message - in this case, we can just
     * point to the buffer as the caller is not allowed
     * to modify or remove that data until we callback
     * that the send is complete. Note that this is the
     * original data, and that we require the OOB to
     * callback with this struct so we can properly
     * respond to our caller
     */
    snd = OBJ_NEW(orte_rml_send_t);
    snd->peer = *peer;
    snd->tag = tag;
    snd->cbfunc.buffer = cbfunc;
    snd->buffer = buffer;
    snd->cbdata = cbdata;

    OPAL_OUTPUT_VERBOSE((1, orte_rml_base_framework.framework_output,
                         "rml_send_buffer_nb %s -> %s tag %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(peer), tag));

    /* activate the OOB send state */
    ORTE_OOB_SEND(snd);

    return ORTE_SUCCESS;
}

