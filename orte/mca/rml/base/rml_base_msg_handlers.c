/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2015 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include <string.h>

#include "orte/constants.h"
#include "orte/types.h"

#include "opal/dss/dss.h"
#include "opal/util/output.h"
#include "opal/util/timings.h"
#include "opal/class/opal_list.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/name_fns.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"
#include "orte/mca/qos/base/base.h"


static void msg_match_recv(orte_rml_posted_recv_t *rcv, bool get_all);


void orte_rml_base_post_recv(int sd, short args, void *cbdata)
{
    orte_rml_recv_request_t *req = (orte_rml_recv_request_t*)cbdata;
    orte_rml_posted_recv_t *post, *recv;
    orte_ns_cmp_bitmask_t mask = ORTE_NS_CMP_ALL | ORTE_NS_CMP_WILD;

    opal_output_verbose(5, orte_rml_base_framework.framework_output,
                        "%s posting recv",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    if (NULL == req) {
        /* this can only happen if something is really wrong, but
         * someone managed to get here in a bizarre test */
        opal_output(0, "%s CANNOT POST NULL RML RECV REQUEST",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return;
    }
    post = req->post;

    /* if the request is to cancel a recv, then find the recv
     * and remove it from our list
     */
    if (req->cancel) {
        OPAL_LIST_FOREACH(recv, &orte_rml_base.posted_recvs, orte_rml_posted_recv_t) {
            if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &post->peer, &recv->peer) &&
                post->tag == recv->tag) {
                opal_output_verbose(5, orte_rml_base_framework.framework_output,
                                    "%s canceling recv %d for peer %s",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    post->tag, ORTE_NAME_PRINT(&recv->peer));
                /* got a match - remove it */
                opal_list_remove_item(&orte_rml_base.posted_recvs, &recv->super);
                OBJ_RELEASE(recv);
                break;
            }
        }
        OBJ_RELEASE(req);
        return;
    }

    /* bozo check - cannot have two receives for the same peer/tag combination */
    OPAL_LIST_FOREACH(recv, &orte_rml_base.posted_recvs, orte_rml_posted_recv_t) {
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &post->peer, &recv->peer) &&
            post->tag == recv->tag) {
            opal_output(0, "%s TWO RECEIVES WITH SAME PEER %s AND TAG %d - ABORTING",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&post->peer), post->tag);
            abort();
        }
    }

    opal_output_verbose(5, orte_rml_base_framework.framework_output,
                        "%s posting %s recv on tag %d for peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (post->persistent) ? "persistent" : "non-persistent",
                        post->tag, ORTE_NAME_PRINT(&post->peer));
    /* add it to the list of recvs */
    opal_list_append(&orte_rml_base.posted_recvs, &post->super);
    req->post = NULL;
    /* handle any messages that may have already arrived for this recv */
    msg_match_recv(post, post->persistent);

    /* cleanup */
    OBJ_RELEASE(req);
}

void orte_rml_base_complete_recv_msg (orte_rml_recv_t **recv_msg)
{
    orte_rml_posted_recv_t *post;
    orte_ns_cmp_bitmask_t mask = ORTE_NS_CMP_ALL | ORTE_NS_CMP_WILD;
    opal_buffer_t buf;
    orte_rml_recv_t *msg = *recv_msg;
    /* see if we have a waiting recv for this message */
    OPAL_LIST_FOREACH(post, &orte_rml_base.posted_recvs, orte_rml_posted_recv_t) {
        /* since names could include wildcards, must use
         * the more generalized comparison function
         */
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &msg->sender, &post->peer) &&
            msg->tag == post->tag) {
            /* deliver the data to this location */
            if (post->buffer_data) {
                /* deliver it in a buffer */
                OBJ_CONSTRUCT(&buf, opal_buffer_t);
                opal_dss.load(&buf, msg->iov.iov_base, msg->iov.iov_len);
                /* xfer ownership of the malloc'd data to the buffer */
                msg->iov.iov_base = NULL;
                post->cbfunc.buffer(ORTE_SUCCESS, &msg->sender, &buf, msg->tag, post->cbdata);
                /* the user must have unloaded the buffer if they wanted
                 * to retain ownership of it, so release whatever remains
                 */
                OPAL_OUTPUT_VERBOSE((5, orte_rml_base_framework.framework_output,
                                     "%s message received  bytes from %s for tag %d on channel=%d called callback",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&msg->sender),
                                     msg->tag,
                                     msg->channel_num));
                OBJ_DESTRUCT(&buf);
            } else {
                /* deliver as an iovec */
                post->cbfunc.iov(ORTE_SUCCESS, &msg->sender, &msg->iov, 1, msg->tag, post->cbdata);
                /* the user should have shifted the data to
                 * a local variable and NULL'd the iov_base
                 * if they wanted ownership of the data
                 */
            }
            /* release the message */
            OBJ_RELEASE(msg);
            OPAL_OUTPUT_VERBOSE((5, orte_rml_base_framework.framework_output,
                                 "%s message tag %d on released",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 post->tag));
            /* if the recv is non-persistent, remove it */
            if (!post->persistent) {
                opal_list_remove_item(&orte_rml_base.posted_recvs, &post->super);
                /*OPAL_OUTPUT_VERBOSE((5, orte_rml_base_framework.framework_output,
                                     "%s non persistent recv %p remove success releasing now",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     post));*/
                OBJ_RELEASE(post);

            }
            return;
        }
    }
    /* we get here if no matching recv was found - we then hold
     * the message until such a recv is issued
     */
     OPAL_OUTPUT_VERBOSE((5, orte_rml_base_framework.framework_output,
                            "%s message received  bytes from %s for tag %d on channel=%d Not Matched adding to unmatched msgs",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&msg->sender),
                            msg->tag,
                            msg->channel_num));
     opal_list_append(&orte_rml_base.unmatched_msgs, &msg->super);
}

static void msg_match_recv(orte_rml_posted_recv_t *rcv, bool get_all)
{
    opal_list_item_t *item, *next;
    orte_rml_recv_t *msg;
    orte_ns_cmp_bitmask_t mask = ORTE_NS_CMP_ALL | ORTE_NS_CMP_WILD;

    /* scan thru the list of unmatched recvd messages and
     * see if any matches this spec - if so, push the first
     * into the recvd msg queue and look no further
     */
    item = opal_list_get_first(&orte_rml_base.unmatched_msgs);
    while (item != opal_list_get_end(&orte_rml_base.unmatched_msgs)) {
        next = opal_list_get_next(item);
        msg = (orte_rml_recv_t*)item;
        opal_output_verbose(5, orte_rml_base_framework.framework_output,
                            "%s checking recv for %s against unmatched msg from %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&rcv->peer),
                            ORTE_NAME_PRINT(&msg->sender));

        /* since names could include wildcards, must use
         * the more generalized comparison function
         */
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &msg->sender, &rcv->peer) &&
            msg->tag == rcv->tag) {
            ORTE_RML_REACTIVATE_MESSAGE(msg);
            opal_list_remove_item(&orte_rml_base.unmatched_msgs, item);
            if (!get_all) {
                break;
            }
        }
        item = next;
    }
}

void orte_rml_base_process_msg(int fd, short flags, void *cbdata)
{
    orte_rml_recv_t *msg = (orte_rml_recv_t*)cbdata;
    OPAL_OUTPUT_VERBOSE((5, orte_rml_base_framework.framework_output,
                         "%s message received from %s for tag %d on channel=%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&msg->sender),
                         msg->tag,
                         msg->channel_num));

    OPAL_TIMING_EVENT((&tm_rml,"from %s %d bytes",
                       ORTE_NAME_PRINT(&msg->sender), msg->iov.iov_len));
    if ((ORTE_RML_INVALID_CHANNEL_NUM != msg->channel_num) &&
            (NULL != orte_rml_base_get_channel(msg->channel_num) )) {

        // call channel for recv post processing
        if (ORTE_SUCCESS != (orte_rml_base_process_recv_channel (orte_rml_base_get_channel(msg->channel_num), msg)))
        {
            /* the qos channel has determined an error so we cannot complete this msg to the caller */
            OPAL_OUTPUT_VERBOSE((5, orte_rml_base_framework.framework_output,
                                 "%s QoS channel receive error - cannot complete msg on channel=%d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 msg->channel_num));
            return;
        }

    }
    orte_rml_base_complete_recv_msg (&msg);
}

void orte_rml_base_reprocess_msg(int fd, short flags, void *cbdata)
{
    orte_rml_recv_t *msg = (orte_rml_recv_t*)cbdata;
    OPAL_OUTPUT_VERBOSE((5, orte_rml_base_framework.framework_output,
                         "%s reprocessing msg received from %s for tag %d on channel=%d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&msg->sender),
                         msg->tag,
                         msg->channel_num));

    OPAL_TIMING_EVENT((&tm_rml,"from %s %d bytes",
                       ORTE_NAME_PRINT(&msg->sender), msg->iov.iov_len));
    orte_rml_base_complete_recv_msg ( &msg);
    /* the msg should be matched and released in this path
     add an assert (msg!= NULL) ?? */
}
