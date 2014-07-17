/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC. 
 *                         All rights reserved.
 * Copyright (c) 2009-2012 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/types.h"

#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"

/*
 * Local utility functions
 */
static void recv_handler(int sd, short flags, void* user);

void pmix_usock_process_send(int fd, short args, void *cbdata)
{
    pmix_usock_msg_op_t *op = (pmix_usock_msg_op_t*)cbdata;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "pmix:usock processing send to server");

    /* add the msg to the send queue if we are already connected*/
    if (PMIX_USOCK_CONNECTED == mca_pmix_native_component.state) {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "usock:send_nb: already connected to server - queueing for send");
        PMIX_USOCK_QUEUE_SEND(op->msg);
        goto cleanup;
    }

    /* add the message to the queue for sending after the
     * connection is formed
     */
    PMIX_USOCK_QUEUE_PENDING(op->msg);

    if (PMIX_USOCK_CONNECTING != peer->state &&
        PMIX_USOCK_CONNECT_ACK != peer->state) {
        /* we have to initiate the connection - again, we do not
         * want to block while the connection is created.
         * So throw us into an event that will create
         * the connection via a mini-state-machine :-)
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s usock:send_nb: initiating connection to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name));
        peer->state = PMIX_USOCK_CONNECTING;
        ORTE_ACTIVATE_USOCK_CONN_STATE(peer, pmix_usock_peer_try_connect);
    }

 cleanup:
    OBJ_RELEASE(op);
}

void pmix_usock_post_recv(int fd, short args, void *cbdata)
{
    pmix_usock_recv_t *req = (pmix_usock_recv_t*)cbdata;
    pmix_usock_recv_t *rcv;
    pmix_usock_msg_t *msg;
    opal_buffer_t buf;

    opal_output_verbose(5, orte_rml_base_framework.framework_output,
                        "%s posting recv",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* bozo check - cannot have two receives for the same tag */
    OPAL_LIST_FOREACH(rcv, &mca_pmix_native_component.posted_recvs, pmix_usock_recv_t) {
        if (req->tag == recv->tag) {
            opal_output(0, "TWO RECEIVES WITH SAME TAG %d - ABORTING", req->tag);
            abort();
        }
    }

    opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                        "posting recv on tag %d", req->tag);
    /* add it to the list of recvs */
    opal_list_append(&mca_pmix_native_component.posted_recvs, &req->super);

    /* check to see if a message has already arrived for this recv */
    OPAL_LIST_FOREACH(msg, &mca_pmix_native_component.unmatched_msgs, pmix_usock_msg_t) {
        opal_output_verbose(5, orte_rml_base_framework.framework_output,
                            "checking unmatched msg on tag %u for tag %u",
                            msg->tag, req->tag);

        if (msg->tag == req->tag) {
            /* construct and load the buffer */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            opal_dss.load(&buf, msg->data, msg->total_bytes);
            if (NULL != req->cbfunc) {
                req->cbfunc(&buf, req->cbdata);
            }
            OBJ_DESTRUCT(&buf);  // free's the msg data
            opal_list_remove_item(&mca_pmix_native_component.unmatched_msgs, &msg->super);
            OBJ_RELEASE(msg);
            /* also done with the request */
            opal_list_remove_item(&mca_pmix_native_component.posted_recvs, &req->super);
            OBJ_RELEASE(req);
            break;
        }
    }
}

void orte_rml_base_process_msg(int fd, short flags, void *cbdata)
{
    orte_rml_recv_t *msg = (orte_rml_recv_t*)cbdata;
    pmix_usock_recv_t *rcv;
    opal_buffer_t buf;

    OPAL_OUTPUT_VERBOSE((5, opal_pmix_base_framework.framework_output,
                         "message received %d bytes for tag %u",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         (int)msg->total_bytes, msg->tag));

    /* see if we have a waiting recv for this message */
    OPAL_LIST_FOREACH(rcv, &mca_pmix_native_component.posted_recvs, pmix_usock_recv_t) {
        opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                            "checking msg on tag %u for tag %u",
                            msg->tag, rcv->tag);

        if (msg->tag == rcv->tag) {
            if (NULL != rcv->cbfunc) {
                /* construct and load the buffer */
                OBJ_CONSTRUCT(&buf, opal_buffer_t);
                opal_dss.load(&buf, msg->data, msg->total_bytes);
                if (NULL != rcv->cbfunc) {
                    req->cbfunc(&buf, rcv->cbdata);
                }
                OBJ_DESTRUCT(&buf);  // free's the msg data
                opal_list_remove_item(&mca_pmix_native_component.unmatched_msgs, &msg->super);
                OBJ_RELEASE(msg);
                /* also done with the recv */
                opal_list_remove_item(&mca_pmix_native_component.posted_recvs, &req->super);
                OBJ_RELEASE(req);
                break;
            }
        }
    }

    /* we get here if no matching recv was found - we then hold
     * the message until such a recv is issued
     */
    opal_list_append(&mca_pmix_native_component.unmatched_msgs, &msg->super);
}

/*
 * Event callback when there is data available on the registered
 * socket to recv.  This is called for the listen sockets to accept an
 * incoming connection, on new sockets trying to complete the software
 * connection process, and for probes.  Data on an established
 * connection is handled elsewhere. 
 */
static void recv_handler(int sd, short flags, void *cbdata)
{
    pmix_usock_conn_op_t *op = (pmix_usock_conn_op_t*)cbdata;
    pmix_usock_hdr_t hdr;
    pmix_usock_peer_t *peer;
    uint64_t *ui64;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:usock:recv:handler called",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* get the handshake */
    if (ORTE_SUCCESS != pmix_usock_peer_recv_connect_ack(NULL, sd, &hdr)) {
        goto cleanup;
    }

    /* finish processing ident */
    if (PMIX_USOCK_IDENT == hdr.type) {
        if (NULL == (peer = pmix_usock_peer_lookup(&hdr.origin))) {
            /* should never happen */
            pmix_usock_peer_close(peer);
            goto cleanup;
        }
        /* set socket up to be non-blocking */
        if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
            opal_output(0, "%s pmix_usock_recv_connect: fcntl(F_GETFL) failed: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
        } else {
            flags |= O_NONBLOCK;
            if (fcntl(sd, F_SETFL, flags) < 0) {
                opal_output(0, "%s pmix_usock_recv_connect: fcntl(F_SETFL) failed: %s (%d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
            }
        }

        /* is the peer instance willing to accept this connection */
        peer->sd = sd;
        if (pmix_usock_peer_accept(peer) == false) {
            if (OOB_USOCK_DEBUG_CONNECT <= opal_output_get_verbosity(orte_oob_base_framework.framework_output)) {
                opal_output(0, "%s-%s pmix_usock_recv_connect: "
                            "rejected connection state %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)),
                            peer->state);
            }
            CLOSE_THE_SOCKET(sd);
            ui64 = (uint64_t*)(&peer->name);
            opal_hash_table_set_value_uint64(&pmix_usock_module.peers, (*ui64), NULL);
            OBJ_RELEASE(peer);
        }
    }

 cleanup:
    OBJ_RELEASE(op);
}
