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

#include "orte_config.h"
#include "orte/types.h"
#include "opal/types.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <ctype.h>

#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/opal_socket_errno.h"
#include "opal/util/if.h"
#include "opal/util/net.h"
#include "opal/util/argv.h"
#include "opal/class/opal_hash_table.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/routed/routed.h"
#include "orte/util/name_fns.h"
#include "orte/util/parse_options.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/oob/usock/oob_usock.h"
#include "orte/mca/oob/usock/oob_usock_component.h"
#include "orte/mca/oob/usock/oob_usock_peer.h"
#include "orte/mca/oob/usock/oob_usock_connection.h"
#include "orte/mca/oob/usock/oob_usock_ping.h"

static void usock_init(void);
static void usock_fini(void);
static void accept_connection(const int accepted_fd,
                              const struct sockaddr *addr);
static void ping(const orte_process_name_t *proc);
static void send_nb(orte_rml_send_t *msg);
static void ft_event(int state);

mca_oob_usock_module_t mca_oob_usock_module = {
    {
        usock_init,
        usock_fini,
        accept_connection,
        ping,
        send_nb,
        ft_event
    }
};

/*
 * Local utility functions
 */
static void recv_handler(int sd, short flags, void* user);
static void* progress_thread_engine(opal_object_t *obj)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s USOCK PROGRESS THREAD RUNNING",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    while (mca_oob_usock_module.ev_active) {
        opal_event_loop(mca_oob_usock_module.ev_base, OPAL_EVLOOP_ONCE);
    }
    return OPAL_THREAD_CANCELLED;
}


/*
 * Initialize global variables used w/in this module.
 */
static void usock_init(void)
{
    /* setup the module's state variables */
    OBJ_CONSTRUCT(&mca_oob_usock_module.peers, opal_hash_table_t);
    opal_hash_table_init(&mca_oob_usock_module.peers, 32);
    mca_oob_usock_module.ev_active = false;

    if (orte_oob_base.use_module_threads) {
        /* if we are to use independent progress threads at
         * the module level, start it now
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s STARTING USOCK PROGRESS THREAD",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        mca_oob_usock_module.ev_base = opal_event_base_create();
        /* construct the thread object */
        OBJ_CONSTRUCT(&mca_oob_usock_module.progress_thread, opal_thread_t);
        /* fork off a thread to progress it */
        mca_oob_usock_module.progress_thread.t_run = progress_thread_engine;
        mca_oob_usock_module.progress_thread.t_arg = NULL;
        mca_oob_usock_module.ev_active = true;
        if (OPAL_SUCCESS != opal_thread_start(&mca_oob_usock_module.progress_thread)) {
            opal_output(0, "%s USOCK progress thread failed to start",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        }
    } else {
        mca_oob_usock_module.ev_base = orte_event_base;
    }
}

/*
 * Module cleanup.
 */
static void usock_fini(void)
{
    /* cleanup all peers */
    OBJ_DESTRUCT(&mca_oob_usock_module.peers);

    if (mca_oob_usock_module.ev_active) {
        /* if we used an independent progress thread at
         * the module level, stop it now
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s STOPPING USOCK PROGRESS THREAD",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* stop the progress thread */
        mca_oob_usock_module.ev_active = false;
        /* break the event loop */
        opal_event_base_loopexit(mca_oob_usock_module.ev_base);
        /* wait for thread to exit */
        opal_thread_join(&mca_oob_usock_module.progress_thread, NULL);
        OBJ_DESTRUCT(&mca_oob_usock_module.progress_thread);
        /* release the event base */
        opal_event_base_free(mca_oob_usock_module.ev_base);
    }
}

/* Called by mca_oob_usock_accept() and connection_handler() on
 * a socket that has been accepted.  This call finishes processing the
 * socket by registering for the OOB-level connection handshake.  Used
 * in both the threaded and event listen modes.
 */
static void accept_connection(const int accepted_fd,
                              const struct sockaddr *addr)
{
    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s accept_connection",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* use a one-time event to wait for receipt of peer's
     * process ident message to complete this connection
     */
    ORTE_ACTIVATE_USOCK_ACCEPT_STATE(accepted_fd, addr, recv_handler);
}

/* API functions */
static void process_ping(int fd, short args, void *cbdata)
{
    mca_oob_usock_ping_t *op = (mca_oob_usock_ping_t*)cbdata;
    mca_oob_usock_peer_t *peer;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:[%s:%d] processing ping to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        __FILE__, __LINE__,
                        ORTE_NAME_PRINT(&op->peer));

    /* do we know this peer? */
    if (NULL == (peer = mca_oob_usock_peer_lookup(&op->peer))) {
        /* push this back to the framework so another component can try */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] hop %s unknown",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&op->peer));
#if 0
        ORTE_ACTIVATE_USOCK_MSG_ERROR(NULL, NULL, &op->peer, mca_oob_usock_component_hop_unknown);
#endif
        goto cleanup;
    }

    /* if we are already connected, there is nothing to do */
    if (MCA_OOB_USOCK_CONNECTED == peer->state) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] already connected to peer %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&op->peer));
        goto cleanup;
    }

    /* if we are already connecting, there is nothing to do */
    if (MCA_OOB_USOCK_CONNECTING == peer->state &&
        MCA_OOB_USOCK_CONNECT_ACK == peer->state) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s:[%s:%d] already connecting to peer %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            __FILE__, __LINE__,
                            ORTE_NAME_PRINT(&op->peer));
        goto cleanup;
    }

    /* attempt the connection */
    peer->state = MCA_OOB_USOCK_CONNECTING;
    ORTE_ACTIVATE_USOCK_CONN_STATE(peer, mca_oob_usock_peer_try_connect);

 cleanup:
    OBJ_RELEASE(op);
}

static void ping(const orte_process_name_t *proc)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:[%s:%d] pinging peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        __FILE__, __LINE__,
                        ORTE_NAME_PRINT(proc));

    /* push this into our event base for processing */
    ORTE_ACTIVATE_USOCK_PING(proc, process_ping);
}

static void process_send(int fd, short args, void *cbdata)
{
    mca_oob_usock_msg_op_t *op = (mca_oob_usock_msg_op_t*)cbdata;
    mca_oob_usock_peer_t *peer;

    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s:[%s:%d] processing send to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        __FILE__, __LINE__,
                        ORTE_NAME_PRINT(&op->msg->dst));

    /* if I am an app, the only route is to my daemon, so
     * send the msg there
     */
    if (ORTE_PROC_IS_APP) {
        if (NULL == (peer = mca_oob_usock_peer_lookup(ORTE_PROC_MY_DAEMON))) {
            /* we don't know how to talk to our daemon,
             * which is strange since we already got here.
             * likely means we lost a race condition, so
             * 
             */
            ORTE_ACTIVATE_USOCK_MSG_ERROR(NULL, op->msg,
                                          ORTE_PROC_MY_DAEMON,
                                          mca_oob_usock_component_cannot_send);
            goto cleanup;
        }
    } else if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        /* if I am a daemon, the only way I should be given this
         * message to send is if the proc is local to me
         */
        if (NULL == (peer = mca_oob_usock_peer_lookup(&op->msg->dst))) {
            /* we don't know how to talk to this proc,
             * so send this back up to the OOB base so it
             * can try another transport
             */
            ORTE_ACTIVATE_USOCK_MSG_ERROR(NULL, op->msg,
                                          &op->msg->dst,
                                          mca_oob_usock_component_cannot_send);
            goto cleanup;
        }
    } else {
        /* otherwise, this message can't be handled by me, so
         * notify the component of the mistake
         */
        opal_output(0, "CAN'T BE HANDLED");
        goto cleanup;
    }

    /* add the msg to the target's send queue */
    if (MCA_OOB_USOCK_CONNECTED == peer->state) {
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s usock:send_nb: already connected to %s - queueing for send",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name));
        MCA_OOB_USOCK_QUEUE_SEND(op->msg, peer);
        goto cleanup;
    }

    /* add the message to the queue for sending after the
     * connection is formed
     */
    MCA_OOB_USOCK_QUEUE_PENDING(op->msg, peer);

    if (MCA_OOB_USOCK_CONNECTING != peer->state &&
        MCA_OOB_USOCK_CONNECT_ACK != peer->state) {
        /* we have to initiate the connection - again, we do not
         * want to block while the connection is created.
         * So throw us into an event that will create
         * the connection via a mini-state-machine :-)
         */
        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                            "%s usock:send_nb: initiating connection to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name));
        peer->state = MCA_OOB_USOCK_CONNECTING;
        ORTE_ACTIVATE_USOCK_CONN_STATE(peer, mca_oob_usock_peer_try_connect);
    }

 cleanup:
    OBJ_RELEASE(op);
}

static void send_nb(orte_rml_send_t *msg)
{
    opal_output_verbose(2, orte_oob_base_framework.framework_output,
                        "%s usock:send_nb to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&msg->dst));

    /* push this into our event base for processing */
    ORTE_ACTIVATE_USOCK_POST_SEND(msg, process_send);
}

/*
 * Handle probe
 */
static void recv_probe(int sd, mca_oob_usock_hdr_t* hdr)
{
    unsigned char* ptr = (unsigned char*)hdr;
    size_t cnt = 0;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:usock:recv:probe called",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    hdr->type = MCA_OOB_USOCK_PROBE;
    hdr->dst = *ORTE_PROC_MY_NAME;

    while (cnt < sizeof(mca_oob_usock_hdr_t)) {
        int retval = send(sd, (char *)ptr+cnt, sizeof(mca_oob_usock_hdr_t)-cnt, 0);
        if (retval < 0) {
            if (opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                opal_output(0, "%s-%s mca_oob_usock_peer_recv_probe: send() failed: %s (%d)\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(hdr->dst)),
                            strerror(opal_socket_errno),
                            opal_socket_errno);
                CLOSE_THE_SOCKET(sd);
                return;
            }
            continue;
        }
        cnt += retval;
    }
    CLOSE_THE_SOCKET(sd);
}

/*
 * Complete the OOB-level handshake to establish a connection with
 * another peer.  Called when the remote peer replies with his process
 * identifier.  Used in both the threaded and event listen modes.
 */
static void recv_connect(int sd, mca_oob_usock_hdr_t* hdr)
{
    mca_oob_usock_peer_t* peer;
    int flags;
    int cmpval;
    uint64_t *ui64;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:usock:recv:connect called",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* check for invalid name - if this is true, then we have an error
     */
    cmpval = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &hdr->dst, ORTE_NAME_INVALID);
    if (cmpval == OPAL_EQUAL) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        return;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s mca_oob_usock_recv_connect: processing connection from %s for socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&hdr->dst), sd);

    /* lookup the corresponding process */
    peer = mca_oob_usock_peer_lookup(&hdr->dst);
    if (NULL == peer) {
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s mca_oob_usock_recv_connect: connection from new peer %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(&hdr->dst));
        peer = OBJ_NEW(mca_oob_usock_peer_t);
        peer->name = hdr->dst;
        peer->state = MCA_OOB_USOCK_ACCEPTING;
        ui64 = (uint64_t*)(&peer->name);
        if (OPAL_SUCCESS != opal_hash_table_set_value_uint64(&mca_oob_usock_module.peers, (*ui64), peer)) {
            OBJ_RELEASE(peer);
            return;
        }
    } else {
        /* check for a race condition - if I was in the process of
         * creating a connection to the peer, or have already established
         * such a connection, then we need to reject this connection. We will
         * let the higher ranked process retry - if I'm the lower ranked
         * process, I'll simply defer until I receive the request
         */
        if (MCA_OOB_USOCK_CONNECTED == peer->state ||
            MCA_OOB_USOCK_CONNECTING == peer->state ||
            MCA_OOB_USOCK_CONNECT_ACK == peer->state) {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                "%s SIMUL CONNECTION WITH %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&hdr->dst));
            if (peer->recv_ev_active) {
                opal_event_del(&peer->recv_event);
                peer->recv_ev_active = false;
            }
            if (peer->send_ev_active) {
                opal_event_del(&peer->send_event);
                peer->send_ev_active = false;
            }
            if (0 < peer->sd) {
                CLOSE_THE_SOCKET(peer->sd);
                peer->sd = -1;
            }
            CLOSE_THE_SOCKET(sd);
            cmpval = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &hdr->dst, ORTE_PROC_MY_NAME);
            if (OPAL_VALUE1_GREATER == cmpval) {
                /* force the other end to retry the connection */
                peer->state = MCA_OOB_USOCK_UNCONNECTED;
                return;
            } else {
                /* retry the connection */
                peer->state = MCA_OOB_USOCK_CONNECTING;
                ORTE_ACTIVATE_USOCK_CONN_STATE(peer, mca_oob_usock_peer_try_connect);
                return;
            }
        }
    }
    
    /* set socket up to be non-blocking */
    if ((flags = fcntl(sd, F_GETFL, 0)) < 0) {
        opal_output(0, "%s mca_oob_usock_recv_connect: fcntl(F_GETFL) failed: %s (%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if (fcntl(sd, F_SETFL, flags) < 0) {
            opal_output(0, "%s mca_oob_usock_recv_connect: fcntl(F_SETFL) failed: %s (%d)",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
        }
    }

    /* is the peer instance willing to accept this connection */
    peer->sd = sd;
    if (mca_oob_usock_peer_accept(peer) == false) {
        if (OOB_USOCK_DEBUG_CONNECT <= opal_output_get_verbosity(orte_oob_base_framework.framework_output)) {
            opal_output(0, "%s-%s mca_oob_usock_recv_connect: "
                        "rejected connection state %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)),
                        peer->state);
        }
        CLOSE_THE_SOCKET(sd);
        ui64 = (uint64_t*)(&peer->name);
        opal_hash_table_set_value_uint64(&mca_oob_usock_module.peers, (*ui64), NULL);
        OBJ_RELEASE(peer);
    }
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
    mca_oob_usock_conn_op_t *op = (mca_oob_usock_conn_op_t*)cbdata;
    mca_oob_usock_hdr_t hdr;
    int rc;
    size_t cnt;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:usock:recv:handler called",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* ensure all is zero'd */
    memset(&hdr, 0, sizeof(hdr));

    /* recv the process identifier */
    cnt = 0;
    while (cnt < sizeof(hdr)) {
        rc = recv(sd, (char *)&hdr, sizeof(hdr), 0);
        if (0 == rc) {
            if (OOB_USOCK_DEBUG_CONNECT <= opal_output_get_verbosity(orte_oob_base_framework.framework_output)) {
                opal_output(0, "%s mca_oob_usock_recv_handler: peer closed connection",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            }
            CLOSE_THE_SOCKET(sd);
            goto cleanup;
        } else if (rc < 0) {
            if (opal_socket_errno != EINTR && 
                opal_socket_errno != EAGAIN && 
                opal_socket_errno != EWOULDBLOCK) {
                opal_output(0, "%s mca_oob_usock_recv_handler: recv() failed: %s (%d)\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), strerror(opal_socket_errno), opal_socket_errno);
                CLOSE_THE_SOCKET(sd);
                goto cleanup;
            }
            continue;
        }
        cnt += rc;
    }

    /* dispatch based on message type */
    switch (hdr.type) {
    case MCA_OOB_USOCK_PROBE:
        recv_probe(sd, &hdr);
        break;
    case MCA_OOB_USOCK_IDENT:
        recv_connect(sd, &hdr);
        break;
    default:
        opal_output(0, "%s mca_oob_usock_recv_handler: invalid message type: %d\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), hdr.type);
        CLOSE_THE_SOCKET(sd);
        break;
    }

 cleanup:
    OBJ_RELEASE(op);
}

/* Dummy function for when we are not using FT. */
#if OPAL_ENABLE_FT_CR == 0
static void ft_event(int state)
{
    return;
}

#else
static void ft_event(int state) {
    opal_list_item_t *item;

    if(OPAL_CRS_CHECKPOINT == state) {
#if 0
        /*
         * Disable event processing while we are working
         */
        opal_event_disable();
#endif
    }
    else if(OPAL_CRS_CONTINUE == state) {
#if 0
        /*
         * Resume event processing
         */
        opal_event_enable();
    }
    else if(OPAL_CRS_RESTART == state) {
        /*
         * Clean out cached connection information
         * Select pieces of finalize/init
         */
        for (item = opal_list_remove_first(&mod->peer_list);
            item != NULL;
            item = opal_list_remove_first(&mod->peer_list)) {
            mca_oob_usock_peer_t* peer = (mca_oob_usock_peer_t*)item;
            /* JJH: Use the below command for debugging restarts with invalid sockets
             * mca_oob_usock_peer_dump(peer, "RESTART CLEAN")
             */
            MCA_OOB_USOCK_PEER_RETURN(peer);
        }

        OBJ_DESTRUCT(&mod->peer_free);
        OBJ_DESTRUCT(&mod->peer_names);
        OBJ_DESTRUCT(&mod->peers);
        OBJ_DESTRUCT(&mod->peer_list);

        OBJ_CONSTRUCT(&mod->peer_list,     opal_list_t);
        OBJ_CONSTRUCT(&mod->peers,         opal_hash_table_t);
        OBJ_CONSTRUCT(&mod->peer_names,    opal_hash_table_t);
        OBJ_CONSTRUCT(&mod->peer_free,     opal_free_list_t);

        /*
         * Resume event processing
         */
        opal_event_enable();
#endif
    }
    else if(OPAL_CRS_TERM == state ) {
        ;
    }
    else {
        ;
    }

    return;
}
#endif
