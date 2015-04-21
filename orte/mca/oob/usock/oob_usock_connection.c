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
 * Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2014 Intel, Inc.  All rights reserved. 
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <fcntl.h>
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "opal/opal_socket_errno.h"
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif

#include "opal/types.h"
#include "opal_stdint.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/mca/base/mca_base_var.h"
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/sec/sec.h"
#include "opal/util/output.h"
#include "opal/util/net.h"
#include "opal/util/error.h"
#include "opal/util/fd.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/event/event.h"

#include "orte/util/name_fns.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/runtime/orte_wait.h"

#include "oob_usock.h"
#include "orte/mca/oob/usock/oob_usock_component.h"
#include "orte/mca/oob/usock/oob_usock_peer.h"
#include "orte/mca/oob/usock/oob_usock_connection.h"

static void usock_peer_event_init(mca_oob_usock_peer_t* peer);
static int  usock_peer_send_connect_ack(mca_oob_usock_peer_t* peer);
static int usock_peer_send_blocking(mca_oob_usock_peer_t* peer,
                                    int sd, void* data, size_t size);
static bool usock_peer_recv_blocking(mca_oob_usock_peer_t* peer,
                                     int sd, void* data, size_t size);
static void usock_peer_connected(mca_oob_usock_peer_t* peer);

static int usock_peer_create_socket(mca_oob_usock_peer_t* peer)
{
    int flags;

    if (peer->sd > 0) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_oob_base_framework.framework_output,
                         "%s oob:usock:peer creating socket to %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(&(peer->name))));
    
    peer->sd = socket(PF_UNIX, SOCK_STREAM, 0);

    if (peer->sd < 0) {
        opal_output(0, "%s-%s usock_peer_create_socket: socket() failed: %s (%d)\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    strerror(opal_socket_errno),
                    opal_socket_errno);
        return ORTE_ERR_UNREACH;
    }
    /* Set this fd to be close-on-exec so that subsequent children don't see it */
    if (opal_fd_set_cloexec(peer->sd) != OPAL_SUCCESS) {
        opal_output(0, "%s unable to set socket to CLOEXEC",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        close(peer->sd);
        peer->sd = -1;
        return ORTE_ERROR;
    }

    /* setup event callbacks */
    usock_peer_event_init(peer);

    /* setup the socket as non-blocking */
    if (peer->sd >= 0) {
        if ((flags = fcntl(peer->sd, F_GETFL, 0)) < 0) {
            opal_output(0, "%s-%s usock_peer_connect: fcntl(F_GETFL) failed: %s (%d)\n", 
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)),
                        strerror(opal_socket_errno),
                        opal_socket_errno);
        } else {
            flags |= O_NONBLOCK;
            if(fcntl(peer->sd, F_SETFL, flags) < 0)
                opal_output(0, "%s-%s usock_peer_connect: fcntl(F_SETFL) failed: %s (%d)\n", 
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)),
                            strerror(opal_socket_errno),
                            opal_socket_errno);
        }
    }

    return ORTE_SUCCESS;
}


/*
 * Try connecting to a peer
 */
void mca_oob_usock_peer_try_connect(int fd, short args, void *cbdata)
{
    mca_oob_usock_conn_op_t *op = (mca_oob_usock_conn_op_t*)cbdata;
    mca_oob_usock_peer_t *peer = op->peer;
    int rc;
    opal_socklen_t addrlen = 0;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s orte_usock_peer_try_connect: "
                        "attempting to connect to proc %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)));

    rc = usock_peer_create_socket(peer);
    if (ORTE_SUCCESS != rc) {
        /* FIXME: we cannot create a USOCK socket - report
         * back to the component that this peer is
         * unreachable so it can remove the peer
         * from its list and report back to the base
         * NOTE: this could be a reconnect attempt,
         * so we also need to mark any queued messages
         * and return them as "unreachable"
         */
        opal_output(0, "%s CANNOT CREATE SOCKET", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        ORTE_FORCED_TERMINATE(1);
        OBJ_RELEASE(op);
        return;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s orte_usock_peer_try_connect: "
                        "attempting to connect to proc %s on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)), peer->sd);

    addrlen = sizeof(struct sockaddr_un);
    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s orte_usock_peer_try_connect: "
                        "attempting to connect to proc %s - %d retries",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)),
                        peer->retries);

 retry_connect:
    peer->retries++;
    if (connect(peer->sd, (struct sockaddr *) &mca_oob_usock_component.address, addrlen) < 0) {
        /* non-blocking so wait for completion */
        if (opal_socket_errno == EINPROGRESS || opal_socket_errno == EWOULDBLOCK) {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                "%s waiting for connect completion to %s - activating send event",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&peer->name));
            /* just ensure the send_event is active */
            if (!peer->send_ev_active) {
                opal_event_add(&peer->send_event, 0);
                peer->send_ev_active = true;
            }
            OBJ_RELEASE(op);
            return;
        }

        /* Some kernels (Linux 2.6) will automatically software
           abort a connection that was ECONNREFUSED on the last
           attempt, without even trying to establish the
           connection.  Handle that case in a semi-rational
           way by trying twice before giving up */
        if (ECONNABORTED == opal_socket_errno) {
            if (peer->retries < mca_oob_usock_component.max_retries) {
                opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                    "%s connection aborted by OS to %s - retrying",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&peer->name));
                goto retry_connect;
            } else {
                /* We were unsuccessful in establishing this connection, and are
                 * not likely to suddenly become successful,
                 */
                opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                    "%s orte_usock_peer_try_connect: "
                                    "Connection across unix domain socket to local proc %s failed",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&peer->name));
                peer->state = MCA_OOB_USOCK_FAILED;
                CLOSE_THE_SOCKET(peer->sd);
                /* let the USOCK component know that this module failed to make
                 * the connection so it can try other modules, and/or fail back
                 * to the OOB level so another component can try. This will activate
                 * an event in the component event base, and so it will fire async
                 * from us if we are in our own progress thread
                 */
                ORTE_ACTIVATE_USOCK_CMP_OP(peer, mca_oob_usock_component_failed_to_connect);
                OBJ_RELEASE(op);
                return;
            }
        }
    }

    /* connection succeeded */
    peer->retries = 0;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s orte_usock_peer_try_connect: "
                        "Connection across to proc %s succeeded",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));
 
    /* setup our recv to catch the return ack call */
    if (!peer->recv_ev_active) {
        opal_event_add(&peer->recv_event, 0);
        peer->recv_ev_active = true;
    }

    /* send our globally unique process identifier to the peer */
    if (ORTE_SUCCESS == (rc = usock_peer_send_connect_ack(peer))) {
        peer->state = MCA_OOB_USOCK_CONNECT_ACK;
    } else {
        opal_output(0, 
                    "%s orte_usock_peer_try_connect: "
                    "usock_peer_send_connect_ack to proc %s failed: %s (%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    opal_strerror(rc), rc);
        ORTE_FORCED_TERMINATE(1);
    }

    OBJ_RELEASE(op);
}

static int usock_peer_send_connect_ack(mca_oob_usock_peer_t* peer)
{
    char *msg;
    mca_oob_usock_hdr_t hdr;
    int rc;
    size_t sdsize;
    char *cred;
    size_t credsize;
    
    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s SEND CONNECT ACK", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* send a handshake that includes our process identifier
     * to ensure we are talking to another OMPI process
    */
    hdr.origin = *ORTE_PROC_MY_NAME;
    hdr.dst = peer->name;
    hdr.type = MCA_OOB_USOCK_IDENT;
    hdr.tag = 0;

    /* get our security credential*/
    if (OPAL_SUCCESS != (rc = opal_sec.get_my_credential(peer->auth_method,
                                                         opal_dstore_internal,
                                                         ORTE_PROC_MY_NAME, &cred, &credsize))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* set the number of bytes to be read beyond the header */
    hdr.nbytes = strlen(orte_version_string) + 1 + credsize;

    /* create a space for our message */
    sdsize = (sizeof(hdr) + strlen(orte_version_string) + 1 + credsize);
    if (NULL == (msg = (char*)malloc(sdsize))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the message */
    memcpy(msg, &hdr, sizeof(hdr));
    memcpy(msg+sizeof(hdr), orte_version_string, strlen(orte_version_string));
    memcpy(msg+sizeof(hdr)+strlen(orte_version_string)+1, cred, credsize);
    free(cred);
    
    if (ORTE_SUCCESS != usock_peer_send_blocking(peer, peer->sd, msg, sdsize)) {
        ORTE_ERROR_LOG(ORTE_ERR_UNREACH);
        free(msg);
        return ORTE_ERR_UNREACH;
    }
    free(msg);
    return ORTE_SUCCESS;
}

/*
 * Initialize events to be used by the peer instance for USOCK select/poll callbacks.
 */
static void usock_peer_event_init(mca_oob_usock_peer_t* peer)
{
    if (peer->sd >= 0) {
        opal_event_set(mca_oob_usock_module.ev_base,
                       &peer->recv_event,
                       peer->sd,
                       OPAL_EV_READ|OPAL_EV_PERSIST,
                       mca_oob_usock_recv_handler,
                       peer);
        opal_event_set_priority(&peer->recv_event, ORTE_MSG_PRI);
        if (peer->recv_ev_active) {
            opal_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
        }
        
        opal_event_set(mca_oob_usock_module.ev_base,
                       &peer->send_event,
                       peer->sd,
                       OPAL_EV_WRITE|OPAL_EV_PERSIST,
                       mca_oob_usock_send_handler,
                       peer);
        opal_event_set_priority(&peer->send_event, ORTE_MSG_PRI);
        if (peer->send_ev_active) {
            opal_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
    }
}

/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the peer on the
 * newly connected socket.
 */
void mca_oob_usock_peer_complete_connect(mca_oob_usock_peer_t *peer)
{
    int so_error = 0;
    opal_socklen_t so_length = sizeof(so_error);

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:usock:complete_connect called for peer %s on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name), peer->sd);

    /* check connect completion status */
    if (getsockopt(peer->sd, SOL_SOCKET, SO_ERROR, (char *)&so_error, &so_length) < 0) {
        opal_output(0, "%s usock_peer_complete_connect: getsockopt() to %s failed: %s (%d)\n", 
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    strerror(opal_socket_errno),
                    opal_socket_errno);
        peer->state = MCA_OOB_USOCK_FAILED;
        mca_oob_usock_peer_close(peer);
        return;
    }

    if (so_error == EINPROGRESS) {
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s:usock:send:handler still in progress",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return;
    } else if (so_error == ECONNREFUSED || so_error == ETIMEDOUT) {
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s-%s usock_peer_complete_connect: connection failed: %s (%d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)),
                            strerror(so_error),
                            so_error);
        mca_oob_usock_peer_close(peer);
        return;
    } else if (so_error != 0) {
        /* No need to worry about the return code here - we return regardless
           at this point, and if an error did occur a message has already been
           printed for the user */
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s-%s usock_peer_complete_connect: "
                            "connection failed with error %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)), so_error);
        mca_oob_usock_peer_close(peer);
        return;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock_peer_complete_connect: "
                        "sending ack to %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)));

    if (usock_peer_send_connect_ack(peer) == ORTE_SUCCESS) {
        peer->state = MCA_OOB_USOCK_CONNECT_ACK;
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s usock_peer_complete_connect: "
                            "setting read event on connection to %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)));
        
        if (!peer->recv_ev_active) {
            opal_event_add(&peer->recv_event, 0);
            peer->recv_ev_active = true;
        }
    } else {
        opal_output(0, "%s usock_peer_complete_connect: unable to send connect ack to %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)));
        peer->state = MCA_OOB_USOCK_FAILED;
        mca_oob_usock_peer_close(peer);
    }
}

/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int usock_peer_send_blocking(mca_oob_usock_peer_t* peer,
                                    int sd, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    int retval;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s send blocking of %"PRIsize_t" bytes to socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        size, sd);

    while (cnt < size) {
        retval = send(sd, (char*)ptr+cnt, size-cnt, 0);
        if (retval < 0) {
            if (opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                opal_output(0, "%s usock_peer_send_blocking: send() to socket %d failed: %s (%d)\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), sd,
                            strerror(opal_socket_errno),
                            opal_socket_errno);
                peer->state = MCA_OOB_USOCK_FAILED;
                mca_oob_usock_peer_close(peer);
                return ORTE_ERR_UNREACH;
            }
            continue;
        }
        cnt += retval;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s blocking send complete to socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), sd);

    return ORTE_SUCCESS;
}

/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */
int mca_oob_usock_peer_recv_connect_ack(mca_oob_usock_peer_t* pr, int sd,
                                        mca_oob_usock_hdr_t *dhdr)
{
    char *msg;
    char *version;
    int rc, cmpval;
    char *cred;
    size_t credsize;
    mca_oob_usock_peer_t *peer;
    mca_oob_usock_hdr_t hdr;
    uint64_t *ui64;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s RECV CONNECT ACK FROM %s ON SOCKET %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == pr) ? "UNKNOWN" : ORTE_NAME_PRINT(&pr->name), sd);

    peer = pr;
    /* ensure all is zero'd */
    memset(&hdr, 0, sizeof(mca_oob_usock_hdr_t));

    if (usock_peer_recv_blocking(peer, sd, &hdr, sizeof(mca_oob_usock_hdr_t))) {
        if (NULL != peer) {
            /* If the peer state is CONNECT_ACK, then we were waiting for
             * the connection to be ack'd
             */
            if (peer->state != MCA_OOB_USOCK_CONNECT_ACK) {
                /* handshake broke down - abort this connection */
                opal_output(0, "%s RECV CONNECT BAD HANDSHAKE FROM %s ON SOCKET %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name), sd);
                mca_oob_usock_peer_close(peer);
                return ORTE_ERR_UNREACH;
            }
        }
    } else {
        /* unable to complete the recv */
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s unable to complete recv of connect-ack from %s ON SOCKET %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&peer->name), sd);
        return ORTE_ERR_UNREACH;
    }
    /* if the requestor wanted the header returned, then do so now */
    if (NULL != dhdr) {
        *dhdr = hdr;
    }

    if (MCA_OOB_USOCK_PROBE == hdr.type) {
        /* send a header back */
        hdr.type = MCA_OOB_USOCK_PROBE;
        hdr.dst = hdr.origin;
        hdr.origin = *ORTE_PROC_MY_NAME;
        usock_peer_send_blocking(peer, sd, &hdr, sizeof(mca_oob_usock_hdr_t));
        CLOSE_THE_SOCKET(sd);
        return ORTE_SUCCESS;
    }

    if (hdr.type != MCA_OOB_USOCK_IDENT) {
        opal_output(0, "usock_peer_recv_connect_ack: invalid header type: %d\n", hdr.type);
        if (NULL != peer) {
            peer->state = MCA_OOB_USOCK_FAILED;
            mca_oob_usock_peer_close(peer);
        } else {
            CLOSE_THE_SOCKET(sd);
        }
        return ORTE_ERR_UNREACH;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s connect-ack recvd from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&peer->name));

    /* if we don't already have it, get the peer */
    if (NULL == peer) {
        peer = mca_oob_usock_peer_lookup(&hdr.origin);
        if (NULL == peer) {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                "%s mca_oob_usock_recv_connect: connection from new peer",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            peer = OBJ_NEW(mca_oob_usock_peer_t);
            peer->name = hdr.origin;
            peer->state = MCA_OOB_USOCK_ACCEPTING;
            peer->sd = sd;
            ui64 = (uint64_t*)(&peer->name);
            if (OPAL_SUCCESS != opal_hash_table_set_value_uint64(&mca_oob_usock_module.peers, (*ui64), peer)) {
                OBJ_RELEASE(peer);
                CLOSE_THE_SOCKET(sd);
                return ORTE_ERR_UNREACH;
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
                                    ORTE_NAME_PRINT(&hdr.origin));
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
                peer->retries = 0;
                cmpval = orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &hdr.origin, ORTE_PROC_MY_NAME);
                if (OPAL_VALUE1_GREATER == cmpval) {
                    /* force the other end to retry the connection */
                    peer->state = MCA_OOB_USOCK_UNCONNECTED;
                    return ORTE_ERR_UNREACH;
                } else {
                    /* retry the connection */
                    peer->state = MCA_OOB_USOCK_CONNECTING;
                    ORTE_ACTIVATE_USOCK_CONN_STATE(peer, mca_oob_usock_peer_try_connect);
                    return ORTE_ERR_UNREACH;
                }
            }
        }
    } else {
        /* compare the peers name to the expected value */
        if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &peer->name, &hdr.origin)) {
            opal_output(0, "%s usock_peer_recv_connect_ack: "
                        "received unexpected process identifier %s from %s\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(hdr.origin)),
                        ORTE_NAME_PRINT(&(peer->name)));
            peer->state = MCA_OOB_USOCK_FAILED;
            mca_oob_usock_peer_close(peer);
            return ORTE_ERR_UNREACH;
        }
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s connect-ack header from %s is okay",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    /* get the authentication and version payload */
    if (NULL == (msg = (char*)malloc(hdr.nbytes))) {
        peer->state = MCA_OOB_USOCK_FAILED;
        mca_oob_usock_peer_close(peer);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (!usock_peer_recv_blocking(peer, sd, msg, hdr.nbytes)) {
        /* unable to complete the recv */
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s unable to complete recv of connect-ack from %s ON SOCKET %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name), peer->sd);
        free(msg);
        return ORTE_ERR_UNREACH;
    }

    /* check that this is from a matching version */
    version = (char*)(msg);
    if (0 != strcmp(version, orte_version_string)) {
        opal_output(0, "%s usock_peer_recv_connect_ack: "
                    "received different version from %s: %s instead of %s\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    version, orte_version_string);
        peer->state = MCA_OOB_USOCK_FAILED;
        mca_oob_usock_peer_close(peer);
        free(msg);
        return ORTE_ERR_UNREACH;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s connect-ack version from %s matches ours",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    /* check security token */
    cred = (char*)(msg + strlen(version) + 1);
    credsize = hdr.nbytes - strlen(version) - 1;
    if (OPAL_SUCCESS != (rc = opal_sec.authenticate(cred, credsize, &peer->auth_method))) {
        ORTE_ERROR_LOG(rc);
    }
    free(msg);

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s connect-ack %s authenticated",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    /* if the requestor wanted the header returned, then they
     * will complete their processing
     */
    if (NULL != dhdr) {
        return ORTE_SUCCESS;
    }

    /* set the peer into the component and OOB-level peer tables to indicate
     * that we know this peer and we will be handling him
     */
    ORTE_ACTIVATE_USOCK_CMP_OP(peer, mca_oob_usock_component_set_module);

    /* connected */
    usock_peer_connected(peer);
    if (OOB_USOCK_DEBUG_CONNECT <= opal_output_get_verbosity(orte_oob_base_framework.framework_output)) {
        mca_oob_usock_peer_dump(peer, "connected");
    }
    return ORTE_SUCCESS;
}

/*
 *  Setup peer state to reflect that connection has been established,
 *  and start any pending sends.
 */
static void usock_peer_connected(mca_oob_usock_peer_t* peer)
{
    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s-%s usock_peer_connected on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)), peer->sd);

    if (peer->timer_ev_active) {
        opal_event_del(&peer->timer_event);
        peer->timer_ev_active = false;
    }
    peer->state = MCA_OOB_USOCK_CONNECTED;

    /* initiate send of first message on queue */
    if (NULL == peer->send_msg) {
        peer->send_msg = (mca_oob_usock_send_t*)
            opal_list_remove_first(&peer->send_queue);
    }
    if (NULL != peer->send_msg && !peer->send_ev_active) {
        opal_event_add(&peer->send_event, 0);
        peer->send_ev_active = true;
    }
}

/*
 * Remove any event registrations associated with the socket
 * and update the peer state to reflect the connection has
 * been closed.
 */
void mca_oob_usock_peer_close(mca_oob_usock_peer_t *peer)
{
    mca_oob_usock_send_t *snd;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock_peer_close for %s sd %d state %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)),
                        peer->sd, mca_oob_usock_state_print(peer->state));

    peer->state = MCA_OOB_USOCK_CLOSED;

    /* release the socket */
    close(peer->sd);

    /* inform the component-level that we have lost a connection so
     * it can decide what to do about it.
     */
    ORTE_ACTIVATE_USOCK_CMP_OP(peer, mca_oob_usock_component_lost_connection);

    if (orte_orteds_term_ordered || orte_finalizing || orte_abnormal_term_ordered) {
        /* nothing more to do */
        return;
    }

    /* FIXME: push any queued messages back onto the OOB for retry - note that
     * this must be done after the prior call to ensure that the component
     * processes the "lost connection" notice before the OOB begins to
     * handle these recycled messages. This prevents us from unintentionally
     * attempting to send the message again across the now-failed interface
     */
    if (NULL != peer->send_msg) {
    }
    while (NULL != (snd = (mca_oob_usock_send_t*)opal_list_remove_first(&peer->send_queue))) {
    }
}

/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static bool usock_peer_recv_blocking(mca_oob_usock_peer_t* peer,
                                     int sd, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s waiting for connect ack from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)));

    while (cnt < size) {
        int retval = recv(sd, (char *)ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if (retval == 0) {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                "%s-%s usock_peer_recv_blocking: "
                                "peer closed connection: peer state %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)),
                                (NULL == peer) ? 0 : peer->state);
            mca_oob_usock_peer_close(peer);
            return false;
        }

        /* socket is non-blocking so handle errors */
        if (retval < 0) {
            if (opal_socket_errno != EINTR && 
                opal_socket_errno != EAGAIN && 
                opal_socket_errno != EWOULDBLOCK) {
                if (peer->state == MCA_OOB_USOCK_CONNECT_ACK) {
                    /* If we overflow the listen backlog, it's
                       possible that even though we finished the three
                       way handshake, the remote host was unable to
                       transition the connection from half connected
                       (received the initial SYN) to fully connected
                       (in the listen backlog).  We likely won't see
                       the failure until we try to receive, due to
                       timing and the like.  The first thing we'll get
                       in that case is a RST packet, which receive
                       will turn into a connection reset by peer
                       errno.  In that case, leave the socket in
                       CONNECT_ACK and propogate the error up to
                       recv_connect_ack, who will try to establish the
                       connection again */
                    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                        "%s connect ack received error %s from %s",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        strerror(opal_socket_errno),
                                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)));
                    return false;
                } else {
                    opal_output(0, 
                                "%s usock_peer_recv_blocking: "
                                "recv() failed for %s: %s (%d)\n",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)),
                                strerror(opal_socket_errno),
                                opal_socket_errno);
                    if (NULL != peer) {
                        peer->state = MCA_OOB_USOCK_FAILED;
                        mca_oob_usock_peer_close(peer);
                    } else {
                        CLOSE_THE_SOCKET(sd);
                    }
                    return false;
                }
            }
            continue;
        }
        cnt += retval;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s connect ack received from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)));
    return true;
}

/*
 * Routine for debugging to print the connection state and socket options
 */
void mca_oob_usock_peer_dump(mca_oob_usock_peer_t* peer, const char* msg)
{
    char buff[255];
    int nodelay,flags;

    if ((flags = fcntl(peer->sd, F_GETFL, 0)) < 0) {
        opal_output(0, "usock_peer_dump: fcntl(F_GETFL) failed: %s (%d)\n",
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
                                                                                                            
#if defined(USOCK_NODELAY)
    optlen = sizeof(nodelay);
    if (getsockopt(peer->sd, IPPROTO_USOCK, USOCK_NODELAY, (char *)&nodelay, &optlen) < 0) {
        opal_output(0, "usock_peer_dump: USOCK_NODELAY option: %s (%d)\n", 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#else
    nodelay = 0;
#endif

    snprintf(buff, sizeof(buff), "%s-%s %s: nodelay %d flags %08x\n",
        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
        ORTE_NAME_PRINT(&(peer->name)),
        msg, nodelay, flags);
    opal_output(0, "%s", buff);
}

/*
 * Accept incoming connection - if not already connected
 */

bool mca_oob_usock_peer_accept(mca_oob_usock_peer_t* peer)
{
    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:peer_accept called for peer %s in state %s on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name),
                        mca_oob_usock_state_print(peer->state), peer->sd);

    if (peer->state != MCA_OOB_USOCK_CONNECTED) {

        usock_peer_event_init(peer);

        if (usock_peer_send_connect_ack(peer) != ORTE_SUCCESS) {
            opal_output(0, "%s-%s usock_peer_accept: "
                        "usock_peer_send_connect_ack failed\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)));
            peer->state = MCA_OOB_USOCK_FAILED;
            mca_oob_usock_peer_close(peer);
            return false;
        }

        /* set the peer into the component and OOB-level peer tables to indicate
         * that we know this peer and we will be handling him
         */
        ORTE_ACTIVATE_USOCK_CMP_OP(peer, mca_oob_usock_component_set_module);

        usock_peer_connected(peer);
        if (!peer->recv_ev_active) {
            opal_event_add(&peer->recv_event, 0);
            peer->recv_ev_active = true;
        }
        /* if a message is waiting to be sent, ensure the send event is active */
        if (NULL != peer->send_msg && !peer->send_ev_active) {
            opal_event_add(&peer->send_event, 0);
            peer->send_ev_active = true;
        }
        if (OOB_USOCK_DEBUG_CONNECT <= opal_output_get_verbosity(orte_oob_base_framework.framework_output)) {
            mca_oob_usock_peer_dump(peer, "accepted");
        }
        return true;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:peer_accept ignored for peer %s in state %s on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name),
                        mca_oob_usock_state_print(peer->state), peer->sd);
    return false;
}
