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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved. 
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
#include "opal/class/opal_hash_table.h"
#include "opal/mca/event/event.h"
#include "opal/runtime/opal.h"

#include "orte/util/name_fns.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_wait.h"

#include "pmix_server_internal.h"

static int usock_peer_send_blocking(pmix_server_peer_t* peer,
                                    int sd, void* data, size_t size);
static bool usock_peer_recv_blocking(pmix_server_peer_t* peer,
                                     int sd, void* data, size_t size);

int pmix_server_send_connect_ack(pmix_server_peer_t* peer)
{
    char *msg;
    pmix_server_hdr_t hdr;
    int rc;
    size_t sdsize;
    char *cred;
    size_t credsize;
    
    opal_output_verbose(2, pmix_server_output,
                        "%s SEND CONNECT ACK", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));

    /* send a handshake that includes our process identifier
     * to ensure we are talking to another OMPI process
    */
    hdr.id = *ORTE_PROC_MY_NAME;
    hdr.type = PMIX_USOCK_IDENT;
    hdr.tag = UINT32_MAX;

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
    sdsize = (sizeof(hdr) + strlen(opal_version_string) + 1 + credsize);
    if (NULL == (msg = (char*)malloc(sdsize))) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the message */
    memcpy(msg, &hdr, sizeof(hdr));
    memcpy(msg+sizeof(hdr), opal_version_string, strlen(opal_version_string));
    memcpy(msg+sizeof(hdr)+strlen(opal_version_string)+1, cred, credsize);
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
void pmix_server_peer_event_init(pmix_server_peer_t* peer)
{
    if (peer->sd >= 0) {
        opal_event_set(orte_event_base,
                       &peer->recv_event,
                       peer->sd,
                       OPAL_EV_READ|OPAL_EV_PERSIST,
                       pmix_server_recv_handler,
                       peer);
        opal_event_set_priority(&peer->recv_event, ORTE_MSG_PRI);
        if (peer->recv_ev_active) {
            opal_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
        }
        
        opal_event_set(orte_event_base,
                       &peer->send_event,
                       peer->sd,
                       OPAL_EV_WRITE|OPAL_EV_PERSIST,
                       pmix_server_send_handler,
                       peer);
        opal_event_set_priority(&peer->send_event, ORTE_MSG_PRI);
        if (peer->send_ev_active) {
            opal_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
    }
}

/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int usock_peer_send_blocking(pmix_server_peer_t* peer,
                                    int sd, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;
    int retval;

    opal_output_verbose(2, pmix_server_output,
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
                peer->state = PMIX_SERVER_FAILED;
                CLOSE_THE_SOCKET(peer->sd);
                return ORTE_ERR_UNREACH;
            }
            continue;
        }
        cnt += retval;
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s blocking send complete to socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), sd);

    return ORTE_SUCCESS;
}

/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */
int pmix_server_recv_connect_ack(pmix_server_peer_t* pr, int sd,
                                 pmix_server_hdr_t *dhdr)
{
    char *msg;
    char *version;
    int rc;
    char *cred;
    size_t credsize;
    pmix_server_peer_t *peer;
    pmix_server_hdr_t hdr;
    orte_process_name_t sender;

    opal_output_verbose(2, pmix_server_output,
                        "%s RECV CONNECT ACK FROM %s ON SOCKET %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == pr) ? "UNKNOWN" : ORTE_NAME_PRINT(&pr->name), sd);

    peer = pr;
    /* ensure all is zero'd */
    memset(&hdr, 0, sizeof(pmix_server_hdr_t));

    if (usock_peer_recv_blocking(peer, sd, &hdr, sizeof(pmix_server_hdr_t))) {
        if (NULL != peer) {
            /* If the peer state is CONNECT_ACK, then we were waiting for
             * the connection to be ack'd
             */
            if (peer->state != PMIX_SERVER_CONNECT_ACK) {
                /* handshake broke down - abort this connection */
                opal_output(0, "%s RECV CONNECT BAD HANDSHAKE FROM %s ON SOCKET %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name), sd);
                peer->state = PMIX_SERVER_FAILED;
                CLOSE_THE_SOCKET(peer->sd);
                return ORTE_ERR_UNREACH;
            }
        }
    } else {
        /* unable to complete the recv */
        opal_output_verbose(2, pmix_server_output,
                            "%s unable to complete recv of connect-ack from %s ON SOCKET %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&peer->name), sd);
        return ORTE_ERR_UNREACH;
    }
    /* if the requestor wanted the header returned, then do so now */
    if (NULL != dhdr) {
        *dhdr = hdr;
    }

    if (hdr.type != PMIX_USOCK_IDENT) {
        opal_output(0, "usock_peer_recv_connect_ack: invalid header type: %d\n", hdr.type);
        if (NULL != peer) {
            peer->state = PMIX_SERVER_FAILED;
            CLOSE_THE_SOCKET(peer->sd);
        } else {
            CLOSE_THE_SOCKET(sd);
        }
        return ORTE_ERR_UNREACH;
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s connect-ack recvd from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&peer->name));

    sender = hdr.id;
    /* if we don't already have it, get the peer */
    if (NULL == peer) {
        peer = pmix_server_peer_lookup(sd);
        if (NULL == peer) {
            opal_output_verbose(2, pmix_server_output,
                                "%s pmix_server_recv_connect: connection from new peer",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            peer = OBJ_NEW(pmix_server_peer_t);
            peer->name = sender;
            peer->state = PMIX_SERVER_ACCEPTING;
            peer->sd = sd;
            if (OPAL_SUCCESS != opal_hash_table_set_value_uint64(pmix_server_peers, sd, peer)) {
                OBJ_RELEASE(peer);
                CLOSE_THE_SOCKET(sd);
                return ORTE_ERR_UNREACH;
            }
        } else if (PMIX_SERVER_CONNECTED == peer->state ||
                   PMIX_SERVER_CONNECTING == peer->state ||
                   PMIX_SERVER_CONNECT_ACK == peer->state) {
            /* if I already have an established such a connection, then we need
             * to reject this connection */
            opal_output_verbose(2, pmix_server_output,
                                "%s EXISTING CONNECTION WITH %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&sender));
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
            peer->retries = 0;
        }
    } else {
        /* compare the peers name to the expected value */
        if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &peer->name, &sender)) {
            opal_output(0, "%s usock_peer_recv_connect_ack: "
                        "received unexpected process identifier %s from %s\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&sender),
                        ORTE_NAME_PRINT(&(peer->name)));
            peer->state = PMIX_SERVER_FAILED;
            CLOSE_THE_SOCKET(peer->sd);
            return ORTE_ERR_UNREACH;
        }
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s connect-ack header from %s is okay",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    /* get the authentication and version payload */
    if (NULL == (msg = (char*)malloc(hdr.nbytes))) {
        peer->state = PMIX_SERVER_FAILED;
        CLOSE_THE_SOCKET(peer->sd);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, hdr.nbytes);
    
    if (!usock_peer_recv_blocking(peer, sd, msg, hdr.nbytes)) {
        /* unable to complete the recv */
        opal_output_verbose(2, pmix_server_output,
                            "%s unable to complete recv of connect-ack from %s ON SOCKET %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer->name), peer->sd);
        free(msg);
        return ORTE_ERR_UNREACH;
    }

    /* check that this is from a matching version */
    version = (char*)(msg);
    if (0 != strcmp(version, opal_version_string)) {
        opal_output(0, "%s usock_peer_recv_connect_ack: "
                    "received different version from %s: %s instead of %s\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    version, opal_version_string);
        peer->state = PMIX_SERVER_FAILED;
        CLOSE_THE_SOCKET(peer->sd);
        free(msg);
        return ORTE_ERR_UNREACH;
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s connect-ack version from %s matches ours",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    /* check security token */
    cred =  (char*)(msg + strlen(version) + 1);
    credsize = hdr.nbytes - strlen(version) - 1;
    if (OPAL_SUCCESS != (rc = opal_sec.authenticate(cred, credsize, &peer->auth_method))) {
        ORTE_ERROR_LOG(rc);
        peer->state = PMIX_SERVER_FAILED;
        CLOSE_THE_SOCKET(peer->sd);
        free(msg);
        return ORTE_ERR_UNREACH;
    }
    free(msg);

    opal_output_verbose(2, pmix_server_output,
                        "%s connect-ack %s authenticated",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    /* if the requestor wanted the header returned, then they
     * will complete their processing
     */
    if (NULL != dhdr) {
        return ORTE_SUCCESS;
    }

    /* connected */
    pmix_server_peer_connected(peer);
    if (2 <= opal_output_get_verbosity(pmix_server_output)) {
        pmix_server_peer_dump(peer, "connected");
    }
    return ORTE_SUCCESS;
}

/*
 *  Setup peer state to reflect that connection has been established,
 *  and start any pending sends.
 */
void pmix_server_peer_connected(pmix_server_peer_t* peer)
{
    opal_output_verbose(2, pmix_server_output,
                        "%s-%s usock_peer_connected on socket %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)), peer->sd);

    if (peer->timer_ev_active) {
        opal_event_del(&peer->timer_event);
        peer->timer_ev_active = false;
    }
    peer->state = PMIX_SERVER_CONNECTED;

    /* ensure the recv event is active */
    if (!peer->recv_ev_active) {
        opal_event_add(&peer->recv_event, 0);
        peer->recv_ev_active = true;
    }

    /* initiate send of first message on queue */
    if (NULL == peer->send_msg) {
        peer->send_msg = (pmix_server_send_t*)
            opal_list_remove_first(&peer->send_queue);
    }
    if (NULL != peer->send_msg && !peer->send_ev_active) {
        opal_event_add(&peer->send_event, 0);
        peer->send_ev_active = true;
    }
}

/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static bool usock_peer_recv_blocking(pmix_server_peer_t* peer,
                                     int sd, void* data, size_t size)
{
    unsigned char* ptr = (unsigned char*)data;
    size_t cnt = 0;

    opal_output_verbose(2, pmix_server_output,
                        "%s waiting for connect ack from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)));

    while (cnt < size) {
        int retval = recv(sd, (char *)ptr+cnt, size-cnt, 0);

        /* remote closed connection */
        if (retval == 0) {
            opal_output_verbose(2, pmix_server_output,
                                "%s-%s usock_peer_recv_blocking: "
                                "peer closed connection: peer state %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)),
                                (NULL == peer) ? 0 : peer->state);
            if (NULL != peer) {
                peer->state = PMIX_SERVER_FAILED;
                CLOSE_THE_SOCKET(peer->sd);
            }
            return false;
        }

        /* socket is non-blocking so handle errors */
        if (retval < 0) {
            if (opal_socket_errno != EINTR && 
                opal_socket_errno != EAGAIN && 
                opal_socket_errno != EWOULDBLOCK) {
                if (peer->state == PMIX_SERVER_CONNECT_ACK) {
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
                    opal_output_verbose(2, pmix_server_output,
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
                        peer->state = PMIX_SERVER_FAILED;
                        CLOSE_THE_SOCKET(peer->sd);
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

    opal_output_verbose(2, pmix_server_output,
                        "%s connect ack received from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)));
    return true;
}

