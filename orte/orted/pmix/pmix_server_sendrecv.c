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
 * Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
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

#include "opal_stdint.h"
#include "opal/types.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/util/output.h"
#include "opal/util/net.h"
#include "opal/util/error.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/event/event.h"
#include "opal/mca/sec/sec.h"
#include "opal/runtime/opal.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_wait.h"

#include "pmix_server_internal.h"

static void complete_connect(pmix_server_peer_t *peer);

static int send_bytes(pmix_server_peer_t* peer)
{
    pmix_server_send_t* msg = peer->send_msg;
    int rc;

    while (0 < msg->sdbytes) {
        rc = write(peer->sd, msg->sdptr, msg->sdbytes);
        if (rc < 0) {
            if (opal_socket_errno == EINTR) {
                continue;
            } else if (opal_socket_errno == EAGAIN) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return ORTE_ERR_RESOURCE_BUSY;
            } else if (opal_socket_errno == EWOULDBLOCK) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return ORTE_ERR_WOULD_BLOCK;
            }
            /* we hit an error and cannot progress this message */
            opal_output(0, "%s->%s pmix_server_msg_send_bytes: write failed: %s (%d) [sd = %d]",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)),
                        strerror(opal_socket_errno),
                        opal_socket_errno,
                        peer->sd);
            return ORTE_ERR_COMM_FAILURE;
        }
        /* update location */
        msg->sdbytes -= rc;
        msg->sdptr += rc;
    }
    /* we sent the full data block */
    return ORTE_SUCCESS;
}

/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */
void pmix_server_send_handler(int sd, short flags, void *cbdata)
{
    pmix_server_peer_t* peer = (pmix_server_peer_t*)cbdata;
    pmix_server_send_t* msg = peer->send_msg;
    int rc;

    opal_output_verbose(2, pmix_server_output,
                        "%s usock:send_handler called to send to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    switch (peer->state) {
    case PMIX_SERVER_CONNECTING:
    case PMIX_SERVER_CLOSED:
        opal_output_verbose(2, pmix_server_output,
                            "%s usock:send_handler %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            pmix_server_state_print(peer->state));
        complete_connect(peer);
        /* de-activate the send event until the connection
         * handshake completes
         */
        if (peer->send_ev_active) {
            opal_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
        break;
    case PMIX_SERVER_CONNECTED:
        opal_output_verbose(2, pmix_server_output,
                            "%s usock:send_handler SENDING TO %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            (NULL == peer->send_msg) ? "NULL" : ORTE_NAME_PRINT(&peer->name));
        if (NULL != msg) {
            /* if the header hasn't been completely sent, send it */
            if (!msg->hdr_sent) {
                if (ORTE_SUCCESS == (rc = send_bytes(peer))) {
                    /* header is completely sent */
                    msg->hdr_sent = true;
                    /* setup to send the data */
                    if (NULL == msg->data) {
                        /* this was a zero-byte msg - nothing more to do */
                        OBJ_RELEASE(msg);
                        peer->send_msg = NULL;
                        goto next;
                    } else {
                        msg->sdptr = msg->data->base_ptr;
                        msg->sdbytes = msg->hdr.nbytes;
                    }
                    /* fall thru and let the send progress */
                } else if (ORTE_ERR_RESOURCE_BUSY == rc ||
                           ORTE_ERR_WOULD_BLOCK == rc) {
                    /* exit this event and let the event lib progress */
                    return;
                } else {
                    // report the error
                    opal_output(0, "%s-%s pmix_server_peer_send_handler: unable to send header",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->name)));
                    opal_event_del(&peer->send_event);
                    peer->send_ev_active = false;
                    OBJ_RELEASE(msg);
                    peer->send_msg = NULL;
                    goto next;
                }
            }
            /* progress the data transmission */
            if (msg->hdr_sent) {
                if (ORTE_SUCCESS == (rc = send_bytes(peer))) {
                    /* this message is complete */
                    OBJ_RELEASE(msg);
                    peer->send_msg = NULL;
                    /* fall thru to queue the next message */
                } else if (ORTE_ERR_RESOURCE_BUSY == rc ||
                           ORTE_ERR_WOULD_BLOCK == rc) {
                    /* exit this event and let the event lib progress */
                    return;
                } else {
                    // report the error
                    opal_output(0, "%s-%s pmix_server_peer_send_handler: unable to send message ON SOCKET %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->name)), peer->sd);
                    opal_event_del(&peer->send_event);
                    peer->send_ev_active = false;
                    OBJ_RELEASE(msg);
                    peer->send_msg = NULL;
                    return;
                }
            }

        next:
            /* if current message completed - progress any pending sends by
             * moving the next in the queue into the "on-deck" position. Note
             * that this doesn't mean we send the message right now - we will
             * wait for another send_event to fire before doing so. This gives
             * us a chance to service any pending recvs.
             */
            peer->send_msg = (pmix_server_send_t*)
                opal_list_remove_first(&peer->send_queue);
        }

        /* if nothing else to do unregister for send event notifications */
        if (NULL == peer->send_msg && peer->send_ev_active) {
            opal_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
        break;

    default:
        opal_output(0, "%s-%s pmix_server_peer_send_handler: invalid connection state (%d) on socket %d",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    peer->state, peer->sd);
        if (peer->send_ev_active) {
            opal_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
        break;
    }
}

static int read_bytes(pmix_server_peer_t* peer)
{
    int rc;

    /* read until all bytes recvd or error */
    while (0 < peer->recv_msg->rdbytes) {
        rc = read(peer->sd, peer->recv_msg->rdptr, peer->recv_msg->rdbytes);
        if (rc < 0) {
            if(opal_socket_errno == EINTR) {
                continue;
            } else if (opal_socket_errno == EAGAIN) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return ORTE_ERR_RESOURCE_BUSY;
            } else if (opal_socket_errno == EWOULDBLOCK) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return ORTE_ERR_WOULD_BLOCK;
            }
            /* we hit an error and cannot progress this message - report
             * the error back to the RML and let the caller know
             * to abort this message
             */
            opal_output_verbose(2, pmix_server_output,
                                "%s-%s pmix_server_msg_recv: readv failed: %s (%d)",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->name)),
                                strerror(opal_socket_errno),
                                opal_socket_errno);
            // pmix_server_peer_close(peer);
            // if (NULL != pmix_server.oob_exception_callback) {
            // pmix_server.oob_exception_callback(&peer->name, ORTE_RML_PEER_DISCONNECTED);
            //}
            return ORTE_ERR_COMM_FAILURE;
        } else if (rc == 0)  {
            /* the remote peer closed the connection - report that condition
             * and let the caller know
             */
            opal_output_verbose(2, pmix_server_output,
                                "%s-%s pmix_server_msg_recv: peer closed connection",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->name)));
            /* stop all events */
            if (peer->recv_ev_active) {
                opal_event_del(&peer->recv_event);
                peer->recv_ev_active = false;
            }
            if (peer->timer_ev_active) {
                opal_event_del(&peer->timer_event);
                peer->timer_ev_active = false;
            }
            if (peer->send_ev_active) {
                opal_event_del(&peer->send_event);
                peer->send_ev_active = false;
            }
            if (NULL != peer->recv_msg) {
                OBJ_RELEASE(peer->recv_msg);
                peer->recv_msg = NULL;
            }
            peer->state = PMIX_SERVER_CLOSED;
            CLOSE_THE_SOCKET(peer->sd);
            //if (NULL != pmix_server.oob_exception_callback) {
            //   pmix_server.oob_exception_callback(&peer->peer_name, ORTE_RML_PEER_DISCONNECTED);
            //}
            return ORTE_ERR_CONNECTION_FAILED;
        }
        /* we were able to read something, so adjust counters and location */
        peer->recv_msg->rdbytes -= rc;
        peer->recv_msg->rdptr += rc;
    }

    /* we read the full data block */
    return ORTE_SUCCESS;
}

void pmix_server_recv_handler(int sd, short flags, void *cbdata)
{
    pmix_server_peer_t* peer = (pmix_server_peer_t*)cbdata;
    int rc;

    opal_output_verbose(2, pmix_server_output,
                        "%s:usock:recv:handler called for peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    switch (peer->state) {
    case PMIX_SERVER_CONNECT_ACK:
        if (ORTE_SUCCESS == (rc = pmix_server_peer_recv_connect_ack(peer, peer->sd, NULL))) {
            opal_output_verbose(2, pmix_server_output,
                                "%s:usock:recv:handler starting send/recv events",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            /* we connected! Start the send/recv events */
            if (!peer->recv_ev_active) {
                opal_event_add(&peer->recv_event, 0);
                peer->recv_ev_active = true;
            }
            if (peer->timer_ev_active) {
                opal_event_del(&peer->timer_event);
                peer->timer_ev_active = false;
            }
            /* if there is a message waiting to be sent, queue it */
            if (NULL == peer->send_msg) {
                peer->send_msg = (pmix_server_send_t*)opal_list_remove_first(&peer->send_queue);
            }
            if (NULL != peer->send_msg && !peer->send_ev_active) {
                opal_event_add(&peer->send_event, 0);
                peer->send_ev_active = true;
            }
            /* update our state */
            peer->state = PMIX_SERVER_CONNECTED;
        } else {
            opal_output_verbose(2, pmix_server_output,
                                "%s UNABLE TO COMPLETE CONNECT ACK WITH %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&peer->name));
            opal_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
            return;
        }
        break;
    case PMIX_SERVER_CONNECTED:
        opal_output_verbose(2, pmix_server_output,
                            "%s:usock:recv:handler CONNECTED",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* allocate a new message and setup for recv */
        if (NULL == peer->recv_msg) {
            opal_output_verbose(2, pmix_server_output,
                                "%s:usock:recv:handler allocate new recv msg",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            peer->recv_msg = OBJ_NEW(pmix_server_recv_t);
            if (NULL == peer->recv_msg) {
                opal_output(0, "%s-%s pmix_server_peer_recv_handler: unable to allocate recv message\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)));
                return;
            }
            /* start by reading the header */
            peer->recv_msg->rdptr = (char*)&peer->recv_msg->hdr;
            peer->recv_msg->rdbytes = sizeof(pmix_server_hdr_t);
        }
        /* if the header hasn't been completely read, read it */
        if (!peer->recv_msg->hdr_recvd) {
            opal_output_verbose(2, pmix_server_output,
                                "%s:usock:recv:handler read hdr",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            if (ORTE_SUCCESS == (rc = read_bytes(peer))) {
                /* completed reading the header */
                peer->recv_msg->hdr_recvd = true;
                /* if this is a zero-byte message, then we are done */
                if (0 == peer->recv_msg->hdr.nbytes) {
                    opal_output_verbose(2, pmix_server_output,
                                        "%s RECVD ZERO-BYTE MESSAGE FROM %s for tag %d",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        ORTE_NAME_PRINT(&peer->name), peer->recv_msg->hdr.tag);
                    peer->recv_msg->data = NULL;  // make sure
                    peer->recv_msg->rdptr = NULL;
                    peer->recv_msg->rdbytes = 0;
                } else {
                    opal_output_verbose(2, pmix_server_output,
                                        "%s:usock:recv:handler allocate data region of size %lu",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (unsigned long)peer->recv_msg->hdr.nbytes);
                    /* allocate the data region */
                    peer->recv_msg->data = (char*)malloc(peer->recv_msg->hdr.nbytes);
                    /* point to it */
                    peer->recv_msg->rdptr = peer->recv_msg->data;
                    peer->recv_msg->rdbytes = peer->recv_msg->hdr.nbytes;
                }
                /* fall thru and attempt to read the data */
            } else if (ORTE_ERR_RESOURCE_BUSY == rc ||
                       ORTE_ERR_WOULD_BLOCK == rc) {
                /* exit this event and let the event lib progress */
                return;
            } else {
                /* close the connection */
                opal_output_verbose(2, pmix_server_output,
                                    "%s:usock:recv:handler error reading bytes - closing connection",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                peer->state = PMIX_SERVER_CLOSED;
                CLOSE_THE_SOCKET(peer->sd);
                return;
            }
        }

        if (peer->recv_msg->hdr_recvd) {
            /* continue to read the data block - we start from
             * wherever we left off, which could be at the
             * beginning or somewhere in the message
             */
            if (ORTE_SUCCESS == (rc = read_bytes(peer))) {
                /* we recvd all of the message */
                opal_output_verbose(2, pmix_server_output,
                                    "%s RECVD COMPLETE MESSAGE FROM %s OF %d BYTES TAG %d",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT((orte_process_name_t*)&(peer->recv_msg->hdr.id)),
                                    (int)peer->recv_msg->hdr.nbytes,
                                    peer->recv_msg->hdr.tag);
                /* process the message */
                pmix_server_process_message(peer);
            } else if (ORTE_ERR_RESOURCE_BUSY == rc ||
                       ORTE_ERR_WOULD_BLOCK == rc) {
                /* exit this event and let the event lib progress */
                return;
            } else {
                // report the error
                opal_output(0, "%s-%s pmix_server_peer_recv_handler: unable to recv message",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)));
                /* turn off the recv event */
                opal_event_del(&peer->recv_event);
                peer->recv_ev_active = false;
                peer->state = PMIX_SERVER_CLOSED;
                CLOSE_THE_SOCKET(peer->sd);
                return;
            }
        }
        break;
    default:
        opal_output(0, "%s-%s pmix_server_peer_recv_handler: invalid socket state(%d)",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    peer->state);
        // pmix_server_peer_close(peer);
        break;
    }
}

/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static bool peer_recv_blocking(pmix_server_peer_t* peer, int sd,
                               void* data, size_t size)
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
                                "%s-%s tcp_peer_recv_blocking: "
                                "peer closed connection: peer state %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)),
                                (NULL == peer) ? 0 : peer->state);
            if (NULL != peer) {
                peer->state = PMIX_SERVER_FAILED;
            }
            CLOSE_THE_SOCKET(sd);
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
                                "%s tcp_peer_recv_blocking: "
                                "recv() failed for %s: %s (%d)\n",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&(peer->name)),
                                strerror(opal_socket_errno),
                                opal_socket_errno);
                    if (NULL != peer) {
                        peer->state = PMIX_SERVER_FAILED;
                    }
                    CLOSE_THE_SOCKET(sd);
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

/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */
int pmix_server_peer_recv_connect_ack(pmix_server_peer_t* pr,
                                      int sd, pmix_server_hdr_t *dhdr)
{
    char *msg;
    char *version;
    int rc;
    char *cred;
    size_t credsize;
    pmix_server_hdr_t hdr;
    pmix_server_peer_t *peer;
    uint64_t *ui64;
    orte_process_name_t sender;

    opal_output_verbose(2, pmix_server_output,
                        "%s RECV CONNECT ACK FROM %s ON SOCKET %d",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == pr) ? "UNKNOWN" : ORTE_NAME_PRINT(&pr->name), sd);

    peer = pr;
    /* get the header */
    if (peer_recv_blocking(peer, sd, &hdr, sizeof(pmix_server_hdr_t))) {
        if (NULL != peer) {
            /* If the peer state is CONNECT_ACK, then we were waiting for
             * the connection to be ack'd
             */
            if (peer->state != PMIX_SERVER_CONNECT_ACK) {
                /* handshake broke down - abort this connection */
                opal_output(0, "%s RECV CONNECT BAD HANDSHAKE (%d) FROM %s ON SOCKET %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), peer->state,
                            ORTE_NAME_PRINT(&(peer->name)), sd);
                peer->state = PMIX_SERVER_CLOSED;
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

    opal_output_verbose(2, pmix_server_output,
                        "%s connect-ack recvd from %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        (NULL == peer) ? "UNKNOWN" : ORTE_NAME_PRINT(&peer->name));

    /* if the requestor wanted the header returned, then do so now */
    if (NULL != dhdr) {
        *dhdr = hdr;
    }

    if (hdr.type != PMIX_USOCK_IDENT) {
        opal_output(0, "%s tcp_peer_recv_connect_ack: invalid header type: %d\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), hdr.type);
        if (NULL != peer) {
            peer->state = PMIX_SERVER_FAILED;
        }
        CLOSE_THE_SOCKET(sd);
        return ORTE_ERR_UNREACH;
    }

    /* if we don't already have it, get the peer */
    if (NULL == peer) {
        sender = hdr.id;
        peer = pmix_server_peer_lookup(sd);
        if (NULL == peer) {
            opal_output_verbose(2, pmix_server_output,
                                "%s pmix_server_recv_connect: connection from new peer",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            peer = OBJ_NEW(pmix_server_peer_t);
            peer->sd = sd;
            peer->name = sender;
            peer->state = PMIX_SERVER_ACCEPTING;
            ui64 = (uint64_t*)(&peer->name);
            if (OPAL_SUCCESS != opal_hash_table_set_value_uint64(pmix_server_peers, (*ui64), peer)) {
                OBJ_RELEASE(peer);
                CLOSE_THE_SOCKET(sd);
                return ORTE_ERR_UNREACH;
            }
        } else if (PMIX_SERVER_CONNECTED == peer->state ||
                   PMIX_SERVER_CONNECTING == peer->state ||
                   PMIX_SERVER_CONNECT_ACK == peer->state) {
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
            CLOSE_THE_SOCKET(sd);
        }
    } else {
        /* compare the peers name to the expected value */
        if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &peer->name, &sender)) {
            opal_output(0, "%s tcp_peer_recv_connect_ack: "
                        "received unexpected process identifier %s from %s\n",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(sender)),
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
    if (!peer_recv_blocking(peer, sd, msg, hdr.nbytes)) {
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
    if (0 != strcmp(version, orte_version_string)) {
        opal_output(0, "%s tcp_peer_recv_connect_ack: "
                    "received different version from %s: %s instead of %s\n",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    version, orte_version_string);
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
    cred = (char*)(msg + strlen(version) + 1);
    credsize = hdr.nbytes - strlen(version) - 1;
    if (OPAL_SUCCESS != (rc = opal_sec.authenticate(cred, credsize, NULL))) {
        ORTE_ERROR_LOG(rc);
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
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this processes identifier to the peer on the
 * newly connected socket.
 */
static void complete_connect(pmix_server_peer_t *peer)
{
    int so_error = 0;
    opal_socklen_t so_length = sizeof(so_error);

    opal_output_verbose(2, pmix_server_output,
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
        peer->state = PMIX_SERVER_FAILED;
        CLOSE_THE_SOCKET(peer->sd);
        return;
    }

    if (so_error == EINPROGRESS) {
        opal_output_verbose(2, pmix_server_output,
                            "%s:usock:send:handler still in progress",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        return;
    } else if (so_error == ECONNREFUSED || so_error == ETIMEDOUT) {
        opal_output_verbose(2, pmix_server_output,
                            "%s-%s usock_peer_complete_connect: connection failed: %s (%d)",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)),
                            strerror(so_error),
                            so_error);
        peer->state = PMIX_SERVER_FAILED;
        CLOSE_THE_SOCKET(peer->sd);
        return;
    } else if (so_error != 0) {
        /* No need to worry about the return code here - we return regardless
           at this point, and if an error did occur a message has already been
           printed for the user */
        opal_output_verbose(2, pmix_server_output,
                            "%s-%s usock_peer_complete_connect: "
                            "connection failed with error %d",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)), so_error);
        peer->state = PMIX_SERVER_FAILED;
        CLOSE_THE_SOCKET(peer->sd);
        return;
    }

    opal_output_verbose(2, pmix_server_output,
                        "%s usock_peer_complete_connect: "
                        "sending ack to %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&(peer->name)));

    if (pmix_server_send_connect_ack(peer) == ORTE_SUCCESS) {
        peer->state = PMIX_SERVER_CONNECT_ACK;
        opal_output_verbose(2, pmix_server_output,
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
        peer->state = PMIX_SERVER_FAILED;
        CLOSE_THE_SOCKET(peer->sd);
    }
}
