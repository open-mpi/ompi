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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * In windows, many of the socket functions return an EWOULDBLOCK
 * instead of \ things like EAGAIN, EINPROGRESS, etc. It has been
 * verified that this will \ not conflict with other error codes that
 * are returned by these functions \ under UNIX/Linux environments 
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
#include "opal/mca/event/event.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"
#include "orte/runtime/orte_wait.h"

#include "oob_usock.h"
#include "orte/mca/oob/usock/oob_usock_component.h"
#include "orte/mca/oob/usock/oob_usock_peer.h"
#include "orte/mca/oob/usock/oob_usock_connection.h"

static int send_bytes(mca_oob_usock_peer_t* peer)
{
    mca_oob_usock_send_t* msg = peer->send_msg;
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
            opal_output(0, "%s->%s mca_oob_usock_msg_send_bytes: write failed: %s (%d) [sd = %d]", 
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
void mca_oob_usock_send_handler(int sd, short flags, void *cbdata)
{
    mca_oob_usock_peer_t* peer = (mca_oob_usock_peer_t*)cbdata;
    mca_oob_usock_send_t* msg = peer->send_msg;
    int rc;

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s usock:send_handler called to send to peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    switch (peer->state) {
    case MCA_OOB_USOCK_CONNECTING:
    case MCA_OOB_USOCK_CLOSED:
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s usock:send_handler %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            mca_oob_usock_state_print(peer->state));
        mca_oob_usock_peer_complete_connect(peer);
        /* de-activate the send event until the connection
         * handshake completes
         */
        if (peer->send_ev_active) {
            opal_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
        break;
    case MCA_OOB_USOCK_CONNECTED:
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
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
                    if (NULL == msg->msg) {
                        /* this was a zero-byte msg - nothing more to do */
                        OBJ_RELEASE(msg);
                        peer->send_msg = NULL;
                        goto next;
                    } else if (NULL != msg->msg->buffer) {
                        /* send the buffer data as a single block */
                        msg->sdptr = msg->msg->buffer->base_ptr;
                        msg->sdbytes = msg->msg->buffer->bytes_used;
                    } else if (NULL != msg->msg->iov) {
                        /* start with the first iovec */
                        msg->sdptr = msg->msg->iov[0].iov_base;
                        msg->sdbytes = msg->msg->iov[0].iov_len;
                        msg->iovnum = 0;
                    } else {
                        msg->sdptr = msg->msg->data;
                        msg->sdbytes = msg->msg->count;
                    }
                    /* fall thru and let the send progress */
                } else if (ORTE_ERR_RESOURCE_BUSY == rc ||
                           ORTE_ERR_WOULD_BLOCK == rc) {
                    /* exit this event and let the event lib progress */
                    return;
                } else {
                    // report the error
                    opal_output(0, "%s-%s mca_oob_usock_peer_send_handler: unable to send header",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->name)));
                    opal_event_del(&peer->send_event);
                    peer->send_ev_active = false;
                    msg->msg->status = rc;
                    ORTE_RML_SEND_COMPLETE(msg->msg);
                    OBJ_RELEASE(msg);
                    peer->send_msg = NULL;
                    goto next;
                }
            }
            /* progress the data transmission */
            if (msg->hdr_sent) {
                if (ORTE_SUCCESS == (rc = send_bytes(peer))) {
                    /* this block is complete */
                    if (NULL != msg->msg->buffer) {
                        /* we are done - notify the RML */
                        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                                            "%s MESSAGE SEND COMPLETE TO %s OF %d BYTES ON SOCKET %d",
                                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                            ORTE_NAME_PRINT(&(peer->name)),
                                            msg->hdr.nbytes, peer->sd);
                        msg->msg->status = ORTE_SUCCESS;
                        ORTE_RML_SEND_COMPLETE(msg->msg);
                        OBJ_RELEASE(msg);
                        peer->send_msg = NULL;
                    } else if (NULL != msg->msg->data) {
                        /* this was a relay message - nothing more to do */
                        opal_output_verbose(2, orte_oob_base_framework.framework_output,
                                            "%s MESSAGE SEND COMPLETE TO %s OF %d BYTES ON SOCKET %d",
                                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                            ORTE_NAME_PRINT(&(peer->name)),
                                            msg->hdr.nbytes, peer->sd);
                        OBJ_RELEASE(msg);
                        peer->send_msg = NULL;
                    } else {
                        /* rotate to the next iovec */
                        msg->iovnum++;
                        if (msg->iovnum < msg->msg->count) {
                            msg->sdptr = msg->msg->iov[msg->iovnum].iov_base;
                            msg->sdbytes = msg->msg->iov[msg->iovnum].iov_len;
                            /* exit this event to give the event lib
                             * a chance to progress any other pending
                             * actions
                             */
                            return;
                        } else {
                            /* this message is complete - notify the RML */
                            opal_output_verbose(2, orte_oob_base_framework.framework_output,
                                                "%s MESSAGE SEND COMPLETE TO %s OF %d BYTES ON SOCKET %d",
                                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                                ORTE_NAME_PRINT(&(peer->name)),
                                                msg->hdr.nbytes, peer->sd);
                            msg->msg->status = ORTE_SUCCESS;
                            ORTE_RML_SEND_COMPLETE(msg->msg);
                            OBJ_RELEASE(msg);
                            peer->send_msg = NULL;
                        }
                    }
                    /* fall thru to queue the next message */
                } else if (ORTE_ERR_RESOURCE_BUSY == rc ||
                           ORTE_ERR_WOULD_BLOCK == rc) {
                    /* exit this event and let the event lib progress */
                    return;
                } else {
                    // report the error
                    opal_output(0, "%s-%s mca_oob_usock_peer_send_handler: unable to send message ON SOCKET %d",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->name)), peer->sd);
                    opal_event_del(&peer->send_event);
                    peer->send_ev_active = false;
                    msg->msg->status = rc;
                    ORTE_RML_SEND_COMPLETE(msg->msg);
                    OBJ_RELEASE(msg);
                    peer->send_msg = NULL;
                    ORTE_FORCED_TERMINATE(1);
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
            peer->send_msg = (mca_oob_usock_send_t*)
                opal_list_remove_first(&peer->send_queue);
        }
        
        /* if nothing else to do unregister for send event notifications */
        if (NULL == peer->send_msg && peer->send_ev_active) {
            opal_event_del(&peer->send_event);
            peer->send_ev_active = false;
        }
        break;
    default:
        opal_output(0, "%s-%s mca_oob_usock_peer_send_handler: invalid connection state (%d) on socket %d",
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

static int read_bytes(mca_oob_usock_peer_t* peer)
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
            opal_output_verbose(OOB_USOCK_DEBUG_FAIL, orte_oob_base_framework.framework_output,
                                "%s-%s mca_oob_usock_msg_recv: readv failed: %s (%d)", 
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&(peer->name)),
                                strerror(opal_socket_errno),
                                opal_socket_errno);
            // mca_oob_usock_peer_close(peer);
            // if (NULL != mca_oob_usock.oob_exception_callback) {
            // mca_oob_usock.oob_exception_callback(&peer->name, ORTE_RML_PEER_DISCONNECTED);
            //}
            return ORTE_ERR_COMM_FAILURE;
        } else if (rc == 0)  {
            /* the remote peer closed the connection - report that condition
             * and let the caller know
             */
            opal_output_verbose(OOB_USOCK_DEBUG_FAIL, orte_oob_base_framework.framework_output,
                                "%s-%s mca_oob_usock_msg_recv: peer closed connection", 
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
            mca_oob_usock_peer_close(peer);
            //if (NULL != mca_oob_usock.oob_exception_callback) {
            //   mca_oob_usock.oob_exception_callback(&peer->peer_name, ORTE_RML_PEER_DISCONNECTED);
            //}
            return ORTE_ERR_WOULD_BLOCK;
        }
        /* we were able to read something, so adjust counters and location */
        peer->recv_msg->rdbytes -= rc;
        peer->recv_msg->rdptr += rc;
    }

    /* we read the full data block */
    return ORTE_SUCCESS;
}

/*
 * Dispatch to the appropriate action routine based on the state
 * of the connection with the peer.
 */

void mca_oob_usock_recv_handler(int sd, short flags, void *cbdata)
{
    mca_oob_usock_peer_t* peer = (mca_oob_usock_peer_t*)cbdata;
    int rc;
    orte_rml_send_t *snd;

    if (orte_abnormal_term_ordered) {
        return;
    }

    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                        "%s:usock:recv:handler called for peer %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&peer->name));

    switch (peer->state) {
    case MCA_OOB_USOCK_CONNECT_ACK:
        if (ORTE_SUCCESS == (rc = mca_oob_usock_peer_recv_connect_ack(peer, peer->sd, NULL))) {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
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
                peer->send_msg = (mca_oob_usock_send_t*)opal_list_remove_first(&peer->send_queue);
            }
            if (NULL != peer->send_msg && !peer->send_ev_active) {
                opal_event_add(&peer->send_event, 0);
                peer->send_ev_active = true;
            }
            /* update our state */
            peer->state = MCA_OOB_USOCK_CONNECTED;
        } else {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                "%s UNABLE TO COMPLETE CONNECT ACK WITH %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                ORTE_NAME_PRINT(&peer->name));
            opal_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
            ORTE_FORCED_TERMINATE(1);
            return;
        }
        break;
    case MCA_OOB_USOCK_CONNECTED:
        opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                            "%s:usock:recv:handler CONNECTED",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        /* allocate a new message and setup for recv */
        if (NULL == peer->recv_msg) {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                "%s:usock:recv:handler allocate new recv msg",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            peer->recv_msg = OBJ_NEW(mca_oob_usock_recv_t);
            if (NULL == peer->recv_msg) {
                opal_output(0, "%s-%s mca_oob_usock_peer_recv_handler: unable to allocate recv message\n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)));
                return;
            }
            /* start by reading the header */
            peer->recv_msg->rdptr = (char*)&peer->recv_msg->hdr;
            peer->recv_msg->rdbytes = sizeof(mca_oob_usock_hdr_t);
        }
        /* if the header hasn't been completely read, read it */
        if (!peer->recv_msg->hdr_recvd) {
            opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                "%s:usock:recv:handler read hdr",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
            if (ORTE_SUCCESS == (rc = read_bytes(peer))) {
                /* completed reading the header */
                peer->recv_msg->hdr_recvd = true;
                /* if this is a zero-byte message, then we are done */
                if (0 == peer->recv_msg->hdr.nbytes) {
                    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                        "%s RECVD ZERO-BYTE MESSAGE FROM %s for tag %d",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                        ORTE_NAME_PRINT(&peer->name), peer->recv_msg->hdr.tag);
                    peer->recv_msg->data = NULL;  // make sure
                    peer->recv_msg->rdptr = NULL;
                    peer->recv_msg->rdbytes = 0;
                } else {
                    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
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
                opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                    "%s:usock:recv:handler error reading bytes - closing connection",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                mca_oob_usock_peer_close(peer);
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
                opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                    "%s RECVD COMPLETE MESSAGE FROM %s OF %d BYTES FOR DEST %s TAG %d",
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                    ORTE_NAME_PRINT(&peer->recv_msg->hdr.origin),
                                    (int)peer->recv_msg->hdr.nbytes,
                                    ORTE_NAME_PRINT(&peer->recv_msg->hdr.dst),
                                    peer->recv_msg->hdr.tag);
                /* am I the intended recipient? */
                if (peer->recv_msg->hdr.dst.jobid == ORTE_PROC_MY_NAME->jobid &&
                    peer->recv_msg->hdr.dst.vpid == ORTE_PROC_MY_NAME->vpid) {
                    /* yes - post it to the RML for delivery */
                    opal_output_verbose(OOB_USOCK_DEBUG_CONNECT, orte_oob_base_framework.framework_output,
                                        "%s DELIVERING TO RML",
                                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
                    ORTE_RML_POST_MESSAGE(&peer->recv_msg->hdr.origin, peer->recv_msg->hdr.tag,
                                          peer->recv_msg->data,
                                          peer->recv_msg->hdr.nbytes);
                    OBJ_RELEASE(peer->recv_msg);
                } else {
                    /* no - we don't route things, so we promote this
                     * back to the OOB and let another transport move
                     * it along. If we are a daemon and it is intended
                     * for another of our local procs, it will just come
                     * back to us and be handled then
                     */
                    snd = OBJ_NEW(orte_rml_send_t);
                    snd->dst = peer->recv_msg->hdr.dst;
                    snd->origin = peer->recv_msg->hdr.origin;
                    snd->tag = peer->recv_msg->hdr.tag;
                    snd->data = peer->recv_msg->data;
                    snd->count = peer->recv_msg->hdr.nbytes;
                    snd->cbfunc.iov = NULL;
                    snd->cbdata = NULL;
                    /* activate the OOB send state */
                    ORTE_OOB_SEND(snd);
                    /* protect the data */
                    peer->recv_msg->data = NULL;
                    /* cleanup */
                    OBJ_RELEASE(peer->recv_msg);
                    return;
                }
            } else if (ORTE_ERR_RESOURCE_BUSY == rc ||
                       ORTE_ERR_WOULD_BLOCK == rc) {
                /* exit this event and let the event lib progress */
                return;
            } else {
                // report the error
                opal_output(0, "%s-%s mca_oob_usock_peer_recv_handler: unable to recv message",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&(peer->name)));
                /* turn off the recv event */
                opal_event_del(&peer->recv_event);
                peer->recv_ev_active = false;
                ORTE_FORCED_TERMINATE(1);
                return;
            }
        }
        break;
    default: 
        opal_output(0, "%s-%s mca_oob_usock_peer_recv_handler: invalid socket state(%d)", 
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(&(peer->name)),
                    peer->state);
        // mca_oob_usock_peer_close(peer);
        break;
    }
}

static void snd_cons(mca_oob_usock_send_t *ptr)
{
    ptr->msg = NULL;
    ptr->data = NULL;
    ptr->hdr_sent = false;
    ptr->iovnum = 0;
    ptr->sdptr = NULL;
    ptr->sdbytes = 0;
}
/* we don't destruct any RML msg that is
 * attached to our send as the RML owns
 * that memory. However, if we relay a
 * msg, the data in the relay belongs to
 * us and must be free'd
 */
static void snd_des(mca_oob_usock_send_t *ptr)
{
    if (NULL != ptr->data) {
        free(ptr->data);
    }
}
OBJ_CLASS_INSTANCE(mca_oob_usock_send_t,
                   opal_list_item_t,
                   snd_cons, snd_des);

static void rcv_cons(mca_oob_usock_recv_t *ptr)
{
    ptr->hdr_recvd = false;
    ptr->rdptr = NULL;
    ptr->rdbytes = 0;
}
OBJ_CLASS_INSTANCE(mca_oob_usock_recv_t,
                   opal_list_item_t,
                   rcv_cons, NULL);

static void err_cons(mca_oob_usock_msg_error_t *ptr)
{
    ptr->rmsg = NULL;
    ptr->snd = NULL;
}
OBJ_CLASS_INSTANCE(mca_oob_usock_msg_error_t,
                   opal_object_t,
                   err_cons, NULL);

