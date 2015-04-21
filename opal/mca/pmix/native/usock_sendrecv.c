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

#include "opal_config.h"

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
#include "opal/runtime/opal.h"
#include "opal/opal_socket_errno.h"
#include "opal/mca/backtrace/backtrace.h"
#include "opal/util/output.h"
#include "opal/util/net.h"
#include "opal/util/error.h"
#include "opal/class/opal_hash_table.h"
#include "opal/mca/event/event.h"
#include "opal/mca/sec/sec.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix_native.h"

static void usock_complete_connect(void);
static int usock_recv_connect_ack(void);

static int send_bytes(pmix_usock_send_t *msg)
{
    int rc;

    while (0 < msg->sdbytes) {
        rc = write(mca_pmix_native_component.sd, msg->sdptr, msg->sdbytes);
        if (rc < 0) {
            if (opal_socket_errno == EINTR) {
                continue;
            } else if (opal_socket_errno == EAGAIN) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return OPAL_ERR_RESOURCE_BUSY;
            } else if (opal_socket_errno == EWOULDBLOCK) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return OPAL_ERR_WOULD_BLOCK;
            }
            /* we hit an error and cannot progress this message */
            opal_output(0, "%s pmix_usock_msg_send_bytes: write failed: %s (%d) [sd = %d]",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), 
                        strerror(opal_socket_errno),
                        opal_socket_errno,
                        mca_pmix_native_component.sd);
            return OPAL_ERR_COMM_FAILURE;
        }
        /* update location */
        msg->sdbytes -= rc;
        msg->sdptr += rc;
    }
    /* we sent the full data block */
    return OPAL_SUCCESS;
}

/*
 * A file descriptor is available/ready for send. Check the state
 * of the socket and take the appropriate action.
 */
void pmix_usock_send_handler(int sd, short flags, void *cbdata)
{
    pmix_usock_send_t *msg = mca_pmix_native_component.send_msg;
    int rc;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s usock:send_handler called to send to server",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    switch (mca_pmix_native_component.state) {
    case PMIX_USOCK_CONNECTING:
    case PMIX_USOCK_CLOSED:
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "usock:send_handler %s",
                            pmix_usock_state_print(mca_pmix_native_component.state));
        usock_complete_connect();
        /* de-activate the send event until the connection
         * handshake completes
         */
        if (mca_pmix_native_component.send_ev_active) {
            opal_event_del(&mca_pmix_native_component.send_event);
            mca_pmix_native_component.send_ev_active = false;
        }
        break;
    case PMIX_USOCK_CONNECTED:
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock:send_handler SENDING TO SERVER with %s msg",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            (NULL == msg) ? "NULL" : "NON-NULL");
        if (NULL != msg) {
            if (!msg->hdr_sent) {
                opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                    "%s usock:send_handler SENDING HEADER",
                                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                if (OPAL_SUCCESS == (rc = send_bytes(msg))) {
                    /* header is completely sent */
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s usock:send_handler HEADER SENT",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                    msg->hdr_sent = true;
                    /* setup to send the data */
                    if (NULL == msg->data) {
                        /* this was a zero-byte msg - nothing more to do */
                        OBJ_RELEASE(msg);
                        mca_pmix_native_component.send_msg = NULL;
                        goto next;
                    } else {
                        /* send the data as a single block */
                        msg->sdptr = msg->data;
                        msg->sdbytes = msg->hdr.nbytes;
                    }
                    /* fall thru and let the send progress */
                } else if (OPAL_ERR_RESOURCE_BUSY == rc ||
                           OPAL_ERR_WOULD_BLOCK == rc) {
                    /* exit this event and let the event lib progress */
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s usock:send_handler RES BUSY OR WOULD BLOCK",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                    return;
                } else {
                    // report the error
                    opal_output(0, "%s pmix_usock_peer_send_handler: unable to send message ON SOCKET %d",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                mca_pmix_native_component.sd);
                    opal_event_del(&mca_pmix_native_component.send_event);
                    mca_pmix_native_component.send_ev_active = false;
                    OBJ_RELEASE(msg);
                    mca_pmix_native_component.send_msg = NULL;
                    return;
                }
            }

            if (msg->hdr_sent) {
                opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                    "%s usock:send_handler SENDING BODY OF MSG",
                                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                if (OPAL_SUCCESS == (rc = send_bytes(msg))) {
                    // message is complete
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s usock:send_handler BODY SENT",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                    OBJ_RELEASE(msg);
                    mca_pmix_native_component.send_msg = NULL;
                    goto next;
                } else if (OPAL_ERR_RESOURCE_BUSY == rc ||
                           OPAL_ERR_WOULD_BLOCK == rc) {
                    /* exit this event and let the event lib progress */
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s usock:send_handler RES BUSY OR WOULD BLOCK",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                    return;
                } else {
                    // report the error
                    opal_output(0, "%s pmix_usock_peer_send_handler: unable to send message ON SOCKET %d",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                mca_pmix_native_component.sd);
                    opal_event_del(&mca_pmix_native_component.send_event);
                    mca_pmix_native_component.send_ev_active = false;
                    OBJ_RELEASE(msg);
                    mca_pmix_native_component.send_msg = NULL;
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
            mca_pmix_native_component.send_msg = (pmix_usock_send_t*)
                opal_list_remove_first(&mca_pmix_native_component.send_queue);
        }

        /* if nothing else to do unregister for send event notifications */
        if (NULL == mca_pmix_native_component.send_msg &&
            mca_pmix_native_component.send_ev_active) {
            opal_event_del(&mca_pmix_native_component.send_event);
            mca_pmix_native_component.send_ev_active = false;
        }
        break;

    default:
        opal_output(0, "%s pmix_usock_peer_send_handler: invalid connection state (%d) on socket %d",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    mca_pmix_native_component.state, mca_pmix_native_component.sd);
        if (mca_pmix_native_component.send_ev_active) {
            opal_event_del(&mca_pmix_native_component.send_event);
            mca_pmix_native_component.send_ev_active = false;
        }
        break;
    }
}

static int read_bytes(pmix_usock_recv_t* recv)
{
    int rc;

    /* read until all bytes recvd or error */
    while (0 < recv->rdbytes) {
        rc = read(mca_pmix_native_component.sd, recv->rdptr, recv->rdbytes);
        if (rc < 0) {
            if(opal_socket_errno == EINTR) {
                continue;
            } else if (opal_socket_errno == EAGAIN) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return OPAL_ERR_RESOURCE_BUSY;
            } else if (opal_socket_errno == EWOULDBLOCK) {
                /* tell the caller to keep this message on active,
                 * but let the event lib cycle so other messages
                 * can progress while this socket is busy
                 */
                return OPAL_ERR_WOULD_BLOCK;
            }
            /* we hit an error and cannot progress this message - report
             * the error back to the RML and let the caller know
             * to abort this message
             */
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s pmix_usock_msg_recv: readv failed: %s (%d)",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), 
                                strerror(opal_socket_errno),
                                opal_socket_errno);
            return OPAL_ERR_COMM_FAILURE;
        } else if (rc == 0)  {
            /* the remote peer closed the connection - report that condition
             * and let the caller know
             */
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s pmix_usock_msg_recv: peer closed connection",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
            /* stop all events */
            if (mca_pmix_native_component.recv_ev_active) {
                opal_event_del(&mca_pmix_native_component.recv_event);
                mca_pmix_native_component.recv_ev_active = false;
            }
            if (mca_pmix_native_component.timer_ev_active) {
                opal_event_del(&mca_pmix_native_component.timer_event);
                mca_pmix_native_component.timer_ev_active = false;
            }
            if (mca_pmix_native_component.send_ev_active) {
                opal_event_del(&mca_pmix_native_component.send_event);
                mca_pmix_native_component.send_ev_active = false;
            }
            if (NULL != mca_pmix_native_component.recv_msg) {
                OBJ_RELEASE(mca_pmix_native_component.recv_msg);
                mca_pmix_native_component.recv_msg = NULL;
            }
            CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
            return OPAL_ERR_WOULD_BLOCK;
        }
        /* we were able to read something, so adjust counters and location */
        recv->rdbytes -= rc;
        recv->rdptr += rc;
    }

    /* we read the full data block */
    return OPAL_SUCCESS;
}

/*
 * Dispatch to the appropriate action routine based on the state
 * of the connection with the peer.
 */

void pmix_usock_recv_handler(int sd, short flags, void *cbdata)
{
    int rc;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s usock:recv:handler called",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    switch (mca_pmix_native_component.state) {
    case PMIX_USOCK_CONNECT_ACK:
        if (OPAL_SUCCESS == (rc = usock_recv_connect_ack())) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s usock:recv:handler starting send/recv events",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
            /* we connected! Start the send/recv events */
            if (!mca_pmix_native_component.recv_ev_active) {
                opal_event_add(&mca_pmix_native_component.recv_event, 0);
                mca_pmix_native_component.recv_ev_active = true;
            }
            if (mca_pmix_native_component.timer_ev_active) {
                opal_event_del(&mca_pmix_native_component.timer_event);
                mca_pmix_native_component.timer_ev_active = false;
            }
            /* if there is a message waiting to be sent, queue it */
            if (NULL == mca_pmix_native_component.send_msg) {
                mca_pmix_native_component.send_msg = (pmix_usock_send_t*)opal_list_remove_first(&mca_pmix_native_component.send_queue);
            }
            if (NULL != mca_pmix_native_component.send_msg && !mca_pmix_native_component.send_ev_active) {
                opal_event_add(&mca_pmix_native_component.send_event, 0);
                mca_pmix_native_component.send_ev_active = true;
            }
            /* update our state */
            mca_pmix_native_component.state = PMIX_USOCK_CONNECTED;
        } else {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s UNABLE TO COMPLETE CONNECT ACK WITH SERVER",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
            opal_event_del(&mca_pmix_native_component.recv_event);
            mca_pmix_native_component.recv_ev_active = false;
            return;
        }
        break;
    case PMIX_USOCK_CONNECTED:
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock:recv:handler CONNECTED",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        /* allocate a new message and setup for recv */
        if (NULL == mca_pmix_native_component.recv_msg) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s usock:recv:handler allocate new recv msg",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
            mca_pmix_native_component.recv_msg = OBJ_NEW(pmix_usock_recv_t);
            if (NULL == mca_pmix_native_component.recv_msg) {
                opal_output(0, "%s usock_recv_handler: unable to allocate recv message\n",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                return;
            }
            /* start by reading the header */
            mca_pmix_native_component.recv_msg->rdptr = (char*)&mca_pmix_native_component.recv_msg->hdr;
            mca_pmix_native_component.recv_msg->rdbytes = sizeof(pmix_usock_hdr_t);
        }
        /* if the header hasn't been completely read, read it */
        if (!mca_pmix_native_component.recv_msg->hdr_recvd) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "usock:recv:handler read hdr");
            if (OPAL_SUCCESS == (rc = read_bytes(mca_pmix_native_component.recv_msg))) {
                /* completed reading the header */
                mca_pmix_native_component.recv_msg->hdr_recvd = true;
                /* if this is a zero-byte message, then we are done */
                if (0 == mca_pmix_native_component.recv_msg->hdr.nbytes) {
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s RECVD ZERO-BYTE MESSAGE FROM SERVER for tag %d",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                        mca_pmix_native_component.recv_msg->hdr.tag);
                    mca_pmix_native_component.recv_msg->data = NULL;  // make sure
                    mca_pmix_native_component.recv_msg->rdptr = NULL;
                    mca_pmix_native_component.recv_msg->rdbytes = 0;
                } else {
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s usock:recv:handler allocate data region of size %lu",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                        (unsigned long)mca_pmix_native_component.recv_msg->hdr.nbytes);
                    /* allocate the data region */
                    mca_pmix_native_component.recv_msg->data = (char*)malloc(mca_pmix_native_component.recv_msg->hdr.nbytes);
                    /* point to it */
                    mca_pmix_native_component.recv_msg->rdptr = mca_pmix_native_component.recv_msg->data;
                    mca_pmix_native_component.recv_msg->rdbytes = mca_pmix_native_component.recv_msg->hdr.nbytes;
                }
                /* fall thru and attempt to read the data */
            } else if (OPAL_ERR_RESOURCE_BUSY == rc ||
                       OPAL_ERR_WOULD_BLOCK == rc) {
                /* exit this event and let the event lib progress */
                return;
            } else {
                /* close the connection */
                opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                    "%s usock:recv:handler error reading bytes - closing connection",
                                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
                return;
            }
        }

        if (mca_pmix_native_component.recv_msg->hdr_recvd) {
            /* continue to read the data block - we start from
             * wherever we left off, which could be at the
             * beginning or somewhere in the message
             */
            if (OPAL_SUCCESS == (rc = read_bytes(mca_pmix_native_component.recv_msg))) {
                /* we recvd all of the message */
                opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                    "%s RECVD COMPLETE MESSAGE FROM SERVER OF %d BYTES FOR TAG %d",
                                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                    (int)mca_pmix_native_component.recv_msg->hdr.nbytes,
                                    mca_pmix_native_component.recv_msg->hdr.tag);
                /* post it for delivery */
                PMIX_ACTIVATE_POST_MSG(mca_pmix_native_component.recv_msg);
                mca_pmix_native_component.recv_msg = NULL;
            } else if (OPAL_ERR_RESOURCE_BUSY == rc ||
                       OPAL_ERR_WOULD_BLOCK == rc) {
                /* exit this event and let the event lib progress */
                return;
            } else {
                // report the error
                opal_output(0, "%s usock_peer_recv_handler: unable to recv message",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                /* turn off the recv event */
                opal_event_del(&mca_pmix_native_component.recv_event);
                mca_pmix_native_component.recv_ev_active = false;
                CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
                return;
            }
        }
        break;
    default: 
        opal_output(0, "%s usock_peer_recv_handler: invalid socket state(%d)",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), 
                    mca_pmix_native_component.state);
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        break;
    }
}

/*
 * A blocking recv on a non-blocking socket. Used to receive the small amount of connection
 * information that identifies the peers endpoint.
 */
static bool usock_recv_blocking(char *data, size_t size)
{
    size_t cnt = 0;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s waiting for connect ack from server",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    while (cnt < size) {
        int retval = recv(mca_pmix_native_component.sd, (char *)data+cnt, size-cnt, 0);

        /* remote closed connection */
        if (retval == 0) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s usock_recv_blocking: server closed connection: state %d",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                mca_pmix_native_component.state);
            mca_pmix_native_component.state = PMIX_USOCK_CLOSED;
            CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
            return false;
        }

        /* socket is non-blocking so handle errors */
        if (retval < 0) {
            if (opal_socket_errno != EINTR && 
                opal_socket_errno != EAGAIN && 
                opal_socket_errno != EWOULDBLOCK) {
                if (mca_pmix_native_component.state == PMIX_USOCK_CONNECT_ACK) {
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
                    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                        "%s connect ack received error %s from server",
                                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                        strerror(opal_socket_errno));
                    return false;
                } else {
                    opal_output(0, 
                                "%s usock_recv_blocking: "
                                "recv() failed for server: %s (%d)\n",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                strerror(opal_socket_errno),
                                opal_socket_errno);
                    mca_pmix_native_component.state = PMIX_USOCK_FAILED;
                    CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
                    return false;
                }
            }
            continue;
        }
        cnt += retval;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s connect ack received from server",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
    return true;
}


/*
 *  Receive the peers globally unique process identification from a newly
 *  connected socket and verify the expected response. If so, move the
 *  socket to a connected state.
 */
static int usock_recv_connect_ack(void)
{
    char *msg;
    char *version;
    int rc;
    char *cred;
    size_t credsize;
    pmix_usock_hdr_t hdr;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s RECV CONNECT ACK FROM SERVER ON SOCKET %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        mca_pmix_native_component.sd);

    /* ensure all is zero'd */
    memset(&hdr, 0, sizeof(pmix_usock_hdr_t));

    if (usock_recv_blocking((char*)&hdr, sizeof(pmix_usock_hdr_t))) {
        /* If the state is CONNECT_ACK, then we were waiting for
         * the connection to be ack'd
         */
        if (mca_pmix_native_component.state != PMIX_USOCK_CONNECT_ACK) {
            /* handshake broke down - abort this connection */
            opal_output(0, "%s RECV CONNECT BAD HANDSHAKE FROM SERVER ON SOCKET %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        mca_pmix_native_component.sd);
            mca_pmix_native_component.state = PMIX_USOCK_FAILED;
            CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
            return OPAL_ERR_UNREACH;
        }
    } else {
        /* unable to complete the recv */
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s unable to complete recv of connect-ack from server ON SOCKET %d",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            mca_pmix_native_component.sd);
        return OPAL_ERR_UNREACH;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s connect-ack recvd from server",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* compare the servers name to the expected value */
    if (0 != opal_compare_proc(hdr.id, mca_pmix_native_component.server)) {
        opal_output(0, "usock_peer_recv_connect_ack: "
                    "%s received unexpected process identifier (%s) from server: expected (%s)",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    OPAL_NAME_PRINT(hdr.id),
                    OPAL_NAME_PRINT(mca_pmix_native_component.server));
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        return OPAL_ERR_UNREACH;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s connect-ack header from server is okay",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* get the authentication and version payload */
    if (NULL == (msg = (char*)malloc(hdr.nbytes))) {
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    if (!usock_recv_blocking(msg, hdr.nbytes)) {
        /* unable to complete the recv */
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s unable to complete recv of connect-ack from server ON SOCKET %d",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            mca_pmix_native_component.sd);
        free(msg);
        return OPAL_ERR_UNREACH;
    }

    /* check that this is from a matching version */
    version = (char*)(msg);
    if (0 != strcmp(version, opal_version_string)) {
        opal_output(0, "usock_peer_recv_connect_ack: "
                    "%s received different version from server: %s instead of %s",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    version, opal_version_string);
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        free(msg);
        return OPAL_ERR_UNREACH;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s connect-ack version from server matches ours",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* check security token */
    cred = (char*)(msg + strlen(version) + 1);
    credsize = hdr.nbytes - strlen(version) - 1;
    if (OPAL_SUCCESS != (rc = opal_sec.authenticate(cred, credsize, NULL))) {
        OPAL_ERROR_LOG(rc);
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        free(msg);
        return OPAL_ERR_UNREACH;
    }
    free(msg);

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s connect-ack from server authenticated",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* connected */
    mca_pmix_native_component.state = PMIX_USOCK_CONNECTED;
    /* initiate send of first message on queue */
    if (NULL == mca_pmix_native_component.send_msg) {
        mca_pmix_native_component.send_msg = (pmix_usock_send_t*)
            opal_list_remove_first(&mca_pmix_native_component.send_queue);
    }
    if (NULL != mca_pmix_native_component.send_msg && !mca_pmix_native_component.send_ev_active) {
        opal_event_add(&mca_pmix_native_component.send_event, 0);
        mca_pmix_native_component.send_ev_active = true;
    }
    if (2 <= opal_output_get_verbosity(opal_pmix_base_framework.framework_output)) {
        pmix_usock_dump("connected");
    }
    return OPAL_SUCCESS;
}


/*
 * Check the status of the connection. If the connection failed, will retry
 * later. Otherwise, send this process' identifier to the server on the
 * newly connected socket.
 */
static void usock_complete_connect(void)
{
    int so_error = 0;
    opal_socklen_t so_length = sizeof(so_error);

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s usock:complete_connect called for server on socket %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        mca_pmix_native_component.sd);

    /* check connect completion status */
    if (getsockopt(mca_pmix_native_component.sd, SOL_SOCKET, SO_ERROR, (char *)&so_error, &so_length) < 0) {
        opal_output(0, "%s usock_peer_complete_connect: getsockopt() to server failed: %s (%d)\n",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        return;
    }

    if (so_error == EINPROGRESS) {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock:send:handler still in progress",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        return;
    } else if (so_error == ECONNREFUSED || so_error == ETIMEDOUT) {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock_peer_complete_connect: connection to server failed: %s (%d)",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            strerror(so_error),
                            so_error);
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        return;
    } else if (so_error != 0) {
        /* No need to worry about the return code here - we return regardless
           at this point, and if an error did occur a message has already been
           printed for the user */
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock_peer_complete_connect: "
                            "connection to server failed with error %d",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            so_error);
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        return;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s usock_peer_complete_connect: sending ack to server",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    if (usock_send_connect_ack() == OPAL_SUCCESS) {
        mca_pmix_native_component.state = PMIX_USOCK_CONNECT_ACK;
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock_peer_complete_connect: setting read event on connection to server",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        
        if (!mca_pmix_native_component.recv_ev_active) {
            opal_event_add(&mca_pmix_native_component.recv_event, 0);
            mca_pmix_native_component.recv_ev_active = true;
        }
    } else {
        opal_output(0, "%s usock_complete_connect: unable to send connect ack to server",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
    }
}

