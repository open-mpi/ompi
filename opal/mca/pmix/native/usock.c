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
 * Copyright (c) 2013-2015 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/types.h"

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

#include "opal_stdint.h"
#include "opal/opal_socket_errno.h"
#include "opal/dss/dss.h"
#include "opal/mca/dstore/dstore.h"
#include "opal/mca/sec/sec.h"
#include "opal/runtime/opal.h"
#include "opal/util/show_help.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/base/base.h"
#include "pmix_native.h"

static int usock_send_blocking(char *ptr, size_t size);
static void pmix_usock_try_connect(int fd, short args, void *cbdata);
static int usock_create_socket(void);

/* State machine for internal operations */
typedef struct {
    opal_object_t super;
    opal_event_t ev;
} pmix_usock_op_t;
static OBJ_CLASS_INSTANCE(pmix_usock_op_t,
                          opal_object_t,
                          NULL, NULL);

#define PMIX_ACTIVATE_USOCK_STATE(cbfunc)                               \
    do {                                                                \
        pmix_usock_op_t *op;                                            \
        op = OBJ_NEW(pmix_usock_op_t);                                  \
        opal_event_set(mca_pmix_native_component.evbase, &op->ev, -1,   \
                       OPAL_EV_WRITE, (cbfunc), op);                    \
        opal_event_set_priority(&op->ev, OPAL_EV_MSG_LO_PRI);           \
        opal_event_active(&op->ev, OPAL_EV_WRITE, 1);                    \
    } while(0);

void pmix_usock_send_recv(int fd, short args, void *cbdata)
{
    pmix_usock_sr_t *ms = (pmix_usock_sr_t*)cbdata;
    pmix_usock_posted_recv_t *req;
    pmix_usock_send_t *snd;
    uint32_t tag = UINT32_MAX;

    if (NULL != ms->cbfunc) {
        /* if a callback msg is expected, setup a recv for it */
        req = OBJ_NEW(pmix_usock_posted_recv_t);
        /* take the next tag in the sequence */
        if (UINT32_MAX == mca_pmix_native_component.tag) {
            mca_pmix_native_component.tag = 0;
        }
        req->tag = mca_pmix_native_component.tag++;
        tag = req->tag;
        req->cbfunc = ms->cbfunc;
        req->cbdata = ms->cbdata;
        opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                            "%s posting recv on tag %d",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), req->tag);
        /* add it to the list of recvs - we cannot have unexpected messages
         * in this subsystem as the server never sends us something that
         * we didn't previously request */
        opal_list_append(&mca_pmix_native_component.posted_recvs, &req->super);
    }

    snd = OBJ_NEW(pmix_usock_send_t);
    snd->hdr.id = mca_pmix_native_component.id;
    snd->hdr.type = PMIX_USOCK_USER;
    snd->hdr.tag = tag;
    snd->hdr.nbytes = ms->bfr->bytes_used;
    snd->data = ms->bfr->base_ptr;
    /* always start with the header */
    snd->sdptr = (char*)&snd->hdr;
    snd->sdbytes = sizeof(pmix_usock_hdr_t);

    /* add the msg to the send queue if we are already connected*/
    if (PMIX_USOCK_CONNECTED == mca_pmix_native_component.state) {
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock:send_nb: already connected to server - queueing for send",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        /* if there is no message on-deck, put this one there */
        if (NULL == mca_pmix_native_component.send_msg) {
            mca_pmix_native_component.send_msg = snd;
        } else {
            /* add it to the queue */
            opal_list_append(&mca_pmix_native_component.send_queue, &snd->super);
        }
        /* ensure the send event is active */
        if (!mca_pmix_native_component.send_ev_active) {
            opal_event_add(&mca_pmix_native_component.send_event, 0);
            mca_pmix_native_component.send_ev_active = true;
        }
        return;
    }

    /* add the message to the queue for sending after the
     * connection is formed
     */
    opal_list_append(&mca_pmix_native_component.send_queue, &snd->super);

    if (PMIX_USOCK_CONNECTING != mca_pmix_native_component.state &&
        PMIX_USOCK_CONNECT_ACK != mca_pmix_native_component.state) {
        /* we have to initiate the connection - again, we do not
         * want to block while the connection is created.
         * So throw us into an event that will create
         * the connection via a mini-state-machine :-)
         */
        opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                            "%s usock:send_nb: initiating connection to server",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
        mca_pmix_native_component.state = PMIX_USOCK_CONNECTING;
        PMIX_ACTIVATE_USOCK_STATE(pmix_usock_try_connect);
    }
}

void pmix_usock_process_msg(int fd, short flags, void *cbdata)
{
    pmix_usock_recv_t *msg = (pmix_usock_recv_t*)cbdata;
    pmix_usock_posted_recv_t *rcv;
    opal_buffer_t buf;

    OPAL_OUTPUT_VERBOSE((5, opal_pmix_base_framework.framework_output,
                         "%s message received %d bytes for tag %u",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                         (int)msg->hdr.nbytes, msg->hdr.tag));

    /* see if we have a waiting recv for this message */
    OPAL_LIST_FOREACH(rcv, &mca_pmix_native_component.posted_recvs, pmix_usock_posted_recv_t) {
        opal_output_verbose(5, opal_pmix_base_framework.framework_output,
                            "%s checking msg on tag %u for tag %u",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            msg->hdr.tag, rcv->tag);

        if (msg->hdr.tag == rcv->tag) {
            if (NULL != rcv->cbfunc) {
                /* construct and load the buffer */
                OBJ_CONSTRUCT(&buf, opal_buffer_t);
                if (NULL != msg->data) {
                    opal_dss.load(&buf, msg->data, msg->hdr.nbytes);
                }
                msg->data = NULL;  // protect the data region
                if (NULL != rcv->cbfunc) {
                    rcv->cbfunc(&buf, rcv->cbdata);
                }
                OBJ_DESTRUCT(&buf);  // free's the msg data
                /* also done with the recv */
                opal_list_remove_item(&mca_pmix_native_component.posted_recvs, &rcv->super);
                OBJ_RELEASE(rcv);
                OBJ_RELEASE(msg);
                return;
            }
        }
    }

    /* we get here if no matching recv was found - this is an error */
    opal_output(0, "%s UNEXPECTED MESSAGE",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
    OBJ_RELEASE(msg);
}

static int usock_create_socket(void)
{
   int flags;

   if (mca_pmix_native_component.sd > 0) {
        return OPAL_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((1, opal_pmix_base_framework.framework_output,
                         "%s pmix:usock:peer creating socket to server",
                         OPAL_NAME_PRINT(OPAL_PROC_MY_NAME)));
    
    mca_pmix_native_component.sd = socket(PF_UNIX, SOCK_STREAM, 0);

    if (mca_pmix_native_component.sd < 0) {
        opal_output(0, "%s usock_peer_create_socket: socket() failed: %s (%d)\n",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    strerror(opal_socket_errno),
                    opal_socket_errno);
        return OPAL_ERR_UNREACH;
    }

     /* setup the socket as non-blocking */
    if ((flags = fcntl(mca_pmix_native_component.sd, F_GETFL, 0)) < 0) {
        opal_output(0, "%s usock_peer_connect: fcntl(F_GETFL) failed: %s (%d)\n",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    } else {
        flags |= O_NONBLOCK;
        if(fcntl(mca_pmix_native_component.sd, F_SETFL, flags) < 0)
            opal_output(0, "%s usock_peer_connect: fcntl(F_SETFL) failed: %s (%d)\n",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), 
                        strerror(opal_socket_errno),
                        opal_socket_errno);
    }

    /* setup event callbacks */
    opal_event_set(mca_pmix_native_component.evbase,
                   &mca_pmix_native_component.recv_event,
                   mca_pmix_native_component.sd,
                   OPAL_EV_READ|OPAL_EV_PERSIST,
                   pmix_usock_recv_handler, NULL);
    opal_event_set_priority(&mca_pmix_native_component.recv_event, OPAL_EV_MSG_LO_PRI);
    mca_pmix_native_component.recv_ev_active = false;

    opal_event_set(mca_pmix_native_component.evbase,
                   &mca_pmix_native_component.send_event,
                   mca_pmix_native_component.sd,
                   OPAL_EV_WRITE|OPAL_EV_PERSIST,
                   pmix_usock_send_handler, NULL);
    opal_event_set_priority(&mca_pmix_native_component.send_event, OPAL_EV_MSG_LO_PRI);
    mca_pmix_native_component.send_ev_active = false;

    return OPAL_SUCCESS;
}


/*
 * Try connecting to a peer
 */
static void pmix_usock_try_connect(int fd, short args, void *cbdata)
{
    int rc;
    opal_socklen_t addrlen = 0;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s usock_peer_try_connect: attempting to connect to server",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    if (OPAL_SUCCESS != usock_create_socket()) {
        return;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s usock_peer_try_connect: attempting to connect to server on socket %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        mca_pmix_native_component.sd);

    addrlen = sizeof(struct sockaddr_un);
 retry_connect:
    mca_pmix_native_component.retries++;
    if (connect(mca_pmix_native_component.sd, (struct sockaddr *) &mca_pmix_native_component.address, addrlen) < 0) {
        /* non-blocking so wait for completion */
        if (opal_socket_errno == EINPROGRESS || opal_socket_errno == EWOULDBLOCK) {
            opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                "%s waiting for connect completion to server - activating send event",
                                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
            /* just ensure the send_event is active */
            if (!mca_pmix_native_component.send_ev_active) {
                opal_event_add(&mca_pmix_native_component.send_event, 0);
                mca_pmix_native_component.send_ev_active = true;
            }
            return;
        }

        /* Some kernels (Linux 2.6) will automatically software
           abort a connection that was ECONNREFUSED on the last
           attempt, without even trying to establish the
           connection.  Handle that case in a semi-rational
           way by trying twice before giving up */
        if (ECONNABORTED == opal_socket_errno) {
            if (mca_pmix_native_component.retries < mca_pmix_native_component.max_retries) {
                opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                                    "%s connection to server aborted by OS - retrying",
                                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
                goto retry_connect;
            } else {
                /* We were unsuccessful in establishing this connection, and are
                 * not likely to suddenly become successful,
                 */
                mca_pmix_native_component.state = PMIX_USOCK_FAILED;
                CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
                return;
            }
        }
    }

    /* connection succeeded */
    mca_pmix_native_component.retries = 0;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s sock_peer_try_connect: Connection across to server succeeded",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));
 
   /* setup our recv to catch the return ack call */
    if (!mca_pmix_native_component.recv_ev_active) {
        opal_event_add(&mca_pmix_native_component.recv_event, 0);
        mca_pmix_native_component.recv_ev_active = true;
    }

    /* send our globally unique process identifier to the server */
    if (OPAL_SUCCESS == (rc = usock_send_connect_ack())) {
        mca_pmix_native_component.state = PMIX_USOCK_CONNECT_ACK;
    } else {
        opal_output(0, 
                    "%s usock_peer_try_connect: "
                    "usock_send_connect_ack to server failed: %s (%d)",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    opal_strerror(rc), rc);
        mca_pmix_native_component.state = PMIX_USOCK_FAILED;
        CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
        return;
    }
}

int usock_send_connect_ack(void)
{
    char *msg;
    pmix_usock_hdr_t hdr;
    int rc;
    size_t sdsize;
    char *cred;
    size_t credsize;
    
    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s SEND CONNECT ACK",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME));

    /* setup the header */
    hdr.id = OPAL_PROC_MY_NAME;
    hdr.tag = UINT32_MAX;
    hdr.type = PMIX_USOCK_IDENT;

    /* get our security credential */
    if (OPAL_SUCCESS != (rc = opal_sec.get_my_credential(NULL, opal_dstore_internal, &OPAL_PROC_MY_NAME, &cred, &credsize))) {
        return rc;
    }

    /* set the number of bytes to be read beyond the header */
    hdr.nbytes = strlen(opal_version_string) + 1 + credsize;

    /* create a space for our message */
    sdsize = (sizeof(hdr) + strlen(opal_version_string) + 1 + credsize);
    if (NULL == (msg = (char*)malloc(sdsize))) {
        return OPAL_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the message */
    memcpy(msg, &hdr, sizeof(hdr));
    memcpy(msg+sizeof(hdr), opal_version_string, strlen(opal_version_string));
    memcpy(msg+sizeof(hdr)+strlen(opal_version_string)+1, cred, credsize);
    if (NULL != cred) {
        free(cred);
    }

    if (OPAL_SUCCESS != usock_send_blocking(msg, sdsize)) {
        free(msg);
        return OPAL_ERR_UNREACH;
    }
    free(msg);
    return OPAL_SUCCESS;
}

/*
 * A blocking send on a non-blocking socket. Used to send the small amount of connection
 * information that identifies the peers endpoint.
 */
static int usock_send_blocking(char *ptr, size_t size)
{
    size_t cnt = 0;
    int retval;

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s send blocking of %"PRIsize_t" bytes to socket %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        size, mca_pmix_native_component.sd);

    while (cnt < size) {
        retval = send(mca_pmix_native_component.sd, (char*)ptr+cnt, size-cnt, 0);
        if (retval < 0) {
            if (opal_socket_errno != EINTR && opal_socket_errno != EAGAIN && opal_socket_errno != EWOULDBLOCK) {
                opal_output(0, "%s usock_peer_send_blocking: send() to socket %d failed: %s (%d)\n",
                            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                            mca_pmix_native_component.sd,
                            strerror(opal_socket_errno),
                            opal_socket_errno);
                mca_pmix_native_component.state = PMIX_USOCK_FAILED;
                CLOSE_THE_SOCKET(mca_pmix_native_component.sd);
                return OPAL_ERR_UNREACH;
            }
            continue;
        }
        cnt += retval;
    }

    opal_output_verbose(2, opal_pmix_base_framework.framework_output,
                        "%s blocking send complete to socket %d",
                        OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                        mca_pmix_native_component.sd);

    return OPAL_SUCCESS;
}

/*
 * Routine for debugging to print the connection state and socket options
 */
void pmix_usock_dump(const char* msg)
{
    char buff[255];
    int nodelay,flags;

    if ((flags = fcntl(mca_pmix_native_component.sd, F_GETFL, 0)) < 0) {
        opal_output(0, "%s usock_peer_dump: fcntl(F_GETFL) failed: %s (%d)\n",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
                                                                                                            
#if defined(USOCK_NODELAY)
    optlen = sizeof(nodelay);
    if (getsockopt(mca_pmix_native_component.sd, IPPROTO_USOCK, USOCK_NODELAY, (char *)&nodelay, &optlen) < 0) {
        opal_output(0, "%s usock_peer_dump: USOCK_NODELAY option: %s (%d)\n",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME), 
                    strerror(opal_socket_errno),
                    opal_socket_errno);
    }
#else
    nodelay = 0;
#endif

    snprintf(buff, sizeof(buff), "%s %s: nodelay %d flags %08x\n",
             OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
             msg, nodelay, flags);
    opal_output(0, "%s", buff);
}

char* pmix_usock_state_print(pmix_usock_state_t state)
{
    switch (state) {
    case PMIX_USOCK_UNCONNECTED:
        return "UNCONNECTED";
    case PMIX_USOCK_CLOSED:
        return "CLOSED";
    case PMIX_USOCK_RESOLVE:
        return "RESOLVE";
    case PMIX_USOCK_CONNECTING:
        return "CONNECTING";
    case PMIX_USOCK_CONNECT_ACK:
        return "ACK";
    case PMIX_USOCK_CONNECTED:
        return "CONNECTED";
    case PMIX_USOCK_FAILED:
        return "FAILED";
    default:
        return "UNKNOWN";
    }
}

