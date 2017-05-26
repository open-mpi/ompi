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
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2011-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include <src/include/pmix_config.h>
#include "pmix_common.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/include/pmix_socket_errno.h"
#include "src/mca/psec/base/base.h"

#include "src/mca/ptl/base/base.h"
#include "ptl_usock.h"

static pmix_status_t init(void);
static void finalize(void);
static pmix_status_t connect_to_peer(struct pmix_peer_t *peer,
                                     pmix_info_t *info, size_t ninfo);
static pmix_status_t send_recv(struct pmix_peer_t *peer,
                               pmix_buffer_t *bfr,
                               pmix_ptl_cbfunc_t cbfunc,
                               void *cbdata);
static pmix_status_t send_oneway(struct pmix_peer_t *peer,
                                 pmix_buffer_t *bfr,
                                 pmix_ptl_tag_t tag);

pmix_ptl_module_t pmix_ptl_usock_module = {
    .init = init,
    .finalize = finalize,
    .send_recv = send_recv,
    .send = send_oneway,
    .connect_to_peer = connect_to_peer
};

static pmix_status_t recv_connect_ack(int sd);
static pmix_status_t send_connect_ack(int sd);

static pmix_status_t init(void)
{
    return PMIX_SUCCESS;
}

static void finalize(void)
{
}

static pmix_status_t connect_to_peer(struct pmix_peer_t *peer,
                                     pmix_info_t *info, size_t ninfo)
{
    struct sockaddr_un *address;
    char *evar, **uri;
    pmix_status_t rc;
    int sd;
    pmix_socklen_t len;

    /* if we are not a client, there is nothing we can do */
    if (!PMIX_PROC_IS_CLIENT) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* if we don't have a path to the daemon rendezvous point,
     * then we need to return an error */
    if (NULL == (evar = getenv("PMIX_SERVER_URI"))) {
        /* let the caller know that the server isn't available */
        return PMIX_ERR_SERVER_NOT_AVAIL;
    }
    uri = pmix_argv_split(evar, ':');
    if (3 != pmix_argv_count(uri)) {
        pmix_argv_free(uri);
        return PMIX_ERROR;
    }

    /* set the server nspace */
    pmix_client_globals.myserver.info = PMIX_NEW(pmix_rank_info_t);
    pmix_client_globals.myserver.nptr = PMIX_NEW(pmix_nspace_t);
    pmix_client_globals.myserver.nptr->nspace = strdup(uri[0]);
    pmix_client_globals.myserver.info->pname.nspace = strdup(uri[0]);

    /* set the server rank */
    pmix_client_globals.myserver.info->pname.rank = strtoull(uri[1], NULL, 10);

    /* setup the path to the daemon rendezvous point */
    memset(&mca_ptl_usock_component.connection, 0, sizeof(struct sockaddr_storage));
    address = (struct sockaddr_un*)&mca_ptl_usock_component.connection;
    address->sun_family = AF_UNIX;
    snprintf(address->sun_path, sizeof(address->sun_path)-1, "%s", uri[2]);
    /* if the rendezvous file doesn't exist, that's an error */
    if (0 != access(uri[2], R_OK)) {
        pmix_argv_free(uri);
        return PMIX_ERR_NOT_FOUND;
    }
    pmix_argv_free(uri);

    /* establish the connection */
    len = sizeof(struct sockaddr_un);
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_connect(&mca_ptl_usock_component.connection, len, &sd))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    pmix_client_globals.myserver.sd = sd;

    /* send our identity and any authentication credentials to the server */
    if (PMIX_SUCCESS != (rc = send_connect_ack(sd))) {
        CLOSE_THE_SOCKET(sd);
        return rc;
    }

    /* do whatever handshake is required */
    if (PMIX_SUCCESS != (rc = recv_connect_ack(sd))) {
        CLOSE_THE_SOCKET(sd);
        return rc;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sock_peer_try_connect: Connection across to server succeeded");

    /* mark the connection as made */
    pmix_globals.connected = true;

    pmix_ptl_base_set_nonblocking(sd);

    /* setup recv event */
    pmix_event_assign(&pmix_client_globals.myserver.recv_event,
                      pmix_globals.evbase,
                      pmix_client_globals.myserver.sd,
                      EV_READ | EV_PERSIST,
                      pmix_ptl_base_recv_handler, &pmix_client_globals.myserver);
    pmix_event_add(&pmix_client_globals.myserver.recv_event, 0);
    pmix_client_globals.myserver.recv_ev_active = true;

    /* setup send event */
    pmix_event_assign(&pmix_client_globals.myserver.send_event,
                      pmix_globals.evbase,
                      pmix_client_globals.myserver.sd,
                      EV_WRITE|EV_PERSIST,
                      pmix_ptl_base_send_handler, &pmix_client_globals.myserver);
    pmix_client_globals.myserver.send_ev_active = false;

    return PMIX_SUCCESS;
}

static pmix_status_t send_recv(struct pmix_peer_t *peer,
                               pmix_buffer_t *bfr,
                               pmix_ptl_cbfunc_t cbfunc,
                               void *cbdata)
{
    pmix_ptl_sr_t *ms;

    pmix_output_verbose(5, pmix_globals.debug_output,
                        "[%s:%d] post send to server",
                        __FILE__, __LINE__);

    ms = PMIX_NEW(pmix_ptl_sr_t);
    ms->peer = peer;
    ms->bfr = bfr;
    ms->cbfunc = cbfunc;
    ms->cbdata = cbdata;
    pmix_event_assign(&ms->ev, pmix_globals.evbase, -1,
                      EV_WRITE, pmix_ptl_base_send_recv, ms);
    pmix_event_active(&ms->ev, EV_WRITE, 1);
    return PMIX_SUCCESS;
}

static pmix_status_t send_oneway(struct pmix_peer_t *peer,
                                 pmix_buffer_t *bfr,
                                 pmix_ptl_tag_t tag)
{
    pmix_ptl_queue_t *q;
    pmix_peer_t *pr = (pmix_peer_t*)peer;

    /* we have to transfer this to an event for thread
     * safety as we need to post this message on the
     * peer's send queue */
    q = PMIX_NEW(pmix_ptl_queue_t);
    PMIX_RETAIN(pr);
    q->peer = peer;
    q->buf = bfr;
    q->tag = tag;
    pmix_event_assign(&q->ev, pmix_globals.evbase, -1,
                      EV_WRITE, pmix_ptl_base_send, q);
    pmix_event_active(&q->ev, EV_WRITE, 1);

    return PMIX_SUCCESS;
}

static pmix_status_t send_connect_ack(int sd)
{
    char *msg;
    pmix_usock_hdr_t hdr;
    size_t sdsize=0, csize=0, len;
    char *cred = NULL;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: SEND CONNECT ACK");

    /* setup the header */
    memset(&hdr, 0, sizeof(pmix_usock_hdr_t));
    hdr.pindex = -1;
    hdr.tag = UINT32_MAX;

    /* reserve space for the nspace and rank info */
    sdsize = strlen(pmix_globals.myid.nspace) + 1 + sizeof(int);

    /* get a credential, if the security system provides one. Not
     * every SPC will do so, thus we must first check */
    PMIX_PSEC_CREATE_CRED(rc, &pmix_client_globals.myserver,
                          PMIX_PROTOCOL_V1, &cred, &len);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* set the number of bytes to be read beyond the header */
    hdr.nbytes = sdsize + strlen(PMIX_VERSION) + 1 + len;  // must NULL terminate the VERSION string!

    /* create a space for our message */
    sdsize = (sizeof(hdr) + hdr.nbytes);
    if (NULL == (msg = (char*)malloc(sdsize))) {
        if (NULL != cred) {
            free(cred);
        }
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    memset(msg, 0, sdsize);

    /* load the message */
    csize=0;
    memcpy(msg, &hdr, sizeof(pmix_usock_hdr_t));
    csize += sizeof(pmix_usock_hdr_t);
    memcpy(msg+csize, pmix_globals.myid.nspace, strlen(pmix_globals.myid.nspace));
    csize += strlen(pmix_globals.myid.nspace)+1;
    memcpy(msg+csize, &pmix_globals.myid.rank, sizeof(int));
    csize += sizeof(int);
    memcpy(msg+csize, PMIX_VERSION, strlen(PMIX_VERSION));
    csize += strlen(PMIX_VERSION)+1;
    if (NULL != cred) {
        memcpy(msg+csize, cred, strlen(cred));  // leaves last position in msg set to NULL
    }

    if (PMIX_SUCCESS != pmix_ptl_base_send_blocking(sd, msg, sdsize)) {
        free(msg);
        if (NULL != cred) {
            free(cred);
        }
        return PMIX_ERR_UNREACH;
    }
    free(msg);
    if (NULL != cred) {
        free(cred);
    }
    return PMIX_SUCCESS;
}

/* we receive a connection acknowledgement from the server,
 * consisting of nothing more than a status report. If success,
 * then we initiate authentication method */
static pmix_status_t recv_connect_ack(int sd)
{
    pmix_status_t reply;
    pmix_status_t rc;
    struct timeval tv, save;
    pmix_socklen_t sz;
    bool sockopt = true;
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: RECV CONNECT ACK FROM SERVER");

    /* get the current timeout value so we can reset to it */
    sz = sizeof(save);
    if (0 != getsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, (void*)&save, &sz)) {
        if (ENOPROTOOPT == errno) {
            sockopt = false;
        } else {
             return PMIX_ERR_UNREACH;
        }
    } else {
        /* set a timeout on the blocking recv so we don't hang */
        tv.tv_sec  = 2;
        tv.tv_usec = 0;
        if (0 != setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv))) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix: recv_connect_ack could not setsockopt SO_RCVTIMEO");
            return PMIX_ERR_UNREACH;
        }
    }

    /* receive the status reply */
    rc = pmix_ptl_base_recv_blocking(sd, (char*)&reply, sizeof(int));
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* see if they want us to do the handshake */
    if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
        PMIX_PSEC_CLIENT_HANDSHAKE(rc, &pmix_client_globals.myserver, sd);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
    } else if (PMIX_SUCCESS != reply) {
        return reply;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: RECV CONNECT CONFIRMATION");

    /* receive our index into the server's client array */
    rc = pmix_ptl_base_recv_blocking(sd, (char*)&pmix_globals.pindex, sizeof(int));
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    if (sockopt) {
        /* return the socket to normal */
        if (0 != setsockopt(sd, SOL_SOCKET, SO_RCVTIMEO, &save, sz)) {
            return PMIX_ERR_UNREACH;
        }
    }

    return PMIX_SUCCESS;
}
