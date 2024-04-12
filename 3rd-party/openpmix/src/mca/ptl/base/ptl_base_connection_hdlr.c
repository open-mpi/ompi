/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"
#include "include/pmix_stdint.h"

#include <stdio.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif

#include "include/pmix_socket_errno.h"
#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_getid.h"
#include "src/util/pmix_strnlen.h"

#include "src/mca/ptl/base/base.h"
#include "src/mca/ptl/base/ptl_base_handshake.h"

static void process_cbfunc(int sd, short args, void *cbdata);
static void cnct_cbfunc(pmix_status_t status, pmix_proc_t *proc, void *cbdata);
static void _check_cached_events(pmix_peer_t *peer);
static pmix_status_t process_tool_request(pmix_pending_connection_t *pnd, char *mg, size_t cnt);

void pmix_ptl_base_connection_handler(int sd, short args, void *cbdata)
{
    pmix_pending_connection_t *pnd = (pmix_pending_connection_t *) cbdata;
    pmix_ptl_hdr_t hdr;
    pmix_peer_t *peer = NULL;
    pmix_status_t rc, reply;
    char *msg = NULL, *mg, *p, *blob = NULL;
    uint32_t u32;
    size_t cnt;
    size_t len = 0;
    pmix_namespace_t *nptr, *tmp;
    pmix_rank_info_t *info = NULL, *iptr;
    pmix_proc_t proc;
    pmix_info_t ginfo;
    pmix_byte_object_t cred;
    uint8_t major, minor, release;

    /* acquire the object */
    PMIX_ACQUIRE_OBJECT(pnd);

    // must use sd, args to avoid -Werror
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "ptl:base:connection_handler: new connection: %d", pnd->sd);

    /* ensure the socket is in blocking mode */
    pmix_ptl_base_set_blocking(pnd->sd);

    /* ensure all is zero'd */
    memset(&hdr, 0, sizeof(pmix_ptl_hdr_t));

    /* get the header */
    rc = pmix_ptl_base_recv_blocking(pnd->sd, (char *) &hdr, sizeof(pmix_ptl_hdr_t));
    if (PMIX_SUCCESS != rc) {
        goto error;
    }

    /* get the id, authentication and version payload (and possibly
     * security credential) - to guard against potential attacks,
     * we'll set an arbitrary limit per a define */
    if (PMIX_MAX_CRED_SIZE < hdr.nbytes) {
        goto error;
    }
    if (NULL == (msg = (char *) malloc(hdr.nbytes+1))) {
        goto error;
    }
    memset(msg, 0, hdr.nbytes + 1);  // ensure NULL termination of result
    if (PMIX_SUCCESS != pmix_ptl_base_recv_blocking(pnd->sd, msg, hdr.nbytes)) {
        /* unable to complete the recv */
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:tool:connection_handler unable to complete recv of connect-ack "
                            "with client ON SOCKET %d",
                            pnd->sd);
        goto error;
    }

    cnt = hdr.nbytes;
    mg = msg;
    /* extract the name of the sec module they used */
    PMIX_PTL_GET_STRING(pnd->psec);

    /* extract any credential so we can validate this connection
     * before doing anything else */
    PMIX_PTL_GET_U32(pnd->len);

    /* if a credential is present, then create space and
     * extract it for processing */
    PMIX_PTL_GET_BLOB(pnd->cred, pnd->len);

    /* get the process type of the connecting peer */
    PMIX_PTL_GET_U8(pnd->flag);

    switch (pnd->flag) {
    case PMIX_SIMPLE_CLIENT:
        /* simple client process */
        PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_CLIENT);
        /* get their identifier */
        PMIX_PTL_GET_PROCID(pnd->proc);
        break;

    case PMIX_LEGACY_TOOL:
        /* legacy tool - may or may not have an identifier */
        PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_TOOL);
        /* get their uid/gid */
        PMIX_PTL_GET_U32(pnd->uid);
        PMIX_PTL_GET_U32(pnd->gid);
        break;

    case PMIX_LEGACY_LAUNCHER:
        /* legacy launcher - may or may not have an identifier */
        PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_LAUNCHER);
        /* get their uid/gid */
        PMIX_PTL_GET_U32(pnd->uid);
        PMIX_PTL_GET_U32(pnd->gid);
        break;

    case PMIX_TOOL_NEEDS_ID:
    case PMIX_LAUNCHER_NEEDS_ID:
        /* self-started tool/launcher process that needs an identifier */
        if (PMIX_TOOL_NEEDS_ID == pnd->flag) {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_TOOL);
        } else {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_LAUNCHER);
        }
        /* get their uid/gid */
        PMIX_PTL_GET_U32(pnd->uid);
        PMIX_PTL_GET_U32(pnd->gid);
        /* they need an id */
        pnd->need_id = true;
        break;

    case PMIX_TOOL_GIVEN_ID:
    case PMIX_LAUNCHER_GIVEN_ID:
    case PMIX_SINGLETON_CLIENT:
    case PMIX_SCHEDULER_WITH_ID:
        /* self-started tool/launcher process that was given an identifier by caller */
        if (PMIX_TOOL_GIVEN_ID == pnd->flag) {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_TOOL);
        } else if (PMIX_LAUNCHER_GIVEN_ID == pnd->flag) {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_LAUNCHER);
        } else if (PMIX_SCHEDULER_WITH_ID == pnd->flag) {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_SCHEDULER);
        } else {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_CLIENT);
        }
        /* get their uid/gid */
        PMIX_PTL_GET_U32(pnd->uid);
        PMIX_PTL_GET_U32(pnd->gid);
        /* get their identifier */
        PMIX_PTL_GET_PROCID(pnd->proc);
        break;

    case PMIX_TOOL_CLIENT:
    case PMIX_LAUNCHER_CLIENT:
        /* tool/launcher that was started by a PMIx server - identifier specified by server */
        if (PMIX_TOOL_CLIENT == pnd->flag) {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_TOOL);
        } else {
            PMIX_SET_PROC_TYPE(&pnd->proc_type, PMIX_PROC_LAUNCHER);
        }
        /* get their uid/gid */
        PMIX_PTL_GET_U32(pnd->uid);
        PMIX_PTL_GET_U32(pnd->gid);
        /* get their identifier */
        PMIX_PTL_GET_PROCID(pnd->proc);
        break;

    default:
        /* we don't know what they are! */
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        goto error;
    }

    /* extract their VERSION */
    PMIX_PTL_GET_STRING(pnd->version);
    major = strtoul(pnd->version, &p, 10);
    ++p;
    minor = strtoul(p, &p, 10);
    ++p;
    release = strtoul(p, NULL, 10);
    PMIX_SET_PROC_MAJOR(&pnd->proc_type, major);
    PMIX_SET_PROC_MINOR(&pnd->proc_type, minor);
    PMIX_SET_PROC_RELEASE(&pnd->proc_type, release);

    if (2 == major && 0 == minor) {
        /* the 2.0 release handshake ends with the version string */
        pnd->bfrops = strdup("v20");
        pnd->buffer_type = pmix_bfrops_globals.default_type; // we can't know any better
        pnd->gds = strdup("ds12,hash");
        cnt = 0;
    } else {
        /* extract the name of the bfrops module they used */
        PMIX_PTL_GET_STRING(pnd->bfrops);

        /* extract the type of buffer they used */
        PMIX_PTL_GET_U8(pnd->buffer_type);

        /* extract the name of the gds module they used */
        PMIX_PTL_GET_STRING(pnd->gds);

        /* extract the blob */
        if (0 < cnt) {
            len = cnt;
            PMIX_PTL_GET_BLOB(blob, len);
        }
    }

    /* see if this is a tool connection request */
    if (PMIX_SIMPLE_CLIENT != pnd->flag) {
        /* nope, it's for a tool, so process it
         * separately - it is a 2-step procedure */
        rc = process_tool_request(pnd, blob, len);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
        if (NULL != blob) {
            free(blob);
            blob = NULL;
        }
        free(msg);
        return;
    }

    /* it is a client that is connecting, so it should have
     * been registered with us prior to being started.
     * See if we know this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH (tmp, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(tmp->nspace, pnd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        /* we don't know this namespace, reject it */
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }

    /* likewise, we should have this peer in our list */
    info = NULL;
    PMIX_LIST_FOREACH (iptr, &nptr->ranks, pmix_rank_info_t) {
        if (iptr->pname.rank == pnd->proc.rank) {
            info = iptr;
            break;
        }
    }
    if (NULL == info) {
        /* rank unknown, reject it */
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }

    /* save the version in the namespace object */
    if (0 == nptr->version.major) {
        nptr->version.major = pnd->proc_type.major;
        nptr->version.minor = pnd->proc_type.minor;
        nptr->version.release = pnd->proc_type.release;
    }

    /* a peer can connect on multiple sockets since it can fork/exec
     * a child that also calls PMIX_Init, so add it here if necessary.
     * Create the tracker for this peer */
    peer = PMIX_NEW(pmix_peer_t);
    if (NULL == peer) {
        goto error;
    }
    /* mark that this peer is a client of the given type */
    memcpy(&peer->proc_type, &pnd->proc_type, sizeof(pmix_proc_type_t));
    /* save the protocol */
    peer->protocol = pnd->protocol;
    /* add in the nspace pointer */
    PMIX_RETAIN(nptr);
    peer->nptr = nptr;
    PMIX_RETAIN(info);
    peer->info = info;
    /* update the epilog fields */
    peer->epilog.uid = info->uid;
    peer->epilog.gid = info->gid;
    /* ensure the nspace epilog is updated too */
    nptr->epilog.uid = info->uid;
    nptr->epilog.gid = info->gid;
    info->proc_cnt++; /* increase number of processes on this rank */
    peer->sd = pnd->sd;
    if (0 > (peer->index = pmix_pointer_array_add(&pmix_server_globals.clients, peer))) {
        goto error;
    }
    info->peerid = peer->index;

    /* set the sec module to match this peer */
    peer->nptr->compat.psec = pmix_psec_base_assign_module(pnd->psec);
    if (NULL == peer->nptr->compat.psec) {
        goto error;
    }

    /* set the bfrops module to match this peer */
    peer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(pnd->bfrops);
    if (NULL == peer->nptr->compat.bfrops) {
        goto error;
    }
    /* and the buffer type to match */
    peer->nptr->compat.type = pnd->buffer_type;

    /* set the gds module to match this peer */
    if (NULL != pnd->gds) {
        PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, pnd->gds, PMIX_STRING);
        peer->nptr->compat.gds = pmix_gds_base_assign_module(&ginfo, 1);
        PMIX_INFO_DESTRUCT(&ginfo);
    } else {
        peer->nptr->compat.gds = pmix_gds_base_assign_module(NULL, 0);
    }
    if (NULL == peer->nptr->compat.gds) {
        goto error;
    }

    /* if we haven't previously stored the version for this
     * nspace, do so now */
    if (!nptr->version_stored) {
        PMIX_INFO_LOAD(&ginfo, PMIX_BFROPS_MODULE, pnd->version, PMIX_STRING);
        PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, peer->nptr, &ginfo, 1);
        PMIX_INFO_DESTRUCT(&ginfo);
        nptr->version_stored = true;
    }

    free(msg); // can now release the data buffer
    msg = NULL;

    /* validate the connection */
    cred.bytes = pnd->cred;
    cred.size = pnd->len;
    PMIX_PSEC_VALIDATE_CONNECTION(reply, peer, NULL, 0, NULL, NULL, &cred);
    if (PMIX_SUCCESS != reply) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "validation of client connection failed");
        goto error;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output, "client connection validated");

    /* tell the client all is good */
    u32 = htonl(reply);
    if (PMIX_SUCCESS
        != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char *) &u32, sizeof(uint32_t)))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    /* If needed, perform the handshake. The macro will update reply */
    PMIX_PSEC_SERVER_HANDSHAKE_IFNEED(reply, peer, NULL, 0, NULL, NULL, &cred);

    /* It is possible that connection validation failed */
    if (PMIX_SUCCESS != reply) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "validation of client connection failed");
        goto error;
    }

    /* send the client's array index */
    u32 = htonl(peer->index);
    if (PMIX_SUCCESS
        != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char *) &u32, sizeof(uint32_t)))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "connect-ack from client completed");

    /* let the host server know that this client has connected */
    if (NULL != pmix_host_server.client_connected2) {
        PMIX_LOAD_PROCID(&proc, peer->info->pname.nspace, peer->info->pname.rank);
        rc = pmix_host_server.client_connected2(&proc, peer->info->server_object, NULL, 0, NULL,
                                                NULL);
        if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
            PMIX_ERROR_LOG(rc);
        }
    } else if (NULL != pmix_host_server.client_connected) {
        PMIX_LOAD_PROCID(&proc, peer->info->pname.nspace, peer->info->pname.rank);
        rc = pmix_host_server.client_connected(&proc, peer->info->server_object, NULL, NULL);
        if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    pmix_ptl_base_set_nonblocking(pnd->sd);

    /* start the events for this client */
    pmix_event_assign(&peer->recv_event, pmix_globals.evbase, pnd->sd, EV_READ | EV_PERSIST,
                      pmix_ptl_base_recv_handler, peer);
    pmix_event_add(&peer->recv_event, NULL);
    peer->recv_ev_active = true;
    pmix_event_assign(&peer->send_event, pmix_globals.evbase, pnd->sd, EV_WRITE | EV_PERSIST,
                      pmix_ptl_base_send_handler, peer);
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:server client %s:%u has connected on socket %d",
                        peer->info->pname.nspace, peer->info->pname.rank, peer->sd);
    PMIX_RELEASE(pnd);

    /* check the cached events and update the client */
    _check_cached_events(peer);
    if (NULL != blob) {
        free(blob);
        blob = NULL;
    }

    return;

error:
    if (NULL != info) {
        info->proc_cnt--;
        PMIX_RELEASE(info);
    }
    if (NULL != msg) {
        free(msg);
    }
    if (NULL != blob) {
        free(blob);
    }
    if (NULL != peer) {
        pmix_pointer_array_set_item(&pmix_server_globals.clients, peer->index, NULL);
        PMIX_RELEASE(peer);
    }
    CLOSE_THE_SOCKET(pnd->sd);
    PMIX_RELEASE(pnd);
    return;
}

/* process the host's callback with tool connection info */
static void process_cbfunc(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_pending_connection_t *pnd = (pmix_pending_connection_t *) cd->cbdata;
    pmix_namespace_t *nptr;
    pmix_rank_info_t *info;
    pmix_peer_t *peer;
    pmix_status_t rc, reply;
    uint32_t u32;
    pmix_info_t ginfo;
    pmix_byte_object_t cred;
    pmix_iof_req_t *req = NULL;

    /* acquire the object */
    PMIX_ACQUIRE_OBJECT(cd);
    // must use sd, args to avoid -Werror
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* shortcuts */
    peer = (pmix_peer_t *) pnd->peer;
    nptr = peer->nptr;

    /* send this status so they don't hang */
    u32 = ntohl(cd->status);
    if (PMIX_SUCCESS
        != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char *) &u32, sizeof(uint32_t)))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* if the request failed, then we are done */
    if (PMIX_SUCCESS != cd->status) {
        goto error;
    }

    /* if we got an identifier, send it back to the tool */
    if (pnd->need_id) {
        /* start with the nspace */
        if (PMIX_SUCCESS
            != (rc = pmix_ptl_base_send_blocking(pnd->sd, cd->proc.nspace, PMIX_MAX_NSLEN + 1))) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }

        /* now the rank, suitably converted */
        u32 = ntohl(cd->proc.rank);
        if (PMIX_SUCCESS
            != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char *) &u32, sizeof(uint32_t)))) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    /* send my nspace back to the tool */
    if (PMIX_SUCCESS
        != (rc = pmix_ptl_base_send_blocking(pnd->sd, pmix_globals.myid.nspace,
                                             PMIX_MAX_NSLEN + 1))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* send my rank back to the tool */
    u32 = ntohl(pmix_globals.myid.rank);
    if (PMIX_SUCCESS
        != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char *) &u32, sizeof(uint32_t)))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* if this tool wasn't initially registered as a client,
     * then add some required structures */
    if (PMIX_TOOL_CLIENT != pnd->flag && PMIX_LAUNCHER_CLIENT != pnd->flag) {
        PMIX_RETAIN(nptr);
        nptr->nspace = strdup(cd->proc.nspace);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
        info = PMIX_NEW(pmix_rank_info_t);
        info->pname.nspace = strdup(nptr->nspace);
        info->pname.rank = cd->proc.rank;
        info->uid = pnd->uid;
        info->gid = pnd->gid;
        pmix_list_append(&nptr->ranks, &info->super);
        PMIX_RETAIN(info);
        peer->info = info;
    }

    /* mark the peer proc type */
    memcpy(&peer->proc_type, &pnd->proc_type, sizeof(pmix_proc_type_t));
    /* save the protocol */
    peer->protocol = pnd->protocol;
    /* save the uid/gid */
    peer->epilog.uid = peer->info->uid;
    peer->epilog.gid = peer->info->gid;
    nptr->epilog.uid = peer->info->uid;
    nptr->epilog.gid = peer->info->gid;
    peer->proc_cnt = 1;
    peer->sd = pnd->sd;

    /* get the appropriate compatibility modules based on the
     * info provided by the tool during the initial connection request */
    peer->nptr->compat.psec = pmix_psec_base_assign_module(pnd->psec);
    if (NULL == peer->nptr->compat.psec) {
        goto error;
    }
    /* set the gds */
    PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, pnd->gds, PMIX_STRING);
    peer->nptr->compat.gds = pmix_gds_base_assign_module(&ginfo, 1);
    PMIX_INFO_DESTRUCT(&ginfo);
    if (NULL == peer->nptr->compat.gds) {
        goto error;
    }

    /* if we haven't previously stored the version for this
     * nspace, do so now */
    if (!peer->nptr->version_stored) {
        PMIX_INFO_LOAD(&ginfo, PMIX_BFROPS_MODULE, pnd->version, PMIX_STRING);
        PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, peer->nptr, &ginfo, 1);
        PMIX_INFO_DESTRUCT(&ginfo);
        nptr->version_stored = true;
    }

    /* automatically setup to forward output to the tool */
    req = PMIX_NEW(pmix_iof_req_t);
    if (NULL == req) {
        goto error;
    }
    PMIX_RETAIN(peer);
    req->requestor = peer;
    req->nprocs = 1;
    PMIX_PROC_CREATE(req->procs, req->nprocs);
    PMIX_LOAD_PROCID(&req->procs[0], pmix_globals.myid.nspace, pmix_globals.myid.rank);
    req->channels = PMIX_FWD_STDOUT_CHANNEL | PMIX_FWD_STDERR_CHANNEL | PMIX_FWD_STDDIAG_CHANNEL;
    req->remote_id = 0; // default ID for tool during init
    req->local_id = pmix_pointer_array_add(&pmix_globals.iof_requests, req);

    /* validate the connection */
    cred.bytes = pnd->cred;
    cred.size = pnd->len;
    PMIX_PSEC_VALIDATE_CONNECTION(reply, peer, NULL, 0, NULL, NULL, &cred);
    /* communicate the result to the other side */
    u32 = htonl(reply);
    if (PMIX_SUCCESS
        != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char *) &u32, sizeof(uint32_t)))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* If needed perform the handshake. The macro will update reply */
    PMIX_PSEC_SERVER_HANDSHAKE_IFNEED(reply, peer, NULL, 0, NULL, NULL, &cred);

    /* If verification wasn't successful - stop here */
    if (PMIX_SUCCESS != reply) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "validation of tool credentials failed: %s", PMIx_Error_string(rc));
        goto error;
    }

    /* set the socket non-blocking for all further operations */
    pmix_ptl_base_set_nonblocking(pnd->sd);

    if (0 > (peer->index = pmix_pointer_array_add(&pmix_server_globals.clients, peer))) {
        goto error;
    }
    peer->info->peerid = peer->index;

    /* start the events for this tool */
    pmix_event_assign(&peer->recv_event, pmix_globals.evbase, peer->sd, EV_READ | EV_PERSIST,
                      pmix_ptl_base_recv_handler, peer);
    pmix_event_add(&peer->recv_event, NULL);
    peer->recv_ev_active = true;
    pmix_event_assign(&peer->send_event, pmix_globals.evbase, peer->sd, EV_WRITE | EV_PERSIST,
                      pmix_ptl_base_send_handler, peer);
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:server tool %s:%d has connected on socket %d",
                        peer->info->pname.nspace, peer->info->pname.rank, peer->sd);

    /* check the cached events and update the tool */
    _check_cached_events(peer);
    PMIX_RELEASE(pnd);
    PMIX_RELEASE(cd);
    return;

error:
    CLOSE_THE_SOCKET(pnd->sd);
    PMIX_RELEASE(pnd);
    PMIX_RELEASE(peer);
    pmix_list_remove_item(&pmix_globals.nspaces, &nptr->super);
    PMIX_RELEASE(nptr); // will release the info object
    PMIX_RELEASE(cd);
    if (NULL != req) {
        pmix_pointer_array_set_item(&pmix_globals.iof_requests, req->local_id, NULL);
        PMIX_RELEASE(req);
    }
}

/* receive a callback from the host RM with an nspace
 * for a connecting tool */
static void cnct_cbfunc(pmix_status_t status, pmix_proc_t *proc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:tool:cnct_cbfunc returning %s:%d %s", proc->nspace, proc->rank,
                        PMIx_Error_string(status));

    /* need to thread-shift this into our context */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return;
    }
    cd->status = status;
    PMIX_LOAD_PROCID(&cd->proc, proc->nspace, proc->rank);
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, process_cbfunc);
}

static pmix_status_t process_tool_request(pmix_pending_connection_t *pnd,
                                          char *mg, size_t cnt)
{
    pmix_peer_t *peer;
    pmix_namespace_t *nptr, *tmp;
    pmix_rank_info_t *info;
    bool found;
    size_t n;
    pmix_buffer_t buf;
    pmix_status_t rc;

    peer = PMIX_NEW(pmix_peer_t);
    if (NULL == peer) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return PMIX_ERR_NOMEM;
    }
    pnd->peer = peer;
    /* if this is a tool we launched, then the host may
     * have already registered it as a client - so check
     * to see if we already have a peer for it */
    if (PMIX_TOOL_CLIENT == pnd->flag || PMIX_LAUNCHER_CLIENT == pnd->flag) {
        /* registration only adds the nspace and a rank in that
         * nspace - it doesn't add the peer object to our array
         * of local clients. So let's start by searching for
         * the nspace object */
        nptr = NULL;
        PMIX_LIST_FOREACH (tmp, &pmix_globals.nspaces, pmix_namespace_t) {
            if (0 == strcmp(tmp->nspace, pnd->proc.nspace)) {
                nptr = tmp;
                break;
            }
        }
        if (NULL == nptr) {
            /* it is possible that this is a tool inside of
             * a job-script as part of a multi-spawn operation.
             * Since each tool invocation may have finalized and
             * terminated, the tool will appear to "terminate", thus
             * causing us to cleanup all references to it, and then
             * reappear. So we don't reject this connection request.
             * Instead, we create the nspace and rank objects for
             * it and let the RM/host decide if this behavior
             * is allowed */
            nptr = PMIX_NEW(pmix_namespace_t);
            if (NULL == nptr) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return PMIX_ERR_NOMEM;
            }
            nptr->nspace = strdup(pnd->proc.nspace);
            /* save the version */
            nptr->version.major = pnd->proc_type.major;
            nptr->version.minor = pnd->proc_type.minor;
            nptr->version.release = pnd->proc_type.release;
        }
        /* now look for the rank */
        info = NULL;
        found = false;
        PMIX_LIST_FOREACH (info, &nptr->ranks, pmix_rank_info_t) {
            if (info->pname.rank == pnd->proc.rank) {
                found = true;
                break;
            }
        }
        if (!found) {
            /* see above note about not finding nspace */
            info = PMIX_NEW(pmix_rank_info_t);
            info->pname.nspace = strdup(pnd->proc.nspace);
            info->pname.rank = pnd->proc.rank;
            info->uid = pnd->uid;
            info->gid = pnd->gid;
            pmix_list_append(&nptr->ranks, &info->super);
        }
        PMIX_RETAIN(info);
        peer->info = info;
        PMIX_RETAIN(nptr);
    } else {
        nptr = PMIX_NEW(pmix_namespace_t);
        if (NULL == nptr) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            PMIX_RELEASE(peer);
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }
    }
    peer->nptr = nptr;
    /* select their bfrops compat module so we can unpack
     * any provided pmix_info_t structs */
    peer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(pnd->bfrops);
    if (NULL == peer->nptr->compat.bfrops) {
        PMIX_RELEASE(peer);
        PMIX_ERROR_LOG(PMIX_ERR_NOT_AVAILABLE);
        return PMIX_ERR_NOT_AVAILABLE;
    }
    /* set the buffer type */
    peer->nptr->compat.type = pnd->buffer_type;
    n = 0;
    /* if info structs need to be passed along, then unpack them */
    if (0 < cnt) {
        int32_t foo;
        PMIX_CONSTRUCT(&buf, pmix_buffer_t);
        PMIX_LOAD_BUFFER_NON_DESTRUCT(peer, &buf, mg, cnt); // allocates no memory
        foo = 1;
        PMIX_BFROPS_UNPACK(rc, peer, &buf, &pnd->ninfo, &foo, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(peer);
            return rc;
        }
        foo = (int32_t) pnd->ninfo;
        /* if we have an identifier, then we leave room to pass it */
        if (!pnd->need_id) {
            pnd->ninfo += 5;
        } else {
            pnd->ninfo += 3;
        }
        PMIX_INFO_CREATE(pnd->info, pnd->ninfo);
        PMIX_BFROPS_UNPACK(rc, peer, &buf, pnd->info, &foo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(peer);
            return rc;
        }
        n = foo;
    } else {
        if (!pnd->need_id) {
            pnd->ninfo = 5;
        } else {
            pnd->ninfo = 3;
        }
        PMIX_INFO_CREATE(pnd->info, pnd->ninfo);
    }

    /* does the server support tool connections? */
    if (NULL == pmix_host_server.tool_connected) {
        if (pnd->need_id) {
            /* we need someone to provide the tool with an
             * identifier and they aren't available */
            /* send an error reply to the client */
            PMIX_RELEASE(peer);
            return PMIX_ERR_NOT_SUPPORTED;
        } else {
            /* just process it locally */
            cnct_cbfunc(PMIX_SUCCESS, &pnd->proc, (void *) pnd);
            /* release the msg */
            return PMIX_SUCCESS;
        }
    }

    /* setup the info array to pass the relevant info
     * to the server */
    /* provide the version */
    PMIX_INFO_LOAD(&pnd->info[n], PMIX_VERSION_INFO, pnd->version, PMIX_STRING);
    ++n;
    /* provide the user id */
    PMIX_INFO_LOAD(&pnd->info[n], PMIX_USERID, &pnd->uid, PMIX_UINT32);
    ++n;
    /* and the group id */
    PMIX_INFO_LOAD(&pnd->info[n], PMIX_GRPID, &pnd->gid, PMIX_UINT32);
    ++n;
    /* if we have it, pass along their ID */
    if (!pnd->need_id) {
        PMIX_INFO_LOAD(&pnd->info[n], PMIX_NSPACE, pnd->proc.nspace, PMIX_STRING);
        ++n;
        PMIX_INFO_LOAD(&pnd->info[n], PMIX_RANK, &pnd->proc.rank, PMIX_PROC_RANK);
        ++n;
    }

    /* pass it up for processing */
    pmix_host_server.tool_connected(pnd->info, pnd->ninfo, cnct_cbfunc, pnd);
    return PMIX_SUCCESS;
}

static void _check_cached_events(pmix_peer_t *peer)
{
    pmix_notify_caddy_t *cd;
    int i;
    size_t n;
    pmix_range_trkr_t rngtrk;
    pmix_buffer_t *relay;
    pmix_proc_t proc;
    pmix_status_t ret;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;
    bool matched, found;

    PMIX_LOAD_PROCID(&proc, peer->info->pname.nspace, peer->info->pname.rank);

    for (i = 0; i < pmix_globals.max_events; i++) {
        pmix_hotel_knock(&pmix_globals.notifications, i, (void **) &cd);
        if (NULL == cd) {
            continue;
        }
        /* check the range */
        if (NULL == cd->targets) {
            rngtrk.procs = &cd->source;
            rngtrk.nprocs = 1;
        } else {
            rngtrk.procs = cd->targets;
            rngtrk.nprocs = cd->ntargets;
        }
        rngtrk.range = cd->range;
        if (!pmix_notify_check_range(&rngtrk, &proc)) {
            continue;
        }
        found = false;
        /* if we were given specific targets, check if this is one */
        if (NULL != cd->targets) {
            matched = false;
            for (n = 0; n < cd->ntargets; n++) {
                if (PMIX_CHECK_PROCID(&proc, &cd->targets[n])) {
                    matched = true;
                    /* track the number of targets we have left to notify */
                    --cd->nleft;
                    /* if this is the last one, then evict this event
                     * from the cache */
                    if (0 == cd->nleft) {
                        pmix_hotel_checkout(&pmix_globals.notifications, cd->room);
                        found = true; // mark that we should release cd
                    }
                    break;
                }
            }
            if (!matched) {
                /* do not notify this one */
                continue;
            }
        }

        /* all matches - notify */
        relay = PMIX_NEW(pmix_buffer_t);
        if (NULL == relay) {
            /* nothing we can do */
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            break;
        }
        /* pack the info data stored in the event */
        PMIX_BFROPS_PACK(ret, peer, relay, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(relay);
            break;
        }
        PMIX_BFROPS_PACK(ret, peer, relay, &cd->status, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(relay);
            break;
        }
        PMIX_BFROPS_PACK(ret, peer, relay, &cd->source, 1, PMIX_PROC);
        if (PMIX_SUCCESS != ret) {
            PMIX_RELEASE(relay);
            PMIX_ERROR_LOG(ret);
            break;
        }
        PMIX_BFROPS_PACK(ret, peer, relay, &cd->ninfo, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(relay);
            break;
        }
        if (0 < cd->ninfo) {
            PMIX_BFROPS_PACK(ret, peer, relay, cd->info, cd->ninfo, PMIX_INFO);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PMIX_RELEASE(relay);
                break;
            }
        }
        PMIX_SERVER_QUEUE_REPLY(ret, peer, 0, relay);
        if (PMIX_SUCCESS != ret) {
            PMIX_RELEASE(relay);
        }
        if (found) {
            PMIX_RELEASE(cd);
        }
    }
}
