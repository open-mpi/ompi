/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennptlee and The University
 *                         of Tennptlee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2015      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2016-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include <src/include/pmix_config.h>
#include <pmix_common.h>

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
#include <errno.h>

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/fd.h"
#include "src/util/show_help.h"
#include "src/util/strnlen.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/psec/base/base.h"
#include "src/server/pmix_server_ops.h"

#include "src/mca/ptl/base/base.h"
#include "src/mca/ptl/usock/ptl_usock.h"

static pmix_status_t component_open(void);
static pmix_status_t component_close(void);
static int component_query(pmix_mca_base_module_t **module, int *priority);
static pmix_status_t setup_listener(pmix_info_t info[], size_t ninfo,
                                    bool *need_listener);

/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
PMIX_EXPORT pmix_ptl_usock_component_t mca_ptl_usock_component = {
    .super = {
        .base = {
            PMIX_PTL_BASE_VERSION_1_0_0,

            /* Component name and version */
            .pmix_mca_component_name = "usock",
            PMIX_MCA_BASE_MAKE_VERSION(component,
                                       PMIX_MAJOR_VERSION,
                                       PMIX_MINOR_VERSION,
                                       PMIX_RELEASE_VERSION),

            /* Component open and close functions */
            .pmix_mca_open_component = component_open,
            .pmix_mca_close_component = component_close,
            .pmix_mca_query_component = component_query
        },
        .priority = 15,
        .uri = NULL,
        .setup_listener = setup_listener
    },
    .tmpdir = NULL,
    .filename = NULL
};

static void connection_handler(int sd, short args, void *cbdata);
static void listener_cb(int incoming_sd, void *cbdata);
static char *sec_mode = NULL;

pmix_status_t component_open(void)
{
    char *tdir;

    memset(&mca_ptl_usock_component.connection, 0, sizeof(mca_ptl_usock_component.connection));

    /* check for environ-based directives
     * on system tmpdir to use */
    if (NULL == (tdir = getenv("PMIX_SYSTEM_TMPDIR"))) {
        if (NULL == (tdir = getenv("TMPDIR"))) {
            if (NULL == (tdir = getenv("TEMP"))) {
                if (NULL == (tdir = getenv("TMP"))) {
                    tdir = "/tmp";
                }
            }
        }
    }
    if (NULL != tdir) {
        mca_ptl_usock_component.tmpdir = strdup(tdir);
    }

    return PMIX_SUCCESS;
}


pmix_status_t component_close(void)
{
    if (NULL != sec_mode) {
        free(sec_mode);
    }
    if (NULL != mca_ptl_usock_component.tmpdir) {
        free(mca_ptl_usock_component.tmpdir);
    }
    if (NULL != mca_ptl_usock_component.super.uri) {
        free(mca_ptl_usock_component.super.uri);
    }
    if (NULL != mca_ptl_usock_component.filename) {
        /* remove the file */
        unlink(mca_ptl_usock_component.filename);
        free(mca_ptl_usock_component.filename);
    }

    return PMIX_SUCCESS;
}

static int component_query(pmix_mca_base_module_t **module, int *priority)
{
    *module = (pmix_mca_base_module_t*)&pmix_ptl_usock_module;
    return PMIX_SUCCESS;
}

/* if we are the server, then we need to setup a usock rendezvous
 * point for legacy releases, but only do so if requested as some
 * systems may not wish to support older releases. The system can,
 * of course, simply use the MCA param method to disable this
 * component (PMIX_MCA_ptl=^usock), or can tell us to disqualify
 * ourselves using an info key to this API.
 *
 * NOTE: we accept MCA parameters, but info keys override them
 */
static pmix_status_t setup_listener(pmix_info_t info[], size_t ninfo,
                                    bool *need_listener)
{
    int flags;
    size_t n;
    pmix_listener_t *lt;
    pmix_status_t rc;
    socklen_t addrlen;
    struct sockaddr_un *address;
    bool disabled = false;
    char *secmods, **options, *pmix_pid;
    pid_t mypid;

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:usock setup_listener");

    /* if we are not a server, then we shouldn't be doing this */
    if (!PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* scan the info keys and process any override instructions */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_USOCK_DISABLE)) {
                disabled = PMIX_INFO_TRUE(&info[n]);;
                break;
            }
        }
    }

    /* see if we have been disabled */
    if (disabled) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "ptl:usock not available");
        return PMIX_ERR_NOT_AVAILABLE;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "ptl:usock setting up listener");

    addrlen = sizeof(struct sockaddr_un);
    address = (struct sockaddr_un*)&mca_ptl_usock_component.connection;
    address->sun_family = AF_UNIX;

    /* any client we hear from will be using v1.x protocols. This
     * means that they cannot tell us what security module they
     * are using as this wasn't included in their handshake. So
     * the best we can assume is that they are using the highest
     * priority default we have */
    secmods = pmix_psec_base_get_available_modules();
    options = pmix_argv_split(secmods, ',');
    sec_mode = strdup(options[0]);
    pmix_argv_free(options);
    free(secmods);

    /* define the listener */
    lt = PMIX_NEW(pmix_listener_t);

    /* for now, just setup the v1.1 series rendezvous point
     * we use the pid to reduce collisions */
    mypid = getpid();
    if (0 > asprintf(&pmix_pid, "%s/pmix-%d", mca_ptl_usock_component.tmpdir, mypid)) {
        PMIX_RELEASE(lt);
        return PMIX_ERR_NOMEM;
    }
    if ((strlen(pmix_pid) + 1) > sizeof(address->sun_path)-1) {
        pmix_show_help("help-pmix-server.txt", "rnd-path-too-long", true,
                       mca_ptl_usock_component.tmpdir, pmix_pid);
        free(pmix_pid);
        PMIX_RELEASE(lt);
        return PMIX_ERR_INVALID_LENGTH;
    }
    snprintf(address->sun_path, sizeof(address->sun_path)-1, "%s", pmix_pid);
    free(pmix_pid);
    /* set the URI */
    lt->varname = strdup("PMIX_SERVER_URI");
    if (0 > asprintf(&lt->uri, "%s:%lu:%s", pmix_globals.myid.nspace,
                    (unsigned long)pmix_globals.myid.rank, address->sun_path)) {
        PMIX_RELEASE(lt);
        return PMIX_ERR_NOMEM;
    }
    /* save the rendezvous filename for later removal */
    mca_ptl_usock_component.filename = strdup(address->sun_path);

    lt->protocol = PMIX_PROTOCOL_V1;
    lt->ptl = (struct pmix_ptl_module_t*)&pmix_ptl_usock_module;
    lt->cbfunc = connection_handler;
    pmix_list_append(&pmix_ptl_globals.listeners, &lt->super);

    /* create a listen socket for incoming connection attempts */
    lt->socket = socket(PF_UNIX, SOCK_STREAM, 0);
    if (lt->socket < 0) {
        printf("%s:%d socket() failed\n", __FILE__, __LINE__);
        goto sockerror;
    }
    /* Set the socket to close-on-exec so that no children inherit
     * this FD */
    if (pmix_fd_set_cloexec(lt->socket) != PMIX_SUCCESS) {
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    if (bind(lt->socket, (struct sockaddr*)address, addrlen) < 0) {
        printf("%s:%d bind() failed\n", __FILE__, __LINE__);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }
    /* chown as required */
    if (lt->owner_given) {
        if (0 != chown(address->sun_path, lt->owner, -1)) {
            pmix_output(0, "CANNOT CHOWN socket %s: %s", address->sun_path, strerror (errno));
            CLOSE_THE_SOCKET(lt->socket);
            goto sockerror;
        }
    }
    if (lt->group_given) {
        if (0 != chown(address->sun_path, -1, lt->group)) {
            pmix_output(0, "CANNOT CHOWN socket %s: %s", address->sun_path, strerror (errno));
            CLOSE_THE_SOCKET(lt->socket);
            goto sockerror;
        }
    }
    /* set the mode as required */
    if (0 != chmod(address->sun_path, lt->mode)) {
        pmix_output(0, "CANNOT CHMOD socket %s: %s", address->sun_path, strerror (errno));
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    /* setup listen backlog to maximum allowed by kernel */
    if (listen(lt->socket, SOMAXCONN) < 0) {
        printf("%s:%d listen() failed\n", __FILE__, __LINE__);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    /* set socket up to be non-blocking, otherwise accept could block */
    if ((flags = fcntl(lt->socket, F_GETFL, 0)) < 0) {
        printf("%s:%d fcntl(F_GETFL) failed\n", __FILE__, __LINE__);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }
    flags |= O_NONBLOCK;
    if (fcntl(lt->socket, F_SETFL, flags) < 0) {
        printf("%s:%d fcntl(F_SETFL) failed\n", __FILE__, __LINE__);
        CLOSE_THE_SOCKET(lt->socket);
        goto sockerror;
    }

    /* if the server will listen for us, then ask it to do so now */
    rc = PMIX_ERR_NOT_SUPPORTED;
    if (NULL != pmix_host_server.listener) {
        rc = pmix_host_server.listener(lt->socket, listener_cb, (void*)lt);
    }

    if (PMIX_SUCCESS != rc) {
        *need_listener = true;
    }

    return PMIX_SUCCESS;

  sockerror:
      pmix_list_remove_item(&pmix_ptl_globals.listeners, &lt->super);
      PMIX_RELEASE(lt);
      return PMIX_ERROR;
}

static void listener_cb(int incoming_sd, void *cbdata)
{
    pmix_pending_connection_t *pending_connection;

    /* throw it into our event library for processing */
    pmix_output_verbose(8, pmix_ptl_base_framework.framework_output,
                        "listen_cb: pushing new connection %d into evbase",
                        incoming_sd);
    pending_connection = PMIX_NEW(pmix_pending_connection_t);
    pending_connection->sd = incoming_sd;
    pmix_event_assign(&pending_connection->ev, pmix_globals.evbase, -1,
                      EV_WRITE, connection_handler, pending_connection);
    pmix_event_active(&pending_connection->ev, EV_WRITE, 1);
}

/* Parse init-ack message:
 *    NSPACE<0><rank>VERSION<0>[CRED<0>]
 */
static pmix_status_t parse_connect_ack (char *msg, unsigned int len,
                                        char **nspace, unsigned int *rank,
                                        char **version, char **cred)
{
    unsigned int msglen;

    PMIX_STRNLEN(msglen, msg, len);
    if (msglen < len) {
        *nspace = msg;
        msg += strlen(*nspace) + 1;
        len -= strlen(*nspace) + 1;
    } else {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_STRNLEN(msglen, msg, len);
    if (msglen <= len) {
        memcpy(rank, msg, sizeof(int));
        msg += sizeof(int);
        len -= sizeof(int);
    } else {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_STRNLEN(msglen, msg, len);
    if (msglen < len) {
        *version = msg;
        msg += strlen(*version) + 1;
        len -= strlen(*version) + 1;
    } else {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_STRNLEN(msglen, msg, len);
    if (msglen < len)
        *cred = msg;
    else {
        *cred = NULL;
    }

    return PMIX_SUCCESS;
}


static void connection_handler(int sd, short args, void *cbdata)
{
    pmix_pending_connection_t *pnd = (pmix_pending_connection_t*)cbdata;
    char *msg, *nspace, *version, *cred;
    pmix_status_t rc;
    unsigned int rank;
    pmix_usock_hdr_t hdr;
    pmix_nspace_t *nptr, *tmp;
    pmix_rank_info_t *info;
    pmix_peer_t *psave = NULL;
    bool found;
    pmix_proc_t proc;
    size_t len;

    /* acquire the object */
    PMIX_ACQUIRE_OBJECT(pnd);

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "USOCK CONNECTION FROM PEER ON SOCKET %d", pnd->sd);

    /* ensure all is zero'd */
    memset(&hdr, 0, sizeof(pmix_usock_hdr_t));

    /* get the header */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_recv_blocking(pnd->sd, (char*)&hdr, sizeof(pmix_usock_hdr_t)))) {
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }

    /* get the id, authentication and version payload (and possibly
     * security credential) - to guard against potential attacks,
     * we'll set an arbitrary limit per a define */
    if (PMIX_MAX_CRED_SIZE < hdr.nbytes) {
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    if (NULL == (msg = (char*)malloc(hdr.nbytes))) {
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    if (PMIX_SUCCESS != pmix_ptl_base_recv_blocking(pnd->sd, msg, hdr.nbytes)) {
        /* unable to complete the recv */
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "unable to complete recv of connect-ack with client ON SOCKET %d", pnd->sd);
        free(msg);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }

    if (PMIX_SUCCESS != (rc = parse_connect_ack(msg, hdr.nbytes, &nspace,
                                                &rank, &version, &cred))) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "error parsing connect-ack from client ON SOCKET %d", pnd->sd);
        free(msg);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "connect-ack recvd from peer %s:%d:%s on socket %d",
                        nspace, rank, version, pnd->sd);

    /* do not check the version - we only retain it at this
     * time in case we need to check it at some future date.
     * For now, our intent is to retain backward compatibility
     * and so we will assume that all versions are compatible. */

    /* see if we know this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(tmp, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        /* we don't know this namespace, reject it */
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }

    /* see if we have this peer in our list */
    info = NULL;
    found = false;
    PMIX_LIST_FOREACH(info, &nptr->ranks, pmix_rank_info_t) {
        if (info->pname.rank == rank) {
            found = true;
            break;
        }
    }
    if (!found) {
        /* rank unknown, reject it */
        free(msg);
        /* send an error reply to the client */
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }
    /* a peer can connect on multiple sockets since it can fork/exec
     * a child that also calls PMIx_Init, so add it here if necessary.
     * Create the tracker for this peer */
    psave = PMIX_NEW(pmix_peer_t);
    if (NULL == psave) {
        free(msg);
        rc = PMIX_ERR_NOMEM;
        goto error;
    }
    /* mark it as being a v1 type */
    psave->proc_type = PMIX_PROC_CLIENT | PMIX_PROC_V1;
    /* add the nspace tracker */
    PMIX_RETAIN(nptr);
    psave->nptr = nptr;
    PMIX_RETAIN(info);
    psave->info = info;
    info->proc_cnt++; /* increase number of processes on this rank */
    psave->sd = pnd->sd;
    if (0 > (psave->index = pmix_pointer_array_add(&pmix_server_globals.clients, psave))) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        PMIX_RELEASE(psave);
        /* probably cannot send an error reply if we are out of memory */
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    info->peerid = psave->index;

    /* get the appropriate compatibility modules */
    nptr->compat.psec = pmix_psec_base_assign_module(sec_mode);
    if (NULL == nptr->compat.psec) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, psave->index, NULL);
        PMIX_RELEASE(psave);
        /* send an error reply to the client */
        goto error;
    }
    /* we need the v1.2 bfrops module */
    nptr->compat.bfrops = pmix_bfrops_base_assign_module("v12");
    if (NULL == nptr->compat.bfrops) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, psave->index, NULL);
        PMIX_RELEASE(psave);
       /* send an error reply to the client */
        goto error;
    }
    /* we have no way of knowing their buffer type, so take our default */
    nptr->compat.type = pmix_bfrops_globals.default_type;

    /* take the highest priority gds module - in the absence of any info,
     * we assume they can handle both dstore and hash */
    nptr->compat.gds = pmix_gds_base_assign_module(NULL, 0);
    if (NULL == nptr->compat.gds) {
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, psave->index, NULL);
        PMIX_RELEASE(psave);
        /* send an error reply to the client */
        goto error;
    }

    /* the choice of PTL module was obviously made by the connecting
     * tool as we received this request via that channel, so simply
     * record it here for future use */
    nptr->compat.ptl = &pmix_ptl_usock_module;

    /* validate the connection */
    if (NULL == cred) {
        len = 0;
    } else {
        len = strlen(cred);
    }
    PMIX_PSEC_VALIDATE_CONNECTION(rc, psave,
                                  PMIX_PROTOCOL_V1, cred, len);
    if (PMIX_SUCCESS != rc) {
        pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                            "validation of client credentials failed: %s",
                            PMIx_Error_string(rc));
        free(msg);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, psave->index, NULL);
        PMIX_RELEASE(psave);
        /* error reply was sent by the above macro */
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }
    free(msg);

    /* send the client's array index */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_send_blocking(pnd->sd, (char*)&psave->index, sizeof(int)))) {
        PMIX_ERROR_LOG(rc);
        info->proc_cnt--;
        PMIX_RELEASE(info);
        pmix_pointer_array_set_item(&pmix_server_globals.clients, psave->index, NULL);
        PMIX_RELEASE(psave);
        CLOSE_THE_SOCKET(pnd->sd);
        PMIX_RELEASE(pnd);
        return;
    }

    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "connect-ack from client completed");

    /* let the host server know that this client has connected */
    if (NULL != pmix_host_server.client_connected) {
        (void)strncpy(proc.nspace, psave->info->pname.nspace, PMIX_MAX_NSLEN);
        proc.rank = psave->info->pname.rank;
        rc = pmix_host_server.client_connected(&proc, psave->info->server_object, NULL, NULL);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            info->proc_cnt--;
            PMIX_RELEASE(info);
            pmix_pointer_array_set_item(&pmix_server_globals.clients, psave->index, NULL);
            PMIX_RELEASE(psave);
            CLOSE_THE_SOCKET(pnd->sd);
        }
    }

    /* start the events for this client */
    pmix_event_assign(&psave->recv_event, pmix_globals.evbase, pnd->sd,
                      EV_READ|EV_PERSIST, pmix_usock_recv_handler, psave);
    pmix_event_add(&psave->recv_event, NULL);
    psave->recv_ev_active = true;
    pmix_event_assign(&psave->send_event, pmix_globals.evbase, pnd->sd,
                      EV_WRITE|EV_PERSIST, pmix_usock_send_handler, psave);
    pmix_output_verbose(2, pmix_ptl_base_framework.framework_output,
                        "pmix:server client %s:%u has connected on socket %d",
                        psave->info->pname.nspace, psave->info->pname.rank, psave->sd);

    PMIX_RELEASE(pnd);
    return;

  error:
    /* send an error reply to the client */
    if (PMIX_SUCCESS != pmix_ptl_base_send_blocking(pnd->sd, (char*)&rc, sizeof(int))) {
        PMIX_ERROR_LOG(rc);
    }
    CLOSE_THE_SOCKET(pnd->sd);
    PMIX_RELEASE(pnd);
    return;
}
