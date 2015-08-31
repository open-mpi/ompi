/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>
#include <private/pmix_socket_errno.h>

#include <pmix.h>

#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <fcntl.h>
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
#include PMIX_EVENT_HEADER

#include "src/class/pmix_list.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/util/progress_threads.h"
#include "src/usock/usock.h"
#include "src/sec/pmix_sec.h"

#include "pmix_client_ops.h"

#define PMIX_MAX_RETRIES 10

static int usock_connect(struct sockaddr *address);
static void myerrhandler(pmix_status_t status,
                         pmix_proc_t procs[], size_t nprocs,
                         pmix_info_t info[], size_t ninfo)
{
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client default errhandler activated");
}

static void pmix_client_notify_recv(struct pmix_peer_t *peer, pmix_usock_hdr_t *hdr,
                                    pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t pstatus;
    int status, rc;
    int32_t cnt;
    pmix_proc_t *procs=NULL;
    size_t nprocs, ninfo;
    pmix_info_t *info=NULL;

    if (NULL == pmix_globals.errhandler) {
        return;
    }

    /* unpack the status */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &status, &cnt, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    pstatus = status;

    /* unpack the procs that are impacted */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nprocs, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < nprocs) {
        PMIX_PROC_CREATE(procs, nprocs);
        cnt = nprocs;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, procs, &cnt, PMIX_PROC))) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    /* unpack the info that might have been provided */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    pmix_globals.errhandler(pstatus, procs, nprocs, info, ninfo);

    /* cleanup */
    PMIX_PROC_FREE(procs, nprocs);
    PMIX_INFO_FREE(info, ninfo);
    return;

 error:
    /* we always need to return */
    pmix_globals.errhandler(rc, NULL, 0, NULL, 0);
    PMIX_PROC_FREE(procs, nprocs);
    PMIX_INFO_FREE(info, ninfo);

}


pmix_client_globals_t pmix_client_globals = {{{0}}};

/* callback for wait completion */
static void wait_cbfunc(struct pmix_peer_t *pr, pmix_usock_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client recv callback activated with %d bytes",
                        (NULL == buf) ? -1 : (int)buf->bytes_used);

    cb->active = false;
}

static int connect_to_server(struct sockaddr_un *address)
{
    int rc;

    rc = usock_connect((struct sockaddr *)address);
    if( rc < 0 ){
        return rc;
    }
    pmix_client_globals.myserver.sd = rc;
    /* setup recv event */
    event_assign(&pmix_client_globals.myserver.recv_event,
                 pmix_globals.evbase,
                 pmix_client_globals.myserver.sd,
                 EV_READ | EV_PERSIST,
                 pmix_usock_recv_handler, &pmix_client_globals.myserver);
    event_add(&pmix_client_globals.myserver.recv_event, 0);
    pmix_client_globals.myserver.recv_ev_active = true;

    /* setup send event */
    event_assign(&pmix_client_globals.myserver.send_event,
                 pmix_globals.evbase,
                 pmix_client_globals.myserver.sd,
                 EV_WRITE|EV_PERSIST,
                 pmix_usock_send_handler, &pmix_client_globals.myserver);
    pmix_client_globals.myserver.send_ev_active = false;
    return PMIX_SUCCESS;
}

const char* PMIx_Get_version(void)
{
    return pmix_version_string;
}

int PMIx_Init(pmix_proc_t *proc)
{
    char **uri, *evar;
    int rc, debug_level;
    struct sockaddr_un address;
    pmix_nspace_t *nsptr;

    if (NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    ++pmix_globals.init_cntr;
    if (1 < pmix_globals.init_cntr) {
        /* since we have been called before, the nspace and
         * rank should be known. So return them here if
         * requested */
        (void)strncpy(proc->nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
        proc->rank = pmix_globals.myid.rank;
        return PMIX_SUCCESS;
    }

    /* setup the globals */
    pmix_globals_init();
    PMIX_CONSTRUCT(&pmix_client_globals.pending_requests, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_client_globals.myserver, pmix_peer_t);
    /* mark that we are a client */
    pmix_globals.server = false;

    /* initialize the output system */
    if (!pmix_output_init()) {
        return PMIX_ERROR;
    }

    /* see if debug is requested */
    if (NULL != (evar = getenv("PMIX_DEBUG"))) {
        debug_level = strtol(evar, NULL, 10);
        pmix_globals.debug_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_globals.debug_output, debug_level);
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: init called");

    pmix_bfrop_open();
    pmix_usock_init(pmix_client_notify_recv);
    pmix_sec_init();

    /* we require the nspace */
    if (NULL == (evar = getenv("PMIX_NAMESPACE"))) {
        /* let the caller know that the server isn't available yet */
        return PMIX_ERR_INVALID_NAMESPACE;
    }
    (void)strncpy(proc->nspace, evar, PMIX_MAX_NSLEN);
    (void)strncpy(pmix_globals.myid.nspace, evar, PMIX_MAX_NSLEN);
    nsptr = PMIX_NEW(pmix_nspace_t);
    (void)strncpy(nsptr->nspace, evar, PMIX_MAX_NSLEN);
    pmix_list_append(&pmix_globals.nspaces, &nsptr->super);

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
    pmix_client_globals.myserver.info->nptr = PMIX_NEW(pmix_nspace_t);
    (void)strncpy(pmix_client_globals.myserver.info->nptr->nspace, uri[0], PMIX_MAX_NSLEN);

    /* set the server rank */
    pmix_client_globals.myserver.info->rank = strtoull(uri[1], NULL, 10);

    /* setup the path to the daemon rendezvous point */
    memset(&address, 0, sizeof(struct sockaddr_un));
    address.sun_family = AF_UNIX;
    snprintf(address.sun_path, sizeof(address.sun_path)-1, "%s", uri[2]);
    /* if the rendezvous file doesn't exist, that's an error */
    if (0 != access(uri[2], R_OK)) {
        pmix_argv_free(uri);
        return PMIX_ERR_NOT_FOUND;
    }
    pmix_argv_free(uri);

    /* we also require our rank */
    if (NULL == (evar = getenv("PMIX_RANK"))) {
        /* let the caller know that the server isn't available yet */
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;
    }
    pmix_globals.myid.rank = strtol(evar, NULL, 10);
    proc->rank = pmix_globals.myid.rank;
    pmix_globals.pindex = -1;

    /* create an event base and progress thread for us */
    if (NULL == (pmix_globals.evbase = pmix_start_progress_thread())) {
        return -1;
    }

    /* connect to the server - returns job info if successful */
    if (PMIX_SUCCESS != (rc = connect_to_server(&address))){
        return rc;
    }

    return PMIX_SUCCESS;
}

int PMIx_Initialized(void)
{
    if (0 < pmix_globals.init_cntr) {
        return true;
    }
    return false;
}

int PMIx_Finalize(void)
{
    pmix_buffer_t *msg;
    pmix_cb_t *cb;
    pmix_cmd_t cmd = PMIX_FINALIZE_CMD;
    int rc;

    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client finalize called");

    if ( 0 <= pmix_client_globals.myserver.sd ) {
        /* setup a cmd message to notify the PMIx
         * server that we are normally terminating */
        msg = PMIX_NEW(pmix_buffer_t);
        /* pack the cmd */
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }

        /* create a callback object as we need to pass it to the
         * recv routine so we know which callback to use when
         * the return message is recvd */
        cb = PMIX_NEW(pmix_cb_t);
        cb->active = true;

        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:client sending finalize sync to server");

        /* push the message into our event base to send to the server */
        PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msg, wait_cbfunc, cb);

        /* wait for the ack to return */
        PMIX_WAIT_FOR_COMPLETION(cb->active);
        PMIX_RELEASE(cb);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:client finalize sync received");
    }

    pmix_stop_progress_thread(pmix_globals.evbase);
    event_base_free(pmix_globals.evbase);
#ifdef HAVE_LIBEVENT_GLOBAL_SHUTDOWN
    libevent_global_shutdown();
#endif

    pmix_usock_finalize();
    PMIX_DESTRUCT(&pmix_client_globals.myserver);
    PMIX_LIST_DESTRUCT(&pmix_client_globals.pending_requests);

    if (0 <= pmix_client_globals.myserver.sd) {
        CLOSE_THE_SOCKET(pmix_client_globals.myserver.sd);
    }
    pmix_bfrop_close();
    pmix_sec_finalize();

    pmix_globals_finalize();

    pmix_output_close(pmix_globals.debug_output);
    pmix_output_finalize();
    pmix_class_finalize();

    return PMIX_SUCCESS;
}

int PMIx_Abort(int flag, const char msg[],
               pmix_proc_t procs[], size_t nprocs)
{
    pmix_buffer_t *bfr;
    pmix_cmd_t cmd = PMIX_ABORT_CMD;
    int rc;
    pmix_cb_t *cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client abort called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* create a buffer to hold the message */
    bfr = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(bfr, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack the status flag */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(bfr, &flag, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack the string message - a NULL is okay */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(bfr, &msg, 1, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack the number of procs */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(bfr, &nprocs, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack any provided procs */
    if (0 < nprocs) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(bfr, procs, 1, PMIX_PROC))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(bfr);
            return rc;
        }
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, bfr, wait_cbfunc, cb);

    /* wait for the release */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    PMIX_RELEASE(cb);
    return PMIX_SUCCESS;
}

int PMIx_Put(pmix_scope_t scope, const char key[], pmix_value_t *val)
{
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_nspace_t *ns;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: executing put for key %s type %d",
                        key, val->type);

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* setup to xfer the data */
    kv = PMIX_NEW(pmix_kval_t);
    kv->key = strdup((char*)key);
    kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
    rc = pmix_value_xfer(kv->value, val);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(kv);
        return rc;
    }
    /* put it in our own modex hash table in case something
     * internal to us wants it - our nsrecord is always
     * first on the list */
    if (NULL == (ns = (pmix_nspace_t*)pmix_list_get_first(&pmix_globals.nspaces))) {
        /* shouldn't be possible */
        PMIX_RELEASE(kv);
        return PMIX_ERR_INIT;
    }
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&ns->modex, pmix_globals.myid.rank, kv))) {
        PMIX_ERROR_LOG(rc);
    }

    /* pack the cache that matches the scope - global scope needs
     * to go into both local and remote caches */
    if (PMIX_LOCAL == scope || PMIX_GLOBAL == scope) {
        if (NULL == pmix_globals.cache_local) {
            pmix_globals.cache_local = PMIX_NEW(pmix_buffer_t);
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: put %s data for key %s in local cache",
                            key, (PMIX_GLOBAL == scope) ? "global" : "local");
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(pmix_globals.cache_local, kv, 1, PMIX_KVAL))) {
            PMIX_ERROR_LOG(rc);
        }
    }

    if (PMIX_REMOTE == scope || PMIX_GLOBAL == scope) {
        if (NULL == pmix_globals.cache_remote) {
            pmix_globals.cache_remote = PMIX_NEW(pmix_buffer_t);
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: put %s data for key %s in remote cache",
                            key, (PMIX_GLOBAL == scope) ? "global" : "remote");
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(pmix_globals.cache_remote, kv, 1, PMIX_KVAL))) {
            PMIX_ERROR_LOG(rc);
        }
    }

    PMIX_RELEASE(kv);  // maintain accounting

    return rc;
}

pmix_status_t PMIx_Commit(void)
{
    int rc;
    pmix_scope_t scope;
    pmix_buffer_t *msgout;
    pmix_cmd_t cmd=PMIX_COMMIT_CMD;

    /* if we are a server, or we aren't connected, don't attempt to send */
    if (pmix_globals.server) {
        return PMIX_SUCCESS;  // not an error
    }
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    msgout = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msgout);
        return rc;
    }

    /* if we haven't already done it, ensure we have committed our values */
    if (NULL != pmix_globals.cache_local) {
        scope = PMIX_LOCAL;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &scope, 1, PMIX_SCOPE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &pmix_globals.cache_local, 1, PMIX_BUFFER))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            return rc;
        }
        PMIX_RELEASE(pmix_globals.cache_local);
    }
    if (NULL != pmix_globals.cache_remote) {
        scope = PMIX_REMOTE;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &scope, 1, PMIX_SCOPE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &pmix_globals.cache_remote, 1, PMIX_BUFFER))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            return rc;
        }
        PMIX_RELEASE(pmix_globals.cache_remote);
    }

    /* push the message into our event base to send to the server - always
     * send, even if we have nothing to contribute, so the server knows
     * that we contributed whatever we had */
    PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msgout, NULL, NULL);
    return PMIX_SUCCESS;
}

pmix_status_t PMIx_Resolve_peers(const char *nodename, const char *nspace,
                                 pmix_proc_t **procs, size_t *nprocs)
{
    char **nsprocs=NULL, **nsps=NULL, **tmp;
    pmix_nspace_t *nsptr;
    pmix_nrec_t *nptr;
    size_t i;

    /* set the default */
    *procs = NULL;
    *nprocs = 0;

    /* cycle across our known nspaces */
    tmp = NULL;
    PMIX_LIST_FOREACH(nsptr, &pmix_globals.nspaces, pmix_nspace_t) {
        if (NULL == nspace || 0 == strcmp(nsptr->nspace, nspace)) {
            /* cycle across the nodes in this nspace */
            PMIX_LIST_FOREACH(nptr, &nsptr->nodes, pmix_nrec_t) {
                if (0 == strcmp(nodename, nptr->name)) {
                    /* add the contribution from this node */
                    tmp = pmix_argv_split(nptr->procs, ',');
                    for (i=0; NULL != tmp[i]; i++) {
                        pmix_argv_append_nosize(&nsps, nsptr->nspace);
                        pmix_argv_append_nosize(&nsprocs, tmp[i]);
                    }
                    pmix_argv_free(tmp);
                    tmp = NULL;
                }
            }
        }
    }
    if (0 == (i = pmix_argv_count(nsps))) {
        /* if we don't already have a record for this nspace,
         * see if we have the data in our local cache */

        return PMIX_ERR_NOT_FOUND;
    }

    /* create the required storage */
    i = pmix_argv_count(nsps);
    PMIX_PROC_CREATE(*procs, i);
    *nprocs = pmix_argv_count(nsps);

    /* transfer the data */
    for (i=0; NULL != nsps[i]; i++) {
        (void)strncpy((*procs)[i].nspace, nsps[i], PMIX_MAX_NSLEN);
        (*procs)[i].rank = strtol(nsprocs[i], NULL, 10);
    }
    pmix_argv_free(nsps);
    pmix_argv_free(nsprocs);

    return PMIX_SUCCESS;
}

pmix_status_t PMIx_Resolve_nodes(const char *nspace, char **nodelist)
{
    char **tmp;
    pmix_nspace_t *nsptr;
    pmix_nrec_t *nptr;

    /* set the default */
    *nodelist = NULL;

    /* cycle across our known nspaces */
    tmp = NULL;
    PMIX_LIST_FOREACH(nsptr, &pmix_globals.nspaces, pmix_nspace_t) {
        if (NULL == nspace || 0 == strcmp(nsptr->nspace, nspace)) {
            /* cycle across the nodes in this nspace */
            PMIX_LIST_FOREACH(nptr, &nsptr->nodes, pmix_nrec_t) {
                pmix_argv_append_unique_nosize(&tmp, nptr->name, false);
            }
        }
    }
    if (NULL == tmp) {
        return PMIX_ERR_NOT_FOUND;
    }
    *nodelist = pmix_argv_join(tmp, ',');
    pmix_argv_free(tmp);
    return PMIX_SUCCESS;
}


static int send_connect_ack(int sd)
{
    char *msg;
    pmix_usock_hdr_t hdr;
    size_t sdsize=0, csize=0;
    char *cred = NULL;

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
    if (NULL != pmix_sec.create_cred) {
        if (NULL == (cred = pmix_sec.create_cred())) {
            /* an error occurred - we cannot continue */
            return PMIX_ERR_INVALID_CRED;
        }
        csize = strlen(cred) + 1;  // must NULL terminate the string!
    }
    /* set the number of bytes to be read beyond the header */
    hdr.nbytes = sdsize + strlen(PMIX_VERSION) + 1 + csize;  // must NULL terminate the VERSION string!

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

    if (PMIX_SUCCESS != pmix_usock_send_blocking(sd, msg, sdsize)) {
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
static int recv_connect_ack(int sd)
{
    pmix_usock_hdr_t hdr;
    int32_t reply;
    int rc;
    int32_t cnt;
    char *msg = NULL;
    pmix_buffer_t buf;
    char *nspace;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: RECV CONNECT ACK FROM SERVER");
    /* receive the header */
    rc = pmix_usock_recv_blocking(sd, (char*)&hdr, sizeof(pmix_usock_hdr_t));
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* get whatever else was sent */
    msg = (char*)malloc(hdr.nbytes);
    if (PMIX_SUCCESS != (rc = pmix_usock_recv_blocking(sd, msg, hdr.nbytes))) {
        free(msg);
        return rc;
    }
    /* load the buffer for unpacking */
    PMIX_CONSTRUCT(&buf, pmix_buffer_t);
    PMIX_LOAD_BUFFER(&buf, msg, hdr.nbytes);

    /* unpack the status */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf, &reply, &cnt, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* see if they want us to do the handshake */
    if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
        free(msg);
        msg = NULL;
        if (NULL == pmix_sec.client_handshake) {
            rc = PMIX_ERR_HANDSHAKE_FAILED;
            goto cleanup;
        }
        if (PMIX_SUCCESS != pmix_sec.client_handshake(sd)) {
            goto cleanup;
        }
        /* if we successfully did the handshake, there will be a follow-on
         * message that contains any job info */
        rc = pmix_usock_recv_blocking(sd, (char*)&hdr, sizeof(pmix_usock_hdr_t));
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        /* get whatever else was sent */
        msg = (char*)malloc(hdr.nbytes);
        if (PMIX_SUCCESS != (rc = pmix_usock_recv_blocking(sd, msg, hdr.nbytes))) {
            goto cleanup;
        }
        PMIX_DESTRUCT(&buf);
        PMIX_CONSTRUCT(&buf, pmix_buffer_t);
        PMIX_LOAD_BUFFER(&buf, msg, hdr.nbytes);
        cnt = 1;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf, &reply, &cnt, PMIX_INT))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* see if we succeeded */
    if (PMIX_SUCCESS != reply) {
        rc = reply;
        goto cleanup;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: RECV CONNECT CONFIRMATION AND INITIAL DATA FROM SERVER OF %d BYTES",
                        (int)hdr.nbytes);

    /* unpack our index into the server's client array */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf, &pmix_globals.pindex, &cnt, PMIX_INT))) {
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
            /* this isn't an error - the host must provide us
             * the localid */
            rc = PMIX_SUCCESS;
            goto cleanup;
        }
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* unpack the nspace - we don't need it, but need
     * to step over it */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf, &nspace, &cnt, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* do a sanity check */
    if (NULL == nspace || 0 != strcmp(nspace, pmix_globals.myid.nspace)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        if (NULL != nspace) {
            free(nspace);
        }
        goto cleanup;
    }
    if (NULL != nspace) {
        free(nspace);
    }

    /* unpack any info structs provided */
    pmix_client_process_nspace_blob(pmix_globals.myid.nspace, &buf);

 cleanup:
    buf.base_ptr = NULL;  // protect data region from double-free
    PMIX_DESTRUCT(&buf);
    if (NULL != msg) {
        free(msg);
    }
    return rc;
}

void pmix_client_process_nspace_blob(const char *nspace, pmix_buffer_t *bptr)
{
    pmix_status_t rc;
    int32_t cnt;
    int rank;
    pmix_kval_t *kptr, *kp2, kv;
    pmix_buffer_t buf2;
    pmix_byte_object_t *bo;
    size_t nnodes, i, j;
    pmix_nspace_t *nsptr, *nsptr2;
    pmix_nrec_t *nrec, *nr2;
    char **procs;

    /* cycle across our known nspaces */
    nsptr = NULL;
    PMIX_LIST_FOREACH(nsptr2, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(nsptr2->nspace, nspace)) {
            nsptr = nsptr2;
            break;
        }
    }
    if (NULL == nsptr) {
        /* we don't know this nspace - add it */
        nsptr = PMIX_NEW(pmix_nspace_t);
        (void)strncpy(nsptr->nspace, nspace, PMIX_MAX_NSLEN);
        pmix_list_append(&pmix_globals.nspaces, &nsptr->super);
    }

    /* unpack any info structs provided */
    cnt = 1;
    kptr = PMIX_NEW(pmix_kval_t);
    while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(bptr, kptr, &cnt, PMIX_KVAL))) {
        if (0 == strcmp(kptr->key, PMIX_PROC_BLOB)) {
            /* transfer the byte object for unpacking */
            bo = &(kptr->value->data.bo);
            PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
            PMIX_LOAD_BUFFER(&buf2, bo->bytes, bo->size);
            /* protect the data */
            bo->bytes = NULL;
            bo->size = 0;
            /* start by unpacking the rank */
            cnt = 1;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf2, &rank, &cnt, PMIX_INT))) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&buf2);
                return;
            }
            kp2 = PMIX_NEW(pmix_kval_t);
            kp2->key = strdup(PMIX_RANK);
            PMIX_VALUE_CREATE(kp2->value, 1);
            kp2->value->type = PMIX_INT;
            kp2->value->data.integer = rank;
            if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, rank, kp2))) {
                PMIX_ERROR_LOG(rc);
            }
            PMIX_RELEASE(kp2); // maintain accounting
            cnt = 1;
            kp2 = PMIX_NEW(pmix_kval_t);
            while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(&buf2, kp2, &cnt, PMIX_KVAL))) {
                /* this is data provided by a job-level exchange, so store it
                 * in the job-level data hash_table */
                if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, rank, kp2))) {
                    PMIX_ERROR_LOG(rc);
                }
                PMIX_RELEASE(kp2); // maintain accounting
                kp2 = PMIX_NEW(pmix_kval_t);
            }
            /* cleanup */
            PMIX_DESTRUCT(&buf2);
            PMIX_RELEASE(kp2);
        } else if (0 == strcmp(kptr->key, PMIX_MAP_BLOB)) {
            /* transfer the byte object for unpacking */
            bo = &(kptr->value->data.bo);
            PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
            PMIX_LOAD_BUFFER(&buf2, bo->bytes, bo->size);
            /* protect the data */
            bo->bytes = NULL;
            bo->size = 0;
            /* start by unpacking the number of nodes */
            cnt = 1;
            if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf2, &nnodes, &cnt, PMIX_SIZE))) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&buf2);
                return;
            }
            /* unpack the list of procs on each node */
            for (i=0; i < nnodes; i++) {
                cnt = 1;
                PMIX_CONSTRUCT(&kv, pmix_kval_t);
                if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(&buf2, &kv, &cnt, PMIX_KVAL))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&buf2);
                    PMIX_DESTRUCT(&kv);
                    return;
                }
                /* the name of the node is in the key, and the value is
                 * a comma-delimited list of procs on that node. See if we already
                 * have this node */
                nrec = NULL;
                PMIX_LIST_FOREACH(nr2, &nsptr->nodes, pmix_nrec_t) {
                    if (0 == strcmp(nr2->name, kv.key)) {
                        nrec = nr2;
                        break;
                    }
                }
                if (NULL == nrec) {
                    /* Create a node record and store that list */
                    nrec = PMIX_NEW(pmix_nrec_t);
                    nrec->name = strdup(kv.key);
                    pmix_list_append(&nsptr->nodes, &nrec->super);
                } else {
                    /* refresh the list */
                    if (NULL != nrec->procs) {
                        free(nrec->procs);
                    }
                }
                nrec->procs = strdup(kv.value->data.string);
                /* split the list of procs so we can store their
                 * individual location data */
                procs = pmix_argv_split(nrec->procs, ',');
                for (j=0; NULL != procs[j]; j++) {
                    /* store the hostname for each proc - again, this is
                     * data obtained via a job-level exchange, so store it
                     * in the job-level data hash_table */
                    kp2 = PMIX_NEW(pmix_kval_t);
                    kp2->key = strdup(PMIX_HOSTNAME);
                    kp2->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
                    kp2->value->type = PMIX_STRING;
                    kp2->value->data.string = strdup(nrec->name);
                    rank = strtol(procs[j], NULL, 10);
                    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, rank, kp2))) {
                        PMIX_ERROR_LOG(rc);
                    }
                    PMIX_RELEASE(kp2); // maintain accounting
                }
                pmix_argv_free(procs);
                PMIX_DESTRUCT(&kv);
            }
        } else {
            /* this is job-level data, so just add it to that hash_table
             * with the wildcard rank */
            if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, PMIX_RANK_WILDCARD, kptr))) {
                PMIX_ERROR_LOG(rc);
            }
            PMIX_RELEASE(kptr);
        }
        kptr = PMIX_NEW(pmix_kval_t);
        cnt = 1;
    }
}

static int usock_connect(struct sockaddr *addr)
{
    int rc, sd=-1;
    pmix_socklen_t addrlen = 0;
    int retries = 0;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "usock_peer_try_connect: attempting to connect to server");

    addrlen = sizeof(struct sockaddr_un);
    while (retries < PMIX_MAX_RETRIES) {
        retries++;
        /* Create the new socket */
        sd = socket(PF_UNIX, SOCK_STREAM, 0);
        if (sd < 0) {
            pmix_output(0, "pmix:create_socket: socket() failed: %s (%d)\n",
                        strerror(pmix_socket_errno),
                        pmix_socket_errno);
            continue;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "usock_peer_try_connect: attempting to connect to server on socket %d", sd);
        /* try to connect */
        if (connect(sd, addr, addrlen) < 0) {
            if (pmix_socket_errno == ETIMEDOUT) {
                /* The server may be too busy to accept new connections */
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "timeout connecting to server");
                CLOSE_THE_SOCKET(sd);
                continue;
            }

            /* Some kernels (Linux 2.6) will automatically software
               abort a connection that was ECONNREFUSED on the last
               attempt, without even trying to establish the
               connection.  Handle that case in a semi-rational
               way by trying twice before giving up */
            if (ECONNABORTED == pmix_socket_errno) {
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "connection to server aborted by OS - retrying");
                CLOSE_THE_SOCKET(sd);
                continue;
            }
        }
        /* otherwise, the connect succeeded - so break out of the loop */
        break;
    }

    if (retries == PMIX_MAX_RETRIES || sd < 0){
        /* We were unsuccessful in establishing this connection, and are
         * not likely to suddenly become successful */
        if (0 <= sd) {
            CLOSE_THE_SOCKET(sd);
        }
        return PMIX_ERR_UNREACH;
    }

    /* send our identity and any authentication credentials to the server */
    if (PMIX_SUCCESS != (rc = send_connect_ack(sd))) {
        CLOSE_THE_SOCKET(sd);
        return sd;
    }

    /* do whatever handshake is required */
    if (PMIX_SUCCESS != (rc = recv_connect_ack(sd))) {
        CLOSE_THE_SOCKET(sd);
        return sd;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "sock_peer_try_connect: Connection across to server succeeded");

    /* mark the connection as made */
    pmix_globals.connected = true;

    pmix_usock_set_nonblocking(sd);
    return sd;
}
