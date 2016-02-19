/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
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

#if PMIX_CC_USE_PRAGMA_IDENT
#pragma ident PMIX_VERSION
#elif PMIX_CC_USE_IDENT
#ident PMIX_VERSION
#endif
static const char pmix_version_string[] = PMIX_VERSION;


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
    pmix_status_t pstatus, status, rc;
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

/* callback to receive job info */
static void job_data(struct pmix_peer_t *pr, pmix_usock_hdr_t *hdr,
                     pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t rc;
    char *nspace;
    int32_t cnt = 1;
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    /* unpack the nspace - we don't really need it, but have to
     * unpack it to maintain sequence */
    nspace = NULL;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nspace, &cnt, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    if (NULL != nspace) {
        free(nspace);
    }
    /* decode it */
    pmix_client_process_nspace_blob(pmix_globals.myid.nspace, buf);
    cb->status = PMIX_SUCCESS;
    cb->active = false;
}

static pmix_status_t connect_to_server(struct sockaddr_un *address, void *cbdata)
{
    int rc;
    pmix_status_t ret;
    pmix_cmd_t cmd = PMIX_REQ_CMD;
    pmix_buffer_t *req;

    rc = usock_connect((struct sockaddr *)address);
    if( rc < 0 ){
        PMIX_ERROR_LOG((pmix_status_t)rc);
        return (pmix_status_t)rc;
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

    /* send a request for our job info - we do this as a non-blocking
     * transaction because some systems cannot handle very large
     * blocking operations and error out if we try them. */
    req = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (ret = pmix_bfrop.pack(req, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(ret);
        PMIX_RELEASE(req);
        return ret;
    }
    PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, req, job_data, cbdata);

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
    pmix_cb_t cb;

    if (0 < pmix_globals.init_cntr) {
        /* since we have been called before, the nspace and
         * rank should be known. So return them here if
         * requested */
        if (NULL != proc) {
            (void)strncpy(proc->nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
            proc->rank = pmix_globals.myid.rank;
        }
        return PMIX_SUCCESS;
    }

    /* if we don't see the required info, then we cannot init */
    if (NULL == getenv("PMIX_NAMESPACE")) {
        return PMIX_ERR_INVALID_NAMESPACE;
    }

    /* setup the globals */
    pmix_globals_init();
    PMIX_CONSTRUCT(&pmix_client_globals.pending_requests, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_client_globals.myserver, pmix_peer_t);
    /* mark that we are a client */
    pmix_globals.server = false;
    /* get our effective id's */
    pmix_globals.uid = geteuid();
    pmix_globals.gid = getegid();
    /* default to our internal errhandler */
    pmix_globals.errhandler = myerrhandler;

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

    /* we require the nspace */
    if (NULL == (evar = getenv("PMIX_NAMESPACE"))) {
        /* let the caller know that the server isn't available yet */
        pmix_output_close(pmix_globals.debug_output);
        pmix_output_finalize();
        pmix_class_finalize();
        return PMIX_ERR_INVALID_NAMESPACE;
    }
    if (NULL != proc) {
        (void)strncpy(proc->nspace, evar, PMIX_MAX_NSLEN);
    }
    (void)strncpy(pmix_globals.myid.nspace, evar, PMIX_MAX_NSLEN);
    nsptr = PMIX_NEW(pmix_nspace_t);
    (void)strncpy(nsptr->nspace, evar, PMIX_MAX_NSLEN);
    pmix_list_append(&pmix_globals.nspaces, &nsptr->super);

    /* if we don't have a path to the daemon rendezvous point,
     * then we need to return an error */
    if (NULL == (evar = getenv("PMIX_SERVER_URI"))) {
        /* let the caller know that the server isn't available */
        pmix_output_close(pmix_globals.debug_output);
        pmix_output_finalize();
        pmix_class_finalize();
        return PMIX_ERR_SERVER_NOT_AVAIL;
    }
    uri = pmix_argv_split(evar, ':');
    if (3 != pmix_argv_count(uri)) {
        pmix_argv_free(uri);
        pmix_output_close(pmix_globals.debug_output);
        pmix_output_finalize();
        pmix_class_finalize();
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
        pmix_output_close(pmix_globals.debug_output);
        pmix_output_finalize();
        pmix_class_finalize();
        return PMIX_ERR_NOT_FOUND;
    }
    pmix_argv_free(uri);

    /* we also require our rank */
    if (NULL == (evar = getenv("PMIX_RANK"))) {
        /* let the caller know that the server isn't available yet */
        pmix_output_close(pmix_globals.debug_output);
        pmix_output_finalize();
        pmix_class_finalize();
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;
    }
    pmix_globals.myid.rank = strtol(evar, NULL, 10);
    if (NULL != proc) {
        proc->rank = pmix_globals.myid.rank;
    }
    pmix_globals.pindex = -1;

    /* setup the support */
    pmix_bfrop_open();
    pmix_usock_init(pmix_client_notify_recv);
    pmix_sec_init();

    /* create an event base and progress thread for us */
    if (NULL == (pmix_globals.evbase = pmix_start_progress_thread())) {
        pmix_sec_finalize();
        pmix_usock_finalize();
        pmix_bfrop_close();
        pmix_output_close(pmix_globals.debug_output);
        pmix_output_finalize();
        pmix_class_finalize();
        return -1;
    }

    /* setup an object to track server connection */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.active = true;
    /* connect to the server - returns job info if successful */
    if (PMIX_SUCCESS != (rc = connect_to_server(&address, &cb))){
        PMIX_DESTRUCT(&cb);
        pmix_stop_progress_thread(pmix_globals.evbase);
        pmix_sec_finalize();
        pmix_usock_finalize();
        pmix_bfrop_close();
        pmix_output_close(pmix_globals.debug_output);
        pmix_output_finalize();
        pmix_class_finalize();
        return rc;
    }
    PMIX_WAIT_FOR_COMPLETION(cb.active);
    rc = cb.status;
    PMIX_DESTRUCT(&cb);

    if (PMIX_SUCCESS == rc) {
        pmix_globals.init_cntr++;
    }
    return rc;
}

int PMIx_Initialized(void)
{
    if (0 < pmix_globals.init_cntr) {
        return true;
    }
    return false;
}

pmix_status_t PMIx_Finalize(void)
{
    pmix_buffer_t *msg;
    pmix_cb_t *cb;
    pmix_cmd_t cmd = PMIX_FINALIZE_CMD;
    pmix_status_t rc;

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

    pmix_usock_finalize();
    PMIX_DESTRUCT(&pmix_client_globals.myserver);
    PMIX_LIST_DESTRUCT(&pmix_client_globals.pending_requests);

    if (0 <= pmix_client_globals.myserver.sd) {
        CLOSE_THE_SOCKET(pmix_client_globals.myserver.sd);
    }
    event_base_free(pmix_globals.evbase);
#ifdef HAVE_LIBEVENT_GLOBAL_SHUTDOWN
    libevent_global_shutdown();
#endif

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
    pmix_status_t rc;
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

static void _putfn(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_nspace_t *ns;

    /* setup to xfer the data */
    kv = PMIX_NEW(pmix_kval_t);
    kv->key = strdup(cb->key);  // need to copy as the input belongs to the user
    kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
    rc = pmix_value_xfer(kv->value, cb->value);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }
    /* put it in our own modex hash table in case something
     * internal to us wants it - our nsrecord is always
     * first on the list */
    if (NULL == (ns = (pmix_nspace_t*)pmix_list_get_first(&pmix_globals.nspaces))) {
        /* shouldn't be possible */
        goto done;
    }
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&ns->modex, pmix_globals.myid.rank, kv))) {
        PMIX_ERROR_LOG(rc);
    }

    /* pack the cache that matches the scope - global scope needs
     * to go into both local and remote caches */
    if (PMIX_LOCAL == cb->scope || PMIX_GLOBAL == cb->scope) {
        if (NULL == pmix_globals.cache_local) {
            pmix_globals.cache_local = PMIX_NEW(pmix_buffer_t);
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: put %s data for key %s in local cache",
                            cb->key, (PMIX_GLOBAL == cb->scope) ? "global" : "local");
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(pmix_globals.cache_local, kv, 1, PMIX_KVAL))) {
            PMIX_ERROR_LOG(rc);
        }
    }

    if (PMIX_REMOTE == cb->scope || PMIX_GLOBAL == cb->scope) {
        if (NULL == pmix_globals.cache_remote) {
            pmix_globals.cache_remote = PMIX_NEW(pmix_buffer_t);
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: put %s data for key %s in remote cache",
                            cb->key, (PMIX_GLOBAL == cb->scope) ? "global" : "remote");
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(pmix_globals.cache_remote, kv, 1, PMIX_KVAL))) {
            PMIX_ERROR_LOG(rc);
        }
    }

  done:
    PMIX_RELEASE(kv);  // maintain accounting
    cb->pstatus = rc;
    cb->active = false;
}

pmix_status_t PMIx_Put(pmix_scope_t scope, const char key[], pmix_value_t *val)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: executing put for key %s type %d",
                        key, val->type);

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    cb->scope = scope;
    cb->key = (char*)key;
    cb->value = val;

    /* pass this into the event library for thread protection */
    PMIX_THREAD_SHIFT(cb, _putfn);

    /* wait for the result */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->pstatus;
    PMIX_RELEASE(cb);

    return rc;
}

static void _commitfn(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    pmix_scope_t scope;
    pmix_buffer_t *msgout;
    pmix_cmd_t cmd=PMIX_COMMIT_CMD;

    msgout = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msgout);
        goto done;
    }

    /* if we haven't already done it, ensure we have committed our values */
    if (NULL != pmix_globals.cache_local) {
        scope = PMIX_LOCAL;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &scope, 1, PMIX_SCOPE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &pmix_globals.cache_local, 1, PMIX_BUFFER))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }
        PMIX_RELEASE(pmix_globals.cache_local);
    }
    if (NULL != pmix_globals.cache_remote) {
        scope = PMIX_REMOTE;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &scope, 1, PMIX_SCOPE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msgout, &pmix_globals.cache_remote, 1, PMIX_BUFFER))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }
        PMIX_RELEASE(pmix_globals.cache_remote);
    }

    /* push the message into our event base to send to the server - always
     * send, even if we have nothing to contribute, so the server knows
     * that we contributed whatever we had */
    PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msgout, NULL, NULL);

  done:
    cb->pstatus = rc;
    cb->active = false;
}

pmix_status_t PMIx_Commit(void)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    /* if we are a server, or we aren't connected, don't attempt to send */
    if (pmix_globals.server) {
        return PMIX_SUCCESS;  // not an error
    }
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    /* pass this into the event library for thread protection */
    PMIX_THREAD_SHIFT(cb, _commitfn);

    /* wait for the result */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->pstatus;
    PMIX_RELEASE(cb);

    return rc;
}

static void _peersfn(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    char **nsprocs=NULL, **nsps=NULL, **tmp;
    pmix_nspace_t *nsptr;
    pmix_nrec_t *nptr;
    size_t i;

    /* cycle across our known nspaces */
    tmp = NULL;
    PMIX_LIST_FOREACH(nsptr, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strncmp(nsptr->nspace, cb->nspace, PMIX_MAX_NSLEN)) {
            /* cycle across the nodes in this nspace */
            PMIX_LIST_FOREACH(nptr, &nsptr->nodes, pmix_nrec_t) {
                if (0 == strcmp(cb->key, nptr->name)) {
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
        /* we don't know this nspace */
        rc = PMIX_ERR_NOT_FOUND;
        goto done;
    }

    /* create the required storage */
    PMIX_PROC_CREATE(cb->procs, i);
    cb->nvals = pmix_argv_count(nsps);

    /* transfer the data */
    for (i=0; NULL != nsps[i]; i++) {
        (void)strncpy(cb->procs[i].nspace, nsps[i], PMIX_MAX_NSLEN);
        cb->procs[i].rank = strtol(nsprocs[i], NULL, 10);
    }
    pmix_argv_free(nsps);
    pmix_argv_free(nsprocs);
    rc = PMIX_SUCCESS;

  done:
    cb->pstatus = rc;
    cb->active = false;
}

pmix_status_t PMIx_Resolve_peers(const char *nodename, const char *nspace,
                                 pmix_proc_t **procs, size_t *nprocs)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    cb->key = (char*)nodename;
    if (NULL != nspace) {
        (void)strncpy(cb->nspace, nspace, PMIX_MAX_NSLEN);
    }

    /* pass this into the event library for thread protection */
    PMIX_THREAD_SHIFT(cb, _peersfn);

    /* wait for the result */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->pstatus;
    /* transfer the result */
    *procs = cb->procs;
    *nprocs = cb->nvals;

    /* cleanup */
    PMIX_RELEASE(cb);

    return rc;
}

static void _nodesfn(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    char **tmp;
    pmix_nspace_t *nsptr;
    pmix_nrec_t *nptr;

    /* cycle across our known nspaces */
    tmp = NULL;
    PMIX_LIST_FOREACH(nsptr, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strncmp(nsptr->nspace, cb->nspace, PMIX_MAX_NSLEN)) {
            /* cycle across the nodes in this nspace */
            PMIX_LIST_FOREACH(nptr, &nsptr->nodes, pmix_nrec_t) {
                pmix_argv_append_unique_nosize(&tmp, nptr->name, false);
            }
        }
    }
    if (NULL == tmp) {
        rc = PMIX_ERR_NOT_FOUND;
    } else {
        cb->key = pmix_argv_join(tmp, ',');
        pmix_argv_free(tmp);
        rc = PMIX_SUCCESS;
    }

    cb->pstatus = rc;
    cb->active = false;
}

pmix_status_t PMIx_Resolve_nodes(const char *nspace, char **nodelist)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    if (NULL != nspace) {
        (void)strncpy(cb->nspace, nspace, PMIX_MAX_NSLEN);
    }

    /* pass this into the event library for thread protection */
    PMIX_THREAD_SHIFT(cb, _nodesfn);

    /* wait for the result */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->pstatus;
    *nodelist = cb->key;
    PMIX_RELEASE(cb);

    return rc;
}


static pmix_status_t send_connect_ack(int sd)
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
    rc = pmix_usock_recv_blocking(sd, (char*)&reply, sizeof(int));
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* see if they want us to do the handshake */
    if (PMIX_ERR_READY_FOR_HANDSHAKE == reply) {
        if (NULL == pmix_sec.client_handshake) {
            return PMIX_ERR_HANDSHAKE_FAILED;
        }
        if (PMIX_SUCCESS != (rc = pmix_sec.client_handshake(sd))) {
            return rc;
        }
    } else if (PMIX_SUCCESS != reply) {
        return reply;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: RECV CONNECT CONFIRMATION");

    /* receive our index into the server's client array */
    rc = pmix_usock_recv_blocking(sd, (char*)&pmix_globals.pindex, sizeof(int));
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

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: PROCESSING BLOB FOR NSPACE %s", nspace);

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
            PMIX_RELEASE(kptr);
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
            PMIX_DESTRUCT(&buf2);  // releases the original kptr data
            PMIX_RELEASE(kp2);
        } else if (0 == strcmp(kptr->key, PMIX_MAP_BLOB)) {
            /* transfer the byte object for unpacking */
            bo = &(kptr->value->data.bo);
            PMIX_CONSTRUCT(&buf2, pmix_buffer_t);
            PMIX_LOAD_BUFFER(&buf2, bo->bytes, bo->size);
            PMIX_RELEASE(kptr);
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
            /* cleanup */
            PMIX_DESTRUCT(&buf2);  // releases the original kptr data
        } else {
            /* this is job-level data, so just add it to that hash_table
             * with the wildcard rank */
            if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, PMIX_RANK_WILDCARD, kptr))) {
                PMIX_ERROR_LOG(rc);
            }
            /* maintain accounting - but note that the kptr remains
             * alive and stored in the hash table! So we cannot reuse
             * it for some other purpose */
            PMIX_RELEASE(kptr);
        }
        kptr = PMIX_NEW(pmix_kval_t);
        cnt = 1;
    }
    /* need to release the leftover kptr */
    PMIX_RELEASE(kptr);
}

static int usock_connect(struct sockaddr *addr)
{
    int sd=-1;
    pmix_status_t rc;
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
