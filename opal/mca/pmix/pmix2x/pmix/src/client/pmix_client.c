/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

#include <src/include/types.h>
#include <src/include/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#include <pmix.h>
#include <pmix_rename.h>

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

#if PMIX_HAVE_ZLIB
#include <zlib.h>
#endif
#include PMIX_EVENT_HEADER
#include PMIX_EVENT2_THREAD_HEADER

#if PMIX_CC_USE_PRAGMA_IDENT
#pragma ident PMIX_VERSION
#elif PMIX_CC_USE_IDENT
#ident PMIX_VERSION
#endif
 static const char pmix_version_string[] = PMIX_VERSION;


#include "src/class/pmix_list.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/event/pmix_event.h"
#include "src/util/argv.h"
#include "src/util/compress.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/mca/ptl/ptl.h"
#include "src/include/pmix_globals.h"
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
#include "src/dstore/pmix_dstore.h"
#endif /* PMIX_ENABLE_DSTORE */
#ifdef HAVE_ZLIB_H
#include <zlib.h>
#endif

#include "pmix_client_ops.h"
#include "src/include/pmix_jobdata.h"

#define PMIX_MAX_RETRIES 10

static void _notify_complete(pmix_status_t status, void *cbdata)
{
    pmix_event_chain_t *chain = (pmix_event_chain_t*)cbdata;
    PMIX_RELEASE(chain);
}

static void pmix_client_notify_recv(struct pmix_peer_t *peer,
                                    pmix_ptl_hdr_t *hdr,
                                    pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t rc;
    int32_t cnt;
    pmix_cmd_t cmd;
    pmix_event_chain_t *chain;
    size_t ninfo;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client_notify_recv - processing event");

    /* start the local notification chain */
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->final_cbfunc = _notify_complete;
    chain->final_cbdata = chain;

    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cmd, &cnt, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    /* unpack the status */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &chain->status, &cnt, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* unpack the source of the event */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &chain->source, &cnt, PMIX_PROC))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* unpack the info that might have been provided */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* we always leave space for a callback object */
    chain->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(chain->info, chain->ninfo);

    if (0 < ninfo) {
        cnt = ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, chain->info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }
    /* now put the callback object tag in the last element */
    PMIX_INFO_LOAD(&chain->info[ninfo], PMIX_EVENT_RETURN_OBJECT, NULL, PMIX_POINTER);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "[%s:%d] pmix:client_notify_recv - processing event %d, calling errhandler",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank, chain->status);

    pmix_invoke_local_event_hdlr(chain);
    return;

  error:
    /* we always need to return */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client_notify_recv - unpack error status =%d, calling def errhandler", rc);
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->status = rc;
    pmix_invoke_local_event_hdlr(chain);
}


pmix_client_globals_t pmix_client_globals = {{{0}}};

/* callback for wait completion */
static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client wait_cbfunc received");

    *active = false;
}

/* callback to receive job info */
static void job_data(struct pmix_peer_t *pr,
                     pmix_ptl_hdr_t *hdr,
                     pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t rc;
    char *nspace;
    int32_t cnt = 1;
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    /* unpack the nspace - we don't really need it, but have to
     * unpack it to maintain sequence */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nspace, &cnt, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        cb->status = PMIX_ERROR;
        cb->active = false;
        return;
    }
    assert(NULL != nspace);
    free(nspace);

    /* decode it */
#if !(defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1))
    pmix_job_data_htable_store(pmix_globals.myid.nspace, buf);
#endif
    cb->status = PMIX_SUCCESS;
    cb->active = false;
}

PMIX_EXPORT const char* PMIx_Get_version(void)
{
    return pmix_version_string;
}

volatile bool waiting_for_debugger = true;
static void notification_fn(size_t evhdlr_registration_id,
                            pmix_status_t status,
                            const pmix_proc_t *source,
                            pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc,
                            void *cbdata)
{
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
    waiting_for_debugger = false;
}
static void evhandler_reg_callbk(pmix_status_t status,
                                 size_t evhandler_ref,
                                 void *cbdata)
{
    volatile int *active = (volatile int*)cbdata;
    *active = status;
}

PMIX_EXPORT pmix_status_t PMIx_Init(pmix_proc_t *proc,
                                    pmix_info_t info[], size_t ninfo)
{
    char *evar;
    pmix_status_t rc;
    pmix_nspace_t *nsptr;
    pmix_cb_t cb;
    pmix_buffer_t *req;
    pmix_cmd_t cmd = PMIX_REQ_CMD;
    volatile int active;
    pmix_status_t code = PMIX_ERR_DEBUGGER_RELEASE;
    pmix_proc_t wildcard;
    pmix_info_t ginfo;
    pmix_value_t *val = NULL;

    if (NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    if (0 < pmix_globals.init_cntr || PMIX_PROC_SERVER == pmix_globals.proc_type) {
        /* since we have been called before, the nspace and
         * rank should be known. So return them here if
         * requested */
         if (NULL != proc) {
            (void)strncpy(proc->nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
            proc->rank = pmix_globals.myid.rank;
        }
        ++pmix_globals.init_cntr;
        return PMIX_SUCCESS;
    }
    /* if we don't see the required info, then we cannot init */
    if (NULL == getenv("PMIX_NAMESPACE")) {
        return PMIX_ERR_INVALID_NAMESPACE;
    }

    /* setup the runtime - this init's the globals,
     * opens and initializes the required frameworks */
    if (PMIX_SUCCESS != (rc = pmix_rte_init(PMIX_PROC_CLIENT, info, ninfo,
                                            pmix_client_notify_recv))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* setup the globals */
    PMIX_CONSTRUCT(&pmix_client_globals.pending_requests, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_client_globals.myserver, pmix_peer_t);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: init called");

    /* we require our nspace */
    if (NULL == (evar = getenv("PMIX_NAMESPACE"))) {
        /* let the caller know that the server isn't available yet */
        return PMIX_ERR_INVALID_NAMESPACE;
    }
    if (NULL != proc) {
        (void)strncpy(proc->nspace, evar, PMIX_MAX_NSLEN);
    }
    (void)strncpy(pmix_globals.myid.nspace, evar, PMIX_MAX_NSLEN);
    nsptr = PMIX_NEW(pmix_nspace_t);
    (void)strncpy(nsptr->nspace, evar, PMIX_MAX_NSLEN);
    pmix_list_append(&pmix_globals.nspaces, &nsptr->super);

    /* we also require our rank */
    if (NULL == (evar = getenv("PMIX_RANK"))) {
        /* let the caller know that the server isn't available yet */
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;
    }
    pmix_globals.myid.rank = strtol(evar, NULL, 10);
    if (NULL != proc) {
        proc->rank = pmix_globals.myid.rank;
    }
    pmix_globals.pindex = -1;

    /* select our psec compat module - the selection will be based
     * on the corresponding envars that should have been passed
     * to us at launch */
    evar = getenv("PMIX_SECURITY_MODE");
    if (PMIX_SUCCESS != (rc = pmix_psec.assign_module(pmix_globals.mypeer, evar))) {
        return PMIX_ERR_INIT;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver.compat.psec = pmix_globals.mypeer->compat.psec;

    /* setup the shared memory support */
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (PMIX_SUCCESS != (rc = pmix_dstore_init(NULL, 0))) {
        return PMIX_ERR_DATA_VALUE_NOT_FOUND;
    }
#endif /* PMIX_ENABLE_DSTORE */

    /* connect to the server */
    if (PMIX_SUCCESS != (rc = pmix_ptl.connect_to_peer(&pmix_client_globals.myserver, info, ninfo))){
        return rc;
    }

    /* send a request for our job info - we do this as a non-blocking
     * transaction because some systems cannot handle very large
     * blocking operations and error out if we try them. */
     req = PMIX_NEW(pmix_buffer_t);
     if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(req, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }
    /* send to the server */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.active = true;
    if (PMIX_SUCCESS != (rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, req, job_data, (void*)&cb))){
        PMIX_DESTRUCT(&cb);
        return rc;
    }
    /* wait for the data to return */
    PMIX_WAIT_FOR_COMPLETION(cb.active);
    rc = cb.status;
    PMIX_DESTRUCT(&cb);

    if (PMIX_SUCCESS == rc) {
        pmix_globals.init_cntr++;
    } else {
        return rc;
    }

    /* lood for a debugger attach key */
    (void)strncpy(wildcard.nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
    wildcard.rank = PMIX_RANK_WILDCARD;
    PMIX_INFO_LOAD(&ginfo, PMIX_IMMEDIATE, NULL, PMIX_BOOL);
    if (PMIX_SUCCESS == PMIx_Get(&wildcard, PMIX_DEBUG_STOP_IN_INIT, &ginfo, 1, &val)) {
        PMIX_VALUE_FREE(val, 1); // cleanup memory
        /* if the value was found, then we need to wait for debugger attach here */
        /* register for the debugger release notificaation */
        active = -1;
        PMIx_Register_event_handler(&code, 1, NULL, 0,
                                    notification_fn, evhandler_reg_callbk, (void*)&active);
        while (-1 == active) {
            usleep(100);
        }
        if (0 != active) {
            return active;
        }
        /* wait for it to arrive */
        PMIX_WAIT_FOR_COMPLETION(waiting_for_debugger);
    }
    PMIX_INFO_DESTRUCT(&ginfo);

    return PMIX_SUCCESS;
}

PMIX_EXPORT int PMIx_Initialized(void)
{
    if (0 < pmix_globals.init_cntr) {
        return true;
    }
    return false;
}

PMIX_EXPORT pmix_status_t PMIx_Finalize(const pmix_info_t info[], size_t ninfo)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FINALIZE_CMD;
    pmix_status_t rc;
    size_t n;
    volatile bool active;

    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client finalize called");

    if ( 0 <= pmix_client_globals.myserver.sd ) {
        /* check to see if we are supposed to execute a
         * blocking fence prior to actually finalizing */
        if (NULL != info && 0 < ninfo) {
            for (n=0; n < ninfo; n++) {
                if (0 == strcmp(PMIX_EMBED_BARRIER, info[n].key)) {
                    /* did they specify a value? */
                    if (PMIX_BOOL == info[n].value.type) {
                        if (info[n].value.data.flag) {
                            /* they do want the barrier */
                            PMIx_Fence(NULL, 0, NULL, 0);
                        }
                    } else {
                        /* providing this attribute is considered
                         * to be "true" by default */
                        PMIx_Fence(NULL, 0, NULL, 0);
                    }
                    break;
                }
            }
        }

        /* setup a cmd message to notify the PMIx
         * server that we are normally terminating */
        msg = PMIX_NEW(pmix_buffer_t);
        /* pack the cmd */
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }


        pmix_output_verbose(2, pmix_globals.debug_output,
                             "pmix:client sending finalize sync to server");

        /* send to the server */
        active = true;;
        if (PMIX_SUCCESS != (rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, msg,
                                                     wait_cbfunc, (void*)&active))){
            return rc;
        }

        /* wait for the ack to return */
        PMIX_WAIT_FOR_COMPLETION(active);
        pmix_output_verbose(2, pmix_globals.debug_output,
                             "pmix:client finalize sync received");
    }

    if (!pmix_globals.external_evbase) {
        /* stop the progress thread, but leave the event base
         * still constructed. This will allow us to safely
         * tear down the infrastructure, including removal
         * of any events objects may be holding */
        (void)pmix_progress_thread_pause(NULL);
    }

    PMIX_DESTRUCT(&pmix_client_globals.myserver);

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (0 > (rc = pmix_dstore_nspace_del(pmix_globals.myid.nspace))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
#endif

    PMIX_LIST_DESTRUCT(&pmix_client_globals.pending_requests);

    if (0 <= pmix_client_globals.myserver.sd) {
        CLOSE_THE_SOCKET(pmix_client_globals.myserver.sd);
    }

    pmix_rte_finalize();

    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_Abort(int flag, const char msg[],
                                     pmix_proc_t procs[], size_t nprocs)
{
    pmix_buffer_t *bfr;
    pmix_cmd_t cmd = PMIX_ABORT_CMD;
    pmix_status_t rc;
    volatile bool active;

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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(bfr, &flag, 1, PMIX_STATUS))) {
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

    /* send to the server */
    active = true;
    if (PMIX_SUCCESS != (rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, bfr,
                                                 wait_cbfunc, (void*)&active))){
        return rc;
    }

    /* wait for the release */
     PMIX_WAIT_FOR_COMPLETION(active);
     return PMIX_SUCCESS;
 }

static void _putfn(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    pmix_kval_t *kv = NULL;
    pmix_nspace_t *ns;
    uint8_t *tmp;
    size_t len;

    /* no need to push info that starts with "pmix" as that is
     * info we would have been provided at startup */
    if (0 == strncmp(cb->key, "pmix", 4)) {
        rc = PMIX_SUCCESS;
        goto done;
    }

    /* setup to xfer the data */
    kv = PMIX_NEW(pmix_kval_t);
    kv->key = strdup(cb->key);  // need to copy as the input belongs to the user
    kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
    if (PMIX_STRING_SIZE_CHECK(cb->value)) {
        /* compress large strings */
        if (pmix_util_compress_string(cb->value->data.string, &tmp, &len)) {
            if (NULL == tmp) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                rc = PMIX_ERR_NOMEM;
                PMIX_ERROR_LOG(rc);
                goto done;
            }
            kv->value->type = PMIX_COMPRESSED_STRING;
            kv->value->data.bo.bytes = (char*)tmp;
            kv->value->data.bo.size = len;
            rc = PMIX_SUCCESS;
        } else {
            rc = pmix_value_xfer(kv->value, cb->value);
        }
    } else {
        rc = pmix_value_xfer(kv->value, cb->value);
    }
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
    if (NULL != kv) {
        PMIX_RELEASE(kv);  // maintain accounting
    }
    cb->pstatus = rc;
    cb->active = false;
}

PMIX_EXPORT pmix_status_t PMIx_Put(pmix_scope_t scope, const char key[], pmix_value_t *val)
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
    PMIX_THREADSHIFT(cb, _putfn);

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

    /* always send, even if we have nothing to contribute, so the server knows
     * that we contributed whatever we had */
    if (PMIX_SUCCESS == (rc = pmix_ptl.send_recv(&pmix_client_globals.myserver, msgout,
                                                 wait_cbfunc, (void*)&cb->active))){
        cb->pstatus = PMIX_SUCCESS;
        return;
    }

  done:
    cb->pstatus = rc;
    cb->active = false;
 }

 PMIX_EXPORT pmix_status_t PMIx_Commit(void)
 {
    pmix_cb_t *cb;
    pmix_status_t rc;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we are a server, or we aren't connected, don't attempt to send */
    if (PMIX_PROC_SERVER == pmix_globals.proc_type) {
        return PMIX_SUCCESS;  // not an error
    }
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    /* pass this into the event library for thread protection */
    PMIX_THREADSHIFT(cb, _commitfn);

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
#if !(defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1))
    pmix_nspace_t *nsptr;
    pmix_nrec_t *nptr;
#endif
    size_t i;

    /* cycle across our known nspaces */
    tmp = NULL;
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
    if (PMIX_SUCCESS == (rc = pmix_dstore_fetch(cb->nspace, PMIX_RANK_WILDCARD,
                      cb->key, &cb->value))) {

        tmp = pmix_argv_split(cb->value->data.string, ',');
        for (i=0; NULL != tmp[i]; i++) {
            pmix_argv_append_nosize(&nsps, cb->nspace);
            pmix_argv_append_nosize(&nsprocs, tmp[i]);
        }
        pmix_argv_free(tmp);
        tmp = NULL;
    }
#else
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
#endif
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

PMIX_EXPORT pmix_status_t PMIx_Resolve_peers(const char *nodename,
                                             const char *nspace,
                                             pmix_proc_t **procs, size_t *nprocs)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    cb->key = (char*)nodename;
    if (NULL != nspace) {
        (void)strncpy(cb->nspace, nspace, PMIX_MAX_NSLEN);
    }

    /* pass this into the event library for thread protection */
    PMIX_THREADSHIFT(cb, _peersfn);

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

PMIX_EXPORT pmix_status_t PMIx_Resolve_nodes(const char *nspace, char **nodelist)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;
    if (NULL != nspace) {
        (void)strncpy(cb->nspace, nspace, PMIX_MAX_NSLEN);
    }

    /* pass this into the event library for thread protection */
    PMIX_THREADSHIFT(cb, _nodesfn);

    /* wait for the result */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->pstatus;
    *nodelist = cb->key;
    PMIX_RELEASE(cb);

    return rc;
}
