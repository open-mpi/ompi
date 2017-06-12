/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
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
#include <src/include/pmix_socket_errno.h>

#include "src/client/pmix_client_ops.h"
#include <pmix_tool.h>
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
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif  /* HAVE_DIRENT_H */

#include PMIX_EVENT_HEADER
#include PMIX_EVENT2_THREAD_HEADER

#if PMIX_CC_USE_PRAGMA_IDENT
#pragma ident PMIX_VERSION
#elif PMIX_CC_USE_IDENT
#ident PMIX_VERSION
#endif

extern pmix_client_globals_t pmix_client_globals;

#include "src/class/pmix_list.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/mca/ptl/ptl.h"
#include "src/mca/psec/psec.h"
#include "src/include/pmix_globals.h"
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
#include "src/dstore/pmix_dstore.h"
#endif /* PMIX_ENABLE_DSTORE */

#define PMIX_MAX_RETRIES 10

static void _notify_complete(pmix_status_t status, void *cbdata)
{
    pmix_event_chain_t *chain = (pmix_event_chain_t*)cbdata;
    PMIX_RELEASE(chain);
}

static void pmix_tool_notify_recv(struct pmix_peer_t *peer,
                                  pmix_ptl_hdr_t *hdr,
                                  pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t rc;
    int32_t cnt;
    pmix_cmd_t cmd;
    pmix_event_chain_t *chain;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool_notify_recv - processing event");

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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &chain->ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < chain->ninfo) {
        PMIX_INFO_CREATE(chain->info, chain->ninfo);
        cnt = chain->ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, chain->info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "[%s:%d] pmix:tool_notify_recv - processing event %d, calling errhandler",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank, chain->status);

    pmix_invoke_local_event_hdlr(chain);
    return;

  error:
    /* we always need to return */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool_notify_recv - unpack error status =%d, calling def errhandler", rc);
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->status = rc;
    pmix_invoke_local_event_hdlr(chain);
}


PMIX_EXPORT int PMIx_tool_init(pmix_proc_t *proc,
                               pmix_info_t info[], size_t ninfo)
{
    pmix_kval_t *kptr;
    pmix_status_t rc;
    pmix_nspace_t *nptr, *nsptr;
    char hostname[PMIX_MAX_NSLEN];

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (NULL == proc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_BAD_PARAM;
    }
    if (0 < pmix_globals.init_cntr) {
        /* since we have been called before, the nspace and
         * rank should be known. So return them here if
         * requested */
         if (NULL != proc) {
            (void)strncpy(proc->nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
            proc->rank = pmix_globals.myid.rank;
        }
        ++pmix_globals.init_cntr;
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_SUCCESS;
    }

    /* if we were given an nspace in the environment, then we
     * must have been spawned by a PMIx server - so even though
     * we technically will operate as a tool, we are actually
     * a "client" of the PMIx server and should connect that way */
    if (NULL != getenv("PMIX_NAMESPACE")) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIx_Init(proc, info, ninfo);
    }

    /* setup the runtime - this init's the globals,
     * opens and initializes the required frameworks */
    if (PMIX_SUCCESS != (rc = pmix_rte_init(PMIX_PROC_TOOL, info, ninfo,
                                            pmix_tool_notify_recv))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    PMIX_CONSTRUCT(&pmix_client_globals.pending_requests, pmix_list_t);
    pmix_client_globals.myserver = PMIX_NEW(pmix_peer_t);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: init called");

    /* select our psec module - we take the default as we cannot
     * do any better */
    if (PMIX_SUCCESS != (rc = pmix_psec.assign_module(pmix_globals.mypeer, NULL))) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    /* the server will have to use the same */
    pmix_client_globals.myserver->compat.psec = pmix_globals.mypeer->compat.psec;

    /* connect to the server - returns job info if successful */
    if (PMIX_SUCCESS != (rc = pmix_ptl.connect_to_peer(pmix_client_globals.myserver, info, ninfo))){
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* increment our init reference counter */
    pmix_globals.init_cntr++;

    /* Success, so copy the nspace and rank */
    (void)strncpy(proc->nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
    proc->rank = pmix_globals.myid.rank;

    /* now finish the initialization by filling our local
     * datastore with typical job-related info. No point
     * in having the server generate these as we are
     * obviously a singleton, and so the values are well-known */
    nsptr = NULL;
    PMIX_LIST_FOREACH(nptr, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strncmp(pmix_globals.myid.nspace, nptr->nspace, PMIX_MAX_NSLEN)) {
            nsptr = nptr;
            break;
        }
    }
    if (NULL == nsptr) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOT_FOUND;
    }

    /* the jobid is just our nspace */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_JOBID);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup(nsptr->nspace);
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* our rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_INT;
    kptr->value->data.integer = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* nproc offset */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_NPROC_OFFSET);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* node size */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_NODE_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local peers */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_PEERS);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup("0");
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local leader */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCALLDR);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* universe size */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_UNIV_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* job size - we are our very own job, so we have no peers */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_JOB_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local size - only us in our job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* max procs - since we are a self-started tool, there is no
     * allocation within which we can grow ourselves */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_MAX_PROCS);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app number */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APPNUM);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app leader */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APPLDR);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APP_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* global rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_GLOBAL_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local rank - we are alone in our job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* we cannot know the node rank as we don't know what
     * other processes are executing on this node - so
     * we'll add that info to the server-tool handshake
     * and load it from there */

    /* hostname */
     gethostname(hostname, PMIX_MAX_NSLEN);
     kptr = PMIX_NEW(pmix_kval_t);
     kptr->key = strdup(PMIX_HOSTNAME);
     PMIX_VALUE_CREATE(kptr->value, 1);
     kptr->value->type = PMIX_STRING;
     kptr->value->data.string = strdup(hostname);
     if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* we cannot know the RM's nodeid for this host, so
     * we'll add that info to the server-tool handshake
     * and load it from there */

    /* the nodemap is simply our hostname as there is no
     * regex to generate */
     kptr = PMIX_NEW(pmix_kval_t);
     kptr->key = strdup(PMIX_NODE_MAP);
     PMIX_VALUE_CREATE(kptr->value, 1);
     kptr->value->type = PMIX_STRING;
     kptr->value->data.string = strdup(hostname);
     if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* likewise, the proc map is just our rank as we are
     * the only proc in this job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_PROC_MAP);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup("0");
    if (PMIX_SUCCESS != (rc = pmix_hash_store(&nsptr->internal, pmix_globals.myid.rank, kptr))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    PMIX_RELEASE_THREAD(&pmix_global_lock);
    return rc;
}

typedef struct {
    pmix_lock_t lock;
    pmix_event_t ev;
    bool active;
} pmix_tool_timeout_t;

/* timer callback */
static void fin_timeout(int sd, short args, void *cbdata)
{
    pmix_tool_timeout_t *tev;
    tev = (pmix_tool_timeout_t*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool finwait timeout fired");
    if (tev->active) {
        tev->active = false;
        PMIX_WAKEUP_THREAD(&tev->lock);
    }
}
/* callback for finalize completion */
static void finwait_cbfunc(struct pmix_peer_t *pr,
                           pmix_ptl_hdr_t *hdr,
                           pmix_buffer_t *buf, void *cbdata)
{
    pmix_tool_timeout_t *tev;
    tev = (pmix_tool_timeout_t*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool finwait_cbfunc received");
    if (tev->active) {
        tev->active = false;
        pmix_event_del(&tev->ev);  // stop the timer
        PMIX_WAKEUP_THREAD(&tev->lock);
    }
}

PMIX_EXPORT pmix_status_t PMIx_tool_finalize(void)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FINALIZE_CMD;
    pmix_status_t rc;
    pmix_tool_timeout_t tev;
    struct timeval tv = {2, 0};

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool finalize called");

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
                         "pmix:tool sending finalize sync to server");

    /* setup a timer to protect ourselves should the server be unable
     * to answer for some reason */
    PMIX_CONSTRUCT_LOCK(&tev.lock);
    pmix_event_assign(&tev.ev, pmix_globals.evbase, -1, 0,
                      fin_timeout, &tev);
    tev.active = true;
    PMIX_POST_OBJECT(&tev);
    pmix_event_add(&tev.ev, &tv);
    if (PMIX_SUCCESS != (rc = pmix_ptl.send_recv(pmix_client_globals.myserver, msg,
                                                 finwait_cbfunc, (void*)&tev))){
        return rc;
    }

    /* wait for the ack to return */
    PMIX_WAIT_THREAD(&tev.lock);
    PMIX_DESTRUCT_LOCK(&tev.lock);
    if (tev.active) {
        pmix_event_del(&tev.ev);
    }
    pmix_output_verbose(2, pmix_globals.debug_output,
                         "pmix:tool finalize sync received");

    if (!pmix_globals.external_evbase) {
        /* stop the progress thread, but leave the event base
         * still constructed. This will allow us to safely
         * tear down the infrastructure, including removal
         * of any events objects may be holding */
        (void)pmix_progress_thread_pause(NULL);
    }

    PMIX_RELEASE(pmix_client_globals.myserver);
    PMIX_LIST_DESTRUCT(&pmix_client_globals.pending_requests);

    /* shutdown services */
    pmix_rte_finalize();

    return PMIX_SUCCESS;
}
