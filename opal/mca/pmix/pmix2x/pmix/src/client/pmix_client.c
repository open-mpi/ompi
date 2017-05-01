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
#include "src/event/pmix_event.h"
#include "src/util/argv.h"
#include "src/util/compress.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/threads/threads.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/ptl/base/base.h"
#include "src/include/pmix_globals.h"

#include "pmix_client_ops.h"

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
    if (NULL == chain) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return;
    }
    chain->final_cbfunc = _notify_complete;
    chain->final_cbdata = chain;

    cnt=1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &cmd, &cnt, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }
    /* unpack the status */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &chain->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }

    /* unpack the source of the event */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &chain->source, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }

    /* unpack the info that might have been provided */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }

    /* we always leave space for a callback object */
    chain->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(chain->info, chain->ninfo);
    if (NULL == chain->info) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(chain);
        return;
    }

    if (0 < ninfo) {
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                           buf, chain->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(chain);
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
    if (NULL == chain) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return;
    }
    chain->status = rc;
    pmix_invoke_local_event_hdlr(chain);
}


pmix_client_globals_t pmix_client_globals = {{{0}}};
pmix_mutex_t pmix_client_bootstrap_mutex = PMIX_MUTEX_STATIC_INIT;

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

    /* unpack the nspace - should be same as our own */
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &nspace, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        cb->status = PMIX_ERROR;
        cb->active = false;
        return;
    }
    assert(NULL != nspace);

    /* decode it */
    PMIX_GDS_STORE_JOB_INFO(cb->status,
                            &pmix_client_globals.myserver,
                            nspace, buf);
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

typedef struct {
    pmix_info_t *info;
    size_t ninfo;
} mydata_t;

static void release_info(pmix_status_t status, void *cbdata)
{
    mydata_t *cd = (mydata_t*)cbdata;
    PMIX_INFO_FREE(cd->info, cd->ninfo);
    free(cd);
}

static void _check_for_notify(pmix_info_t info[], size_t ninfo)
{
    mydata_t *cd;
    size_t n, m=0;
    pmix_info_t *model=NULL, *library=NULL, *vers=NULL, *tmod=NULL;

    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_PROGRAMMING_MODEL, PMIX_MAX_KEYLEN)) {
            /* we need to generate an event indicating that
             * a programming model has been declared */
            model = &info[n];
            ++m;
        } else if (0 == strncmp(info[n].key, PMIX_MODEL_LIBRARY_NAME, PMIX_MAX_KEYLEN)) {
            library = &info[n];
            ++m;
        } else if (0 == strncmp(info[n].key, PMIX_MODEL_LIBRARY_VERSION, PMIX_MAX_KEYLEN)) {
            vers = &info[n];
            ++m;
        } else if (0 == strncmp(info[n].key, PMIX_THREADING_MODEL, PMIX_MAX_KEYLEN)) {
            tmod = &info[n];
            ++m;
        }
    }
    if (0 < m) {
        /* notify anyone listening that a model has been declared */
        cd = (mydata_t*)malloc(sizeof(mydata_t));
        if (NULL == cd) {
            /* nothing we can do */
            return;
        }
        PMIX_INFO_CREATE(cd->info, m+1);
        if (NULL == cd->info) {
            free(cd);
            return;
        }
        cd->ninfo = m+1;
        n = 0;
        if (NULL != model) {
            PMIX_INFO_XFER(&cd->info[n], model);
            ++n;
        }
        if (NULL != library) {
            PMIX_INFO_XFER(&cd->info[n], library);
            ++n;
        }
        if (NULL != vers) {
            PMIX_INFO_XFER(&cd->info[n], vers);
            ++n;
        }
        if (NULL != tmod) {
            PMIX_INFO_XFER(&cd->info[n], tmod);
            ++n;
        }
        /* mark that it is not to go to any default handlers */
        PMIX_INFO_LOAD(&cd->info[n], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
        PMIx_Notify_event(PMIX_MODEL_DECLARED,
                          &pmix_globals.myid, PMIX_RANGE_PROC_LOCAL,
                          cd->info, cd->ninfo, release_info, (void*)cd);
    }
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
    size_t n;
    bool found;

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
        /* we also need to check the info keys to see if something need
         * be done with them - e.g., to notify another library that we
         * also have called init */
        if (NULL != info) {
            _check_for_notify(info, ninfo);
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
    pmix_client_globals.myserver.nptr = PMIX_NEW(pmix_nspace_t);
    pmix_client_globals.myserver.info = PMIX_NEW(pmix_rank_info_t);
    /* construct the global notification ring buffer */
    PMIX_CONSTRUCT(&pmix_globals.notifications, pmix_ring_buffer_t);
    pmix_ring_buffer_init(&pmix_globals.notifications, 256);

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
    /* create a pmix_nspace_t object for our peer */
    nsptr = PMIX_NEW(pmix_nspace_t);
    if (NULL == nsptr){
        return PMIX_ERR_NOMEM;
    }
    nsptr->nspace = strdup(evar);
    pmix_globals.mypeer->nptr = nsptr;

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
    /* setup a rank_info object for us */
    pmix_globals.mypeer->info = PMIX_NEW(pmix_rank_info_t);
    if (NULL == pmix_globals.mypeer->info) {
        return PMIX_ERR_NOMEM;
    }
    pmix_globals.mypeer->info->pname.nspace = strdup(proc->nspace);
    pmix_globals.mypeer->info->pname.rank = proc->rank;

    /* select our bfrops compat module - the selection will be based
     * on the corresponding envars that should have been passed
     * to us at launch */
    evar = getenv("PMIX_BFROPS_MODE");
    pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(evar);
    if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
        return PMIX_ERR_INIT;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver.nptr->compat.bfrops = pmix_globals.mypeer->nptr->compat.bfrops;

    /* set the buffer type - the selection will be based
     * on the corresponding envars that should have been passed
     * to us at launch */
    evar = getenv("PMIX_BFROP_BUFFER_TYPE");
    if (NULL == evar) {
        /* just set to our default */
        pmix_globals.mypeer->nptr->compat.type = pmix_bfrops_globals.default_type;
    } else if (0 == strcmp(evar, "PMIX_BFROP_BUFFER_FULLY_DESC")) {
        pmix_globals.mypeer->nptr->compat.type = PMIX_BFROP_BUFFER_FULLY_DESC;
    } else {
        pmix_globals.mypeer->nptr->compat.type = PMIX_BFROP_BUFFER_NON_DESC;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver.nptr->compat.type = pmix_globals.mypeer->nptr->compat.type;


    /* select our psec compat module - the selection will be based
     * on the corresponding envars that should have been passed
     * to us at launch */
    evar = getenv("PMIX_SECURITY_MODE");
    pmix_globals.mypeer->nptr->compat.psec = pmix_psec_base_assign_module(evar);
    if (NULL == pmix_globals.mypeer->nptr->compat.psec) {
        return PMIX_ERR_INIT;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver.nptr->compat.psec = pmix_globals.mypeer->nptr->compat.psec;

    /* select the gds compat module we will use to interact with
     * our server- the selection will be based
     * on the corresponding envars that should have been passed
     * to us at launch */
    evar = getenv("PMIX_GDS_MODULE");
    PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, evar, PMIX_STRING);
    pmix_client_globals.myserver.nptr->compat.gds = pmix_gds_base_assign_module(&ginfo, 1);
    if (NULL == pmix_client_globals.myserver.nptr->compat.gds) {
        PMIX_INFO_DESTRUCT(&ginfo);
        return PMIX_ERR_INIT;
    }
    PMIX_INFO_DESTRUCT(&ginfo);
    /* now select a GDS module for our own internal use - the user may
     * have passed down a directive for this purpose. If they did, then
     * use it. Otherwise, we want the "hash" module */
    found = false;
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_GDS_MODULE, PMIX_MAX_KEYLEN)) {
                PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, info[n].value.data.string, PMIX_STRING);
                found = true;
                break;
            }
        }
    }
    if (!found) {
        PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, "hash", PMIX_STRING);
    }
    pmix_globals.mypeer->nptr->compat.gds = pmix_gds_base_assign_module(&ginfo, 1);
    if (NULL == pmix_globals.mypeer->nptr->compat.gds) {
        PMIX_INFO_DESTRUCT(&ginfo);
        return PMIX_ERR_INIT;
    }
    PMIX_INFO_DESTRUCT(&ginfo);

    /* connect to the server */
    rc = pmix_ptl_base_connect_to_peer((struct pmix_peer_t*)&pmix_client_globals.myserver, info, ninfo);
    if (PMIX_SUCCESS != rc){
        return rc;
    }

    /* send a request for our job info - we do this as a non-blocking
     * transaction because some systems cannot handle very large
     * blocking operations and error out if we try them. */
     req = PMIX_NEW(pmix_buffer_t);
     PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                      req, &cmd, 1, PMIX_CMD);
     if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(req);
        return rc;
    }
    /* send to the server */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.active = true;
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                       req, job_data, (void*)&cb);
    if (PMIX_SUCCESS != rc) {
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

    /* check to see if we need to notify anyone */
    if (NULL != info) {
        _check_for_notify(info, ninfo);
    }

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
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, &cmd, 1, PMIX_CMD);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }


        pmix_output_verbose(2, pmix_globals.debug_output,
                             "pmix:client sending finalize sync to server");

        /* send to the server */
        active = true;
        PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                           msg, wait_cbfunc, (void*)&active);
        if (PMIX_SUCCESS != rc) {
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
    PMIX_LIST_DESTRUCT(&pmix_client_globals.pending_requests);
    PMIX_DESTRUCT(&pmix_globals.notifications);

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
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     bfr, &cmd, 1, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack the status flag */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     bfr, &flag, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack the string message - a NULL is okay */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     bfr, &msg, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack the number of procs */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     bfr, &nprocs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(bfr);
        return rc;
    }
    /* pack any provided procs */
    if (0 < nprocs) {
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         bfr, procs, nprocs, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(bfr);
            return rc;
        }
    }

    /* send to the server */
    active = true;
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver, bfr,
                       wait_cbfunc, (void*)&active);
    if (PMIX_SUCCESS != rc) {
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
            PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer,
                                   kv->value, cb->value);
        }
    } else {
        PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer,
                               kv->value, cb->value);
    }
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* store it */
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                       &pmix_globals.myid,
                       cb->scope, kv);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }

    /* mark that fresh values have been stored so we know
     * to commit them later */
    pmix_globals.commits_pending = true;

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
    pmix_buffer_t *msgout, bkt;
    pmix_cmd_t cmd=PMIX_COMMIT_CMD;
    pmix_kval_t *kv, *kvn;

    msgout = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msgout, &cmd, 1, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msgout);
        goto done;
    }

    /* if we haven't already done it, ensure we have committed our values */
    if (pmix_globals.commits_pending) {
        /* fetch and pack the local values */
        scope = PMIX_LOCAL;
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msgout, &scope, 1, PMIX_SCOPE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }
        /* allow the GDS module to pass us this info
         * as a local connection as this data would
         * only go to another local client */
        cb->proc = &pmix_globals.myid;
        cb->scope = scope;
        cb->copy = false;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, cb);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }

        PMIX_CONSTRUCT(&bkt, pmix_buffer_t);
        PMIX_LIST_FOREACH_SAFE(kv, kvn, &cb->kvs, pmix_kval_t) {
            PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                             &bkt, kv, 1, PMIX_KVAL);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&bkt);
                PMIX_RELEASE(msgout);
                goto done;
            }
            pmix_list_remove_item(&cb->kvs, &kv->super);
            PMIX_RELEASE(kv);
        }
        /* now pack the result */
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msgout, &bkt, 1, PMIX_BUFFER);
        PMIX_DESTRUCT(&bkt);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }

        /* fetch and pack the remote values */
        scope = PMIX_REMOTE;
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msgout, &scope, 1, PMIX_SCOPE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }
        /* we need real copies here as this data will
         * go to remote procs - so a connection will
         * not suffice */
        cb->proc = &pmix_globals.myid;
        cb->scope = scope;
        cb->copy = true;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, cb);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }

        PMIX_CONSTRUCT(&bkt, pmix_buffer_t);
        PMIX_LIST_FOREACH_SAFE(kv, kvn, &cb->kvs, pmix_kval_t) {
            PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                             &bkt, kv, 1, PMIX_KVAL);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&bkt);
                PMIX_RELEASE(msgout);
                goto done;
            }
            pmix_list_remove_item(&cb->kvs, &kv->super);
            PMIX_RELEASE(kv);
        }
        /* now pack the result */
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msgout, &bkt, 1, PMIX_BUFFER);
        PMIX_DESTRUCT(&bkt);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msgout);
            goto done;
        }

        /* record that all committed data to-date has been sent */
        pmix_globals.commits_pending = false;
    }

    /* always send, even if we have nothing to contribute, so the server knows
     * that we contributed whatever we had */
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver, msgout,
                       wait_cbfunc, (void*)&cb->active);
    if (PMIX_SUCCESS != rc) {
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

static void _resolve_peers(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_proc_t proc;
    char **ptr;
    pmix_info_t *info;
    pmix_proc_t *procs;
    size_t ninfo, nprocs, n, j;

    /* this data isn't going anywhere, so we don't require a copy */
    cb->copy = false;
    /* scope is irrelevant as the info we seek must be local */
    cb->scope = PMIX_SCOPE_UNDEF;
    /* let the proc point to the nspace */
    (void)strncpy(proc.nspace, cb->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    cb->proc = &proc;

    PMIX_GDS_FETCH_KV(rc, &pmix_client_globals.myserver, cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* should just be the one value on the list */
    if (1 != pmix_list_get_size(&cb->kvs)) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        rc = PMIX_ERR_BAD_PARAM;
        goto complete;
    }
    kv = (pmix_kval_t*)pmix_list_get_first(&cb->kvs);
    /* the hostname used as a key with wildcard rank will return
     * a pmix_data_array_t of pmix_info_t structs */
    if (NULL == kv->value ||
        PMIX_DATA_ARRAY != kv->value->type ||
        NULL == kv->value->data.darray ||
        PMIX_INFO != kv->value->data.darray->type) {
        PMIX_ERROR_LOG(PMIX_ERR_DATA_VALUE_NOT_FOUND);
        rc = PMIX_ERR_DATA_VALUE_NOT_FOUND;
        goto complete;
    }
    info = (pmix_info_t*)kv->value->data.darray->array;
    ninfo = kv->value->data.darray->size;
    /* find the PMIX_LOCAL_PEERS key */
    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_LOCAL_PEERS, PMIX_MAX_KEYLEN)) {
            /* split the string */
            ptr = pmix_argv_split(info[n].value.data.string, ',');
            nprocs = pmix_argv_count(ptr);
            PMIX_PROC_CREATE(procs, nprocs);
            if (NULL == procs) {
                rc = PMIX_ERR_NOMEM;
                pmix_argv_free(ptr);
                goto complete;
            }
            for (j=0; j < nprocs; j++) {
                (void)strncpy(procs[j].nspace, cb->pname.nspace, PMIX_MAX_NSLEN);
                procs[j].rank = strtoul(ptr[j], NULL, 10);
            }
            cb->procs = procs;
            cb->nprocs = nprocs;
            rc = PMIX_SUCCESS;
            goto complete;
        }
    }

  complete:
    cb->status = rc;
    if (NULL != cb->info) {
        PMIX_INFO_FREE(cb->info, cb->ninfo);
    }
    cb->active = false;
    return;
}
/* need to thread-shift this request */
PMIX_EXPORT pmix_status_t PMIx_Resolve_peers(const char *nodename,
                                             const char *nspace,
                                             pmix_proc_t **procs, size_t *nprocs)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    cb = PMIX_NEW(pmix_cb_t);
    cb->key = (char*)nodename;
    cb->pname.nspace = strdup(nspace);

    PMIX_THREADSHIFT(cb, _resolve_peers);

    PMIX_WAIT_FOR_COMPLETION(cb->active);
    *procs = cb->procs;
    *nprocs = cb->nprocs;

    rc = cb->status;
    PMIX_RELEASE(cb);
    return rc;
}

static void _resolve_nodes(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    pmix_kval_t *kv;
    pmix_proc_t proc;

      /* create a pmix_info_t so we can pass the nspace
       * into the fetch as a qualifier */
      PMIX_INFO_CREATE(cb->info, 1);
      if (NULL == cb->info) {
          cb->status = PMIX_ERR_NOMEM;
          cb->active = false;
          return;
      }
      cb->ninfo = 1;
      PMIX_INFO_LOAD(&cb->info[0], PMIX_NSPACE, cb->pname.nspace, PMIX_STRING);
      /* tell the GDS what we want */
      cb->key = PMIX_NODE_LIST;
      /* this data isn't going anywhere, so we don't require a copy */
      cb->copy = false;
      /* scope is irrelevant as the info we seek must be local */
      cb->scope = PMIX_SCOPE_UNDEF;
      /* put the nspace in the proc field */
      (void)strncpy(proc.nspace, cb->pname.nspace, PMIX_MAX_NSLEN);
      /* the info will be associated with PMIX_RANK_WILDCARD */
      proc.rank = PMIX_RANK_WILDCARD;
      cb->proc = &proc;

      PMIX_GDS_FETCH_KV(rc, &pmix_client_globals.myserver, cb);
      if (PMIX_SUCCESS != rc) {
          PMIX_ERROR_LOG(rc);
          goto complete;
      }
      /* should just be the one value on the list */
      if (1 != pmix_list_get_size(&cb->kvs)) {
          PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
          rc = PMIX_ERR_BAD_PARAM;
          goto complete;
      }
      kv = (pmix_kval_t*)pmix_list_get_first(&cb->kvs);
      /* the PMIX_NODE_LIST key is supposed to return a comma-delimited
       * string of nodes in this  - check that it did */
      if (NULL == kv->value ||
          PMIX_STRING != kv->value->type) {
          PMIX_ERROR_LOG(PMIX_ERR_DATA_VALUE_NOT_FOUND);
          rc = PMIX_ERR_DATA_VALUE_NOT_FOUND;
          goto complete;
      }
      /* return the string */
      if (NULL != kv->value->data.string) {
        cb->key = strdup(kv->value->data.string);
      }

    complete:
      cb->status = rc;
      if (NULL != cb->info) {
          PMIX_INFO_FREE(cb->info, cb->ninfo);
      }
      cb->active = false;
      return;
}

/* need to thread-shift this request */
PMIX_EXPORT pmix_status_t PMIx_Resolve_nodes(const char *nspace, char **nodelist)
{
    pmix_cb_t *cb;
    pmix_status_t rc;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    cb = PMIX_NEW(pmix_cb_t);
    cb->pname.nspace = (char*)nspace;

    PMIX_THREADSHIFT(cb, _resolve_nodes);

    PMIX_WAIT_FOR_COMPLETION(cb->active);
    /* the string we want is in the key field */
    *nodelist = cb->key;

    rc = cb->status;
    PMIX_RELEASE(cb);
    return rc;
}
