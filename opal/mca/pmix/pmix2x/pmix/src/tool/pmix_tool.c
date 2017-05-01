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
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/ptl/ptl.h"
#include "src/mca/psec/psec.h"
#include "src/include/pmix_globals.h"

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
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &cmd, &cnt, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    /* unpack the status */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &chain->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* unpack the source of the event */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &chain->source, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* unpack the info that might have been provided */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &chain->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < chain->ninfo) {
        PMIX_INFO_CREATE(chain->info, chain->ninfo);
        cnt = chain->ninfo;
        PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                           buf, chain->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
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
    char hostname[PMIX_MAX_NSLEN];
    bool found;
    pmix_info_t ginfo;
    size_t n;

    if (NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* if we were given an nspace in the environment, then we
     * must have been spawned by a PMIx server - so even though
     * we technically will operate as a tool, we are actually
     * a "client" of the PMIx server and should connect that way */
    if (NULL != getenv("PMIX_NAMESPACE")) {
        return PMIx_Init(proc, info, ninfo);
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
        return PMIX_SUCCESS;
    }

    /* setup the runtime - this init's the globals,
     * opens and initializes the required frameworks */
    if (PMIX_SUCCESS != (rc = pmix_rte_init(PMIX_PROC_TOOL, info, ninfo,
                                            pmix_tool_notify_recv))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    PMIX_CONSTRUCT(&pmix_client_globals.pending_requests, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_client_globals.myserver, pmix_peer_t);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: init called");

    /* select our bfrops compat module */
    pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(NULL);
    if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
        return PMIX_ERR_INIT;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver.nptr->compat.bfrops = pmix_globals.mypeer->nptr->compat.bfrops;

    /* set the buffer type */
    pmix_globals.mypeer->nptr->compat.type = pmix_bfrops_globals.default_type;
    /* the server will be using the same */
    pmix_client_globals.myserver.nptr->compat.type = pmix_globals.mypeer->nptr->compat.type;

    /* select our psec compat module */
    pmix_globals.mypeer->nptr->compat.psec = pmix_psec_base_assign_module(NULL);
    if (NULL == pmix_globals.mypeer->nptr->compat.psec) {
        return PMIX_ERR_INIT;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver.nptr->compat.psec = pmix_globals.mypeer->nptr->compat.psec;

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

    /* connect to the server - returns job info if successful */
    PMIX_PTL_CONNECT_TO_PEER(rc, &pmix_client_globals.myserver, info, ninfo);
    if (PMIX_SUCCESS != rc){
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

    /* the jobid is just our nspace */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_JOBID);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup(pmix_globals.myid.nspace);
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* our rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_INT;
    kptr->value->data.integer = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* nproc offset */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_NPROC_OFFSET);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* node size */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_NODE_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local peers */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_PEERS);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup("0");
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local leader */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCALLDR);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* universe size */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_UNIV_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* job size - we are our very own job, so we have no peers */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_JOB_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local size - only us in our job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
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
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app number */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APPNUM);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app leader */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APPLDR);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APP_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* global rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_GLOBAL_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local rank - we are alone in our job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
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
     PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                       &pmix_globals.myid,
                       PMIX_INTERNAL, kptr);
     if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
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
     PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                       &pmix_globals.myid,
                       PMIX_INTERNAL, kptr);
     if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
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
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                      &pmix_globals.myid,
                      PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    return rc;
}

/* callback for wait completion */
static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    volatile bool *active = (volatile bool*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool wait_cbfunc received");

    *active = false;
}

PMIX_EXPORT pmix_status_t PMIx_tool_finalize(void)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FINALIZE_CMD;
    pmix_status_t rc;
    volatile bool active;

    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool finalize called");

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
                         "pmix:tool sending finalize sync to server");

    /* send to the server */
    active = true;;
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver, msg,
                       wait_cbfunc, (void*)&active);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* wait for the ack to return */
    PMIX_WAIT_FOR_COMPLETION(active);
    pmix_output_verbose(2, pmix_globals.debug_output,
                         "pmix:tool finalize sync received");

    if (!pmix_globals.external_evbase) {
        /* stop the progress thread, but leave the event base
         * still constructed. This will allow us to safely
         * tear down the infrastructure, including removal
         * of any events objects may be holding */
        (void)pmix_progress_thread_pause(NULL);
    }

    PMIX_DESTRUCT(&pmix_client_globals.myserver);
    PMIX_LIST_DESTRUCT(&pmix_client_globals.pending_requests);

    /* shutdown services */
    pmix_rte_finalize();

    return PMIX_SUCCESS;
}
