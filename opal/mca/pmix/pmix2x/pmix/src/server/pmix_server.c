/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
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

#include <pmix_server.h>
#include <pmix_common.h>
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
#include <ctype.h>
#include <sys/stat.h>
#include PMIX_EVENT_HEADER
#include PMIX_EVENT2_THREAD_HEADER

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/util/show_help.h"
#include "src/mca/base/base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/mca/pnet/pnet.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/ptl/base/base.h"

/* the server also needs access to client operations
 * as it can, and often does, behave as a client */
#include "src/client/pmix_client_ops.h"
#include "pmix_server_ops.h"

// global variables
pmix_server_globals_t pmix_server_globals = {{{0}}};

// local variables
static char *security_mode = NULL;
static char *ptl_mode = NULL;
static char *bfrops_mode = NULL;
static char *gds_mode = NULL;
static pid_t mypid;

// local functions for connection support
static void server_message_handler(struct pmix_peer_t *pr,
                                   pmix_ptl_hdr_t *hdr,
                                   pmix_buffer_t *buf, void *cbdata);

static inline int _my_client(const char *nspace, pmix_rank_t rank);

static pmix_status_t initialize_server_base(pmix_server_module_t *module)
{
    char *evar;

    /* look for our namespace, if one was given */
    if (NULL == (evar = getenv("PMIX_SERVER_NAMESPACE"))) {
        /* use a fake namespace */
        (void)strncpy(pmix_globals.myid.nspace, "pmix-server", PMIX_MAX_NSLEN);
    } else {
        (void)strncpy(pmix_globals.myid.nspace, evar, PMIX_MAX_NSLEN);
    }
    /* update it in our peer object */
    pmix_globals.mypeer->nptr->nspace = strdup(pmix_globals.myid.nspace);

    /* look for our rank, if one was given */
    mypid = getpid();
    if (NULL == (evar = getenv("PMIX_SERVER_RANK"))) {
        /* use our pid */
        pmix_globals.myid.rank = mypid;
    } else {
        pmix_globals.myid.rank = strtol(evar, NULL, 10);
    }

    /* construct the global notification ring buffer */
    PMIX_CONSTRUCT(&pmix_globals.notifications, pmix_ring_buffer_t);
    pmix_ring_buffer_init(&pmix_globals.notifications, 256);

    /* give ourselves a rank_info object just in case */
    pmix_globals.mypeer->info = PMIX_NEW(pmix_rank_info_t);
    if (NULL == pmix_globals.mypeer->info) {
        return PMIX_ERR_NOMEM;
    }
    pmix_globals.mypeer->info->pname.nspace = strdup(pmix_globals.mypeer->nptr->nspace);
    pmix_globals.mypeer->info->pname.rank = pmix_globals.myid.rank;
    pmix_globals.mypeer->info->uid = pmix_globals.uid;
    pmix_globals.mypeer->info->gid = pmix_globals.gid;

    /* setup the server-specific globals */
    PMIX_CONSTRUCT(&pmix_server_globals.clients, pmix_pointer_array_t);
    pmix_pointer_array_init(&pmix_server_globals.clients, 1, INT_MAX, 1);
    PMIX_CONSTRUCT(&pmix_server_globals.collectives, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.remote_pnd, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.gdata, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.events, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.local_reqs, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.nspaces, pmix_list_t);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server init called");

    /* setup the function pointers */
    memset(&pmix_host_server, 0, sizeof(pmix_server_module_t));
    pmix_host_server = *module;

    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_server_init(pmix_server_module_t *module,
                                           pmix_info_t info[], size_t ninfo)
{
    pmix_ptl_posted_recv_t *req;
    pmix_status_t rc;
    size_t n, m;
    pmix_kval_t *kv;
    bool protect;
    char *protected[] = {
        PMIX_USERID,
        PMIX_GRPID,
        PMIX_SOCKET_MODE,
        PMIX_SERVER_TOOL_SUPPORT,
        PMIX_SERVER_SYSTEM_SUPPORT,
        NULL
    };

    if (0 < pmix_globals.init_cntr) {
        return PMIX_SUCCESS;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server init called");

    /* setup the runtime - this init's the globals,
     * opens and initializes the required frameworks */
    if (PMIX_SUCCESS != (rc = pmix_rte_init(PMIX_PROC_SERVER, info, ninfo, NULL))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    if (0 != (rc = initialize_server_base(module))) {
        return rc;
    }

    /* setup the wildcard recv for inbound messages from clients */
    req = PMIX_NEW(pmix_ptl_posted_recv_t);
    req->tag = UINT32_MAX;
    req->cbfunc = server_message_handler;
    /* add it to the end of the list of recvs */
    pmix_list_append(&pmix_ptl_globals.posted_recvs, &req->super);

    if (PMIX_SUCCESS != pmix_ptl_base_start_listening(info, ninfo)) {
        pmix_show_help("help-pmix-server.txt", "listener-thread-start", true);
        PMIx_server_finalize();
        return PMIX_ERR_INIT;
    }

    /* assign our internal bfrops module */
    pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(NULL);
    if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* and set our buffer type */
    pmix_globals.mypeer->nptr->compat.type = pmix_bfrops_globals.default_type;

    /* assign our internal security module */
    pmix_globals.mypeer->nptr->compat.psec = pmix_psec_base_assign_module(NULL);
    if (NULL == pmix_globals.mypeer->nptr->compat.psec) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* assign our internal ptl module */
    pmix_globals.mypeer->nptr->compat.ptl = pmix_ptl_base_assign_module();
    if (NULL == pmix_globals.mypeer->nptr->compat.ptl) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* assign our internal gds module */
    pmix_globals.mypeer->nptr->compat.gds = pmix_gds_base_assign_module(info, ninfo);
    if (NULL == pmix_globals.mypeer->nptr->compat.gds) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* copy need parts over to the client_globals.myserver field
     * so that calls into client-side functions will use our peer */
    PMIX_CONSTRUCT(&pmix_client_globals.myserver, pmix_peer_t);
    PMIX_RETAIN(pmix_globals.mypeer->nptr);
    PMIX_RETAIN(pmix_globals.mypeer->info);
    pmix_client_globals.myserver.nptr = pmix_globals.mypeer->nptr;
    pmix_client_globals.myserver.info = pmix_globals.mypeer->info;

    /* get our available security modules */
    security_mode = pmix_psec_base_get_available_modules();

    /* get our available ptl modules */
    ptl_mode = pmix_ptl_base_get_available_modules();

    /* get our available bfrop modules */
    bfrops_mode = pmix_bfrops_base_get_available_modules();

    /* get available gds modules */
    gds_mode = pmix_gds_base_get_available_modules();

    /* check the info keys for info we
     * need to provide to every client */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            /* check the list of protected keys */
            protect = false;
            for (m=0; NULL != protected[m]; m++) {
                if (0 == strcmp(info[n].key, protected[m])) {
                    protect = true;
                    break;
                }
            }
            if (protect) {
                continue;
            }
            /* store and pass along to every client */
            kv = PMIX_NEW(pmix_kval_t);
            kv->key = strdup(info[n].key);
            PMIX_VALUE_CREATE(kv->value, 1);
            PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer,
                                   kv->value, &info[n].value);
            if (PMIX_SUCCESS != rc) {
                PMIX_RELEASE(kv);
                PMIX_ERROR_LOG(rc);
                return rc;
            }
            pmix_list_append(&pmix_server_globals.gdata, &kv->super);
        }
    }

    ++pmix_globals.init_cntr;

    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_server_finalize(void)
{
    int i;
    pmix_peer_t *peer;

    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server finalize called");

    if (!pmix_globals.external_evbase) {
        /* stop the progress thread, but leave the event base
         * still constructed. This will allow us to safely
         * tear down the infrastructure, including removal
         * of any events objects may be holding */
        (void)pmix_progress_thread_pause(NULL);
    }

    pmix_ptl_base_stop_listening();

    for (i=0; i < pmix_server_globals.clients.size; i++) {
        if (NULL != (peer = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, i))) {
            PMIX_RELEASE(peer);
        }
    }
    PMIX_DESTRUCT(&pmix_server_globals.clients);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.collectives);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.remote_pnd);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.local_reqs);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.gdata);
    PMIX_DESTRUCT(&pmix_globals.notifications);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.events);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.nspaces);

    if (NULL != security_mode) {
        free(security_mode);
    }

    if (NULL != ptl_mode) {
        free(ptl_mode);
    }

    if (NULL != bfrops_mode) {
        free(bfrops_mode);
    }

    if (NULL != gds_mode) {
        free(gds_mode);
    }
    pmix_rte_finalize();

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server finalize complete");

    return PMIX_SUCCESS;
}

static void _register_nspace(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_nspace_t *nptr, *tmp;
    pmix_status_t rc;
    size_t i;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _register_nspace %s", cd->proc.nspace);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(tmp, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        nptr = PMIX_NEW(pmix_nspace_t);
        if (NULL == nptr) {
            rc = PMIX_ERR_NOMEM;
            goto release;
        }
        nptr->nspace = strdup(cd->proc.nspace);
        pmix_list_append(&pmix_server_globals.nspaces, &nptr->super);
    }
    nptr->nlocalprocs = cd->nlocalprocs;

    /* see if we have everyone */
    if (nptr->nlocalprocs == pmix_list_get_size(&nptr->ranks)) {
        nptr->all_registered = true;
    }

    /* check info directives to see if we want to store this info */
    for (i=0; i < cd->ninfo; i++) {
        if (0 == strcmp(cd->info[i].key, PMIX_REGISTER_NODATA)) {
            /* nope - so we are done */
            rc = PMIX_SUCCESS;
            goto release;
        }
    }

    /* store this data in our own GDS module - we will retrieve
     * it later so it can be passed down to the launched procs
     * once they connect to us and we know what GDS module they
     * are using */
    PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, nptr,
                            cd->info, cd->ninfo);

  release:
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

/* setup the data for a job */
PMIX_EXPORT pmix_status_t PMIx_server_register_nspace(const char nspace[], int nlocalprocs,
                                                      pmix_info_t info[], size_t ninfo,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, nspace, PMIX_MAX_NSLEN);
    cd->nlocalprocs = nlocalprocs;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* copy across the info array, if given */
    if (0 < ninfo) {
        cd->ninfo = ninfo;
        cd->info = info;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _register_nspace);
    return PMIX_SUCCESS;
}

static void _deregister_nspace(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_nspace_t *tmp;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _deregister_nspace %s",
                        cd->proc.nspace);

    /* see if we already have this nspace */
    PMIX_LIST_FOREACH(tmp, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            pmix_list_remove_item(&pmix_server_globals.nspaces, &tmp->super);
            PMIX_RELEASE(tmp);
            break;
        }
    }

    /* let our local storage clean up */
    PMIX_GDS_DEL_NSPACE(rc, pmix_globals.mypeer, cd->proc.nspace);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    /* release any job-level messaging resources */
    pmix_pnet.local_app_finalized(cd->proc.nspace);

    /* release the caller */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_server_deregister_nspace(const char nspace[],
                                               pmix_op_cbfunc_t cbfunc,
                                               void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server deregister nspace %s",
                        nspace);

     cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, nspace, PMIX_MAX_NSLEN);
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_nspace);
}

void pmix_server_execute_collective(int sd, short args, void *cbdata)
{
    pmix_trkr_caddy_t *tcd = (pmix_trkr_caddy_t*)cbdata;
    pmix_server_trkr_t *trk = tcd->trk;
    pmix_server_caddy_t *cd;
    pmix_peer_t *peer;
    char *data = NULL;
    size_t sz = 0;
    pmix_byte_object_t bo;
    pmix_buffer_t bucket, pbkt;
    pmix_kval_t *kv;
    pmix_proc_t proc;
    bool first;
    pmix_status_t rc;
    pmix_list_t pnames;
    pmix_namelist_t *pn;
    bool found;
    pmix_cb_t cb;

    /* we don't need to check for non-NULL APIs here as
     * that was already done when the tracker was created */
    if (PMIX_FENCENB_CMD == trk->type) {
        /* if the user asked us to collect data, then we have
         * to provide any locally collected data to the host
         * server so they can circulate it - only take data
         * from the specified procs as not everyone is necessarily
         * participating! And only take data intended for remote
         * distribution as local data will be added when we send
         * the result to our local clients */
        if (trk->hybrid) {
            /* if this is a hybrid, then we pack everything using
             * the daemon-level bfrops module as each daemon is
             * going to have to unpack it, and then repack it for
             * each participant. */
            peer = pmix_globals.mypeer;
        } else {
            /* since all procs are the same, just use the first proc's module */
            cd = (pmix_server_caddy_t*)pmix_list_get_first(&trk->local_cbs);
            peer = cd->peer;
        }
        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);

        unsigned char tmp = (unsigned char)trk->collect_type;
        PMIX_BFROPS_PACK(rc, peer, &bucket, &tmp, 1, PMIX_BYTE);

        if (PMIX_COLLECT_YES == trk->collect_type) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "fence - assembling data");
            first = true;
            PMIX_CONSTRUCT(&pnames, pmix_list_t);
            PMIX_LIST_FOREACH(cd, &trk->local_cbs, pmix_server_caddy_t) {
                /* see if we have already gotten the contribution from
                 * this proc */
                found = false;
                PMIX_LIST_FOREACH(pn, &pnames, pmix_namelist_t) {
                    if (pn->pname == &cd->peer->info->pname) {
                        /* got it */
                        found = true;
                        break;
                    }
                }
                if (found) {
                    continue;
                } else {
                    pn = PMIX_NEW(pmix_namelist_t);
                    pn->pname = &cd->peer->info->pname;
                }
                if (trk->hybrid || first) {
                    /* setup the nspace */
                    (void)strncpy(proc.nspace, cd->peer->info->pname.nspace, PMIX_MAX_NSLEN);
                    first = false;
                }
                proc.rank = cd->peer->info->pname.rank;
                /* get any remote contribution - note that there
                 * may not be a contribution */
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                cb.proc = &proc;
                cb.scope = PMIX_REMOTE;
                cb.copy = true;
                PMIX_GDS_FETCH_KV(rc, peer, &cb);
                if (PMIX_SUCCESS == rc) {
                    /* pack the returned kvals */
                    PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                    /* start with the proc id */
                    PMIX_BFROPS_PACK(rc, peer, &pbkt, &proc, 1, PMIX_PROC);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_DESTRUCT(&bucket);
                        return;
                    }
                    PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
                        PMIX_BFROPS_PACK(rc, peer, &pbkt, kv, 1, PMIX_KVAL);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_ERROR_LOG(rc);
                            PMIX_DESTRUCT(&cb);
                            PMIX_DESTRUCT(&pbkt);
                            PMIX_DESTRUCT(&bucket);
                            return;
                        }
                    }
                    /* extract the resulting byte object */
                    PMIX_UNLOAD_BUFFER(&pbkt, bo.bytes, bo.size);
                    PMIX_DESTRUCT(&pbkt);
                    /* now pack that into the bucket for return */
                    PMIX_BFROPS_PACK(rc, peer, &bucket, &bo, 1, PMIX_BYTE_OBJECT);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        PMIX_BYTE_OBJECT_DESTRUCT(&bo);
                        PMIX_DESTRUCT(&bucket);
                        PMIX_RELEASE(tcd);
                        return;
                    }
                }
                PMIX_DESTRUCT(&cb);
            }
            PMIX_LIST_DESTRUCT(&pnames);
        }
        PMIX_UNLOAD_BUFFER(&bucket, data, sz);
        PMIX_DESTRUCT(&bucket);
        pmix_host_server.fence_nb(trk->pcs, trk->npcs,
                                  trk->info, trk->ninfo,
                                  data, sz, trk->modexcbfunc, trk);
    } else if (PMIX_CONNECTNB_CMD == trk->type) {
        pmix_host_server.connect(trk->pcs, trk->npcs,
                                 trk->info, trk->ninfo,
                                 trk->op_cbfunc, trk);
    } else if (PMIX_DISCONNECTNB_CMD == trk->type) {
        pmix_host_server.disconnect(trk->pcs, trk->npcs,
                                    trk->info, trk->ninfo,
                                    trk->op_cbfunc, trk);
    } else {
        /* unknown type */
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
        PMIX_RELEASE(trk);
    }
    PMIX_RELEASE(tcd);
}

static void _register_client(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_rank_info_t *info, *iptr;
    pmix_nspace_t *nptr, *ns;
    pmix_server_trkr_t *trk;
    pmix_trkr_caddy_t *tcd;
    bool all_def;
    size_t i;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _register_client for nspace %s rank %d",
                        cd->proc.nspace, cd->proc.rank);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(ns->nspace, cd->proc.nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        nptr = PMIX_NEW(pmix_nspace_t);
        if (NULL == nptr) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        nptr->nspace = strdup(cd->proc.nspace);
        pmix_list_append(&pmix_server_globals.nspaces, &nptr->super);
    }
    /* setup a peer object for this client - since the host server
     * only deals with the original processes and not any clones,
     * we know this function will be called only once per rank */
    info = PMIX_NEW(pmix_rank_info_t);
    if (NULL == info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    info->pname.nspace = strdup(nptr->nspace);
    info->pname.rank = cd->proc.rank;
    info->uid = cd->uid;
    info->gid = cd->gid;
    info->server_object = cd->server_object;
    pmix_list_append(&nptr->ranks, &info->super);
    /* see if we have everyone */
    if (nptr->nlocalprocs == pmix_list_get_size(&nptr->ranks)) {
        nptr->all_registered = true;
        /* check any pending trackers to see if they are
         * waiting for us. There is a slight race condition whereby
         * the host server could have spawned the local client and
         * it called back into the collective -before- our local event
         * would fire the register_client callback. Deal with that here. */
        all_def = true;
        PMIX_LIST_FOREACH(trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
            /* if this tracker is already complete, then we
             * don't need to update it */
            if (trk->def_complete) {
                continue;
            }
            /* see if any of our procs from this nspace are involved - the tracker will
             * have been created because a callback was received, but
             * we may or may not have received _all_ callbacks by this
             * time. So check and see if any procs from this nspace are
             * involved, and add them to the count of local participants */
            for (i=0; i < trk->npcs; i++) {
                /* since we have to do this search, let's see
                 * if the nspaces are all defined */
                if (all_def) {
                    /* so far, they have all been defined - check this one */
                    PMIX_LIST_FOREACH(ns, &pmix_server_globals.nspaces, pmix_nspace_t) {
                        if (0 < ns->nlocalprocs &&
                            0 == strcmp(trk->pcs[i].nspace, ns->nspace)) {
                            all_def = ns->all_registered;
                            break;
                        }
                    }
                }
                /* now see if this proc is local to us */
                if (0 != strncmp(trk->pcs[i].nspace, nptr->nspace, PMIX_MAX_NSLEN)) {
                    continue;
                }
                /* need to check if this rank is one of mine */
                PMIX_LIST_FOREACH(iptr, &nptr->ranks, pmix_rank_info_t) {
                    if (PMIX_RANK_WILDCARD == trk->pcs[i].rank ||
                        iptr->pname.rank == trk->pcs[i].rank) {
                        /* this is one of mine - track the count */
                        ++trk->nlocal;
                        break;
                    }
                }
            }
            /* update this tracker's status */
            trk->def_complete = all_def;
            /* is this now locally completed? */
            if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
                /* it did, so now we need to process it
                 * we don't want to block someone
                 * here, so kick any completed trackers into a
                 * new event for processing */
                PMIX_EXECUTE_COLLECTIVE(tcd, trk, pmix_server_execute_collective);
            }
        }
        /* also check any pending local modex requests to see if
         * someone has been waiting for a request on a remote proc
         * in one of our nspaces, but we didn't know all the local procs
         * and so couldn't determine the proc was remote */
        pmix_pending_nspace_requests(nptr);
    }
    rc = PMIX_SUCCESS;

  cleanup:
    /* let the caller know we are done */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_server_register_client(const pmix_proc_t *proc,
                                                      uid_t uid, gid_t gid, void *server_object,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server register client %s:%d",
                        proc->nspace, proc->rank);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->uid = uid;
    cd->gid = gid;
    cd->server_object = server_object;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _register_client);
    return PMIX_SUCCESS;
}

static void _deregister_client(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_rank_info_t *info;
    pmix_nspace_t *nptr, *tmp;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server _deregister_client for nspace %s rank %d",
                        cd->proc.nspace, cd->proc.rank);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH(tmp, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        /* nothing to do */
        goto cleanup;
    }
    /* find and remove this client */
    PMIX_LIST_FOREACH(info, &nptr->ranks, pmix_rank_info_t) {
        if (info->pname.rank == cd->proc.rank) {
            pmix_list_remove_item(&nptr->ranks, &info->super);
            PMIX_RELEASE(info);
            break;
        }
    }

  cleanup:
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(PMIX_SUCCESS, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_server_deregister_client(const pmix_proc_t *proc,
                                               pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server deregister client %s:%d",
                        proc->nspace, proc->rank);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        if (NULL != cbfunc) {
            cbfunc(PMIX_ERR_NOMEM, cbdata);
        }
        return;
    }
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_client);
}

/* setup the envars for a child process */
PMIX_EXPORT pmix_status_t PMIx_server_setup_fork(const pmix_proc_t *proc, char ***env)
{
    char rankstr[128];
    pmix_listener_t *lt;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server setup_fork for nspace %s rank %d",
                        proc->nspace, proc->rank);

    /* pass the nspace */
    pmix_setenv("PMIX_NAMESPACE", proc->nspace, true, env);
    /* pass the rank */
    (void)snprintf(rankstr, 127, "%d", proc->rank);
    pmix_setenv("PMIX_RANK", rankstr, true, env);
    /* pass our rendezvous info */
    PMIX_LIST_FOREACH(lt, &pmix_ptl_globals.listeners, pmix_listener_t) {
        if (NULL != lt->uri && NULL != lt->varname) {
            pmix_setenv(lt->varname, lt->uri, true, env);
        }
    }
    /* pass our active security modules */
    pmix_setenv("PMIX_SECURITY_MODE", security_mode, true, env);
    /* pass our available ptl modules */
    pmix_setenv("PMIX_PTL_MODULE", ptl_mode, true, env);
    /* pass our available bfrop modes */
    pmix_setenv("PMIX_BFROP_MODULE", bfrops_mode, true, env);
    /* pass the type of buffer we are using */
    if (PMIX_BFROP_BUFFER_FULLY_DESC == pmix_globals.mypeer->nptr->compat.type) {
        pmix_setenv("PMIX_BFROP_BUFFER_TYPE", "PMIX_BFROP_BUFFER_FULLY_DESC", true, env);
    } else {
        pmix_setenv("PMIX_BFROP_BUFFER_TYPE", "PMIX_BFROP_BUFFER_NON_DESC", true, env);
    }
    /* pass our available gds modules */
    pmix_setenv("PMIX_GDS_MODULE", gds_mode, true, env);

    /* get any network contribution */
    if (PMIX_SUCCESS != (rc = pmix_pnet.setup_fork(proc, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* get any GDS contributions */
    if (PMIX_SUCCESS != (rc = pmix_gds_base_setup_fork(proc, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PMIX_SUCCESS;
}

/***************************************************************************************************
 *  Support calls from the host server down to us requesting direct modex data provided by one     *
 *  of our local clients                                                                           *
 ***************************************************************************************************/

static void _dmodex_req(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_rank_info_t *info, *iptr;
    pmix_nspace_t *nptr, *ns;
    char *data;
    size_t sz;
    pmix_dmdx_remote_t *dcd;
    pmix_status_t rc;
    pmix_buffer_t pbkt;
    pmix_kval_t *kv;
    pmix_cb_t cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "DMODX LOOKING FOR %s:%d",
                        cd->proc.nspace, cd->proc.rank);

    /* this should be one of my clients, but a race condition
     * could cause this request to arrive prior to us having
     * been informed of it - so first check to see if we know
     * about this nspace yet */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(ns->nspace, cd->proc.nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* we don't know this namespace yet, and so we obviously
         * haven't received the data from this proc yet - defer
         * the request until we do */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        if (NULL == dcd) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        PMIX_RETAIN(cd);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        cd->active = false;  // ensure the request doesn't hang
        return;
    }

    /* They are asking for job level data for this process */
    if (cd->proc.rank == PMIX_RANK_WILDCARD) {
        /* fetch the job-level info for this nspace */
        data = NULL;
        sz = 0;
        /* this is going to a remote peer, so inform the gds
         * that we need an actual copy of the data */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &cd->proc;
        cb.scope = PMIX_REMOTE;
        cb.copy = true;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        if (PMIX_SUCCESS == rc) {
            /* assemble the provided data into a byte object */
            PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
            PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, kv, 1, PMIX_KVAL);
                if (PMIX_SUCCESS != rc) {
                    PMIX_DESTRUCT(&pbkt);
                    PMIX_DESTRUCT(&cb);
                    goto cleanup;
                }
            }
            PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
            PMIX_DESTRUCT(&pbkt);
        }
        PMIX_DESTRUCT(&cb);
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
        /* execute the callback */
        cd->cbfunc(rc, data, sz, cd->cbdata);
        cd->active = false;
        if (NULL != data) {
            free(data);
        }
        return;
    }

    /* see if we have this peer in our list */
    info = NULL;
    PMIX_LIST_FOREACH(iptr, &nptr->ranks, pmix_rank_info_t) {
        if (iptr->pname.rank == cd->proc.rank) {
            info = iptr;
            break;
        }
    }
    if (NULL == info) {
        /* rank isn't known yet - defer
         * the request until we do */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        PMIX_RETAIN(cd);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        cd->active = false;  // ensure the request doesn't hang
        return;
    }

    /* have we received the modex from this proc yet - if
     * not, then defer */
    if (!info->modex_recvd) {
        /* track the request so we can fulfill it once
         * data is recvd */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        PMIX_RETAIN(cd);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        cd->active = false;  // ensure the request doesn't hang
        return;
    }

    /* collect the remote/global data from this proc */
    data = NULL;
    sz = 0;
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = &cd->proc;
    cb.scope = PMIX_REMOTE;
    cb.copy = true;
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
    if (PMIX_SUCCESS == rc) {
        /* assemble the provided data into a byte object */
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, kv, 1, PMIX_KVAL);
            if (PMIX_SUCCESS != rc) {
                PMIX_DESTRUCT(&pbkt);
                PMIX_DESTRUCT(&cb);
                goto cleanup;
            }
        }
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
    }
    PMIX_DESTRUCT(&cb);

  cleanup:
    /* execute the callback */
    cd->cbfunc(rc, data, sz, cd->cbdata);
    if (NULL != data) {
        free(data);
    }
    cd->active = false;
}

PMIX_EXPORT pmix_status_t PMIx_server_dmodex_request(const pmix_proc_t *proc,
                                                     pmix_dmodex_response_fn_t cbfunc,
                                                     void *cbdata)
{
    pmix_setup_caddy_t *cd;

    /* protect against bozo */
    if (NULL == cbfunc || NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:server dmodex request%s:%d",
                        proc->nspace, proc->rank);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    (void)strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _dmodex_req);

    PMIX_WAIT_FOR_COMPLETION(cd->active);
    PMIX_RELEASE(cd);
    return PMIX_SUCCESS;
}

static void _store_internal(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_proc_t proc;

    (void)strncpy(proc.nspace, cd->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = cd->pname.rank;
    PMIX_GDS_STORE_KV(cd->status, pmix_globals.mypeer,
                      &proc, PMIX_INTERNAL, cd->kv);
    cd->active = false;
 }

PMIX_EXPORT pmix_status_t PMIx_Store_internal(const pmix_proc_t *proc,
                                              const char *key, pmix_value_t *val)
{
    pmix_shift_caddy_t *cd;
    pmix_status_t rc;

    /* setup to thread shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->pname.nspace = strdup(proc->nspace);
    cd->pname.rank = proc->rank;

    cd->kv = PMIX_NEW(pmix_kval_t);
    if (NULL == cd->kv) {
        PMIX_RELEASE(cd);
        return PMIX_ERR_NOMEM;
    }
    cd->kv->key = strdup((char*)key);
    cd->kv->value = (pmix_value_t*)malloc(sizeof(pmix_value_t));
    PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer, cd->kv->value, val);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }

    if (PMIX_PROC_SERVER == pmix_globals.proc_type) {
        PMIX_THREADSHIFT(cd, _store_internal);
        PMIX_WAIT_FOR_COMPLETION(cd->active);
    } else {
        _store_internal(0, 0, cd);
    }
    rc = cd->status;
    PMIX_RELEASE(cd);

    return rc;
}

#define PMIX_MAX_NODE_PREFIX        50

PMIX_EXPORT pmix_status_t PMIx_generate_regex(const char *input, char **regexp)
{
    char *vptr, *vsave;
    char prefix[PMIX_MAX_NODE_PREFIX];
    int i, j, len, startnum, vnum, numdigits;
    bool found, fullval;
    char *suffix, *sfx;
    pmix_regex_value_t *vreg;
    pmix_regex_range_t *range;
    pmix_list_t vids;
    char **regexargs = NULL, *tmp, *tmp2;
    char *cptr;

    /* define the default */
    *regexp = NULL;

    /* setup the list of results */
    PMIX_CONSTRUCT(&vids, pmix_list_t);

    /* cycle thru the array of input values - first copy
     * it so we don't overwrite what we were given*/
    vsave = strdup(input);
    vptr = vsave;
    while (NULL != (cptr = strchr(vptr, ',')) || 0 < strlen(vptr)) {
        if (NULL != cptr) {
            *cptr = '\0';
        }
        /* determine this node's prefix by looking for first non-alpha char */
        fullval = false;
        len = strlen(vptr);
        startnum = -1;
        memset(prefix, 0, PMIX_MAX_NODE_PREFIX);
        numdigits = 0;
        for (i=0, j=0; i < len; i++) {
            if (!isalpha(vptr[i])) {
                /* found a non-alpha char */
                if (!isdigit(vptr[i])) {
                    /* if it is anything but a digit, we just use
                     * the entire name
                     */
                    fullval = true;
                    break;
                }
                /* count the size of the numeric field - but don't
                 * add the digits to the prefix
                 */
                numdigits++;
                if (startnum < 0) {
                    /* okay, this defines end of the prefix */
                    startnum = i;
                }
                continue;
            }
            if (startnum < 0) {
                prefix[j++] = vptr[i];
            }
        }
        if (fullval || startnum < 0) {
            /* can't compress this name - just add it to the list */
            vreg = PMIX_NEW(pmix_regex_value_t);
            vreg->prefix = strdup(vptr);
            pmix_list_append(&vids, &vreg->super);
            /* move to the next posn */
            if (NULL == cptr) {
                break;
            }
            vptr = cptr + 1;
            continue;
        }
        /* convert the digits and get any suffix */
        vnum = strtol(&vptr[startnum], &sfx, 10);
        if (NULL != sfx) {
            suffix = strdup(sfx);
        } else {
            suffix = NULL;
        }
        /* is this value already on our list? */
        found = false;
        PMIX_LIST_FOREACH(vreg, &vids, pmix_regex_value_t) {
            if (0 < strlen(prefix) && NULL == vreg->prefix) {
                continue;
            }
            if (0 == strlen(prefix) && NULL != vreg->prefix) {
                continue;
            }
            if (0 < strlen(prefix) && NULL != vreg->prefix
                && 0 != strcmp(prefix, vreg->prefix)) {
                continue;
            }
            if (NULL == suffix && NULL != vreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL == vreg->suffix) {
                continue;
            }
            if (NULL != suffix && NULL != vreg->suffix &&
                0 != strcmp(suffix, vreg->suffix)) {
                continue;
            }
            if (numdigits != vreg->num_digits) {
                continue;
            }
            /* found a match - flag it */
            found = true;
            /* get the last range on this nodeid - we do this
             * to preserve order
             */
            range = (pmix_regex_range_t*)pmix_list_get_last(&vreg->ranges);
            if (NULL == range) {
                /* first range for this value */
                range = PMIX_NEW(pmix_regex_range_t);
                range->start = vnum;
                range->cnt = 1;
                pmix_list_append(&vreg->ranges, &range->super);
                break;
            }
            /* see if the value is out of sequence */
            if (vnum != (range->start + range->cnt)) {
                /* start a new range */
                range = PMIX_NEW(pmix_regex_range_t);
                range->start = vnum;
                range->cnt = 1;
                pmix_list_append(&vreg->ranges, &range->super);
                break;
            }
            /* everything matches - just increment the cnt */
            range->cnt++;
            break;
        }
        if (!found) {
            /* need to add it */
            vreg = PMIX_NEW(pmix_regex_value_t);
            if (0 < strlen(prefix)) {
                vreg->prefix = strdup(prefix);
            }
            if (NULL != suffix) {
                vreg->suffix = strdup(suffix);
            }
            vreg->num_digits = numdigits;
            pmix_list_append(&vids, &vreg->super);
            /* record the first range for this value - we took
             * care of values we can't compress above
             */
            range = PMIX_NEW(pmix_regex_range_t);
            range->start = vnum;
            range->cnt = 1;
            pmix_list_append(&vreg->ranges, &range->super);
        }
        if (NULL != suffix) {
            free(suffix);
        }
        /* move to the next posn */
        if (NULL == cptr) {
            break;
        }
        vptr = cptr + 1;
    }
    free(vsave);

    /* begin constructing the regular expression */
    while (NULL != (vreg = (pmix_regex_value_t*)pmix_list_remove_first(&vids))) {
        /* if no ranges, then just add the name */
        if (0 == pmix_list_get_size(&vreg->ranges)) {
            if (NULL != vreg->prefix) {
                /* solitary value */
                if (0 > asprintf(&tmp, "%s", vreg->prefix)) {
                    return PMIX_ERR_NOMEM;
                }
                pmix_argv_append_nosize(&regexargs, tmp);
                free(tmp);
            }
            PMIX_RELEASE(vreg);
            continue;
        }
        /* start the regex for this value with the prefix */
        if (NULL != vreg->prefix) {
            if (0 > asprintf(&tmp, "%s[%d:", vreg->prefix, vreg->num_digits)) {
                return PMIX_ERR_NOMEM;
            }
        } else {
            if (0 > asprintf(&tmp, "[%d:", vreg->num_digits)) {
                return PMIX_ERR_NOMEM;
            }
        }
        /* add the ranges */
        while (NULL != (range = (pmix_regex_range_t*)pmix_list_remove_first(&vreg->ranges))) {
            if (1 == range->cnt) {
                if (0 > asprintf(&tmp2, "%s%d,", tmp, range->start)) {
                    return PMIX_ERR_NOMEM;
                }
            } else {
                if (0 > asprintf(&tmp2, "%s%d-%d,", tmp, range->start, range->start + range->cnt - 1)) {
                    return PMIX_ERR_NOMEM;
                }
            }
            free(tmp);
            tmp = tmp2;
            PMIX_RELEASE(range);
        }
        /* replace the final comma */
        tmp[strlen(tmp)-1] = ']';
        if (NULL != vreg->suffix) {
            /* add in the suffix, if provided */
            if (0 > asprintf(&tmp2, "%s%s", tmp, vreg->suffix)) {
                return PMIX_ERR_NOMEM;
            }
            free(tmp);
            tmp = tmp2;
        }
        pmix_argv_append_nosize(&regexargs, tmp);
        free(tmp);
        PMIX_RELEASE(vreg);
    }

    /* assemble final result */
    tmp = pmix_argv_join(regexargs, ',');
    if (0 > asprintf(regexp, "pmix[%s]", tmp)) {
        return PMIX_ERR_NOMEM;
    }
    free(tmp);

    /* cleanup */
    pmix_argv_free(regexargs);

    PMIX_DESTRUCT(&vids);
    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_generate_ppn(const char *input, char **regexp)
{
    char **ppn, **npn;
    int i, j, start, end;
    pmix_regex_value_t *vreg;
    pmix_regex_range_t *rng;
    pmix_list_t nodes;
    char *tmp, *tmp2;
    char *cptr;

    /* define the default */
    *regexp = NULL;

    /* setup the list of results */
    PMIX_CONSTRUCT(&nodes, pmix_list_t);

    /* split the input by node */
    ppn = pmix_argv_split(input, ';');

    /* for each node, split the input by comma */
    for (i=0; NULL != ppn[i]; i++) {
        rng = NULL;
        /* create a record for this node */
        vreg = PMIX_NEW(pmix_regex_value_t);
        pmix_list_append(&nodes, &vreg->super);
        /* split the input for this node */
        npn = pmix_argv_split(ppn[i], ',');
        /* look at each element */
        for (j=0; NULL != npn[j]; j++) {
            /* is this a range? */
            if (NULL != (cptr = strchr(npn[j], '-'))) {
                /* terminate the string */
                *cptr = '\0';
                ++cptr;
                start = strtol(npn[j], NULL, 10);
                end = strtol(cptr, NULL, 10);
                /* are we collecting a range? */
                if (NULL == rng) {
                    /* no - better start one */
                    rng = PMIX_NEW(pmix_regex_range_t);
                    rng->start = start;
                    rng->cnt = end - start + 1;
                    pmix_list_append(&vreg->ranges, &rng->super);
                } else {
                    /* is this a continuation of the current range? */
                    if (start == (rng->start + rng->cnt)) {
                        /* just add it to the end of this range */
                        rng->cnt++;
                    } else {
                        /* nope, there is a break - create new range */
                        rng = PMIX_NEW(pmix_regex_range_t);
                        rng->start = start;
                        rng->cnt = end - start + 1;
                        pmix_list_append(&vreg->ranges, &rng->super);
                    }
                }
            } else {
                /* single rank given */
                start = strtol(npn[j], NULL, 10);
                /* are we collecting a range? */
                if (NULL == rng) {
                    /* no - better start one */
                    rng = PMIX_NEW(pmix_regex_range_t);
                    rng->start = start;
                    rng->cnt = 1;
                    pmix_list_append(&vreg->ranges, &rng->super);
                } else {
                    /* is this a continuation of the current range? */
                    if (start == (rng->start + rng->cnt)) {
                        /* just add it to the end of this range */
                        rng->cnt++;
                    } else {
                        /* nope, there is a break - create new range */
                        rng = PMIX_NEW(pmix_regex_range_t);
                        rng->start = start;
                        rng->cnt = 1;
                        pmix_list_append(&vreg->ranges, &rng->super);
                    }
                }
            }
        }
        pmix_argv_free(npn);
    }
    pmix_argv_free(ppn);


    /* begin constructing the regular expression */
    tmp = strdup("pmix[");
    PMIX_LIST_FOREACH(vreg, &nodes, pmix_regex_value_t) {
        while (NULL != (rng = (pmix_regex_range_t*)pmix_list_remove_first(&vreg->ranges))) {
            if (1 == rng->cnt) {
                if (0 > asprintf(&tmp2, "%s%d,", tmp, rng->start)) {
                    return PMIX_ERR_NOMEM;
                }
            } else {
                if (0 > asprintf(&tmp2, "%s%d-%d,", tmp, rng->start, rng->start + rng->cnt - 1)) {
                    return PMIX_ERR_NOMEM;
                }
            }
            free(tmp);
            tmp = tmp2;
            PMIX_RELEASE(rng);
        }
        /* replace the final comma */
        tmp[strlen(tmp)-1] = ';';
    }

    /* replace the final semi-colon */
    tmp[strlen(tmp)-1] = ']';

    /* assemble final result */
    *regexp = tmp;

    PMIX_LIST_DESTRUCT(&nodes);
    return PMIX_SUCCESS;
}

static void _setup_op(pmix_status_t rc, void *cbdata)
{
    pmix_setup_caddy_t *fcd = (pmix_setup_caddy_t*)cbdata;

    if (NULL != fcd->info) {
        PMIX_INFO_FREE(fcd->info, fcd->ninfo);
    }
    PMIX_RELEASE(fcd);
}

static void _setup_app(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_setup_caddy_t *fcd = NULL;
    pmix_status_t rc;
    pmix_list_t ilist;
    pmix_kval_t *kv;
    size_t n;

    PMIX_CONSTRUCT(&ilist, pmix_list_t);

    /* pass to the network libraries */
    if (PMIX_SUCCESS != (rc = pmix_pnet.setup_app(cd->nspace, &ilist))) {
        goto depart;
    }

    /* setup the return callback */
    fcd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == fcd) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        goto depart;
    }

    /* if anything came back, construct the info array */
    if (0 < (fcd->ninfo = pmix_list_get_size(&ilist))) {
        PMIX_INFO_CREATE(fcd->info, fcd->ninfo);
        n = 0;
        PMIX_LIST_FOREACH(kv, &ilist, pmix_kval_t) {
            (void)strncpy(fcd->info[n].key, kv->key, PMIX_MAX_KEYLEN);
            PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer,
                                   &fcd->info[n].value, kv->value);
            if (PMIX_SUCCESS != rc) {
                PMIX_INFO_FREE(fcd->info, fcd->ninfo);
                PMIX_RELEASE(fcd);
                fcd = NULL;
                goto depart;
            }
        }
    }

  depart:
    /* always execute the callback to avoid hanging */
    if (NULL != cd->setupcbfunc) {
        if (NULL == fcd) {
            cd->setupcbfunc(rc, NULL, 0, cd->cbdata, NULL, NULL);
        } else {
            cd->setupcbfunc(rc, fcd->info, fcd->ninfo, cd->cbdata, _setup_op, fcd);
        }
    }

    /* cleanup memory */
    PMIX_LIST_DESTRUCT(&ilist);
    if (NULL != cd->nspace) {
        free(cd->nspace);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_setup_application(const char nspace[],
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_setup_application_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != nspace) {
        cd->nspace = strdup(nspace);
    }
    cd->info = info;
    cd->ninfo = ninfo;
    cd->setupcbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, _setup_app);

    return PMIX_SUCCESS;
}

static void _setup_local_support(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;
    pmix_status_t rc;

    /* pass to the network libraries */
    rc = pmix_pnet.setup_local_network(cd->nspace, cd->info, cd->ninfo);

    /* pass the info back */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
    /* cleanup memory */
    if (NULL != cd->nspace) {
        free(cd->nspace);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_setup_local_support(const char nspace[],
                                              pmix_info_t info[], size_t ninfo,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != nspace) {
        cd->nspace = strdup(nspace);
    }
    cd->info = info;
    cd->ninfo = ninfo;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, _setup_local_support);

    return PMIX_SUCCESS;
}


/****    THE FOLLOWING CALLBACK FUNCTIONS ARE USED BY THE HOST SERVER    ****
 ****    THEY THEREFORE CAN OCCUR IN EITHER THE HOST SERVER'S THREAD     ****
 ****    CONTEXT, OR IN OUR OWN THREAD CONTEXT IF THE CALLBACK OCCURS    ****
 ****    IMMEDIATELY. THUS ANYTHING THAT ACCESSES A GLOBAL ENTITY        ****
 ****    MUST BE PUSHED INTO AN EVENT FOR PROTECTION                     ****/

static void op_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift here as no global data is
     * being accessed */

    /* setup the reply with the returned status */
    if (NULL == (reply = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        PMIX_RELEASE(cd);
        return;
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - send a copy to the originator */
    PMIX_PTL_SEND_ONEWAY(rc, cd->peer, reply, cd->hdr.tag);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
    }

    /* cleanup */
    PMIX_RELEASE(cd);
}

static void _spcb(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;
    pmix_proc_t proc;
    pmix_cb_t cb;
    pmix_kval_t *kv;

    /* setup the reply with the returned status */
    if (NULL == (reply = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(cd->cd);
        cd->active = false;
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->cd->peer, reply, &cd->status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd->cd);
        cd->active = false;
        return;
    }
    if (PMIX_SUCCESS == cd->status) {
        /* pass back the name of the nspace */
        PMIX_BFROPS_PACK(rc, cd->cd->peer, reply, &cd->pname.nspace, 1, PMIX_STRING);
        /* add the job-level info, if we have it */
        (void)strncpy(proc.nspace, cd->pname.nspace, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_WILDCARD;
        /* this is going to a local client, so let the gds
         * have the option of returning a copy of the data,
         * or a pointer to local storage */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &proc;
        cb.scope = PMIX_SCOPE_UNDEF;
        cb.copy = false;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        if (PMIX_SUCCESS == rc) {
            PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
                PMIX_BFROPS_PACK(rc, cd->cd->peer, reply, kv, 1, PMIX_KVAL);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(cd->cd);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&cb);
                    cd->active = false;
                    return;
                }
            }
            PMIX_DESTRUCT(&cb);
        }
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - tell the originator the result */
    PMIX_SERVER_QUEUE_REPLY(cd->cd->peer, cd->cd->hdr.tag, reply);
    /* cleanup */
    PMIX_RELEASE(cd->cd);
    cd->active = false;
}

static void spawn_cbfunc(pmix_status_t status, char *nspace, void *cbdata)
{
    pmix_shift_caddy_t *cd;

    /* need to thread-shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->status = status;
    cd->pname.nspace = strdup(nspace);
    cd->cd = (pmix_server_caddy_t*)cbdata;;

    PMIX_THREADSHIFT(cd, _spcb);
    PMIX_WAIT_FOR_COMPLETION(cd->active);
    PMIX_RELEASE(cd);
}

static void lookup_cbfunc(pmix_status_t status, pmix_pdata_t pdata[], size_t ndata,
                          void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift as no global data is accessed */
    /* setup the reply with the returned status */
    if (NULL == (reply = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        return;
    }
    if (PMIX_SUCCESS == status) {
        /* pack the returned data objects */
        PMIX_BFROPS_PACK(rc, cd->peer, reply, &ndata, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            return;
        }
        PMIX_BFROPS_PACK(rc, cd->peer, reply, pdata, ndata, PMIX_PDATA);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            return;
        }
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - tell the originator the result */
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    /* cleanup */
    PMIX_RELEASE(cd);
}

/* fence modex calls return here when the host RM has completed
 * the operation - any enclosed data is provided to us as a blob
 * which contains byte objects, one for each set of data. Our
 * peer servers will have packed the blobs using our common
 * GDS module, so use the mypeer one to unpack them */
static void _mdxcbfunc(int sd, short argc, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t*)cbdata;
    pmix_server_trkr_t *tracker = scd->tracker;
    pmix_buffer_t xfer, *reply, bkt;
    pmix_byte_object_t bo, bo2;
    pmix_server_caddy_t *cd;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_nspace_caddy_t *nptr;
    pmix_list_t nslist;
    int32_t cnt = 1;
    char byte;
    bool found;
    pmix_collect_t ctype;

    /* pass the blobs being returned */
    PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
    PMIX_LOAD_BUFFER(pmix_globals.mypeer, &xfer, scd->data, scd->ndata);
    PMIX_CONSTRUCT(&nslist, pmix_list_t);

    if (PMIX_SUCCESS != scd->status) {
        rc = scd->status;
        goto finish_collective;
    }

    if (PMIX_COLLECT_INVALID == tracker->collect_type) {
        rc = PMIX_ERR_INVALID_ARG;
        goto finish_collective;
    }

    // Skip the data if we didn't collect it
    if (PMIX_COLLECT_YES != tracker->collect_type) {
        rc = PMIX_SUCCESS;
        goto finish_collective;
    }

    // collect the pmix_nspace_t's of all local participants
    PMIX_LIST_FOREACH(cd, &tracker->local_cbs, pmix_server_caddy_t) {
        // see if we already have this nspace
        found = false;
        PMIX_LIST_FOREACH(nptr, &nslist, pmix_nspace_caddy_t) {
            if (nptr->ns == cd->peer->nptr) {
                found = true;
                break;
            }
        }
        if (!found) {
            // add it
            nptr = PMIX_NEW(pmix_nspace_caddy_t);
            PMIX_RETAIN(cd->peer->nptr);
            nptr->ns = cd->peer->nptr;
            pmix_list_append(&nslist, &nptr->super);
        }
    }

    /* Loop over the enclosed byte object envelopes and
     * store them in our GDS module */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                       &xfer, &bo, &cnt, PMIX_BYTE_OBJECT);
    while (PMIX_SUCCESS == rc) {
        PMIX_LOAD_BUFFER(pmix_globals.mypeer, &bkt, bo.bytes, bo.size);
        /* unpack the data collection flag */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                           &bkt, &byte, &cnt, PMIX_BYTE);
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
            /* no data was returned, so we are done with this blob */
            break;
        }
        if (PMIX_SUCCESS != rc) {
            /* we have an error */
            break;
        }

        // Check that this blob was accumulated with the same data collection setting
        ctype = (pmix_collect_t)byte;
        if (ctype != tracker->collect_type) {
            rc = PMIX_ERR_INVALID_ARG;
            break;
        }
        /* unpack the enclosed blobs from the various peers */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                           &bkt, &bo2, &cnt, PMIX_BYTE_OBJECT);
        while (PMIX_SUCCESS == rc) {
            /* unpack all the kval's from this peer and store them in
             * our GDS. Note that PMIx by design holds all data at
             * the server level until requested. If our GDS is a
             * shared memory region, then the data may be available
             * right away - but the client still has to be notified
             * of its presence. */
            PMIX_LIST_FOREACH(nptr, &nslist, pmix_nspace_caddy_t) {
                PMIX_GDS_STORE_MODEX(rc, nptr->ns, &tracker->local_cbs, &bo2);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    break;
                }
            }
            PMIX_BYTE_OBJECT_DESTRUCT(&bo2);
            /* get the next blob */
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                               &bkt, &bo2, &cnt, PMIX_BYTE_OBJECT);
        }
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
            rc = PMIX_SUCCESS;
        } else if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto finish_collective;
        }
        /* unpack and process the next blob */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer,
                           &xfer, &bo, &cnt, PMIX_BYTE_OBJECT);
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
        rc = PMIX_SUCCESS;
    } else if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }

  finish_collective:
    /* loop across all procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH(cd, &tracker->local_cbs, pmix_server_caddy_t) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            rc = PMIX_ERR_NOMEM;
            break;
        }
        /* setup the reply, starting with the returned status */
        PMIX_BFROPS_PACK(rc, cd->peer, reply, &rc, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "server:modex_cbfunc reply being sent to %s:%u",
                            cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
        PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    }

  cleanup:
    /* Protect data from being free'd because RM pass
     * the pointer that is set to the middle of some
     * buffer (the case with SLURM).
     * RM is responsible on the release of the buffer
     */
    xfer.base_ptr = NULL;
    xfer.bytes_used = 0;
    PMIX_DESTRUCT(&xfer);

    pmix_list_remove_item(&pmix_server_globals.collectives, &tracker->super);
    PMIX_RELEASE(tracker);
    PMIX_LIST_DESTRUCT(&nslist);

    /* we are done */
    if (NULL != scd->cbfunc.relfn) {
        scd->cbfunc.relfn(scd->cbdata);
    }
    PMIX_RELEASE(scd);
}

static void modex_cbfunc(pmix_status_t status, const char *data, size_t ndata, void *cbdata,
                         pmix_release_cbfunc_t relfn, void *relcbd)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t*)cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:modex_cbfunc called with %d bytes", (int)ndata);

    if (NULL == tracker) {
        /* nothing to do - but be sure to give them
         * a release if they want it */
        if (NULL != relfn) {
            relfn(relcbd);
        }
        return;
    }

    /* need to thread-shift this callback as it accesses global data */
    scd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == scd) {
        /* nothing we can do */
        if (NULL != relfn) {
            relfn(cbdata);
        }
        return;
    }
    scd->status = status;
    scd->data = data;
    scd->ndata = ndata;
    scd->tracker = tracker;
    scd->cbfunc.relfn = relfn;
    scd->cbdata = relcbd;
    PMIX_THREADSHIFT(scd, _mdxcbfunc);
}

static void get_cbfunc(pmix_status_t status, const char *data, size_t ndata, void *cbdata,
                       pmix_release_cbfunc_t relfn, void *relcbd)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply, buf;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:get_cbfunc called with %d elements", (int)ndata);

    /* no need to thread-shift here as no global data is accessed */

    if (NULL == cd) {
        /* nothing to do - but be sure to give them
         * a release if they want it */
        if (NULL != relfn) {
            relfn(relcbd);
        }
        return;
    }

    /* setup the reply, starting with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* pack the blob being returned */
    PMIX_CONSTRUCT(&buf, pmix_buffer_t);
    PMIX_LOAD_BUFFER(cd->peer, &buf, data, ndata);
    PMIX_BFROPS_COPY_PAYLOAD(rc, cd->peer, reply, &buf);
    buf.base_ptr = NULL;
    buf.bytes_used = 0;
    PMIX_DESTRUCT(&buf);
    /* send the data to the requestor */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:get_cbfunc reply being sent to %s:%u",
                        cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
    pmix_output_hexdump(5, pmix_globals.debug_output,
                        reply->base_ptr, (reply->bytes_used < 256 ? reply->bytes_used : 256));

    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);

 cleanup:
    /* if someone wants a release, give it to them */
    if (NULL != relfn) {
        relfn(relcbd);
    }
    PMIX_RELEASE(cd);
}

static void _cnct(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t*)cbdata;
    pmix_server_trkr_t *tracker = scd->tracker;
    pmix_buffer_t *reply, pbkt;
    pmix_byte_object_t bo;
    pmix_status_t rc;
    int i;
    pmix_server_caddy_t *cd;
    char **nspaces=NULL;
    bool found;
    pmix_proc_t proc;
    pmix_cb_t cb;
    pmix_kval_t *kptr;

    if (PMIX_CONNECTNB_CMD == tracker->type) {
      /* find the unique nspaces that are participating */
      PMIX_LIST_FOREACH(cd, &tracker->local_cbs, pmix_server_caddy_t) {
          if (NULL == nspaces) {
              pmix_argv_append_nosize(&nspaces, cd->peer->info->pname.nspace);
          } else {
              found = false;
              for (i=0; NULL != nspaces[i]; i++) {
                  if (0 == strcmp(nspaces[i], cd->peer->info->pname.nspace)) {
                      found = true;
                      break;
                  }
              }
              if (!found) {
                  pmix_argv_append_nosize(&nspaces, cd->peer->info->pname.nspace);
              }
          }
      }
    }

    /* loop across all local procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH(cd, &tracker->local_cbs, pmix_server_caddy_t) {
        /* setup the reply, starting with the returned status */
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        PMIX_BFROPS_PACK(rc, cd->peer, reply, &scd->status, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            goto cleanup;
        }
        if (PMIX_CONNECTNB_CMD == tracker->type) {
            /* loop across all participating nspaces and include their
             * job-related info */
            for (i=0; NULL != nspaces[i]; i++) {
                /* if this is the local proc's own nspace, then
                 * ignore it - it already has this info */
                if (0 == strncmp(nspaces[i], cd->peer->info->pname.nspace, PMIX_MAX_NSLEN)) {
                    continue;
                }
                (void)strncpy(proc.nspace, nspaces[i], PMIX_MAX_NSLEN);
                proc.rank = PMIX_RANK_WILDCARD;
                /* this is a local request, so give the gds the option
                 * of returning a copy of the data, or a pointer to
                 * local storage */
                /* add the job-level info, if necessary */
                (void)strncpy(proc.nspace, cd->peer->info->pname.nspace, PMIX_MAX_NSLEN);
                proc.rank = PMIX_RANK_WILDCARD;
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                /* this is for a local client, so give the gds the
                 * option of returning a complete copy of the data,
                 * or returning a pointer to local storage */
                cb.proc = &proc;
                cb.scope = PMIX_SCOPE_UNDEF;
                cb.copy = false;
                PMIX_GDS_FETCH_KV(rc, cd->peer, &cb);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&cb);
                    goto cleanup;
                }
                PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                /* pack the nspace name */
                PMIX_BFROPS_PACK(rc, cd->peer, &pbkt, &nspaces[i], 1, PMIX_STRING);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&cb);
                    goto cleanup;
                }
                PMIX_LIST_FOREACH(kptr, &cb.kvs, pmix_kval_t) {
                    PMIX_BFROPS_PACK(rc, cd->peer, &pbkt, kptr, 1, PMIX_KVAL);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(reply);
                        PMIX_DESTRUCT(&cb);
                        goto cleanup;
                    }
                }
                PMIX_DESTRUCT(&cb);
                PMIX_UNLOAD_BUFFER(&pbkt, bo.bytes, bo.size);
                PMIX_BFROPS_PACK(rc, cd->peer, reply, &bo, 1, PMIX_BYTE_OBJECT);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&pbkt);
                    goto cleanup;
                }
                PMIX_DESTRUCT(&pbkt);
            }
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "server:cnct_cbfunc reply being sent to %s:%u",
                            cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
        PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    }

  cleanup:
    if (NULL != nspaces) {
      pmix_argv_free(nspaces);
    }
    pmix_list_remove_item(&pmix_server_globals.collectives, &tracker->super);
    PMIX_RELEASE(tracker);

    /* we are done */
    PMIX_RELEASE(scd);
}

static void cnct_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t*)cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:cnct_cbfunc called");

    if (NULL == tracker) {
        /* nothing to do */
        return;
    }

    /* need to thread-shift this callback as it accesses global data */
    scd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == scd) {
        /* nothing we can do */
        return;
    }
    scd->status = status;
    scd->tracker = tracker;
    PMIX_THREADSHIFT(scd, _cnct);
}

static void regevents_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*) cbdata;
    pmix_buffer_t *reply;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:regevents_cbfunc called status = %d", status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    PMIX_RELEASE(cd);
}

static void notifyerror_cbfunc (pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*) cbdata;
    pmix_buffer_t *reply;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server:notifyerror_cbfunc called status = %d", status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    PMIX_RELEASE(cd);
}


static void query_cbfunc(pmix_status_t status,
                         pmix_info_t *info, size_t ninfo,
                         void *cbdata,
                         pmix_release_cbfunc_t release_fn,
                         void *release_cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t*)cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)qcd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query callback with status %d", status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, cd->peer, reply, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

  complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(cd->peer, cd->hdr.tag, reply);
    // cleanup
    if (NULL != qcd->queries) {
        PMIX_QUERY_FREE(qcd->queries, qcd->nqueries);
    }
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
}

/* the switchyard is the primary message handling function. It's purpose
 * is to take incoming commands (packed into a buffer), unpack them,
 * and then call the corresponding host server's function to execute
 * them. Some commands involve only a single proc (i.e., the one
 * sending the command) and can be executed while we wait. In these cases,
 * the switchyard will construct and pack a reply buffer to be returned
 * to the sender.
 *
 * Other cases (either multi-process collective or cmds that require
 * an async reply) cannot generate an immediate reply. In these cases,
 * the reply buffer will be NULL. An appropriate callback function will
 * be called that will be responsible for eventually replying to the
 * calling processes.
 *
 * Should an error be encountered at any time within the switchyard, an
 * error reply buffer will be returned so that the caller can be notified,
 * thereby preventing the process from hanging. */
static pmix_status_t server_switchyard(pmix_peer_t *peer, uint32_t tag,
                                       pmix_buffer_t *buf)
{
    pmix_status_t rc=PMIX_ERR_NOT_SUPPORTED;
    int32_t cnt;
    pmix_cmd_t cmd;
    pmix_server_caddy_t *cd;
    pmix_proc_t proc;
    pmix_buffer_t *reply;
    pmix_regevents_info_t *reginfo;
    pmix_peer_events_info_t *prev;

    /* retrieve the cmd */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cmd, &cnt, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd pmix cmd %d from %s:%u",
                        cmd, peer->info->pname.nspace, peer->info->pname.rank);

    if (PMIX_REQ_CMD == cmd) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }
        PMIX_GDS_REGISTER_JOB_INFO(rc, peer, reply);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        PMIX_SERVER_QUEUE_REPLY(peer, tag, reply);
        return PMIX_SUCCESS; // don't reply twice
    }

    if (PMIX_ABORT_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_abort(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_COMMIT_CMD == cmd) {
        rc = pmix_server_commit(peer, buf);
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }
        PMIX_BFROPS_PACK(rc, peer, reply, &rc, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        PMIX_SERVER_QUEUE_REPLY(peer, tag, reply);
        return PMIX_SUCCESS; // don't reply twice
    }

    if (PMIX_FENCENB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_fence(cd, buf, modex_cbfunc, op_cbfunc))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_GETNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_get(buf, get_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_FINALIZE_CMD == cmd) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "recvd FINALIZE");
        peer->finalized = true;
        /* call the local server, if supported */
        if (NULL != pmix_host_server.client_finalized) {
            PMIX_GDS_CADDY(cd, peer, tag);
            (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
            proc.rank = peer->info->pname.rank;
            /* since the client is finalizing, remove them from any event
             * registrations they may still have on our list */
            PMIX_LIST_FOREACH(reginfo, &pmix_server_globals.events, pmix_regevents_info_t) {
                PMIX_LIST_FOREACH(prev, &reginfo->peers, pmix_peer_events_info_t) {
                    if (prev->peer == peer) {
                        pmix_list_remove_item(&reginfo->peers, &prev->super);
                        PMIX_RELEASE(prev);
                        break;
                    }
                }
            }
            /* now tell the host server */
            if (PMIX_SUCCESS != (rc = pmix_host_server.client_finalized(&proc, peer->info->server_object,
                                                                        op_cbfunc, cd))) {
                PMIX_RELEASE(cd);
            } else {
                /* don't reply to them ourselves - we will do so when the host
                 * server calls us back */
                return rc;
            }
        }
        /* turn off the recv event - we shouldn't hear anything
         * more from this proc */
        if (peer->recv_ev_active) {
            pmix_event_del(&peer->recv_event);
            peer->recv_ev_active = false;
        }
        /* let the network libraries cleanup */
        pmix_pnet.child_finalized(peer);
        return rc;
    }


    if (PMIX_PUBLISHNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_publish(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_LOOKUPNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_lookup(peer, buf, lookup_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_UNPUBLISHNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_unpublish(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_SPAWNNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_spawn(peer, buf, spawn_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }


    if (PMIX_CONNECTNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_connect(cd, buf, false, cnct_cbfunc);
        PMIX_RELEASE(cd);
        return rc;
    }

    if (PMIX_DISCONNECTNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_connect(cd, buf, true, cnct_cbfunc);
        PMIX_RELEASE(cd);
        return rc;
    }

    if (PMIX_REGEVENTS_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_register_events(peer, buf, regevents_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_DEREGEVENTS_CMD == cmd) {
        pmix_server_deregister_events(peer, buf);
        return PMIX_SUCCESS;
    }

    if (PMIX_NOTIFY_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_event_recvd_from_client(peer, buf, notifyerror_cbfunc, cd);
        return rc;
    }

    if (PMIX_QUERY_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_query(peer, buf, query_cbfunc, cd);
        return rc;
    }

    if (PMIX_LOG_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_log(peer, buf, op_cbfunc, cd);
        return rc;
    }

    if (PMIX_ALLOC_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_alloc(peer, buf, query_cbfunc, cd);
        return rc;
    }

    if (PMIX_JOB_CONTROL_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_job_ctrl(peer, buf, query_cbfunc, cd);
        return rc;
    }

    if (PMIX_MONITOR_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_monitor(peer, buf, query_cbfunc, cd);
        return rc;
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

static void server_message_handler(struct pmix_peer_t *pr,
                                   pmix_ptl_hdr_t *hdr,
                                   pmix_buffer_t *buf, void *cbdata)
{
    pmix_peer_t *peer = (pmix_peer_t*)pr;
    pmix_buffer_t *reply;
    pmix_status_t rc, ret;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "SWITCHYARD for %s:%u:%d",
                        peer->info->pname.nspace,
                        peer->info->pname.rank, peer->sd);

    ret = server_switchyard(peer, hdr->tag, buf);
    /* send the return, if there was an error returned */
    if (PMIX_SUCCESS != ret) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return;
        }
        PMIX_BFROPS_PACK(rc, pr, reply, &ret, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        PMIX_SERVER_QUEUE_REPLY(peer, hdr->tag, reply);
    }
}

static inline int _my_client(const char *nspace, pmix_rank_t rank)
{
    pmix_peer_t *peer;
    int i;
    int local = 0;

    for (i = 0; i < pmix_server_globals.clients.size; i++) {
        if (NULL != (peer = (pmix_peer_t *)pmix_pointer_array_get_item(&pmix_server_globals.clients, i))) {
            if (0 == strcmp(peer->info->pname.nspace, nspace) && peer->info->pname.rank == rank) {
                local = 1;
                break;
            }
        }
    }

    return local;
}
