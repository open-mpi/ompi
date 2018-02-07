/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016-2017 Mellanox Technologies, Inc.
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
#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
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
#include "src/mca/bfrops/bfrops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"

#include "pmix_server_ops.h"

pmix_server_module_t pmix_host_server = {0};

pmix_status_t pmix_server_abort(pmix_peer_t *peer, pmix_buffer_t *buf,
                                pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    int status;
    char *msg;
    size_t nprocs;
    pmix_proc_t *procs = NULL;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_server_globals.base_output, "recvd ABORT");

    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    /* unpack the message */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &msg, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    /* unpack the number of procs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* unpack any provided procs - these are the procs the caller
     * wants aborted */
    if (0 < nprocs) {
        PMIX_PROC_CREATE(procs, nprocs);
        if (NULL == procs) {
            if (NULL != msg) {
                free(msg);
            }
            return PMIX_ERR_NOMEM;
        }
        cnt = nprocs;
        PMIX_BFROPS_UNPACK(rc, peer, buf, procs, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            if (NULL != msg) {
                free(msg);
            }
            return rc;
        }
    }

    /* let the local host's server execute it */
    if (NULL != pmix_host_server.abort) {
        (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
        proc.rank = peer->info->pname.rank;
        rc = pmix_host_server.abort(&proc, peer->info->server_object, status, msg,
                                    procs, nprocs, cbfunc, cbdata);
    } else {
        rc = PMIX_ERR_NOT_SUPPORTED;
        /* release the caller */
        if (NULL != cbfunc) {
            cbfunc(rc, cbdata);
        }
    }
    PMIX_PROC_FREE(procs, nprocs);

    /* the client passed this msg to us so we could give
     * it to the host server - we are done with it now */
    if (NULL != msg) {
        free(msg);
    }

    return rc;
}

pmix_status_t pmix_server_commit(pmix_peer_t *peer, pmix_buffer_t *buf)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_buffer_t b2, pbkt;
    pmix_kval_t *kp;
    pmix_scope_t scope;
    pmix_nspace_t *nptr;
    pmix_rank_info_t *info;
    pmix_proc_t proc;
    pmix_dmdx_remote_t *dcd, *dcdnext;
    char *data;
    size_t sz;
    pmix_cb_t cb;

    /* shorthand */
    info = peer->info;
    nptr = peer->nptr;
    (void)strncpy(proc.nspace, nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = info->pname.rank;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "%s:%d EXECUTE COMMIT FOR %s:%d",
                        pmix_globals.myid.nspace,
                        pmix_globals.myid.rank,
                        nptr->nspace, info->pname.rank);

    /* this buffer will contain one or more buffers, each
     * representing a different scope. These need to be locally
     * stored separately so we can provide required data based
     * on the requestor's location */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &scope, &cnt, PMIX_SCOPE);
    while (PMIX_SUCCESS == rc) {
        /* unpack and store the blob */
        cnt = 1;
        PMIX_CONSTRUCT(&b2, pmix_buffer_t);
        PMIX_BFROPS_ASSIGN_TYPE(peer, &b2);
        PMIX_BFROPS_UNPACK(rc, peer, buf, &b2, &cnt, PMIX_BUFFER);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        /* unpack the buffer and store the values - we store them
         * in this peer's native GDS component so that other local
         * procs from that nspace can access it */
        kp = PMIX_NEW(pmix_kval_t);
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, peer, &b2, kp, &cnt, PMIX_KVAL);
        while (PMIX_SUCCESS == rc) {
            if( PMIX_LOCAL == scope || PMIX_GLOBAL == scope){
                PMIX_GDS_STORE_KV(rc, peer, &proc, scope, kp);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(kp);
                    PMIX_DESTRUCT(&b2);
                    return rc;
                }
            }
            if (PMIX_REMOTE == scope || PMIX_GLOBAL == scope) {
                PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &proc, scope, kp);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(kp);
                    PMIX_DESTRUCT(&b2);
                    return rc;
                }
            }
            PMIX_RELEASE(kp);  // maintain accounting
            kp = PMIX_NEW(pmix_kval_t);
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, peer, &b2, kp, &cnt, PMIX_KVAL);

        }
        PMIX_RELEASE(kp);   // maintain accounting
        PMIX_DESTRUCT(&b2);
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &scope, &cnt, PMIX_SCOPE);
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    rc = PMIX_SUCCESS;
    /* mark us as having successfully received a blob from this proc */
    info->modex_recvd = true;

    /* update the commit counter */
    peer->commit_cnt++;

    /* see if anyone remote is waiting on this data - could be more than one */
    PMIX_LIST_FOREACH_SAFE(dcd, dcdnext, &pmix_server_globals.remote_pnd, pmix_dmdx_remote_t) {
        if (0 != strncmp(dcd->cd->proc.nspace, nptr->nspace, PMIX_MAX_NSLEN)) {
            continue;
        }
        if (dcd->cd->proc.rank == info->pname.rank) {
           /* we can now fulfill this request - collect the
             * remote/global data from this proc - note that there
             * may not be a contribution */
            data = NULL;
            sz = 0;
            PMIX_CONSTRUCT(&cb, pmix_cb_t);
            cb.proc = &proc;
            cb.scope = PMIX_REMOTE;
            cb.copy = true;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
            if (PMIX_SUCCESS == rc) {
                /* package it up */
                PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                PMIX_LIST_FOREACH(kp, &cb.kvs, pmix_kval_t) {
                    /* we pack this in our native BFROPS form as it
                     * will be sent to another daemon */
                    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, kp, 1, PMIX_KVAL);
                }
                PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
            }
            PMIX_DESTRUCT(&cb);
            /* execute the callback */
            dcd->cd->cbfunc(rc, data, sz, dcd->cd->cbdata);
            if (NULL != data) {
                free(data);
            }
            /* we have finished this request */
            pmix_list_remove_item(&pmix_server_globals.remote_pnd, &dcd->super);
            PMIX_RELEASE(dcd);
        }
    }
    /* see if anyone local is waiting on this data- could be more than one */
    rc = pmix_pending_resolve(nptr, info->pname.rank, PMIX_SUCCESS, NULL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    return rc;
}

/* get an existing object for tracking LOCAL participation in a collective
 * operation such as "fence". The only way this function can be
 * called is if at least one local client process is participating
 * in the operation. Thus, we know that at least one process is
 * involved AND has called the collective operation.
 *
 * NOTE: the host server *cannot* call us with a collective operation
 * as there is no mechanism by which it can do so. We call the host
 * server only after all participating local procs have called us.
 * So it is impossible for us to be called with a collective without
 * us already knowing about all local participants.
 *
 * procs - the array of procs participating in the collective,
 *         regardless of location
 * nprocs - the number of procs in the array
 */
static pmix_server_trkr_t* get_tracker(pmix_proc_t *procs,
                                       size_t nprocs, pmix_cmd_t type)
{
    pmix_server_trkr_t *trk;
    size_t i, j;
    size_t matches;

    pmix_output_verbose(5, pmix_server_globals.base_output,
                        "get_tracker called with %d procs", (int)nprocs);

    /* bozo check - should never happen outside of programmer error */
    if (NULL == procs) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }

    /* there is no shortcut way to search the trackers - all
     * we can do is perform a brute-force search. Fortunately,
     * it is highly unlikely that there will be more than one
     * or two active at a time, and they are most likely to
     * involve only a single proc with WILDCARD rank - so this
     * shouldn't take long */
    PMIX_LIST_FOREACH(trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
        /* Collective operation if unique identified by
         * the set of participating processes and the type of collective
         */
        if (nprocs != trk->npcs) {
            continue;
        }
        if (type != trk->type) {
            continue;
        }
        matches = 0;
        for (i=0; i < nprocs; i++) {
            /* the procs may be in different order, so we have
             * to do an exhaustive search */
            for (j=0; j < trk->npcs; j++) {
                if (0 == strcmp(procs[i].nspace, trk->pcs[j].nspace) &&
                    procs[i].rank == trk->pcs[j].rank) {
                    ++matches;
                    break;
                }
            }
        }
        if (trk->npcs == matches) {
            return trk;
        }
    }
    /* No tracker was found */
    return NULL;
}

/* create a new object for tracking LOCAL participation in a collective
 * operation such as "fence". The only way this function can be
 * called is if at least one local client process is participating
 * in the operation. Thus, we know that at least one process is
 * involved AND has called the collective operation.
 *
 * NOTE: the host server *cannot* call us with a collective operation
 * as there is no mechanism by which it can do so. We call the host
 * server only after all participating local procs have called us.
 * So it is impossible for us to be called with a collective without
 * us already knowing about all local participants.
 *
 * procs - the array of procs participating in the collective,
 *         regardless of location
 * nprocs - the number of procs in the array
 */
static pmix_server_trkr_t* new_tracker(pmix_proc_t *procs,
                                       size_t nprocs, pmix_cmd_t type)
{
    pmix_server_trkr_t *trk;
    size_t i;
    bool all_def;
    pmix_nspace_t *nptr, *ns;
    pmix_rank_info_t *info;

    pmix_output_verbose(5, pmix_server_globals.base_output,
                        "new_tracker called with %d procs", (int)nprocs);

    /* bozo check - should never happen outside of programmer error */
    if (NULL == procs) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }

    pmix_output_verbose(5, pmix_server_globals.base_output,
                        "adding new tracker with %d procs", (int)nprocs);

    /* this tracker is new - create it */
    trk = PMIX_NEW(pmix_server_trkr_t);
    if (NULL == trk) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return NULL;
    }

    /* copy the procs */
    PMIX_PROC_CREATE(trk->pcs, nprocs);
    if (NULL == trk->pcs) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(trk);
        return NULL;
    }
    trk->npcs = nprocs;
    trk->type = type;

    all_def = true;
    for (i=0; i < nprocs; i++) {
        (void)strncpy(trk->pcs[i].nspace, procs[i].nspace, PMIX_MAX_NSLEN);
        trk->pcs[i].rank = procs[i].rank;
        if (!all_def) {
            continue;
        }
        /* is this nspace known to us? */
        nptr = NULL;
        PMIX_LIST_FOREACH(ns, &pmix_server_globals.nspaces, pmix_nspace_t) {
            if (0 == strcmp(procs[i].nspace, ns->nspace)) {
                nptr = ns;
                break;
            }
        }
        if (NULL == nptr) {
            /* cannot be a local proc */
            pmix_output_verbose(5, pmix_server_globals.base_output,
                                "new_tracker: unknown nspace %s",
                                procs[i].nspace);
            continue;
        }
        /* have all the clients for this nspace been defined? */
        if (!nptr->all_registered) {
            /* nope, so no point in going further on this one - we'll
             * process it once all the procs are known */
            all_def = false;
            pmix_output_verbose(5, pmix_server_globals.base_output,
                                "new_tracker: all clients not registered nspace %s",
                                procs[i].nspace);
            /* we have to continue processing the list of procs
             * to setup the trk->pcs array, so don't break out
             * of the loop */
        }
        /* is this one of my local ranks? */
        PMIX_LIST_FOREACH(info, &nptr->ranks, pmix_rank_info_t) {
            if (procs[i].rank == info->pname.rank ||
                PMIX_RANK_WILDCARD == procs[i].rank) {
                    pmix_output_verbose(5, pmix_server_globals.base_output,
                                        "adding local proc %s.%d to tracker",
                                        info->pname.nspace, info->pname.rank);
                /* track the count */
                ++trk->nlocal;
                if (PMIX_RANK_WILDCARD != procs[i].rank) {
                    break;
                }
            }
        }
    }
    if (all_def) {
        trk->def_complete = true;
    }
    pmix_list_append(&pmix_server_globals.collectives, &trk->super);
    return trk;
}

static void fence_timeout(int sd, short args, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;

    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "ALERT: fence timeout fired");

    /* execute the provided callback function with the error */
    if (NULL != cd->trk->modexcbfunc) {
        cd->trk->modexcbfunc(PMIX_ERR_TIMEOUT, NULL, 0, cd->trk, NULL, NULL);
        return;  // the cbfunc will have cleaned up the tracker
    }
    cd->event_active = false;
    /* remove it from the list */
    pmix_list_remove_item(&cd->trk->local_cbs, &cd->super);
    PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_fence(pmix_server_caddy_t *cd,
                                pmix_buffer_t *buf,
                                pmix_modex_cbfunc_t modexcbfunc,
                                pmix_op_cbfunc_t opcbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    size_t nprocs;
    pmix_proc_t *procs=NULL, pcs;
    bool collect_data = false;
    pmix_server_trkr_t *trk;
    char *data = NULL;
    size_t sz = 0;
    pmix_buffer_t bucket, pbkt;
    pmix_server_caddy_t *scd;
    pmix_cb_t cb;
    pmix_kval_t *kv;
    pmix_byte_object_t bo;
    pmix_info_t *info = NULL;
    size_t ninfo=0, n;
    struct timeval tv = {0, 0};

    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "recvd FENCE");

    if (NULL == pmix_host_server.fence_nb) {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the number of procs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "recvd fence from %s:%u with %d procs",
                        cd->peer->info->pname.nspace, cd->peer->info->pname.rank, (int)nprocs);
    /* there must be at least one as the client has to at least provide
     * their own namespace */
    if (nprocs < 1) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* create space for the procs */
    PMIX_PROC_CREATE(procs, nprocs);
    if (NULL == procs) {
        return PMIX_ERR_NOMEM;
    }
    /* unpack the procs */
    cnt = nprocs;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, procs, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        goto cleanup;
    }

    /* unpack the number of provided info structs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        if (NULL == info) {
            PMIX_PROC_FREE(procs, nprocs);
            return PMIX_ERR_NOMEM;
        }
        /* unpack the info */
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            goto cleanup;
        }
        /* see if we are to collect data or enforce a timeout - we don't internally care
         * about any other directives */
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_COLLECT_DATA)) {
                collect_data = true;
            } else if (0 == strncmp(info[n].key, PMIX_TIMEOUT, PMIX_MAX_KEYLEN)) {
                tv.tv_sec = info[n].value.data.uint32;
            }
        }
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(procs, nprocs, PMIX_FENCENB_CMD))) {
        /* If no tracker was found - create and initialize it once */
        if (NULL == (trk = new_tracker(procs, nprocs, PMIX_FENCENB_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            /* DO NOT HANG */
            if (NULL != opcbfunc) {
                opcbfunc(PMIX_ERROR, cd);
            }
            rc = PMIX_ERROR;
            goto cleanup;
        }
        trk->type = PMIX_FENCENB_CMD;
        trk->modexcbfunc = modexcbfunc;
       /* mark if they want the data back */
        if (collect_data) {
            trk->collect_type = PMIX_COLLECT_YES;
        } else {
            trk->collect_type = PMIX_COLLECT_NO;
        }
    } else {
        switch (trk->collect_type) {
        case PMIX_COLLECT_NO:
            if (collect_data) {
                trk->collect_type = PMIX_COLLECT_INVALID;
            }
            break;
        case PMIX_COLLECT_YES:
            if (!collect_data) {
                trk->collect_type = PMIX_COLLECT_INVALID;
            }
            break;
        default:
            break;
        }
    }
    /* we only save the info structs from the first caller
     * who provides them - it is a user error to provide
     * different values from different participants */
    if (NULL == trk->info) {
        trk->info = info;
        trk->ninfo = ninfo;
    } else {
        /* cleanup */
        PMIX_INFO_FREE(info, ninfo);
        info = NULL;
    }

    /* add this contributor to the tracker so they get
     * notified when we are done */
    pmix_list_append(&trk->local_cbs, &cd->super);
    /* if a timeout was specified, set it */
    if (0 < tv.tv_sec) {
        PMIX_RETAIN(trk);
        cd->trk = trk;
        pmix_event_evtimer_set(pmix_globals.evbase, &cd->ev,
                               fence_timeout, cd);
        pmix_event_evtimer_add(&cd->ev, &tv);
        cd->event_active = true;
    }

    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the barrier
     * across all participants has been completed */
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        pmix_output_verbose(2, pmix_server_globals.base_output,
                            "fence complete");
        /* if the user asked us to collect data, then we have
         * to provide any locally collected data to the host
         * server so they can circulate it - only take data
         * from the specified procs as not everyone is necessarily
         * participating! And only take data intended for remote
         * or global distribution */

        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);

        /* mark the collection type so we can check on the
         * receiving end that all participants did the same */
        unsigned char tmp = (unsigned char)trk->collect_type;
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                         &tmp, 1, PMIX_BYTE);

        if (PMIX_COLLECT_YES == trk->collect_type) {
            pmix_output_verbose(2, pmix_server_globals.fence_output,
                                "fence - assembling data");
            PMIX_LIST_FOREACH(scd, &trk->local_cbs, pmix_server_caddy_t) {
                /* get any remote contribution - note that there
                 * may not be a contribution */
                (void)strncpy(pcs.nspace, scd->peer->info->pname.nspace, PMIX_MAX_NSLEN);
                pcs.rank = scd->peer->info->pname.rank;
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                cb.proc = &pcs;
                cb.scope = PMIX_REMOTE;
                cb.copy = true;
                PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
                if (PMIX_SUCCESS == rc) {
                    PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                    /* pack the proc so we know the source */
                    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt,
                                     &pcs, 1, PMIX_PROC);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        goto cleanup;
                    }
                    /* pack the returned kval's */
                    PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
                        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, kv, 1, PMIX_KVAL);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_ERROR_LOG(rc);
                            PMIX_DESTRUCT(&cb);
                            goto cleanup;
                        }
                    }
                    /* extract the blob */
                    PMIX_UNLOAD_BUFFER(&pbkt, bo.bytes, bo.size);
                    PMIX_DESTRUCT(&pbkt);
                    /* pack the returned blob */
                    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                                     &bo, 1, PMIX_BYTE_OBJECT);
                    PMIX_BYTE_OBJECT_DESTRUCT(&bo);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        goto cleanup;
                    }
                }
                PMIX_DESTRUCT(&cb);
            }
        }
        /* because the remote servers have to unpack things
         * in chunks, we have to pack the bucket as a single
         * byte object to allow remote unpack */
        PMIX_UNLOAD_BUFFER(&bucket, bo.bytes, bo.size);
        PMIX_DESTRUCT(&bucket);
        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                         &bo, 1, PMIX_BYTE_OBJECT);
        PMIX_BYTE_OBJECT_DESTRUCT(&bo);  // releases the data
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&cb);
            goto cleanup;
        }
        /* now unload the blob and pass it upstairs */
        PMIX_UNLOAD_BUFFER(&bucket, data, sz);
        PMIX_DESTRUCT(&bucket);
        pmix_host_server.fence_nb(trk->pcs, trk->npcs,
                                  trk->info, trk->ninfo,
                                  data, sz, trk->modexcbfunc, trk);
    }

  cleanup:
    PMIX_PROC_FREE(procs, nprocs);
    return rc;
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;

    if (NULL != cd->keys) {
        pmix_argv_free(cd->keys);
    }
    if (NULL != cd->codes) {
        free(cd->codes);
    }
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_publish(pmix_peer_t *peer,
                                  pmix_buffer_t *buf,
                                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_status_t rc;
    int32_t cnt;
    size_t ninfo;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_server_globals.pub_output,
                        "recvd PUBLISH");

    if (NULL == pmix_host_server.publish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &uid, &cnt, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of info objects */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* we will be adding one for the user id */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    cd->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    if (NULL == cd->info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* unpack the array of info objects */
    if (0 < cd->ninfo) {
        cnt=cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    (void)strncpy(cd->info[cd->ninfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo-1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo-1].value.data.uint32 = uid;

    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;
    rc = pmix_host_server.publish(&proc, cd->info, cd->ninfo, opcbfunc, cd);

  cleanup:
    if (PMIX_SUCCESS != rc) {
        if (NULL != cd->info) {
            PMIX_INFO_FREE(cd->info, cd->ninfo);
        }
        PMIX_RELEASE(cd);
    }
    return rc;
}

static void lkcbfunc(pmix_status_t status,
                     pmix_pdata_t data[], size_t ndata,
                     void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;

    /* cleanup the caddy */
    if (NULL != cd->keys) {
        pmix_argv_free(cd->keys);
    }
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }

    /* return the results */
    if (NULL != cd->lkcbfunc) {
        cd->lkcbfunc(status, data, ndata, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}
pmix_status_t pmix_server_lookup(pmix_peer_t *peer,
                                 pmix_buffer_t *buf,
                                 pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    int32_t cnt;
    pmix_status_t rc;
    size_t nkeys, i;
    char *sptr;
    size_t ninfo;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_server_globals.pub_output,
                        "recvd LOOKUP");

    if (NULL == pmix_host_server.lookup) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &uid, &cnt, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of keys */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &nkeys, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* setup the caddy */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->lkcbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* unpack the array of keys */
    for (i=0; i < nkeys; i++) {
        cnt=1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &sptr, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        pmix_argv_append_nosize(&cd->keys, sptr);
        free(sptr);
    }
    /* unpack the number of info objects */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* we will be adding one for the user id */
    cd->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    if (NULL == cd->info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* unpack the array of info objects */
    if (0 < ninfo) {
        cnt=ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    (void)strncpy(cd->info[cd->ninfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo-1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo-1].value.data.uint32 = uid;

    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;
    rc = pmix_host_server.lookup(&proc, cd->keys, cd->info, cd->ninfo, lkcbfunc, cd);

  cleanup:
    if (PMIX_SUCCESS != rc) {
        if (NULL != cd->keys) {
            pmix_argv_free(cd->keys);
        }
        if (NULL != cd->info) {
            PMIX_INFO_FREE(cd->info, cd->ninfo);
        }
        PMIX_RELEASE(cd);
    }
    return rc;
}

pmix_status_t pmix_server_unpublish(pmix_peer_t *peer,
                                    pmix_buffer_t *buf,
                                    pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    int32_t cnt;
    pmix_status_t rc;
    size_t i, nkeys, ninfo;
    char *sptr;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_server_globals.pub_output,
                        "recvd UNPUBLISH");

    if (NULL == pmix_host_server.unpublish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &uid, &cnt, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of keys */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &nkeys, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* setup the caddy */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* unpack the array of keys */
    for (i=0; i < nkeys; i++) {
        cnt=1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &sptr, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        pmix_argv_append_nosize(&cd->keys, sptr);
        free(sptr);
    }
    /* unpack the number of info objects */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* we will be adding one for the user id */
    cd->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    if (NULL == cd->info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* unpack the array of info objects */
    if (0 < ninfo) {
        cnt=ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    (void)strncpy(cd->info[cd->ninfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo-1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo-1].value.data.uint32 = uid;

    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;
    rc = pmix_host_server.unpublish(&proc, cd->keys, cd->info, cd->ninfo, opcbfunc, cd);

  cleanup:
    if (PMIX_SUCCESS != rc) {
        if (NULL != cd->keys) {
            pmix_argv_free(cd->keys);
        }
        if (NULL != cd->info) {
            PMIX_INFO_FREE(cd->info, cd->ninfo);
        }
        PMIX_RELEASE(cd);
    }
    return rc;
}

static void spcbfunc(pmix_status_t status,
                     char nspace[], void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;

    /* cleanup the caddy */
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    if (NULL != cd->apps) {
        PMIX_APP_CREATE(cd->apps, cd->napps);
    }
    if (NULL != cd->spcbfunc) {
        cd->spcbfunc(status, nspace, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_spawn(pmix_peer_t *peer,
                                pmix_buffer_t *buf,
                                pmix_spawn_cbfunc_t cbfunc,
                                void *cbdata)
{
    pmix_setup_caddy_t *cd;
    int32_t cnt;
    pmix_status_t rc;
    pmix_proc_t proc;
    size_t ninfo;

    pmix_output_verbose(2, pmix_server_globals.spawn_output,
                        "recvd SPAWN");

    if (NULL == pmix_host_server.spawn) {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->spcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* unpack the number of job-level directives */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }
    /* always add one directive that indicates whether the requestor
     * is a tool or client */
    cd->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    if (NULL == cd->info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }

    /* unpack the array of directives */
    if (0 < ninfo) {
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    /* add the directive to the end */
    if (PMIX_PROC_IS_TOOL(peer)) {
        PMIX_INFO_LOAD(&cd->info[ninfo], PMIX_REQUESTOR_IS_TOOL, NULL, PMIX_BOOL);
    } else {
        PMIX_INFO_LOAD(&cd->info[ninfo], PMIX_REQUESTOR_IS_CLIENT, NULL, PMIX_BOOL);
    }

    /* unpack the number of apps */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->napps, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* unpack the array of apps */
    if (0 < cd->napps) {
        PMIX_APP_CREATE(cd->apps, cd->napps);
        if (NULL == cd->apps) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        cnt = cd->napps;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->apps, &cnt, PMIX_APP);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;
    rc = pmix_host_server.spawn(&proc, cd->info, cd->ninfo, cd->apps, cd->napps, spcbfunc, cd);

  cleanup:
    if (PMIX_SUCCESS != rc) {
        if (NULL != cd->info) {
            PMIX_INFO_FREE(cd->info, cd->ninfo);
        }
        if (NULL != cd->apps) {
            PMIX_APP_FREE(cd->apps, cd->napps);
        }
        PMIX_RELEASE(cd);
    }
    return rc;
}

pmix_status_t pmix_server_disconnect(pmix_server_caddy_t *cd,
                                     pmix_buffer_t *buf,
                                     pmix_op_cbfunc_t cbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_info_t *info = NULL;
    size_t ninfo;
    pmix_server_trkr_t *trk;
    char *nptr;
    pmix_proc_t proc;
    pmix_nspace_t *nsptr, *nspace;

    if (NULL == pmix_host_server.disconnect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the nspace they want to be disconnected from */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &nptr, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    memset(proc.nspace, 0, PMIX_MAX_NSLEN+1);
    (void)strncpy(proc.nspace, nptr, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;
    free(nptr);

    /* unpack the number of provided info structs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        if (NULL == info) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        /* unpack the info */
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            goto cleanup;
        }
    }

    /* we must already know about this nspace, so find its record */
    nspace = NULL;
    PMIX_LIST_FOREACH(nsptr, &pmix_server_globals.nspaces, pmix_nspace_t) {
        if (0 == strncmp(nsptr->nspace, proc.nspace, PMIX_MAX_NSLEN)) {
            nspace = nsptr;
            break;
        }
    }
    if (NULL == nspace) {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        goto cleanup;
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(&proc, 1, PMIX_DISCONNECTNB_CMD))) {
        /* we don't have this tracker yet, so get a new one */
        if (NULL == (trk = new_tracker(&proc, 1, PMIX_DISCONNECTNB_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            /* DO NOT HANG */
            if (NULL != cbfunc) {
                cbfunc(PMIX_ERROR, cd);
            }
            rc = PMIX_ERROR;
            goto cleanup;
        }
        trk->nlocal = nspace->nlocalprocs;
        trk->op_cbfunc = cbfunc;
    }
    (void)strncpy(trk->pname.nspace, proc.nspace, PMIX_MAX_NSLEN);
    trk->pname.rank = PMIX_RANK_WILDCARD;

    /* if the info keys have not been provided yet, pass
     * them along here */
    if (NULL == trk->info && NULL != info) {
        trk->info = info;
        trk->ninfo = ninfo;
        info = NULL;
        ninfo = 0;
    }

    /* add this contributor to the tracker so they get
     * notified when we are done */
    PMIX_RETAIN(cd);  // prevent the caddy from being released when we return
    pmix_list_append(&trk->local_cbs, &cd->super);
    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the [dis]connect
     * across all participants has been completed */
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        rc = pmix_host_server.disconnect(trk->pname.nspace, trk->info, trk->ninfo, cbfunc, trk);
    } else {
        rc = PMIX_SUCCESS;
    }

  cleanup:
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    return rc;
}

static void connect_timeout(int sd, short args, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;

    pmix_output_verbose(2, pmix_server_globals.connect_output,
                        "ALERT: connect timeout fired");

    /* execute the provided callback function with the error */
    if (NULL != cd->trk->cnct_cbfunc) {
        cd->trk->cnct_cbfunc(PMIX_ERR_TIMEOUT, NULL, PMIX_RANK_UNDEF, cd->trk);
        return;  // the cbfunc will have cleaned up the tracker
    }
    cd->event_active = false;
    /* remove it from the list */
    pmix_list_remove_item(&cd->trk->local_cbs, &cd->super);
    PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_connect(pmix_server_caddy_t *cd,
                                  pmix_buffer_t *buf,
                                  pmix_connect_cbfunc_t cbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_proc_t *procs = NULL;
    pmix_info_t *info = NULL;
    size_t nprocs, ninfo, n;
    pmix_server_trkr_t *trk;
    struct timeval tv = {0, 0};

    pmix_output_verbose(2, pmix_server_globals.connect_output,
                        "recvd CONNECT from peer %s:%d",
                        cd->peer->info->pname.nspace,
                        cd->peer->info->pname.rank);

    if (NULL == pmix_host_server.connect) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the number of procs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* there must be at least one proc - we do not allow the client
     * to send us NULL proc as the server has no idea what to do
     * with that situation. Instead, the client should at least send
     * us their own namespace for the use-case where the connection
     * spans all procs in that namespace */
    if (nprocs < 1) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        rc = PMIX_ERR_BAD_PARAM;
        goto cleanup;
    }

    /* unpack the procs */
    PMIX_PROC_CREATE(procs, nprocs);
    if (NULL == procs) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    cnt = nprocs;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, procs, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* unpack the number of provided info structs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        if (NULL == info) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        /* unpack the info */
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            goto cleanup;
        }
        /* check for a timeout */
        for (n=0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_TIMEOUT, PMIX_MAX_KEYLEN)) {
                tv.tv_sec = info[n].value.data.uint32;
                break;
            }
        }
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(procs, nprocs, PMIX_CONNECTNB_CMD))) {
        /* we don't have this tracker yet, so get a new one */
        if (NULL == (trk = new_tracker(procs, nprocs, PMIX_CONNECTNB_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            /* DO NOT HANG */
            if (NULL != cbfunc) {
                cbfunc(PMIX_ERROR, NULL, PMIX_RANK_UNDEF, cd);
            }
            rc = PMIX_ERROR;
            goto cleanup;
        }
        trk->cnct_cbfunc = cbfunc;
    }

    /* if the info keys have not been provided yet, pass
     * them along here */
    if (NULL == trk->info && NULL != info) {
        trk->info = info;
        trk->ninfo = ninfo;
        info = NULL;
        ninfo = 0;
    }

    /* add this contributor to the tracker so they get
     * notified when we are done */
    PMIX_RETAIN(cd);  // prevent the caddy from being released when we return
    pmix_list_append(&trk->local_cbs, &cd->super);
    /* if a timeout was specified, set it */
    if (0 < tv.tv_sec) {
        PMIX_RETAIN(trk);
        cd->trk = trk;
        pmix_event_evtimer_set(pmix_globals.evbase, &cd->ev,
                               connect_timeout, cd);
        pmix_event_evtimer_add(&cd->ev, &tv);
        cd->event_active = true;
    }

    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the [dis]connect
     * across all participants has been completed */
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        rc = pmix_host_server.connect(trk->pcs, trk->npcs, trk->info, trk->ninfo, cbfunc, trk);
    } else {
        rc = PMIX_SUCCESS;
    }

  cleanup:
    if (NULL != procs) {
        PMIX_PROC_FREE(procs, nprocs);
    }
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    return rc;
}

pmix_status_t pmix_server_register_events(pmix_peer_t *peer,
                                          pmix_buffer_t *buf,
                                          pmix_op_cbfunc_t cbfunc,
                                          void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_status_t *codes = NULL;
    pmix_info_t *info = NULL;
    size_t ninfo=0, ncodes, n, k;
    pmix_regevents_info_t *reginfo;
    pmix_peer_events_info_t *prev;
    pmix_notify_caddy_t *cd;
    pmix_setup_caddy_t *scd;
    int i;
    bool enviro_events = false;
    bool found, matched;
    pmix_buffer_t *relay;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;

    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "recvd register events");

    /* unpack the number of codes */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ncodes, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of codes */
    if (0 < ncodes) {
        codes = (pmix_status_t*)malloc(ncodes * sizeof(pmix_status_t));
        if (NULL == codes) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        cnt=ncodes;
        PMIX_BFROPS_UNPACK(rc, peer, buf, codes, &cnt, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* unpack the number of info objects */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of info objects */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        if (NULL == info) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        cnt=ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* see if they asked for enviro events */
    for (n=0; n < ninfo; n++) {
        if (0 == strcmp(info[n].key, PMIX_EVENT_ENVIRO_LEVEL)) {
            enviro_events = PMIX_INFO_TRUE(&info[n]);
            break;
        }
    }

    /* if they asked for enviro events, and our host doesn't support
     * register_events, then we cannot meet the request */
    if (enviro_events && NULL == pmix_host_server.register_events) {
        enviro_events = false;
        rc = PMIX_ERR_NOT_SUPPORTED;
        goto cleanup;
    }

    /* store the event registration info so we can call the registered
     * client when the server notifies the event */
    k=0;
    do {
        found = false;
        PMIX_LIST_FOREACH(reginfo, &pmix_server_globals.events, pmix_regevents_info_t) {
            if (NULL == codes) {
                if (PMIX_MAX_ERR_CONSTANT == reginfo->code) {
                    /* both are default handlers */
                    found = true;
                    break;
                } else {
                    continue;
                }
            } else {
                if (PMIX_MAX_ERR_CONSTANT == reginfo->code) {
                    continue;
                } else if (codes[k] == reginfo->code) {
                    found = true;
                    break;
                }
            }
        }
        if (found) {
            /* found it - add this peer if we don't already have it */
            found = false;
            PMIX_LIST_FOREACH(prev, &reginfo->peers, pmix_peer_events_info_t) {
                if (prev->peer == peer) {
                    /* already have it */
                    rc = PMIX_SUCCESS;
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* get here if we don't already have this peer */
                prev = PMIX_NEW(pmix_peer_events_info_t);
                if (NULL == prev) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                PMIX_RETAIN(peer);
                prev->peer = peer;
                prev->enviro_events = enviro_events;
                pmix_list_append(&reginfo->peers, &prev->super);
            }
        } else {
            /* if we get here, then we didn't find an existing registration for this code */
            reginfo = PMIX_NEW(pmix_regevents_info_t);
            if (NULL == reginfo) {
                rc = PMIX_ERR_NOMEM;
                goto cleanup;
            }
            if (NULL == codes) {
                reginfo->code = PMIX_MAX_ERR_CONSTANT;
            } else {
                reginfo->code = codes[k];
            }
            pmix_list_append(&pmix_server_globals.events, &reginfo->super);
            prev = PMIX_NEW(pmix_peer_events_info_t);
            if (NULL == prev) {
                rc = PMIX_ERR_NOMEM;
                goto cleanup;
            }
            PMIX_RETAIN(peer);
            prev->peer = peer;
            prev->enviro_events = enviro_events;
            pmix_list_append(&reginfo->peers, &prev->super);
        }
        ++k;
    } while (k < ncodes);

    /* if they asked for enviro events, call the local server */
    if (enviro_events) {
        /* need to ensure the arrays don't go away until after the
         * host RM is done with them */
        scd = PMIX_NEW(pmix_setup_caddy_t);
        if (NULL == scd) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        if (NULL != codes) {
            scd->codes = (pmix_status_t*)malloc(ncodes * sizeof(pmix_status_t));
            if (NULL == scd->codes) {
                rc = PMIX_ERR_NOMEM;
                PMIX_RELEASE(scd);
                goto cleanup;
            }
            memcpy(scd->codes, codes, ncodes * sizeof(pmix_status_t));
            scd->ncodes = ncodes;
        }
        if (NULL != info) {
            PMIX_INFO_CREATE(scd->info, ninfo);
            if (NULL == scd->info) {
                rc = PMIX_ERR_NOMEM;
                if (NULL != scd->codes) {
                    free(scd->codes);
                }
                PMIX_RELEASE(scd);
                goto cleanup;
            }
            /* copy the info across */
            for (n=0; n < ninfo; n++) {
                PMIX_INFO_XFER(&scd->info[n], &info[n]);
            }
            scd->ninfo = ninfo;
        }
        scd->opcbfunc = cbfunc;
        scd->cbdata = cbdata;
        if (PMIX_SUCCESS != (rc = pmix_host_server.register_events(scd->codes, scd->ncodes, scd->info, scd->ninfo, opcbfunc, scd))) {
            pmix_output_verbose(2, pmix_server_globals.event_output,
                                 "server register events: host server reg events returned rc =%d", rc);
            if (NULL != scd->codes) {
                free(scd->codes);
            }
            if (NULL != scd->info) {
                PMIX_INFO_FREE(scd->info, scd->ninfo);
            }
            PMIX_RELEASE(scd);
        } else {
            goto check;
        }
    }

  cleanup:
    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "server register events: ninfo =%lu rc =%d", ninfo, rc);
    /* be sure to execute the callback */
    if (NULL != cbfunc) {
        cbfunc(rc, cbdata);
    }
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    if (PMIX_SUCCESS != rc) {
        if (NULL != codes) {
            free(codes);
        }
        return rc;
    }

  check:
    /* check if any matching notifications have been cached */
    for (i=0; i < pmix_globals.notifications.size; i++) {
        if (NULL == (cd = (pmix_notify_caddy_t*)pmix_ring_buffer_poke(&pmix_globals.notifications, i))) {
            break;
        }
        found = false;
        if (NULL == codes) {
            /* they registered a default event handler - always matches */
            found = true;
        } else {
            for (k=0; k < ncodes; k++) {
                if (codes[k] == cd->status) {
                    found = true;
                    break;
                }
            }
        }
        if (found) {
           /* if we were given specific targets, check if this is one */
            if (NULL != cd->targets) {
                matched = false;
                for (n=0; n < cd->ntargets; n++) {
                    if (0 != strncmp(peer->info->pname.nspace, cd->targets[n].nspace, PMIX_MAX_NSLEN)) {
                        continue;
                    }
                    /* if the source of the event is the same peer just registered, then ignore it
                     * as the event notification system will have already locally
                     * processed it */
                    if (0 == strncmp(peer->info->pname.nspace, cd->source.nspace, PMIX_MAX_NSLEN) &&
                        peer->info->pname.rank == cd->source.rank) {
                        continue;
                    }
                    if (PMIX_RANK_WILDCARD == cd->targets[n].rank ||
                        peer->info->pname.rank == cd->targets[n].rank) {
                        matched = true;
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
                return PMIX_ERR_NOMEM;
            }
            /* pack the info data stored in the event */
            PMIX_BFROPS_PACK(rc, peer, relay, &cmd, 1, PMIX_COMMAND);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                break;
            }
            PMIX_BFROPS_PACK(rc, peer, relay, &cd->status, 1, PMIX_STATUS);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                break;
            }
            PMIX_BFROPS_PACK(rc, peer, relay, &cd->source, 1, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                break;
            }
            PMIX_BFROPS_PACK(rc, peer, relay, &cd->ninfo, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                break;
            }
            if (0 < cd->ninfo) {
                PMIX_BFROPS_PACK(rc, peer, relay, cd->info, cd->ninfo, PMIX_INFO);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    break;
                }
            }
            PMIX_SERVER_QUEUE_REPLY(peer, 0, relay);
        }
    }
    if (!enviro_events) {
        if (NULL != codes) {
            free(codes);
        }
    }

    return PMIX_SUCCESS;
}

void pmix_server_deregister_events(pmix_peer_t *peer,
                                   pmix_buffer_t *buf)
{
    int32_t cnt;
    pmix_status_t rc, code;
    pmix_regevents_info_t *reginfo = NULL;
    pmix_regevents_info_t *reginfo_next;
    pmix_peer_events_info_t *prev;

    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "recvd deregister events");

    /* unpack codes and process until done */
    cnt=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &code, &cnt, PMIX_STATUS);
    while (PMIX_SUCCESS == rc) {
        PMIX_LIST_FOREACH_SAFE(reginfo, reginfo_next, &pmix_server_globals.events, pmix_regevents_info_t) {
            if (code == reginfo->code) {
                /* found it - remove this peer from the list */
                PMIX_LIST_FOREACH(prev, &reginfo->peers, pmix_peer_events_info_t) {
                    if (prev->peer == peer) {
                        /* found it */
                        pmix_list_remove_item(&reginfo->peers, &prev->super);
                        PMIX_RELEASE(prev);
                        break;
                    }
                }
                /* if all of the peers for this code are now gone, then remove it */
                if (0 == pmix_list_get_size(&reginfo->peers)) {
                    pmix_list_remove_item(&pmix_server_globals.events, &reginfo->super);
                    /* if this was registered with the host, then deregister it */
                    PMIX_RELEASE(reginfo);
                }
            }
        }
        cnt=1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &code, &cnt, PMIX_STATUS);
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
    }
}


static void local_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t*)cbdata;

    if (NULL != cd->cbfunc) {
        cd->cbfunc(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_event_recvd_from_client(pmix_peer_t *peer,
                                                  pmix_buffer_t *buf,
                                                  pmix_op_cbfunc_t cbfunc,
                                                  void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_notify_caddy_t *cd;

    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "recvd event notification from client");

    if (NULL == pmix_host_server.notify_event) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_notify_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* set the source */
    (void)strncpy(cd->source.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    cd->source.rank = peer->info->pname.rank;

    /* unpack status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the range */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->range, &cnt, PMIX_DATA_RANGE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the info keys */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        if (NULL == cd->info) {
            rc = PMIX_ERR_NOMEM;
            goto exit;
        }
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* check the range directive - if it is LOCAL, then we just
     * process it ourselves. Otherwise, it needs to go up to our
     * host for dissemination */
    if (PMIX_RANGE_LOCAL == cd->range) {
        if (PMIX_SUCCESS != (rc = pmix_server_notify_client_of_event(cd->status,
                                                                     &cd->source,
                                                                     cd->range,
                                                                     cd->info, cd->ninfo,
                                                                     local_cbfunc, cd))) {
            goto exit;
        }
        return PMIX_SUCCESS;
    }

    /* when we receive an event from a client, we just pass it to
     * our host RM for distribution - if any targeted recipients
     * are local to us, the host RM will let us know */
    pmix_host_server.notify_event(cd->status, &cd->source, cd->range,
                                  cd->info, cd->ninfo, local_cbfunc, cd);
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    cbfunc(rc, cbdata);
    return rc;
}

pmix_status_t pmix_server_query(pmix_peer_t *peer,
                                pmix_buffer_t *buf,
                                pmix_info_cbfunc_t cbfunc,
                                void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd query from client");

    if (NULL == pmix_host_server.query) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata;
    /* unpack the number of queries */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->nqueries, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the queries */
    if (0 < cd->nqueries) {
        PMIX_QUERY_CREATE(cd->queries, cd->nqueries);
        if (NULL == cd->queries) {
            rc = PMIX_ERR_NOMEM;
            goto exit;
        }
        cnt = cd->nqueries;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->queries, &cnt, PMIX_QUERY);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host for the info */
    if (PMIX_SUCCESS != (rc = pmix_host_server.query(&proc, cd->queries, cd->nqueries,
                                                     cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}

static void logcbfn(pmix_status_t status, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}
pmix_status_t pmix_server_log(pmix_peer_t *peer,
                              pmix_buffer_t *buf,
                              pmix_op_cbfunc_t cbfunc,
                              void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_shift_caddy_t *cd;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd log from client");

    if (NULL == pmix_host_server.log) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;
    /* unpack the number of data */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the data */
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }
    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ndirs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the directives */
    if (0 < cd->ndirs) {
        PMIX_INFO_CREATE(cd->directives, cd->ndirs);
        cnt = cd->ndirs;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->directives, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to log the info */
    pmix_host_server.log(&proc, cd->info, cd->ninfo,
                         cd->directives, cd->ndirs,
                         logcbfn, cd);
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_alloc(pmix_peer_t *peer,
                                pmix_buffer_t *buf,
                                pmix_info_cbfunc_t cbfunc,
                                void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;
    pmix_alloc_directive_t directive;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd query from client");

    if (NULL == pmix_host_server.allocate) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata;

    /* unpack the directive */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &directive, &cnt, PMIX_ALLOC_DIRECTIVE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the number of info objects */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the info */
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS != (rc = pmix_host_server.allocate(&proc, directive,
                                                        cd->info, cd->ninfo,
                                                        cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_job_ctrl(pmix_peer_t *peer,
                                   pmix_buffer_t *buf,
                                   pmix_info_cbfunc_t cbfunc,
                                   void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;
    size_t n;
    bool recurse = false, leave_topdir = false, duplicate;
    pmix_list_t cachedirs, cachefiles;
    pmix_epilog_t *epi = NULL;
    pmix_cleanup_file_t *cf, *cf2;
    pmix_cleanup_dir_t *cdir, *cdir2;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd job control request from client");

    if (NULL == pmix_host_server.job_control) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata;

    /* unpack the number of targets */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ntargets, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    if (0 < cd->ntargets) {
        PMIX_PROC_CREATE(cd->targets, cd->ntargets);
        cnt = cd->ntargets;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->targets, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* check targets to find proper place to put any epilog requests */
    if (NULL == cd->targets) {
        epi = &peer->nptr->epilog;
    } else if (1 == cd->ntargets) {
        if (0 == strncmp(cd->targets[0].nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN)) {
            if (PMIX_RANK_WILDCARD == cd->targets[0].rank) {
                epi = &peer->nptr->epilog;
            } else {
                epi = &peer->epilog;
            }
        }
    }

    /* unpack the number of info objects */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the info */
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* if this includes a request for post-termination cleanup, we handle
     * that request ourselves */
    PMIX_CONSTRUCT(&cachedirs, pmix_list_t);
    PMIX_CONSTRUCT(&cachefiles, pmix_list_t);
    cnt = 0;  // track how many infos are cleanup related
    for (n=0; n < cd->ninfo; n++) {
        if (0 == strncmp(cd->info[n].key, PMIX_REGISTER_CLEANUP, PMIX_MAX_KEYLEN)) {
            ++cnt;
            /* see if we allow epilog requests */
            if (NULL == epi) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            if (PMIX_STRING != cd->info[n].value.type ||
                NULL == cd->info[n].value.data.string) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            cf = PMIX_NEW(pmix_cleanup_file_t);
            if (NULL == cf) {
                /* return an error */
                rc = PMIX_ERR_NOMEM;
                goto exit;
            }
            cf->path = strdup(cd->info[n].value.data.string);
            pmix_list_append(&cachefiles, &cf->super);
        } else if (0 == strncmp(cd->info[n].key, PMIX_REGISTER_CLEANUP_DIR, PMIX_MAX_KEYLEN)) {
            ++cnt;
            /* see if we allow epilog requests */
            if (NULL == epi) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            if (PMIX_STRING != cd->info[n].value.type ||
                NULL == cd->info[n].value.data.string) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            cdir = PMIX_NEW(pmix_cleanup_dir_t);
            if (NULL == cdir) {
                /* return an error */
                rc = PMIX_ERR_NOMEM;
                goto exit;
            }
            cdir->path = strdup(cd->info[n].value.data.string);
            pmix_list_append(&cachedirs, &cdir->super);
        } else if (0 == strncmp(cd->info[n].key, PMIX_CLEANUP_RECURSIVE, PMIX_MAX_KEYLEN)) {
            /* see if we allow epilog requests */
            if (NULL == epi) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            recurse = PMIX_INFO_TRUE(&cd->info[n]);
            ++cnt;
        } else if (0 == strncmp(cd->info[n].key, PMIX_CLEANUP_IGNORE, PMIX_MAX_KEYLEN)) {
            if (PMIX_STRING != cd->info[n].value.type ||
                NULL == cd->info[n].value.data.string) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            /* see if we allow epilog requests */
            if (NULL == epi) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            /* scan the list of ignores for any duplicate */
            duplicate = false;
            PMIX_LIST_FOREACH(cf, &epi->ignores, pmix_cleanup_file_t) {
                if (0 == strcmp(cf->path, cd->info[n].value.data.string)) {
                    /* we can drop this request */
                    duplicate = true;
                    break;
                }
            }
            if (!duplicate) {
                cf = PMIX_NEW(pmix_cleanup_file_t);
                if (NULL == cf) {
                    /* return an error */
                    rc = PMIX_ERR_NOMEM;
                    goto exit;
                }
                cf->path = strdup(cd->info[n].value.data.string);
                pmix_list_append(&epi->ignores, &cf->super);
            }
            ++cnt;
        } else if (0 == strncmp(cd->info[n].key, PMIX_CLEANUP_LEAVE_TOPDIR, PMIX_MAX_KEYLEN)) {
            /* see if we allow epilog requests */
            if (NULL == epi) {
                /* return an error */
                rc = PMIX_ERR_BAD_PARAM;
                goto exit;
            }
            leave_topdir = PMIX_INFO_TRUE(&cd->info[n]);
            ++cnt;
        }
    }
    if (0 < cnt) {
        while (NULL != (cdir = (pmix_cleanup_dir_t*)pmix_list_remove_first(&cachedirs))) {
            /* scan the existing list of directories for any duplicate */
            PMIX_LIST_FOREACH(cdir2, &epi->cleanup_dirs, pmix_cleanup_dir_t) {
                if (0 == strcmp(cdir2->path, cdir->path)) {
                    /* duplicate - check for difference in flags per RFC
                     * precedence rules */
                    if (!cdir->recurse && recurse) {
                        cdir->recurse = recurse;
                    }
                    if (!cdir->leave_topdir && leave_topdir) {
                        cdir->leave_topdir = leave_topdir;
                    }
                    PMIX_RELEASE(cdir);
                    cdir = NULL;
                    break;
                }
            }
            if (NULL != cdir) {
                /* check for conflict with ignore */
                PMIX_LIST_FOREACH(cf, &epi->ignores, pmix_cleanup_file_t) {
                    if (0 == strcmp(cf->path, cdir->path)) {
                        /* return an error */
                        rc = PMIX_ERR_CONFLICTING_CLEANUP_DIRECTIVES;
                        PMIX_LIST_DESTRUCT(&cachedirs);
                        PMIX_LIST_DESTRUCT(&cachefiles);
                        goto exit;
                    }
                }
                cdir->recurse = recurse;
                cdir->leave_topdir = leave_topdir;
                /* just append it to the end of the list */
                pmix_list_append(&epi->cleanup_dirs, &cdir->super);
            }
        }
        PMIX_DESTRUCT(&cachedirs);
        while (NULL != (cf = (pmix_cleanup_file_t*)pmix_list_remove_first(&cachefiles))) {
            /* scan the existing list of files for any duplicate */
            PMIX_LIST_FOREACH(cf2, &epi->cleanup_files, pmix_cleanup_file_t) {
                if (0 == strcmp(cf2->path, cf->path)) {
                    PMIX_RELEASE(cf);
                    cf = NULL;
                    break;
                }
            }
            if (NULL != cf) {
                /* check for conflict with ignore */
                PMIX_LIST_FOREACH(cf2, &epi->ignores, pmix_cleanup_file_t) {
                    if (0 == strcmp(cf->path, cf2->path)) {
                        /* return an error */
                        rc = PMIX_ERR_CONFLICTING_CLEANUP_DIRECTIVES;
                        PMIX_LIST_DESTRUCT(&cachedirs);
                        PMIX_LIST_DESTRUCT(&cachefiles);
                        goto exit;
                    }
                }
                /* just append it to the end of the list */
                pmix_list_append(&epi->cleanup_files, &cf->super);
            }
        }
        PMIX_DESTRUCT(&cachefiles);
        if (cnt == (int)cd->ninfo) {
            /* nothing more to do */
            if (NULL != cbfunc) {
                cbfunc(PMIX_SUCCESS, NULL, 0, cd, NULL, NULL);
            }
            return PMIX_SUCCESS;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS != (rc = pmix_host_server.job_control(&proc,
                                                           cd->targets, cd->ntargets,
                                                           cd->info, cd->ninfo,
                                                           cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_monitor(pmix_peer_t *peer,
                                  pmix_buffer_t *buf,
                                  pmix_info_cbfunc_t cbfunc,
                                  void *cbdata)
{
    int32_t cnt;
    pmix_info_t monitor;
    pmix_status_t rc, error;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd monitor request from client");

    if (NULL == pmix_host_server.monitor) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata;

    /* unpack what is to be monitored */
    PMIX_INFO_CONSTRUCT(&monitor);
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &monitor, &cnt, PMIX_INFO);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the error code */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &error, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the directives */
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS != (rc = pmix_host_server.monitor(&proc, &monitor, error,
                                                       cd->info, cd->ninfo,
                                                       cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

  exit:
    PMIX_INFO_DESTRUCT(&monitor);
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_get_credential(pmix_peer_t *peer,
                                         pmix_buffer_t *buf,
                                         pmix_credential_cbfunc_t cbfunc,
                                         void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd get credential request from client");

    if (NULL == pmix_host_server.get_credential) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata;

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the directives */
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS != (rc = pmix_host_server.get_credential(&proc, cd->info, cd->ninfo,
                                                              cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_validate_credential(pmix_peer_t *peer,
                                              pmix_buffer_t *buf,
                                              pmix_validation_cbfunc_t cbfunc,
                                              void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd validate credential request from client");

    if (NULL == pmix_host_server.validate_credential) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata;

    /* unpack the credential */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->bo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the directives */
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS != (rc = pmix_host_server.validate_credential(&proc, &cd->bo,
                                                                   cd->info, cd->ninfo,
                                                                   cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}

/*****    INSTANCE SERVER LIBRARY CLASSES    *****/
static void tcon(pmix_server_trkr_t *t)
{
    memset(t->pname.nspace, 0, PMIX_MAX_NSLEN+1);
    t->pname.rank = PMIX_RANK_UNDEF;
    t->pcs = NULL;
    t->npcs = 0;
    PMIX_CONSTRUCT_LOCK(&t->lock);
    t->def_complete = false;
    PMIX_CONSTRUCT(&t->local_cbs, pmix_list_t);
    t->nlocal = 0;
    t->local_cnt = 0;
    t->info = NULL;
    t->ninfo = 0;
    /* this needs to be set explicitly */
    t->collect_type = PMIX_COLLECT_INVALID;
    t->modexcbfunc = NULL;
    t->op_cbfunc = NULL;
    t->cnct_cbfunc = NULL;
}
static void tdes(pmix_server_trkr_t *t)
{
    PMIX_DESTRUCT_LOCK(&t->lock);
    if (NULL != t->pcs) {
        free(t->pcs);
    }
    PMIX_LIST_DESTRUCT(&t->local_cbs);
    if (NULL != t->info) {
        PMIX_INFO_FREE(t->info, t->ninfo);
    }
}
PMIX_CLASS_INSTANCE(pmix_server_trkr_t,
                   pmix_list_item_t,
                   tcon, tdes);

static void cdcon(pmix_server_caddy_t *cd)
{
    memset(&cd->ev, 0, sizeof(pmix_event_t));
    cd->event_active = false;
    cd->trk = NULL;
    cd->peer = NULL;
}
static void cddes(pmix_server_caddy_t *cd)
{
    if (cd->event_active) {
        pmix_event_del(&cd->ev);
    }
    if (NULL != cd->trk) {
        PMIX_RELEASE(cd->trk);
    }
    if (NULL != cd->peer) {
        PMIX_RELEASE(cd->peer);
    }
}
PMIX_CLASS_INSTANCE(pmix_server_caddy_t,
                   pmix_list_item_t,
                   cdcon, cddes);


static void scadcon(pmix_setup_caddy_t *p)
{
    memset(&p->proc, 0, sizeof(pmix_proc_t));
    PMIX_CONSTRUCT_LOCK(&p->lock);
    p->nspace = NULL;
    p->codes = NULL;
    p->ncodes = 0;
    p->procs = NULL;
    p->nprocs = 0;
    p->server_object = NULL;
    p->nlocalprocs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->keys = NULL;
    p->cbfunc = NULL;
    p->opcbfunc = NULL;
    p->setupcbfunc = NULL;
    p->lkcbfunc = NULL;
    p->spcbfunc = NULL;
    p->cbdata = NULL;
}
static void scaddes(pmix_setup_caddy_t *p)
{
    PMIX_DESTRUCT_LOCK(&p->lock);
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_setup_caddy_t,
                                pmix_object_t,
                                scadcon, scaddes);

static void ncon(pmix_notify_caddy_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
    memset(p->source.nspace, 0, PMIX_MAX_NSLEN+1);
    p->source.rank = PMIX_RANK_UNDEF;
    p->range = PMIX_RANGE_UNDEF;
    p->targets = NULL;
    p->ntargets = 0;
    p->nondefault = false;
    p->info = NULL;
    p->ninfo = 0;
}
static void ndes(pmix_notify_caddy_t *p)
{
    PMIX_DESTRUCT_LOCK(&p->lock);
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
    if (NULL != p->targets) {
        free(p->targets);
    }
}
PMIX_CLASS_INSTANCE(pmix_notify_caddy_t,
                    pmix_object_t,
                    ncon, ndes);


PMIX_CLASS_INSTANCE(pmix_trkr_caddy_t,
                    pmix_object_t,
                    NULL, NULL);

static void dmcon(pmix_dmdx_remote_t *p)
{
    p->cd = NULL;
}
static void dmdes(pmix_dmdx_remote_t *p)
{
    if (NULL != p->cd) {
        PMIX_RELEASE(p->cd);
    }
}
PMIX_CLASS_INSTANCE(pmix_dmdx_remote_t,
                    pmix_list_item_t,
                    dmcon, dmdes);

static void dmrqcon(pmix_dmdx_request_t *p)
{
    memset(&p->ev, 0, sizeof(pmix_event_t));
    p->event_active = false;
    p->lcd = NULL;
}
static void dmrqdes(pmix_dmdx_request_t *p)
{
    if (p->event_active) {
        pmix_event_del(&p->ev);
    }
    if (NULL != p->lcd) {
        PMIX_RELEASE(p->lcd);
    }
}
PMIX_CLASS_INSTANCE(pmix_dmdx_request_t,
                    pmix_list_item_t,
                    dmrqcon, dmrqdes);

static void lmcon(pmix_dmdx_local_t *p)
{
    memset(&p->proc, 0, sizeof(pmix_proc_t));
    PMIX_CONSTRUCT(&p->loc_reqs, pmix_list_t);
    p->info = NULL;
    p->ninfo = 0;
}
static void lmdes(pmix_dmdx_local_t *p)
{
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
    PMIX_LIST_DESTRUCT(&p->loc_reqs);
}
PMIX_CLASS_INSTANCE(pmix_dmdx_local_t,
                    pmix_list_item_t,
                    lmcon, lmdes);

static void prevcon(pmix_peer_events_info_t *p)
{
    p->peer = NULL;
}
static void prevdes(pmix_peer_events_info_t *p)
{
    if (NULL != p->peer) {
        PMIX_RELEASE(p->peer);
    }
}
PMIX_CLASS_INSTANCE(pmix_peer_events_info_t,
                    pmix_list_item_t,
                    prevcon, prevdes);

static void regcon(pmix_regevents_info_t *p)
{
    PMIX_CONSTRUCT(&p->peers, pmix_list_t);
}
static void regdes(pmix_regevents_info_t *p)
{
    PMIX_LIST_DESTRUCT(&p->peers);
}
PMIX_CLASS_INSTANCE(pmix_regevents_info_t,
                    pmix_list_item_t,
                    regcon, regdes);
