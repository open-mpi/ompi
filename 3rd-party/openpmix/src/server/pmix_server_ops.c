/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016-2019 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2020 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2023 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_stdint.h"

#include "include/pmix_server.h"
#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#    include <string.h>
#endif
#ifdef HAVE_SYS_STAT_H
#    include <sys/stat.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_TIME_H
#    include <time.h>
#endif
#include <event.h>

#ifndef MAX
#    define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

#include "src/class/pmix_hotel.h"
#include "src/class/pmix_list.h"
#include "src/common/pmix_attributes.h"
#include "src/common/pmix_iof.h"
#include "src/hwloc/pmix_hwloc.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/plog/plog.h"
#include "src/mca/pnet/pnet.h"
#include "src/mca/prm/prm.h"
#include "src/mca/psensor/psensor.h"
#include "src/mca/ptl/base/base.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"

#include "src/client/pmix_client_ops.h"
#include "pmix_server_ops.h"

/* The rank_blob_t type to collect processes blobs,
 * this list afterward will form a node modex blob. */
typedef struct {
    pmix_list_item_t super;
    pmix_buffer_t *buf;
} rank_blob_t;

static void bufcon(rank_blob_t *p)
{
    p->buf = NULL;
}
static void bufdes(rank_blob_t *p)
{
    if (NULL != p->buf) {
        PMIX_RELEASE(p->buf);
    }
}
static PMIX_CLASS_INSTANCE(rank_blob_t,
                           pmix_list_item_t,
                           bufcon, bufdes);

pmix_server_module_t pmix_host_server = {
    .client_connected = NULL,
    .client_finalized = NULL,
    .abort = NULL,
    .fence_nb = NULL,
    .direct_modex = NULL,
    .publish = NULL,
    .lookup = NULL,
    .unpublish = NULL,
    .spawn = NULL,
    .connect = NULL,
    .disconnect = NULL,
    .register_events = NULL,
    .deregister_events = NULL,
    .listener = NULL,
    .notify_event = NULL,
    .query = NULL,
    .tool_connected = NULL,
    .log = NULL,
    .allocate = NULL,
    .job_control = NULL,
    .monitor = NULL,
    .get_credential = NULL,
    .validate_credential = NULL,
    .iof_pull = NULL,
    .push_stdin = NULL,
    .group = NULL,
    .fabric = NULL,
    .client_connected2 = NULL,
    .session_control = NULL
};

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

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd ABORT");

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
        pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
        proc.rank = peer->info->pname.rank;
        rc = pmix_host_server.abort(&proc, peer->info->server_object, status,
                                    msg, procs, nprocs,
                                    cbfunc, cbdata);
    } else {
        rc = PMIX_ERR_NOT_SUPPORTED;
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
    pmix_namespace_t *nptr;
    pmix_rank_info_t *info;
    pmix_proc_t proc;
    pmix_dmdx_remote_t *dcd, *dcdnext;
    char *data;
    size_t sz;
    pmix_cb_t cb;

    /* shorthand */
    info = peer->info;
    nptr = peer->nptr;
    pmix_strncpy(proc.nspace, nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = info->pname.rank;

    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "%s:%d EXECUTE COMMIT FOR %s:%d",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank, nptr->nspace,
                        info->pname.rank);

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
            if (PMIX_LOCAL == scope || PMIX_GLOBAL == scope) {
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
            PMIX_RELEASE(kp); // maintain accounting
            kp = PMIX_NEW(pmix_kval_t);
            cnt = 1;
            PMIX_BFROPS_UNPACK(rc, peer, &b2, kp, &cnt, PMIX_KVAL);
        }
        PMIX_RELEASE(kp); // maintain accounting
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
    PMIX_LIST_FOREACH_SAFE (dcd, dcdnext, &pmix_server_globals.remote_pnd, pmix_dmdx_remote_t) {
        if (0 != strncmp(dcd->cd->proc.nspace, nptr->nspace, PMIX_MAX_NSLEN)) {
            continue;
        }
        if (dcd->cd->proc.rank == info->pname.rank) {
            pmix_list_remove_item(&pmix_server_globals.remote_pnd, &dcd->super);
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
                PMIX_LIST_FOREACH (kp, &cb.kvs, pmix_kval_t) {
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
            PMIX_RELEASE(dcd);
        }
    }
    /* see if anyone local is waiting on this data- could be more than one */
    rc = pmix_pending_resolve(nptr, info->pname.rank, PMIX_SUCCESS, PMIX_LOCAL, NULL);
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
static pmix_server_trkr_t *get_tracker(char *id, pmix_proc_t *procs,
                                       size_t nprocs, pmix_cmd_t type)
{
    pmix_server_trkr_t *trk;
    size_t i, j;
    size_t matches;

    pmix_output_verbose(5, pmix_server_globals.fence_output,
                        "get_tracker called with %d procs",
                        (int) nprocs);

    /* bozo check - should never happen outside of programmer error */
    if (NULL == procs && NULL == id) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }

    /* there is no shortcut way to search the trackers - all
     * we can do is perform a brute-force search. Fortunately,
     * it is highly unlikely that there will be more than one
     * or two active at a time, and they are most likely to
     * involve only a single proc with WILDCARD rank - so this
     * shouldn't take long */
    PMIX_LIST_FOREACH (trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
        /* Collective operation if unique identified by
         * the set of participating processes and the type of collective,
         * or by the operation ID
         */
        if (NULL != id) {
            if (NULL != trk->id && 0 == strcmp(id, trk->id)) {
                return trk;
            }
        } else {
            if (nprocs != trk->npcs) {
                continue;
            }
            if (type != trk->type) {
                continue;
            }
            matches = 0;
            for (i = 0; i < nprocs; i++) {
                /* the procs may be in different order, so we have
                 * to do an exhaustive search */
                for (j = 0; j < trk->npcs; j++) {
                    if (0 == strcmp(procs[i].nspace, trk->pcs[j].nspace)
                        && procs[i].rank == trk->pcs[j].rank) {
                        ++matches;
                        break;
                    }
                }
            }
            if (trk->npcs == matches) {
                return trk;
            }
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
static pmix_server_trkr_t *new_tracker(char *id, pmix_proc_t *procs,
                                       size_t nprocs, pmix_cmd_t type)
{
    pmix_server_trkr_t *trk;
    size_t i;
    bool all_def, found;
    pmix_namespace_t *nptr, *ns;
    pmix_rank_info_t *info;
    pmix_nspace_caddy_t *nm;
    pmix_nspace_t first;

    pmix_output_verbose(5, pmix_server_globals.fence_output,
                        "new_tracker called with %d procs",
                        (int) nprocs);

    /* bozo check - should never happen outside of programmer error */
    if (NULL == procs) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }

    pmix_output_verbose(5, pmix_server_globals.fence_output,
                        "adding new tracker %s with %d procs",
                        (NULL == id) ? "NO-ID" : id, (int) nprocs);

    /* this tracker is new - create it */
    trk = PMIX_NEW(pmix_server_trkr_t);
    if (NULL == trk) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return NULL;
    }

    if (NULL != id) {
        trk->id = strdup(id);
    }

    /* copy the procs */
    PMIX_PROC_CREATE(trk->pcs, nprocs);
    if (NULL == trk->pcs) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(trk);
        return NULL;
    }
    memcpy(trk->pcs, procs, nprocs * sizeof(pmix_proc_t));
    trk->npcs = nprocs;
    trk->type = type;
    trk->local = true;
    trk->nlocal = 0;

    all_def = true;
    PMIX_LOAD_NSPACE(first, NULL);
    for (i = 0; i < nprocs; i++) {
        /* is this nspace known to us? */
        nptr = NULL;
        PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
            if (0 == strcmp(procs[i].nspace, ns->nspace)) {
                nptr = ns;
                break;
            }
        }
        /* check if multiple nspaces are involved in this operation */
        if (0 == strlen(first)) {
            PMIX_LOAD_NSPACE(first, procs[i].nspace);
        } else if (!PMIX_CHECK_NSPACE(first, procs[i].nspace)) {
            trk->hybrid = true;
        }
        if (NULL == nptr) {
            /* we don't know about this nspace. If there is going to
             * be at least one local process participating in a fence,
             * they we require that either at least one process must already
             * have been registered (via "register client") or that the
             * nspace itself have been regisered. So either the nspace
             * wasn't registered because it doesn't include any local
             * procs, or our host has not been told about this nspace
             * because it won't host any local procs. We therefore mark
             * this tracker as including non-local participants.
             *
             * NOTE: It is conceivable that someone might want to review
             * this constraint at a future date. I believe it has to be
             * required (at least for now) as otherwise we wouldn't have
             * a way of knowing when all local procs have participated.
             * It is possible that a new nspace could come along at some
             * later time and add more local participants - but we don't
             * know how long to wait.
             *
             * The only immediately obvious alternative solutions would
             * be to either require that RMs always inform all daemons
             * about the launch of nspaces, regardless of whether or
             * not they will host local procs; or to drop the aggregation
             * of local participants and just pass every fence call
             * directly to the host. Neither of these seems palatable
             * at this time. */
            trk->local = false;
            /* we don't know any more info about this nspace, so
             * there isn't anything more we can do */
            continue;
        }
        /* it is possible we know about this nspace because the host
         * has registered one or more clients via "register_client",
         * but the host has not yet called "register_nspace". There is
         * a very tiny race condition whereby this can happen due
         * to event-driven processing, but account for it here */
        if (SIZE_MAX == nptr->nlocalprocs) {
            /* delay processing until this nspace is registered */
            all_def = false;
            continue;
        }
        if (0 == nptr->nlocalprocs) {
            /* the host has informed us that this nspace has no local procs */
            pmix_output_verbose(5, pmix_server_globals.fence_output,
                                "new_tracker: nspace %s has no local procs", procs[i].nspace);
            trk->local = false;
            continue;
        }

        /* check and add uniq ns into trk nslist */
        found = false;
        PMIX_LIST_FOREACH (nm, &trk->nslist, pmix_nspace_caddy_t) {
            if (0 == strcmp(nptr->nspace, nm->ns->nspace)) {
                found = true;
                break;
            }
        }
        if (!found) {
            nm = PMIX_NEW(pmix_nspace_caddy_t);
            PMIX_RETAIN(nptr);
            nm->ns = nptr;
            pmix_list_append(&trk->nslist, &nm->super);
        }

        /* if they want all the local members of this nspace, then
         * add them in here. They told us how many procs will be
         * local to us from this nspace, but we don't know their
         * ranks. So as long as they want _all_ of them, we can
         * handle that case regardless of whether the individual
         * clients have been "registered" */
        if (PMIX_RANK_WILDCARD == procs[i].rank) {
            trk->nlocal += nptr->nlocalprocs;
            /* the total number of procs in this nspace was provided
             * in the data blob delivered to register_nspace, so check
             * to see if all the procs are local */
            if (nptr->nprocs != nptr->nlocalprocs) {
                trk->local = false;
            }
            continue;
        }

        /* They don't want all the local clients, or they are at
         * least listing them individually. Check if all the clients
         * for this nspace have been registered via "register_client"
         * so we know the specific ranks on this node */
        if (!nptr->all_registered) {
            /* nope, so no point in going further on this one - we'll
             * process it once all the procs are known */
            all_def = false;
            pmix_output_verbose(5, pmix_server_globals.fence_output,
                                "new_tracker: all clients not registered nspace %s",
                                procs[i].nspace);
            continue;
        }
        /* is this one of my local ranks? */
        found = false;
        PMIX_LIST_FOREACH (info, &nptr->ranks, pmix_rank_info_t) {
            if (procs[i].rank == info->pname.rank) {
                pmix_output_verbose(5, pmix_server_globals.fence_output,
                                    "adding local proc %s.%d to tracker", info->pname.nspace,
                                    info->pname.rank);
                found = true;
                /* track the count */
                trk->nlocal++;
                break;
            }
        }
        if (!found) {
            trk->local = false;
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
    pmix_server_trkr_t *trk = (pmix_server_trkr_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_server_globals.fence_output, "ALERT: fence timeout fired");

    /* execute the provided callback function with the error */
    if (NULL != trk->modexcbfunc) {
        trk->modexcbfunc(PMIX_ERR_TIMEOUT, NULL, 0, trk, NULL, NULL);
        return; // the cbfunc will have cleaned up the tracker
    }
    trk->event_active = false;
    PMIX_RELEASE(trk);
}

static pmix_status_t _collect_data(pmix_server_trkr_t *trk,
                                   pmix_buffer_t *buf)
{
    pmix_buffer_t bucket, *pbkt = NULL;
    pmix_cb_t cb;
    pmix_kval_t *kv;
    pmix_byte_object_t bo;
    pmix_server_caddy_t *scd;
    pmix_proc_t pcs;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_rank_t rel_rank;
    pmix_nspace_caddy_t *nm;
    bool found, data_added;
    pmix_list_t rank_blobs;
    rank_blob_t *blob;
    uint32_t kmap_size;
    int key_idx;

    /* key names map, the position of the key name
     * in the array determines the unique key index */
    char **kmap = NULL;
    int i;
    pmix_gds_modex_blob_info_t blob_info_byte = 0;
    pmix_gds_modex_key_fmt_t kmap_type = PMIX_MODEX_KEY_INVALID;

    PMIX_CONSTRUCT(&bucket, pmix_buffer_t);

    if (PMIX_COLLECT_YES == trk->collect_type) {
       pmix_output_verbose(2, pmix_server_globals.fence_output,
                           "fence - assembling data");

        /* Evaluate key names sizes and their count to select
         * a format to store key names:
         * - keymap: use key-map in blob header for key-name resolve
         *   from idx: key names stored as indexes (avoid key duplication)
         * - regular: key-names stored as is */
        if (PMIX_MODEX_KEY_INVALID == kmap_type) {
            size_t key_fmt_size[PMIX_MODEX_KEY_MAX] = {0};
            pmix_value_array_t *key_count_array = PMIX_NEW(pmix_value_array_t);
            uint32_t *key_count = NULL;

            pmix_value_array_init(key_count_array, sizeof(uint32_t));

            PMIX_LIST_FOREACH (scd, &trk->local_cbs, pmix_server_caddy_t) {
                pmix_strncpy(pcs.nspace, scd->peer->info->pname.nspace, PMIX_MAX_NSLEN);
                pcs.rank = scd->peer->info->pname.rank;
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                cb.proc = &pcs;
                cb.scope = PMIX_REMOTE;
                cb.copy = true;
                PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
                if (PMIX_SUCCESS == rc) {
                    PMIX_LIST_FOREACH (kv, &cb.kvs, pmix_kval_t) {
                        rc = pmix_argv_append_unique_idx(&key_idx, &kmap, kv->key);
                        if (pmix_value_array_get_size(key_count_array) < (size_t)(key_idx + 1)) {
                            size_t new_size;
                            size_t old_size = pmix_value_array_get_size(key_count_array);

                            pmix_value_array_set_size(key_count_array, key_idx + 1);
                            new_size = pmix_value_array_get_size(key_count_array);
                            key_count = PMIX_VALUE_ARRAY_GET_BASE(key_count_array, uint32_t);
                            memset(key_count + old_size, 0,
                                   sizeof(uint32_t) * (new_size - old_size));
                        }
                        key_count = PMIX_VALUE_ARRAY_GET_BASE(key_count_array, uint32_t);
                        key_count[key_idx]++;
                    }
                }
                PMIX_DESTRUCT(&cb);
            }
            for (i = 0; i < PMIx_Argv_count(kmap); i++) {
                pmix_buffer_t tmp;
                size_t kname_size;
                size_t kidx_size;

                PMIX_CONSTRUCT(&tmp, pmix_buffer_t);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &tmp, &kmap[i], 1, PMIX_STRING);
                kname_size = tmp.bytes_used;
                PMIX_DESTRUCT(&tmp);
                PMIX_CONSTRUCT(&tmp, pmix_buffer_t);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &tmp, &i, 1, PMIX_UINT32);
                kidx_size = tmp.bytes_used;
                PMIX_DESTRUCT(&tmp);

                /* calculate the key names sizes */
                key_fmt_size[PMIX_MODEX_KEY_NATIVE_FMT] = kname_size * key_count[i];
                key_fmt_size[PMIX_MODEX_KEY_KEYMAP_FMT] = kname_size + key_count[i] * kidx_size;
            }
            PMIX_RELEASE(key_count_array);

            /* select the most efficient key-name pack format */
            kmap_type = key_fmt_size[PMIX_MODEX_KEY_NATIVE_FMT]
                                > key_fmt_size[PMIX_MODEX_KEY_KEYMAP_FMT]
                            ? PMIX_MODEX_KEY_KEYMAP_FMT
                            : PMIX_MODEX_KEY_NATIVE_FMT;
            pmix_output_verbose(5, pmix_server_globals.fence_output, "key packing type %s",
                                kmap_type == PMIX_MODEX_KEY_KEYMAP_FMT ? "kmap" : "native");
        }
        PMIX_CONSTRUCT(&rank_blobs, pmix_list_t);
        PMIX_LIST_FOREACH (scd, &trk->local_cbs, pmix_server_caddy_t) {
            /* get any remote contribution - note that there
             * may not be a contribution */
            pmix_strncpy(pcs.nspace, scd->peer->info->pname.nspace, PMIX_MAX_NSLEN);
            pcs.rank = scd->peer->info->pname.rank;
            pbkt = PMIX_NEW(pmix_buffer_t);
            /* calculate the throughout rank */
            rel_rank = 0;
            found = false;
            if (pmix_list_get_size(&trk->nslist) == 1) {
                found = true;
            } else {
                PMIX_LIST_FOREACH (nm, &trk->nslist, pmix_nspace_caddy_t) {
                    if (0 == strcmp(nm->ns->nspace, pcs.nspace)) {
                        found = true;
                        break;
                    }
                    rel_rank += nm->ns->nprocs;
                }
            }
            if (false == found) {
                rc = PMIX_ERR_NOT_FOUND;
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&cb);
                PMIX_LIST_DESTRUCT(&rank_blobs);
                PMIX_RELEASE(pbkt);
                goto cleanup;
            }
            rel_rank += pcs.rank;

            /* pack the relative rank */
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, pbkt, &rel_rank, 1, PMIX_PROC_RANK);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&cb);
                PMIX_LIST_DESTRUCT(&rank_blobs);
                PMIX_RELEASE(pbkt);
                goto cleanup;
            }
            data_added = false;
            PMIX_CONSTRUCT(&cb, pmix_cb_t);
            cb.proc = &pcs;
            cb.scope = PMIX_REMOTE;
            cb.copy = true;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
            if (PMIX_SUCCESS == rc) {
                /* pack the returned kval's */
                PMIX_LIST_FOREACH (kv, &cb.kvs, pmix_kval_t) {
                    rc = pmix_gds_base_modex_pack_kval(kmap_type, pbkt, &kmap, kv);
                    if (rc != PMIX_SUCCESS) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        PMIX_LIST_DESTRUCT(&rank_blobs);
                        PMIX_RELEASE(pbkt);
                        goto cleanup;
                    }
                }
                data_added = true;
            }
            if (data_added) {
                /* add part of the process modex to the list */
                blob = PMIX_NEW(rank_blob_t);
                blob->buf = pbkt;
                pmix_list_append(&rank_blobs, &blob->super);
                pbkt = NULL;
            }
            PMIX_DESTRUCT(&cb);
            if (NULL != pbkt) {
                PMIX_RELEASE(pbkt);
            }
        }
        /* mark the collection type so we can check on the
         * receiving end that all participants did the same. Note
         * that if the receiving end thinks that the collect flag
         * is false, then store_modex will not be called on that
         * node and this information (and the flag) will be ignored,
         * meaning that no error is generated! */
        blob_info_byte |= PMIX_GDS_COLLECT_BIT;
        if (PMIX_MODEX_KEY_KEYMAP_FMT == kmap_type) {
            blob_info_byte |= PMIX_GDS_KEYMAP_BIT;
        }
        /* pack the modex blob info byte */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &blob_info_byte, 1, PMIX_BYTE);

        if (PMIX_MODEX_KEY_KEYMAP_FMT == kmap_type) {
            /* pack node part of modex to `bucket` */
            /* pack the key names map for the remote server can
             * use it to match key names by index */
            kmap_size = PMIx_Argv_count(kmap);
            if (0 < kmap_size) {
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &kmap_size, 1, PMIX_UINT32);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, kmap, kmap_size, PMIX_STRING);
            }
        }
        /* pack the collected blobs of processes */
        PMIX_LIST_FOREACH (blob, &rank_blobs, rank_blob_t) {
            /* extract the blob */
            PMIX_UNLOAD_BUFFER(blob->buf, bo.bytes, bo.size);
            blob->buf = NULL;
            /* pack the returned blob */
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &bo, 1, PMIX_BYTE_OBJECT);
            PMIX_BYTE_OBJECT_DESTRUCT(&bo); // releases the data
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        PMIX_LIST_DESTRUCT(&rank_blobs);
    } else {
        /* mark the collection type so we can check on the
         * receiving end that all participants did the same.
         * Don't do it for non-debug mode so we don't unnecessarily
         * send the collection bucket. The mdxcbfunc in the
         * server only calls store_modex if the local collect
         * flag is set to true. In debug mode, this check will
         * cause the store_modex function to see that this node
         * thought the collect flag was not set, and therefore
         * generate an error */
#if PMIX_ENABLE_DEBUG
        /* pack the modex blob info byte */
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &blob_info_byte, 1, PMIX_BYTE);
#endif
    }
    if (!PMIX_BUFFER_IS_EMPTY(&bucket)) {
        /* because the remote servers have to unpack things
         * in chunks, we have to pack the bucket as a single
         * byte object to allow remote unpack */
        PMIX_UNLOAD_BUFFER(&bucket, bo.bytes, bo.size);
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, buf, &bo, 1, PMIX_BYTE_OBJECT);
        PMIX_BYTE_OBJECT_DESTRUCT(&bo); // releases the data
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

cleanup:
    PMIX_DESTRUCT(&bucket);
    PMIx_Argv_free(kmap);
    return rc;
}

pmix_status_t pmix_server_fence(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                pmix_modex_cbfunc_t modexcbfunc, pmix_op_cbfunc_t opcbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    size_t nprocs;
    pmix_proc_t *procs = NULL;
    bool collect_data = false;
    pmix_server_trkr_t *trk;
    char *data = NULL;
    size_t sz = 0;
    pmix_buffer_t bucket;
    pmix_info_t *info = NULL;
    size_t ninfo = 0, ninf, n;
    struct timeval tv = {0, 0};

    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "recvd FENCE");

    /* unpack the number of procs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "recvd fence from %s with %d procs",
                        PMIX_PEER_PRINT(cd->peer), (int) nprocs);
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
    /* sort the array */
    qsort(procs, nprocs, sizeof(pmix_proc_t), pmix_util_compare_proc);

    /* unpack the number of provided info structs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &ninf, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    ninfo = ninf + 2;
    PMIX_INFO_CREATE(info, ninfo);
    if (NULL == info) {
        PMIX_PROC_FREE(procs, nprocs);
        return PMIX_ERR_NOMEM;
    }
    /* store the default response */
    rc = PMIX_SUCCESS;
    PMIX_INFO_LOAD(&info[ninf+1], PMIX_LOCAL_COLLECTIVE_STATUS, &rc, PMIX_STATUS);
    PMIX_INFO_LOAD(&info[ninf], PMIX_SORTED_PROC_ARRAY, NULL, PMIX_BOOL);
    if (0 < ninf) {
        /* unpack the info */
        cnt = ninf;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_INFO_FREE(info, ninfo);
            goto cleanup;
        }
        /* see if we are to collect data or enforce a timeout - we don't internally care
         * about any other directives */
        for (n = 0; n < ninf; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_COLLECT_DATA)) {
                collect_data = PMIX_INFO_TRUE(&info[n]);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
                PMIX_VALUE_GET_NUMBER(rc, &info[n].value, tv.tv_sec, uint32_t);
                if (PMIX_SUCCESS != rc) {
                    PMIX_PROC_FREE(procs, nprocs);
                    PMIX_INFO_FREE(info, ninfo);
                    return rc;
                }
            }
        }
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(NULL, procs, nprocs, PMIX_FENCENB_CMD))) {
        /* If no tracker was found - create and initialize it once */
        if (NULL == (trk = new_tracker(NULL, procs, nprocs, PMIX_FENCENB_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            /* DO NOT HANG */
            if (NULL != opcbfunc) {
                opcbfunc(PMIX_ERROR, cd);
            }
            rc = PMIX_ERROR;
            PMIX_INFO_FREE(info, ninfo);
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
        ninfo = 0;
    }

    /* add this contributor to the tracker so they get
     * notified when we are done */
    pmix_list_append(&trk->local_cbs, &cd->super);
    /* if a timeout was specified, set it */
    if (0 < tv.tv_sec && !trk->event_active) {
        PMIX_THREADSHIFT_DELAY(trk, fence_timeout, tv.tv_sec);
        trk->event_active = true;
    }

    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the barrier
     * across all participants has been completed */
    if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        pmix_output_verbose(2, pmix_server_globals.fence_output,
                            "fence LOCALLY complete");
        /* if a timeout was set, then we delete it here as we can
         * ONLY check for local completion. Otherwise, passing
         * the tracker object up to the host can result in
         * competing timeout events, and the host could return
         * the tracker AFTER we released it due to our internal
         * timeout firing */
        if (trk->event_active) {
            pmix_event_del(&trk->ev);
            trk->event_active = false;
        }
        /* if this is a purely local fence (i.e., all participants are local),
         * then it is done and we notify accordingly */
        if (pmix_server_globals.fence_localonly_opt && trk->local) {
            /* the modexcbfunc thread-shifts the call prior to processing,
             * so it is okay to call it directly from here. The switchyard
             * will acknowledge successful acceptance of the fence request,
             * but the client still requires a return from the callback in
             * that scenario, so we leave this caddy on the list of local cbs */
            rc = trk->info[trk->ninfo-1].value.data.status;
            trk->modexcbfunc(rc, NULL, 0, trk, NULL, NULL);
            rc = PMIX_SUCCESS;  // ensure the switchyard doesn't release the caddy
            goto cleanup;
        }
        /* this fence involves non-local procs - check if the
         * host supports it */
        if (NULL == pmix_host_server.fence_nb) {
            rc = PMIX_ERR_NOT_SUPPORTED;
            /* clear the caddy from this tracker so it can be
             * released upon return - the switchyard will send an
             * error to this caller, and so the fence completion
             * function doesn't need to do so */
            pmix_list_remove_item(&trk->local_cbs, &cd->super);
            cd->trk = NULL;
            /* we need to ensure that all other local participants don't
             * just hang waiting for the error return, so execute
             * the fence completion function - it threadshifts the call
             * prior to processing, so it is okay to call it directly
             * from here */
            trk->host_called = false; // the host will not be calling us back
            trk->modexcbfunc(rc, NULL, 0, trk, NULL, NULL);
            goto cleanup;
        }
        /* if the user asked us to collect data, then we have
         * to provide any locally collected data to the host
         * server so they can circulate it - only take data
         * from the specified procs as not everyone is necessarily
         * participating! And only take data intended for remote
         * or global distribution */

        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
        if (PMIX_SUCCESS != (rc = _collect_data(trk, &bucket))) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&bucket);
            /* clear the caddy from this tracker so it can be
             * released upon return - the switchyard will send an
             * error to this caller, and so the fence completion
             * function doesn't need to do so */
            pmix_list_remove_item(&trk->local_cbs, &cd->super);
            cd->trk = NULL;
            /* we need to ensure that all other local participants don't
             * just hang waiting for the error return, so execute
             * the fence completion function - it threadshifts the call
             * prior to processing, so it is okay to call it directly
             * from here */
            trk->modexcbfunc(rc, NULL, 0, trk, NULL, NULL);
            goto cleanup;
        }
        /* now unload the blob and pass it upstairs */
        PMIX_UNLOAD_BUFFER(&bucket, data, sz);
        PMIX_DESTRUCT(&bucket);
        trk->host_called = true;
        rc = pmix_host_server.fence_nb(trk->pcs, trk->npcs, trk->info, trk->ninfo, data, sz,
                                       trk->modexcbfunc, trk);
        if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
            /* clear the caddy from this tracker so it can be
             * released upon return - the switchyard will send an
             * error to this caller, and so the fence completion
             * function doesn't need to do so */
            pmix_list_remove_item(&trk->local_cbs, &cd->super);
            cd->trk = NULL;
            /* we need to ensure that all other local participants don't
             * just hang waiting for the error return, so execute
             * the fence completion function - it threadshifts the call
             * prior to processing, so it is okay to call it directly
             * from here */
            trk->host_called = false; // the host will not be calling us back
            trk->modexcbfunc(rc, NULL, 0, trk, NULL, NULL);
        } else if (PMIX_OPERATION_SUCCEEDED == rc) {
            /* the operation was atomically completed and the host will
             * not be calling us back - ensure we notify all participants.
             * the modexcbfunc thread-shifts the call prior to processing,
             * so it is okay to call it directly from here */
            trk->host_called = false; // the host will not be calling us back
            rc = trk->info[trk->ninfo-1].value.data.status;
            trk->modexcbfunc(rc, NULL, 0, trk, NULL, NULL);
            /* ensure that the switchyard doesn't release the caddy */
            rc = PMIX_SUCCESS;
        }
    }

cleanup:
    PMIX_PROC_FREE(procs, nprocs);
    return rc;
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;

    if (NULL != cd->keys) {
        PMIx_Argv_free(cd->keys);
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

pmix_status_t pmix_server_publish(pmix_peer_t *peer, pmix_buffer_t *buf,
                                  pmix_op_cbfunc_t cbfunc,
                                  void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_status_t rc;
    int32_t cnt;
    size_t ninfo;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_server_globals.pub_output, "recvd PUBLISH");

    if (NULL == pmix_host_server.publish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &uid, &cnt, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of info objects */
    cnt = 1;
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
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    pmix_strncpy(cd->info[cd->ninfo - 1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo - 1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo - 1].value.data.uint32 = uid;

    /* call the local server */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
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

static void lkcbfunc(pmix_status_t status, pmix_pdata_t data[],
                     size_t ndata, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;

    /* cleanup the caddy */
    if (NULL != cd->keys) {
        PMIx_Argv_free(cd->keys);
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
pmix_status_t pmix_server_lookup(pmix_peer_t *peer, pmix_buffer_t *buf,
                                 pmix_lookup_cbfunc_t cbfunc,
                                 void *cbdata)
{
    pmix_setup_caddy_t *cd;
    int32_t cnt;
    pmix_status_t rc;
    size_t nkeys, i;
    char *sptr;
    size_t ninfo;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_server_globals.pub_output, "recvd LOOKUP");

    if (NULL == pmix_host_server.lookup) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &uid, &cnt, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of keys */
    cnt = 1;
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
    for (i = 0; i < nkeys; i++) {
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &sptr, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        PMIx_Argv_append_nosize(&cd->keys, sptr);
        free(sptr);
    }
    /* unpack the number of info objects */
    cnt = 1;
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
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    pmix_strncpy(cd->info[cd->ninfo - 1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo - 1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo - 1].value.data.uint32 = uid;

    /* call the local server */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;
    rc = pmix_host_server.lookup(&proc, cd->keys, cd->info, cd->ninfo, lkcbfunc, cd);

cleanup:
    if (PMIX_SUCCESS != rc) {
        if (NULL != cd->keys) {
            PMIx_Argv_free(cd->keys);
        }
        if (NULL != cd->info) {
            PMIX_INFO_FREE(cd->info, cd->ninfo);
        }
        PMIX_RELEASE(cd);
    }
    return rc;
}

pmix_status_t pmix_server_unpublish(pmix_peer_t *peer, pmix_buffer_t *buf,
                                    pmix_op_cbfunc_t cbfunc,
                                    void *cbdata)
{
    pmix_setup_caddy_t *cd;
    int32_t cnt;
    pmix_status_t rc;
    size_t i, nkeys, ninfo;
    char *sptr;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_server_globals.pub_output, "recvd UNPUBLISH");

    if (NULL == pmix_host_server.unpublish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &uid, &cnt, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of keys */
    cnt = 1;
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
    for (i = 0; i < nkeys; i++) {
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &sptr, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        PMIx_Argv_append_nosize(&cd->keys, sptr);
        free(sptr);
    }
    /* unpack the number of info objects */
    cnt = 1;
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
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    pmix_strncpy(cd->info[cd->ninfo - 1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo - 1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo - 1].value.data.uint32 = uid;

    /* call the local server */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;
    rc = pmix_host_server.unpublish(&proc, cd->keys, cd->info, cd->ninfo, opcbfunc, cd);

cleanup:
    if (PMIX_SUCCESS != rc) {
        if (NULL != cd->keys) {
            PMIx_Argv_free(cd->keys);
        }
        if (NULL != cd->info) {
            PMIX_INFO_FREE(cd->info, cd->ninfo);
        }
        PMIX_RELEASE(cd);
    }
    return rc;
}

void pmix_server_spcbfunc(pmix_status_t status, char nspace[], void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_iof_req_t *req;
    pmix_buffer_t *msg;
    pmix_status_t rc;
    pmix_iof_cache_t *iof, *ionext;

    /* if it was successful, and there are IOF requests, then
     * register them now */
    if (PMIX_SUCCESS == status && PMIX_FWD_NO_CHANNELS != cd->channels) {
        /* record the request */
        req = PMIX_NEW(pmix_iof_req_t);
        if (NULL == req) {
            status = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        PMIX_RETAIN(cd->peer);
        req->requestor = cd->peer;
        req->nprocs = 1;
        PMIX_PROC_CREATE(req->procs, req->nprocs);
        PMIX_LOAD_PROCID(&req->procs[0], nspace, PMIX_RANK_WILDCARD);
        req->channels = cd->channels;
        req->local_id = pmix_pointer_array_add(&pmix_globals.iof_requests, req);
        /* process any cached IO */
        PMIX_LIST_FOREACH_SAFE (iof, ionext, &pmix_server_globals.iof, pmix_iof_cache_t) {
            /* if the channels don't match, then ignore it */
            if (!(iof->channel & req->channels)) {
                continue;
            }
            /* if the source does not match the request, then ignore it */
            if (!PMIX_CHECK_PROCID(&iof->source, &req->procs[0])) {
                continue;
            }
            /* never forward back to the source! This can happen if the source
             * is a launcher */
            if (PMIX_CHECK_NAMES(&iof->source, &req->requestor->info->pname)) {
                continue;
            }
            pmix_output_verbose(2, pmix_server_globals.iof_output,
                                "PMIX:SERVER:SPAWN delivering cached IOF from %s to %s",
                                PMIX_NAME_PRINT(&iof->source),
                                PMIX_PNAME_PRINT(&req->requestor->info->pname));
            /* setup the msg */
            if (NULL == (msg = PMIX_NEW(pmix_buffer_t))) {
                PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                rc = PMIX_ERR_OUT_OF_RESOURCE;
                break;
            }
            /* provide the source */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &iof->source, 1, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* provide the channel */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &iof->channel, 1, PMIX_IOF_CHANNEL);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* provide their local id */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &req->remote_id, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* provide any cached info */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &iof->ninfo, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            if (0 < iof->ninfo) {
                PMIX_BFROPS_PACK(rc, req->requestor, msg, iof->info, iof->ninfo, PMIX_INFO);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(msg);
                    break;
                }
            }
            /* pack the data */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, iof->bo, 1, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* send it to the requestor */
            PMIX_PTL_SEND_ONEWAY(rc, req->requestor, msg, PMIX_PTL_TAG_IOF);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
            }
            /* remove it from the list since it has now been forwarded */
            pmix_list_remove_item(&pmix_server_globals.iof, &iof->super);
            PMIX_RELEASE(iof);
        }
    }

cleanup:
    if (NULL != cd->spcbfunc) {
        cd->spcbfunc(status, nspace, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

void pmix_server_spawn_parser(pmix_peer_t *peer, pmix_setup_caddy_t *cd)
{
    size_t n;
    bool stdout_found = false, stderr_found = false, stddiag_found = false;

    /* run a quick check of the directives to see if any IOF
     * requests were included so we can set that up now - helps
     * to catch any early output - and a request for notification
     * of job termination so we can setup the event registration */
    cd->channels = PMIX_FWD_NO_CHANNELS;
    for (n = 0; n < cd->ninfo; n++) {
        if (PMIX_CHECK_KEY(&cd->info[n], PMIX_FWD_STDIN)) {
            if (PMIX_INFO_TRUE(&cd->info[n])) {
                cd->channels |= PMIX_FWD_STDIN_CHANNEL;
            }
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_FWD_STDOUT)) {
            stdout_found = true;
            if (PMIX_INFO_TRUE(&cd->info[n])) {
                cd->channels |= PMIX_FWD_STDOUT_CHANNEL;
            }
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_FWD_STDERR)) {
            stderr_found = true;
            if (PMIX_INFO_TRUE(&cd->info[n])) {
                cd->channels |= PMIX_FWD_STDERR_CHANNEL;
            }
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_FWD_STDDIAG)) {
            stddiag_found = true;
            if (PMIX_INFO_TRUE(&cd->info[n])) {
                cd->channels |= PMIX_FWD_STDDIAG_CHANNEL;
            }
        } else {
            pmix_iof_check_flags(&cd->info[n], &cd->flags);
        }
    }
    /* we will construct any required iof request tracker upon completion of the spawn
     * as we need the nspace of the spawned application! */

    if (PMIX_PEER_IS_TOOL(peer)) {
        /* if the requestor is a tool, we default to forwarding all
         * output IO channels */
        if (!stdout_found) {
            cd->channels |= PMIX_FWD_STDOUT_CHANNEL;
        }
        if (!stderr_found) {
            cd->channels |= PMIX_FWD_STDERR_CHANNEL;
        }
        if (!stddiag_found) {
            cd->channels |= PMIX_FWD_STDDIAG_CHANNEL;
        }
    }
}

pmix_status_t pmix_server_spawn(pmix_peer_t *peer, pmix_buffer_t *buf,
                                pmix_spawn_cbfunc_t cbfunc,
                                void *cbdata)
{
    pmix_setup_caddy_t *cd;
    int32_t cnt;
    pmix_status_t rc;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_server_globals.spawn_output,
                        "recvd SPAWN from %s",
                        PMIX_PNAME_PRINT(&peer->info->pname));

    if (NULL == pmix_host_server.spawn) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    PMIX_RETAIN(peer);
    cd->peer = peer;
    cd->spcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* unpack the number of job-level directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        if (NULL == cd->info) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        /* run a quick check of the directives to see if any IOF
         * requests were included so we can set that up now - helps
         * to catch any early output - and a request for notification
         * of job termination so we can setup the event registration */
        pmix_server_spawn_parser(peer, cd);
    }

    /* unpack the number of apps */
    cnt = 1;
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
    /* mark that we created the data */
    cd->copied = true;

    /* call the local server */
    PMIX_LOAD_PROCID(&proc, peer->info->pname.nspace, peer->info->pname.rank);
    rc = pmix_host_server.spawn(&proc, cd->info, cd->ninfo,
                                cd->apps, cd->napps,
                                pmix_server_spcbfunc, cd);

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

pmix_status_t pmix_server_disconnect(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                     pmix_op_cbfunc_t cbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_info_t *info = NULL;
    size_t nprocs, ninfo, ninf;
    pmix_server_trkr_t *trk;
    pmix_proc_t *procs = NULL;

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
    /* sort the procs */
    qsort(procs, nprocs, sizeof(pmix_proc_t), pmix_util_compare_proc);

    /* unpack the number of provided info structs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &ninf, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        return rc;
    }
    ninfo = ninf + 2;
    PMIX_INFO_CREATE(info, ninfo);
    if (NULL == info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* store the default response */
    rc = PMIX_SUCCESS;
    PMIX_INFO_LOAD(&info[ninf+1], PMIX_LOCAL_COLLECTIVE_STATUS, &rc, PMIX_STATUS);
    PMIX_INFO_LOAD(&info[ninf], PMIX_SORTED_PROC_ARRAY, NULL, PMIX_BOOL);
    if (0 < ninf) {
        /* unpack the info */
        cnt = ninf;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            goto cleanup;
        }
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(NULL, procs, nprocs, PMIX_DISCONNECTNB_CMD))) {
        /* we don't have this tracker yet, so get a new one */
        if (NULL == (trk = new_tracker(NULL, procs, nprocs, PMIX_DISCONNECTNB_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            rc = PMIX_ERROR;
            goto cleanup;
        }
        trk->op_cbfunc = cbfunc;
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
    pmix_list_append(&trk->local_cbs, &cd->super);
    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the [dis]connect
     * across all participants has been completed */
    if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        if (trk->local) {
            /* the operation is being atomically completed and the host will
             * not be calling us back - ensure we notify all participants.
             * the cbfunc thread-shifts the call prior to processing,
             * so it is okay to call it directly from here */
            trk->host_called = false; // the host will not be calling us back
            cbfunc(PMIX_SUCCESS, trk);
            /* ensure that the switchyard doesn't release the caddy */
            rc = PMIX_SUCCESS;
        } else if (NULL == pmix_host_server.disconnect) {
            PMIX_RELEASE(trk);
            rc = PMIX_ERR_NOT_SUPPORTED;
            goto cleanup;
        } else {
            trk->host_called = true;
            rc = pmix_host_server.disconnect(trk->pcs, trk->npcs, trk->info, trk->ninfo, cbfunc,
                                             trk);
            if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
                /* clear the caddy from this tracker so it can be
                 * released upon return - the switchyard will send an
                 * error to this caller, and so the op completion
                 * function doesn't need to do so */
                pmix_list_remove_item(&trk->local_cbs, &cd->super);
                cd->trk = NULL;
                /* we need to ensure that all other local participants don't
                 * just hang waiting for the error return, so execute
                 * the op completion function - it threadshifts the call
                 * prior to processing, so it is okay to call it directly
                 * from here */
                trk->host_called = false; // the host will not be calling us back
                cbfunc(rc, trk);
            } else if (PMIX_OPERATION_SUCCEEDED == rc) {
                /* the operation was atomically completed and the host will
                 * not be calling us back - ensure we notify all participants.
                 * the cbfunc thread-shifts the call prior to processing,
                 * so it is okay to call it directly from here */
                trk->host_called = false; // the host will not be calling us back
                cbfunc(PMIX_SUCCESS, trk);
                /* ensure that the switchyard doesn't release the caddy */
                rc = PMIX_SUCCESS;
            }
        }
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
    pmix_server_trkr_t *trk = (pmix_server_trkr_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_server_globals.connect_output, "ALERT: connect timeout fired");

    /* execute the provided callback function with the error */
    if (NULL != trk->op_cbfunc) {
        trk->op_cbfunc(PMIX_ERR_TIMEOUT, trk);
        return; // the cbfunc will have cleaned up the tracker
    }
    trk->event_active = false;
    PMIX_RELEASE(trk);
}

pmix_status_t pmix_server_connect(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                  pmix_op_cbfunc_t cbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_proc_t *procs = NULL;
    pmix_info_t *info = NULL;
    size_t nprocs, ninfo, n, ninf;
    pmix_server_trkr_t *trk;
    struct timeval tv = {0, 0};

    pmix_output_verbose(2, pmix_server_globals.connect_output, "recvd CONNECT from peer %s:%d",
                        cd->peer->info->pname.nspace, cd->peer->info->pname.rank);

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
    /* sort the procs */
    qsort(procs, nprocs, sizeof(pmix_proc_t), pmix_util_compare_proc);

    /* unpack the number of provided info structs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &ninf, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    ninfo = ninf + 2;
    PMIX_INFO_CREATE(info, ninfo);
    if (NULL == info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* store the default response */
    rc = PMIX_SUCCESS;
    PMIX_INFO_LOAD(&info[ninf+1], PMIX_LOCAL_COLLECTIVE_STATUS, &rc, PMIX_STATUS);
    PMIX_INFO_LOAD(&info[ninf], PMIX_SORTED_PROC_ARRAY, NULL, PMIX_BOOL);
    if (0 < ninf) {
        /* unpack the info */
        cnt = ninf;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        /* check for a timeout */
        for (n = 0; n < ninf; n++) {
            if (0 == strncmp(info[n].key, PMIX_TIMEOUT, PMIX_MAX_KEYLEN)) {
                tv.tv_sec = info[n].value.data.uint32;
                break;
            }
        }
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(NULL, procs, nprocs, PMIX_CONNECTNB_CMD))) {
        /* we don't have this tracker yet, so get a new one */
        if (NULL == (trk = new_tracker(NULL, procs, nprocs, PMIX_CONNECTNB_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            /* DO NOT HANG */
            if (NULL != cbfunc) {
                cbfunc(PMIX_ERROR, cd);
            }
            rc = PMIX_ERROR;
            goto cleanup;
        }
        trk->op_cbfunc = cbfunc;
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
    pmix_list_append(&trk->local_cbs, &cd->super);

    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the [dis]connect
     * across all participants has been completed */
    if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        /* if all the participants are local, then we don't need the host */
        if (trk->local) {
            /* the operation is being atomically completed and the host will
             * not be calling us back - ensure we notify all participants.
             * the cbfunc thread-shifts the call prior to processing,
             * so it is okay to call it directly from here */
            trk->host_called = false; // the host will not be calling us back
            cbfunc(PMIX_SUCCESS, trk);
            /* ensure that the switchyard doesn't release the caddy */
            rc = PMIX_SUCCESS;
        } else if (NULL == pmix_host_server.connect) {
            PMIX_RELEASE(trk);
            rc = PMIX_ERR_NOT_SUPPORTED;
            goto cleanup;
        } else {
            trk->host_called = true;
            rc = pmix_host_server.connect(trk->pcs, trk->npcs, trk->info, trk->ninfo, cbfunc, trk);
            if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc) {
                /* clear the caddy from this tracker so it can be
                 * released upon return - the switchyard will send an
                 * error to this caller, and so the op completion
                 * function doesn't need to do so */
                pmix_list_remove_item(&trk->local_cbs, &cd->super);
                cd->trk = NULL;
                /* we need to ensure that all other local participants don't
                 * just hang waiting for the error return, so execute
                 * the op completion function - it threadshifts the call
                 * prior to processing, so it is okay to call it directly
                 * from here */
                trk->host_called = false; // the host will not be calling us back
                cbfunc(rc, trk);
            } else if (PMIX_OPERATION_SUCCEEDED == rc) {
                /* the operation was atomically completed and the host will
                 * not be calling us back - ensure we notify all participants.
                 * the cbfunc thread-shifts the call prior to processing,
                 * so it is okay to call it directly from here */
                trk->host_called = false; // the host will not be calling us back
                cbfunc(PMIX_SUCCESS, trk);
                /* ensure that the switchyard doesn't release the caddy */
                rc = PMIX_SUCCESS;
            }
        }
    } else {
        rc = PMIX_SUCCESS;
    }
    /* if a timeout was specified, set it */
    if (PMIX_SUCCESS == rc && 0 < tv.tv_sec) {
        PMIX_RETAIN(trk);
        PMIX_THREADSHIFT_DELAY(trk, connect_timeout, tv.tv_sec);
        trk->event_active = true;
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

static void _check_cached_events(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *scd = (pmix_setup_caddy_t *) cbdata;
    pmix_notify_caddy_t *cd;
    pmix_range_trkr_t rngtrk;
    pmix_proc_t proc;
    int i;
    size_t k, n;
    bool found, matched;
    pmix_buffer_t *relay;
    pmix_status_t ret = PMIX_SUCCESS;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;

    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* check if any matching notifications have been cached */
    rngtrk.procs = NULL;
    rngtrk.nprocs = 0;
    for (i = 0; i < pmix_globals.max_events; i++) {
        pmix_hotel_knock(&pmix_globals.notifications, i, (void **) &cd);
        if (NULL == cd) {
            continue;
        }
        found = false;
        if (NULL == scd->codes) {
            if (!cd->nondefault) {
                /* they registered a default event handler - always matches */
                found = true;
            }
        } else {
            for (k = 0; k < scd->ncodes; k++) {
                if (scd->codes[k] == cd->status) {
                    found = true;
                    break;
                }
            }
        }
        if (!found) {
            continue;
        }
        /* check if the affected procs (if given) match those they
         * wanted to know about */
        if (!pmix_notify_check_affected(cd->affected, cd->naffected, scd->procs, scd->nprocs)) {
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
        PMIX_LOAD_PROCID(&proc, scd->peer->info->pname.nspace, scd->peer->info->pname.rank);
        if (!pmix_notify_check_range(&rngtrk, &proc)) {
            continue;
        }
        /* if we were given specific targets, check if this is one */
        found = false;
        if (NULL != cd->targets) {
            matched = false;
            for (n = 0; n < cd->ntargets; n++) {
                /* if the source of the event is the same peer just registered, then ignore it
                 * as the event notification system will have already locally
                 * processed it */
                if (PMIX_CHECK_NAMES(&cd->source, &scd->peer->info->pname)) {
                    continue;
                }
                if (PMIX_CHECK_NAMES(&scd->peer->info->pname, &cd->targets[n])) {
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
            ret = PMIX_ERR_NOMEM;
            break;
        }
        /* pack the info data stored in the event */
        PMIX_BFROPS_PACK(ret, scd->peer, relay, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            break;
        }
        PMIX_BFROPS_PACK(ret, scd->peer, relay, &cd->status, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            break;
        }
        PMIX_BFROPS_PACK(ret, scd->peer, relay, &cd->source, 1, PMIX_PROC);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            break;
        }
        PMIX_BFROPS_PACK(ret, scd->peer, relay, &cd->ninfo, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            break;
        }
        if (0 < cd->ninfo) {
            PMIX_BFROPS_PACK(ret, scd->peer, relay, cd->info, cd->ninfo, PMIX_INFO);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                break;
            }
        }
        PMIX_SERVER_QUEUE_REPLY(ret, scd->peer, 0, relay);
        if (PMIX_SUCCESS != ret) {
            PMIX_RELEASE(relay);
        }
        if (found) {
            PMIX_RELEASE(cd);
        }
    }
    /* release the caddy */
    if (NULL != scd->codes) {
        free(scd->codes);
    }
    if (NULL != scd->info) {
        PMIX_INFO_FREE(scd->info, scd->ninfo);
    }
    if (NULL != scd->opcbfunc) {
        scd->opcbfunc(ret, scd->cbdata);
    }
    PMIX_RELEASE(scd);
}

/* provide a callback function for the host when it finishes
 * processing the registration */
static void regevopcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;

    /* if the registration succeeded, then check local cache */
    if (PMIX_SUCCESS == status) {
        _check_cached_events(0, 0, cd);
        return;
    }

    /* it didn't succeed, so cleanup and execute the callback
     * so we don't hang */
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

pmix_status_t pmix_server_register_events(pmix_peer_t *peer, pmix_buffer_t *buf,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_status_t *codes = NULL;
    pmix_info_t *info = NULL;
    size_t ninfo = 0, ncodes, n;
    pmix_regevents_info_t *reginfo, *rptr;
    pmix_peer_events_info_t *prev = NULL;
    pmix_setup_caddy_t *scd;
    bool enviro_events = false;
    bool found;
    pmix_proc_t *affected = NULL;
    size_t naffected = 0;

    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "recvd register events for peer %s:%d",
                        peer->info->pname.nspace, peer->info->pname.rank);

    /* unpack the number of codes */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ncodes, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of codes */
    if (0 < ncodes) {
        codes = (pmix_status_t *) malloc(ncodes * sizeof(pmix_status_t));
        if (NULL == codes) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        cnt = ncodes;
        PMIX_BFROPS_UNPACK(rc, peer, buf, codes, &cnt, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* unpack the number of info objects */
    cnt = 1;
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
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* check the directives */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_EVENT_AFFECTED_PROC)) {
            if (NULL != affected) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                rc = PMIX_ERR_BAD_PARAM;
                goto cleanup;
            }
            naffected = 1;
            PMIX_PROC_CREATE(affected, naffected);
            memcpy(affected, info[n].value.data.proc, sizeof(pmix_proc_t));
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_EVENT_AFFECTED_PROCS)) {
            if (NULL != affected) {
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                rc = PMIX_ERR_BAD_PARAM;
                goto cleanup;
            }
            naffected = info[n].value.data.darray->size;
            PMIX_PROC_CREATE(affected, naffected);
            memcpy(affected, info[n].value.data.darray->array, naffected * sizeof(pmix_proc_t));
        }
    }

    /* check the codes for system events */
    for (n = 0; n < ncodes; n++) {
        if (PMIX_SYSTEM_EVENT(codes[n])) {
            enviro_events = true;
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

    /* if they didn't send us any codes, then they are registering a
     * default event handler. In that case, check only for default
     * handlers and add this request to it, if not already present */
    if (0 == ncodes) {
        PMIX_LIST_FOREACH (reginfo, &pmix_server_globals.events, pmix_regevents_info_t) {
            if (PMIX_MAX_ERR_CONSTANT == reginfo->code) {
                /* both are default handlers */
                prev = PMIX_NEW(pmix_peer_events_info_t);
                if (NULL == prev) {
                    rc = PMIX_ERR_NOMEM;
                    goto cleanup;
                }
                PMIX_RETAIN(peer);
                prev->peer = peer;
                if (NULL != affected) {
                    PMIX_PROC_CREATE(prev->affected, naffected);
                    prev->naffected = naffected;
                    memcpy(prev->affected, affected, naffected * sizeof(pmix_proc_t));
                }
                pmix_list_append(&reginfo->peers, &prev->super);
                break;
            }
        }
        rc = PMIX_OPERATION_SUCCEEDED;
        goto cleanup;
    }

    /* store the event registration info so we can call the registered
     * client when the server notifies the event */
    for (n = 0; n < ncodes; n++) {
        found = false;
        PMIX_LIST_FOREACH (reginfo, &pmix_server_globals.events, pmix_regevents_info_t) {
            if (PMIX_MAX_ERR_CONSTANT == reginfo->code) {
                continue;
            } else if (codes[n] == reginfo->code) {
                found = true;
                break;
            }
        }
        if (found) {
            /* found it - add this request */
            prev = PMIX_NEW(pmix_peer_events_info_t);
            if (NULL == prev) {
                rc = PMIX_ERR_NOMEM;
                goto cleanup;
            }
            PMIX_RETAIN(peer);
            prev->peer = peer;
            if (NULL != affected) {
                PMIX_PROC_CREATE(prev->affected, naffected);
                prev->naffected = naffected;
                memcpy(prev->affected, affected, naffected * sizeof(pmix_proc_t));
            }
            prev->enviro_events = enviro_events;
            pmix_list_append(&reginfo->peers, &prev->super);
        } else {
            /* if we get here, then we didn't find an existing registration for this code */
            rptr = PMIX_NEW(pmix_regevents_info_t);
            if (NULL == rptr) {
                rc = PMIX_ERR_NOMEM;
                goto cleanup;
            }
            rptr->code = codes[n];
            pmix_list_append(&pmix_server_globals.events, &rptr->super);
            prev = PMIX_NEW(pmix_peer_events_info_t);
            if (NULL == prev) {
                rc = PMIX_ERR_NOMEM;
                goto cleanup;
            }
            PMIX_RETAIN(peer);
            prev->peer = peer;
            if (NULL != affected) {
                PMIX_PROC_CREATE(prev->affected, naffected);
                prev->naffected = naffected;
                memcpy(prev->affected, affected, naffected * sizeof(pmix_proc_t));
            }
            prev->enviro_events = enviro_events;
            pmix_list_append(&rptr->peers, &prev->super);
        }
    }

    /* if they asked for enviro events, call the local server */
    if (enviro_events) {
        /* if they don't support this, then we cannot do it */
        if (NULL == pmix_host_server.register_events) {
            rc = PMIX_ERR_NOT_SUPPORTED;
            goto cleanup;
        }
        /* need to ensure the arrays don't go away until after the
         * host RM is done with them */
        scd = PMIX_NEW(pmix_setup_caddy_t);
        if (NULL == scd) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        PMIX_RETAIN(peer);
        scd->peer = peer;
        scd->codes = codes;
        scd->ncodes = ncodes;
        scd->info = info;
        scd->ninfo = ninfo;
        scd->opcbfunc = cbfunc;
        scd->cbdata = cbdata;
        if (PMIX_SUCCESS
            == (rc = pmix_host_server.register_events(scd->codes, scd->ncodes, scd->info,
                                                      scd->ninfo, regevopcbfunc, scd))) {
            /* the host will call us back when completed */
            pmix_output_verbose(
                2, pmix_server_globals.event_output,
                "server register events: host server processing event registration");
            if (NULL != affected) {
                free(affected);
            }
            return rc;
        } else if (PMIX_OPERATION_SUCCEEDED == rc) {
            /* we need to check cached notifications, but we want to ensure
             * that occurs _after_ the client returns from registering the
             * event handler in case the event is flagged for do_not_cache.
             * Setup an event to fire after we return as that means it will
             * occur after we send the registration response back to the client,
             * thus guaranteeing that the client will get their registration
             * callback prior to delivery of an event notification */
            PMIX_RETAIN(peer);
            scd->peer = peer;
            scd->procs = affected;
            scd->nprocs = naffected;
            scd->opcbfunc = NULL;
            scd->cbdata = NULL;
            PMIX_THREADSHIFT(scd, _check_cached_events);
            return rc;
        } else {
            /* host returned a genuine error and won't be calling the callback function */
            pmix_output_verbose(2, pmix_server_globals.event_output,
                                "server register events: host server reg events returned rc =%d",
                                rc);
            PMIX_RELEASE(scd);
            goto cleanup;
        }
    } else {
        rc = PMIX_OPERATION_SUCCEEDED;
        /* we need to check cached notifications, but we want to ensure
         * that occurs _after_ the client returns from registering the
         * event handler in case the event is flagged for do_not_cache.
         * Setup an event to fire after we return as that means it will
         * occur after we send the registration response back to the client,
         * thus guaranteeing that the client will get their registration
         * callback prior to delivery of an event notification */
        scd = PMIX_NEW(pmix_setup_caddy_t);
        PMIX_RETAIN(peer);
        scd->peer = peer;
        scd->codes = codes;
        scd->ncodes = ncodes;
        scd->procs = affected;
        scd->nprocs = naffected;
        scd->opcbfunc = NULL;
        scd->cbdata = NULL;
        PMIX_THREADSHIFT(scd, _check_cached_events);
        if (NULL != info) {
            PMIX_INFO_FREE(info, ninfo);
        }
        return rc;
    }

cleanup:
    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "server register events: ninfo =%lu rc =%d", ninfo, rc);
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    if (NULL != codes) {
        free(codes);
    }
    if (NULL != affected) {
        PMIX_PROC_FREE(affected, naffected);
    }
    return rc;
}

void pmix_server_deregister_events(pmix_peer_t *peer, pmix_buffer_t *buf)
{
    int32_t cnt;
    pmix_status_t rc, code;
    pmix_regevents_info_t *reginfo = NULL;
    pmix_regevents_info_t *reginfo_next;
    pmix_peer_events_info_t *prev;

    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "%s recvd deregister events from %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid),
                        PMIX_PEER_PRINT(peer));

    /* unpack codes and process until done */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &code, &cnt, PMIX_STATUS);
    while (PMIX_SUCCESS == rc) {
        PMIX_LIST_FOREACH_SAFE (reginfo, reginfo_next, &pmix_server_globals.events,
                                pmix_regevents_info_t) {
            if (code == reginfo->code) {
                /* found it - remove this peer from the list */
                PMIX_LIST_FOREACH (prev, &reginfo->peers, pmix_peer_events_info_t) {
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
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &code, &cnt, PMIX_STATUS);
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
    }
}

static void local_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t *) cbdata;

    if (NULL != cd->cbfunc) {
        cd->cbfunc(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

static void intermed_step(pmix_status_t status, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t *) cbdata;
    pmix_status_t rc;

    if (PMIX_SUCCESS != status) {
        rc = status;
        goto complete;
    }

    /* check the range directive - if it is LOCAL, then we are
     * done. Otherwise, it needs to go up to our
     * host for dissemination */
    if (PMIX_RANGE_LOCAL == cd->range) {
        rc = PMIX_SUCCESS;
        goto complete;
    }

    /* pass it to our host RM for distribution */
    rc = pmix_prm.notify(cd->status, &cd->source, cd->range,
                         cd->info, cd->ninfo, local_cbfunc, cd);
    if (PMIX_SUCCESS == rc) {
        /* let the callback function respond for us */
        return;
    }
    if (PMIX_OPERATION_SUCCEEDED == rc ||
        PMIX_ERR_NOT_SUPPORTED == rc) {
        rc = PMIX_SUCCESS; // local_cbfunc will not be called
    }

complete:
    if (NULL != cd->cbfunc) {
        cd->cbfunc(rc, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

/* Receive an event sent by the client library. Since it was sent
 * to us by one client, we have to both process it locally to ensure
 * we notify all relevant local clients AND (assuming a range other
 * than LOCAL) deliver to our host, requesting that they send it
 * to all peer servers in the current session */
pmix_status_t pmix_server_event_recvd_from_client(pmix_peer_t *peer, pmix_buffer_t *buf,
                                                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_notify_caddy_t *cd;
    size_t ninfo, n;

    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "%s:%d recvd event notification from client %s:%d",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank, peer->info->pname.nspace,
                        peer->info->pname.rank);

    cd = PMIX_NEW(pmix_notify_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* set the source */
    PMIX_LOAD_PROCID(&cd->source, peer->info->pname.nspace, peer->info->pname.rank);

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
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    cd->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    if (NULL == cd->info) {
        rc = PMIX_ERR_NOMEM;
        goto exit;
    }
    if (0 < ninfo) {
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* check to see if we already processed this event - it is possible
     * that a local client "echoed" it back to us and we want to avoid
     * a potential infinite loop */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&cd->info[n], PMIX_SERVER_INTERNAL_NOTIFY)) {
            /* yep, we did - so don't do it again! */
            rc = PMIX_OPERATION_SUCCEEDED;
            goto exit;
        }
    }

    /* add an info object to mark that we recvd this internally */
    PMIX_INFO_LOAD(&cd->info[cd->ninfo - 1], PMIX_SERVER_INTERNAL_NOTIFY, NULL, PMIX_BOOL);

    /* process it */
    rc = pmix_server_notify_client_of_event(cd->status, &cd->source, cd->range, cd->info,
                                            cd->ninfo, intermed_step, cd);
    if (PMIX_SUCCESS != rc) {
        goto exit;
    }
    return rc;

exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_query(pmix_peer_t *peer, pmix_buffer_t *buf,
                                pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd query from client");

    cd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;  // this is the pmix_server_caddy_t we were given
    /* unpack the number of queries */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->nqueries, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }
    /* unpack the queries */
    if (0 < cd->nqueries) {
        PMIX_QUERY_CREATE(cd->queries, cd->nqueries);
        if (NULL == cd->queries) {
            rc = PMIX_ERR_NOMEM;
            PMIX_RELEASE(cd);
            return rc;
        }
        cnt = cd->nqueries;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->queries, &cnt, PMIX_QUERY);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
            return rc;
        }
    }
    PMIX_THREADSHIFT(cd, pmix_parse_localquery);
    return PMIX_SUCCESS;
}

static void logcbfn(pmix_status_t status, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;

    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}
pmix_status_t pmix_server_log(pmix_peer_t *peer, pmix_buffer_t *buf,
                              pmix_op_cbfunc_t cbfunc,
                              void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_shift_caddy_t *cd;
    pmix_proc_t proc;
    time_t timestamp;

    pmix_output_verbose(2, pmix_server_globals.base_output, "recvd log from client");

    /* we need to deliver this to our internal log capability,
     * which may upcall it to our host if it cannot process
     * the request itself */

    /* setup the requesting peer name */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;
    if (PMIX_PEER_IS_EARLIER(peer, 3, 0, 0)) {
        timestamp = -1;
    } else {
        /* unpack the timestamp */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &timestamp, &cnt, PMIX_TIME);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* unpack the number of data */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    cnt = cd->ninfo;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    /* unpack the data */
    if (0 < cnt) {
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
    cnt = cd->ndirs;
    /* always add the source to the directives so we
     * can tell downstream if this gets "upcalled" to
     * our host for relay */
    cd->ndirs = cnt + 1;
    /* if a timestamp was sent, then we add it to the directives */
    if (0 < timestamp) {
        cd->ndirs++;
        PMIX_INFO_CREATE(cd->directives, cd->ndirs);
        PMIX_INFO_LOAD(&cd->directives[cnt], PMIX_LOG_SOURCE, &proc, PMIX_PROC);
        PMIX_INFO_LOAD(&cd->directives[cnt + 1], PMIX_LOG_TIMESTAMP, &timestamp, PMIX_TIME);
    } else {
        PMIX_INFO_CREATE(cd->directives, cd->ndirs);
        PMIX_INFO_LOAD(&cd->directives[cnt], PMIX_LOG_SOURCE, &proc, PMIX_PROC);
    }

    /* unpack the directives */
    if (0 < cnt) {
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->directives, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* pass it down */
    rc = pmix_plog.log(&proc, cd->info, cd->ninfo, cd->directives, cd->ndirs, logcbfn, cd);
    return rc;

exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_alloc(pmix_peer_t *peer, pmix_buffer_t *buf,
                                pmix_info_cbfunc_t cbfunc,
                                void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;
    pmix_alloc_directive_t directive;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "%s recvd allocate request from client %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid),
                        PMIX_PEER_PRINT(peer));

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
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS
        != (rc = pmix_host_server.allocate(&proc, directive, cd->info, cd->ninfo, cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

exit:
    PMIX_RELEASE(cd);
    return rc;
}

typedef struct {
    pmix_list_item_t super;
    pmix_epilog_t *epi;
} pmix_srvr_epi_caddy_t;
static PMIX_CLASS_INSTANCE(pmix_srvr_epi_caddy_t, pmix_list_item_t, NULL, NULL);

pmix_status_t pmix_server_job_ctrl(pmix_peer_t *peer, pmix_buffer_t *buf,
                                   pmix_info_cbfunc_t cbfunc,
                                   void *cbdata)
{
    int32_t cnt, m;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_namespace_t *nptr, *tmp;
    pmix_peer_t *pr;
    pmix_proc_t proc;
    size_t n;
    bool recurse = false, leave_topdir = false, duplicate;
    pmix_list_t cachedirs, cachefiles, ignorefiles, epicache;
    pmix_srvr_epi_caddy_t *epicd = NULL;
    pmix_cleanup_file_t *cf, *cf2, *cfptr;
    pmix_cleanup_dir_t *cdir, *cdir2, *cdirptr;

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

    PMIX_CONSTRUCT(&epicache, pmix_list_t);

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
        epicd = PMIX_NEW(pmix_srvr_epi_caddy_t);
        epicd->epi = &peer->nptr->epilog;
        pmix_list_append(&epicache, &epicd->super);
    } else {
        for (n = 0; n < cd->ntargets; n++) {
            /* find the nspace of this proc */
            nptr = NULL;
            PMIX_LIST_FOREACH (tmp, &pmix_globals.nspaces, pmix_namespace_t) {
                if (0 == strcmp(tmp->nspace, cd->targets[n].nspace)) {
                    nptr = tmp;
                    break;
                }
            }
            if (NULL == nptr) {
                nptr = PMIX_NEW(pmix_namespace_t);
                if (NULL == nptr) {
                    rc = PMIX_ERR_NOMEM;
                    goto exit;
                }
                nptr->nspace = strdup(cd->targets[n].nspace);
                pmix_list_append(&pmix_globals.nspaces, &nptr->super);
            }
            /* if the rank is wildcard, then we use the epilog for the nspace */
            if (PMIX_RANK_WILDCARD == cd->targets[n].rank) {
                epicd = PMIX_NEW(pmix_srvr_epi_caddy_t);
                epicd->epi = &nptr->epilog;
                pmix_list_append(&epicache, &epicd->super);
            } else {
                /* we need to find the precise peer - we can only
                 * do cleanup for a local client */
                for (m = 0; m < pmix_server_globals.clients.size; m++) {
                    pr = (pmix_peer_t *)pmix_pointer_array_get_item(&pmix_server_globals.clients, m);
                    if (NULL == pr) {
                        continue;
                    }
                    if (!PMIX_CHECK_NSPACE(pr->info->pname.nspace, cd->targets[n].nspace)) {
                        continue;
                    }
                    if (pr->info->pname.rank == cd->targets[n].rank) {
                        epicd = PMIX_NEW(pmix_srvr_epi_caddy_t);
                        epicd->epi = &pr->epilog;
                        pmix_list_append(&epicache, &epicd->super);
                        break;
                    }
                }
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
    PMIX_CONSTRUCT(&ignorefiles, pmix_list_t);

    cnt = 0; // track how many infos are cleanup related
    for (n = 0; n < cd->ninfo; n++) {
        if (PMIX_CHECK_KEY(&cd->info[n], PMIX_REGISTER_CLEANUP)) {
            ++cnt;
            if (PMIX_STRING != cd->info[n].value.type || NULL == cd->info[n].value.data.string) {
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
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_REGISTER_CLEANUP_DIR)) {
            ++cnt;
            if (PMIX_STRING != cd->info[n].value.type || NULL == cd->info[n].value.data.string) {
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
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_CLEANUP_RECURSIVE)) {
            recurse = PMIX_INFO_TRUE(&cd->info[n]);
            ++cnt;
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_CLEANUP_IGNORE)) {
            if (PMIX_STRING != cd->info[n].value.type || NULL == cd->info[n].value.data.string) {
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
            pmix_list_append(&ignorefiles, &cf->super);
            ++cnt;
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_CLEANUP_LEAVE_TOPDIR)) {
            leave_topdir = PMIX_INFO_TRUE(&cd->info[n]);
            ++cnt;
        }
    }
    if (0 < cnt) {
        /* handle any ignore directives first */
        PMIX_LIST_FOREACH (cf, &ignorefiles, pmix_cleanup_file_t) {
            PMIX_LIST_FOREACH (epicd, &epicache, pmix_srvr_epi_caddy_t) {
                /* scan the existing list of files for any duplicate */
                duplicate = false;
                PMIX_LIST_FOREACH (cf2, &epicd->epi->cleanup_files, pmix_cleanup_file_t) {
                    if (0 == strcmp(cf2->path, cf->path)) {
                        duplicate = true;
                        break;
                    }
                }
                if (!duplicate) {
                    /* append it to the end of the list */
                    cfptr = PMIX_NEW(pmix_cleanup_file_t);
                    cfptr->path = strdup(cf->path);
                    pmix_list_append(&epicd->epi->ignores, &cf->super);
                }
            }
        }
        PMIX_LIST_DESTRUCT(&ignorefiles);
        /* now look at the directories */
        PMIX_LIST_FOREACH (cdir, &cachedirs, pmix_cleanup_dir_t) {
            PMIX_LIST_FOREACH (epicd, &epicache, pmix_srvr_epi_caddy_t) {
                /* scan the existing list of directories for any duplicate */
                duplicate = false;
                PMIX_LIST_FOREACH (cdir2, &epicd->epi->cleanup_dirs, pmix_cleanup_dir_t) {
                    if (0 == strcmp(cdir2->path, cdir->path)) {
                        /* duplicate - check for difference in flags per RFC
                         * precedence rules */
                        if (!cdir->recurse && recurse) {
                            cdir->recurse = recurse;
                        }
                        if (!cdir->leave_topdir && leave_topdir) {
                            cdir->leave_topdir = leave_topdir;
                        }
                        duplicate = true;
                        break;
                    }
                }
                if (!duplicate) {
                    /* check for conflict with ignore */
                    PMIX_LIST_FOREACH (cf, &epicd->epi->ignores, pmix_cleanup_file_t) {
                        if (0 == strcmp(cf->path, cdir->path)) {
                            /* return an error */
                            rc = PMIX_ERR_CONFLICTING_CLEANUP_DIRECTIVES;
                            PMIX_LIST_DESTRUCT(&cachedirs);
                            PMIX_LIST_DESTRUCT(&cachefiles);
                            goto exit;
                        }
                    }
                    /* append it to the end of the list */
                    cdirptr = PMIX_NEW(pmix_cleanup_dir_t);
                    cdirptr->path = strdup(cdir->path);
                    cdirptr->recurse = recurse;
                    cdirptr->leave_topdir = leave_topdir;
                    pmix_list_append(&epicd->epi->cleanup_dirs, &cdirptr->super);
                }
            }
        }
        PMIX_LIST_DESTRUCT(&cachedirs);
        PMIX_LIST_FOREACH (cf, &cachefiles, pmix_cleanup_file_t) {
            PMIX_LIST_FOREACH (epicd, &epicache, pmix_srvr_epi_caddy_t) {
                /* scan the existing list of files for any duplicate */
                duplicate = false;
                PMIX_LIST_FOREACH (cf2, &epicd->epi->cleanup_files, pmix_cleanup_file_t) {
                    if (0 == strcmp(cf2->path, cf->path)) {
                        duplicate = true;
                        break;
                    }
                }
                if (!duplicate) {
                    /* check for conflict with ignore */
                    PMIX_LIST_FOREACH (cf2, &epicd->epi->ignores, pmix_cleanup_file_t) {
                        if (0 == strcmp(cf->path, cf2->path)) {
                            /* return an error */
                            rc = PMIX_ERR_CONFLICTING_CLEANUP_DIRECTIVES;
                            PMIX_LIST_DESTRUCT(&cachedirs);
                            PMIX_LIST_DESTRUCT(&cachefiles);
                            goto exit;
                        }
                    }
                    /* append it to the end of the list */
                    cfptr = PMIX_NEW(pmix_cleanup_file_t);
                    cfptr->path = strdup(cf->path);
                    pmix_list_append(&epicd->epi->cleanup_files, &cfptr->super);
                }
            }
        }
        PMIX_LIST_DESTRUCT(&cachefiles);
        if (cnt == (int) cd->ninfo) {
            /* nothing more to do */
            rc = PMIX_OPERATION_SUCCEEDED;
            goto exit;
        }
    }

    /* setup the requesting peer name */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS
        != (rc = pmix_host_server.job_control(&proc, cd->targets, cd->ntargets, cd->info, cd->ninfo,
                                              cbfunc, cd))) {
        goto exit;
    }
    PMIX_LIST_DESTRUCT(&epicache);
    return PMIX_SUCCESS;

exit:
    PMIX_RELEASE(cd);
    PMIX_LIST_DESTRUCT(&epicache);
    return rc;
}

pmix_status_t pmix_server_monitor(pmix_peer_t *peer, pmix_buffer_t *buf,
                                  pmix_info_cbfunc_t cbfunc,
                                  void *cbdata)
{
    int32_t cnt;
    pmix_info_t monitor;
    pmix_status_t rc, error;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_server_globals.base_output, "recvd monitor request from client");

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

    /* see if they are requesting one of the monitoring
     * methods we internally support */
    rc = pmix_psensor.start(peer, error, &monitor, cd->info, cd->ninfo);
    if (PMIX_SUCCESS == rc) {
        rc = PMIX_OPERATION_SUCCEEDED;
        goto exit;
    }
    if (PMIX_ERR_NOT_SUPPORTED != rc) {
        goto exit;
    }

    /* if we don't internally support it, see if
     * our host does */
    if (NULL == pmix_host_server.monitor) {
        rc = PMIX_ERR_NOT_SUPPORTED;
        goto exit;
    }

    /* setup the requesting peer name */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    rc = pmix_host_server.monitor(&proc, &monitor, error,
                                  cd->info, cd->ninfo,
                                  cbfunc, cd);
    if (PMIX_SUCCESS != rc) {
        goto exit;
    }
    return PMIX_SUCCESS;

exit:
    PMIX_INFO_DESTRUCT(&monitor);
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_get_credential(pmix_peer_t *peer, pmix_buffer_t *buf,
                                         pmix_credential_cbfunc_t cbfunc, void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *cd;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_globals.debug_output, "recvd get credential request from client");

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
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS
        != (rc = pmix_host_server.get_credential(&proc, cd->info, cd->ninfo, cbfunc, cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_validate_credential(pmix_peer_t *peer, pmix_buffer_t *buf,
                                              pmix_validation_cbfunc_t cbfunc, void *cbdata)
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
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS
        != (rc = pmix_host_server.validate_credential(&proc, &cd->bo, cd->info, cd->ninfo, cbfunc,
                                                      cd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_iofreg(pmix_peer_t *peer, pmix_buffer_t *buf,
                                 pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_setup_caddy_t *cd;
    pmix_iof_req_t *req;
    size_t refid;

    pmix_output_verbose(2, pmix_server_globals.iof_output, "recvd IOF PULL request from client");

    if (NULL == pmix_host_server.iof_pull) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata; // this is the pmix_server_caddy_t

    /* unpack the number of procs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the procs */
    if (0 < cd->nprocs) {
        PMIX_PROC_CREATE(cd->procs, cd->nprocs);
        cnt = cd->nprocs;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->procs, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
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

    /* unpack the channels */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->channels, &cnt, PMIX_IOF_CHANNEL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack their local reference id */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &refid, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* add this peer/source/channel combination */
    req = PMIX_NEW(pmix_iof_req_t);
    if (NULL == req) {
        rc = PMIX_ERR_NOMEM;
        goto exit;
    }
    PMIX_RETAIN(peer);
    req->requestor = peer;
    req->nprocs = cd->nprocs;
    if (0 < req->nprocs) {
        PMIX_PROC_CREATE(req->procs, cd->nprocs);
        memcpy(req->procs, cd->procs, req->nprocs * sizeof(pmix_proc_t));
    }
    req->channels = cd->channels;
    req->remote_id = refid;
    req->local_id = pmix_pointer_array_add(&pmix_globals.iof_requests, req);
    cd->ncodes = req->local_id;

    /* ask the host to execute the request */
    rc = pmix_host_server.iof_pull(cd->procs, cd->nprocs,
                                   cd->info, cd->ninfo,
                                   cd->channels, cbfunc, cd);
    if (PMIX_OPERATION_SUCCEEDED == rc) {
        /* the host did it atomically - send the response. In
         * this particular case, we can just use the cbfunc
         * ourselves as it will threadshift and guarantee
         * proper handling (i.e. that the refid will be
         * returned in the response to the client) */
        cbfunc(PMIX_SUCCESS, cd);
        /* returning other than SUCCESS will cause the
         * switchyard to release the cd object */
        return PMIX_SUCCESS;
    }

exit:
    PMIX_RELEASE(cd);
    return rc;
}

pmix_status_t pmix_server_iofdereg(pmix_peer_t *peer, pmix_buffer_t *buf,
                                   pmix_op_cbfunc_t cbfunc,
                                   void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_setup_caddy_t *cd;
    pmix_iof_req_t *req;
    size_t ninfo, refid;

    pmix_output_verbose(2, pmix_server_globals.iof_output, "recvd IOF DEREGISTER from client");

    if (NULL == pmix_host_server.iof_pull) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata; // this is the pmix_server_caddy_t

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the directives - note that we have to add one
     * to tell the server to stop forwarding to this channel */
    cd->ninfo = ninfo + 1;
    PMIX_INFO_CREATE(cd->info, cd->ninfo);
    if (0 < ninfo) {
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }
    /* add the directive to stop forwarding */
    PMIX_INFO_LOAD(&cd->info[ninfo], PMIX_IOF_STOP, NULL, PMIX_BOOL);

    /* unpack the handler ID */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &refid, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* get the referenced handler */
    req = (pmix_iof_req_t *) pmix_pointer_array_get_item(&pmix_globals.iof_requests, refid);
    if (NULL == req) {
        /* already gone? */
        rc = PMIX_ERR_NOT_FOUND;
        goto exit;
    }
    pmix_pointer_array_set_item(&pmix_globals.iof_requests, refid, NULL);
    PMIX_RELEASE(req);

    /* tell the server to stop */
    rc = pmix_host_server.iof_pull(cd->procs, cd->nprocs,
                                   cd->info, cd->ninfo,
                                   cd->channels, cbfunc, cd);
    if (PMIX_OPERATION_SUCCEEDED == rc) {
        /* the host did it atomically - send the response. In
         * this particular case, we can just use the cbfunc
         * ourselves as it will threadshift and guarantee
         * proper handling */
        cbfunc(PMIX_SUCCESS, cd);
        /* returning other than SUCCESS will cause the
         * switchyard to release the cd object */
        return PMIX_SUCCESS;
    }

exit:
    return rc;
}

static void stdcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;

    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(status, cd->cbdata);
    }
    if (NULL != cd->procs) {
        PMIX_PROC_FREE(cd->procs, cd->nprocs);
    }
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    if (NULL != cd->bo) {
        PMIX_BYTE_OBJECT_FREE(cd->bo, 1);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_iofstdin(pmix_peer_t *peer,
                                   pmix_buffer_t *buf,
                                   pmix_op_cbfunc_t cbfunc,
                                   void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_proc_t source;
    pmix_setup_caddy_t *cd;

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "recvd stdin IOF data from tool %s",
                        PMIX_PEER_PRINT(peer));

    if (NULL == pmix_host_server.push_stdin) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* unpack the number of targets */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < cd->nprocs) {
        PMIX_PROC_CREATE(cd->procs, cd->nprocs);
        if (NULL == cd->procs) {
            rc = PMIX_ERR_NOMEM;
            goto error;
        }
        cnt = cd->nprocs;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->procs, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        if (NULL == cd->info) {
            rc = PMIX_ERR_NOMEM;
            goto error;
        }
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    /* unpack the data */
    PMIX_BYTE_OBJECT_CREATE(cd->bo, 1);
    if (NULL == cd->bo) {
        rc = PMIX_ERR_NOMEM;
        goto error;
    }

    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, cd->bo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
        /* it is okay for them to not send data */
        PMIX_BYTE_OBJECT_FREE(cd->bo, 1);
    } else if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* pass the data to the host */
    pmix_strncpy(source.nspace, peer->nptr->nspace, PMIX_MAX_NSLEN);
    source.rank = peer->info->pname.rank;
    if (PMIX_SUCCESS
        != (rc = pmix_host_server.push_stdin(&source, cd->procs, cd->nprocs, cd->info, cd->ninfo,
                                             cd->bo, stdcbfunc, cd))) {
        if (PMIX_OPERATION_SUCCEEDED != rc) {
            goto error;
        }
    }
    return rc;

error:
    PMIX_RELEASE(cd);
    return rc;
}

static void _grpcbfunc(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t *) cbdata;
    pmix_server_trkr_t *trk = scd->tracker;
    pmix_server_caddy_t *cd;
    pmix_buffer_t *reply, xfer, dblob, rankblob;
    pmix_status_t ret;
    size_t n, ctxid = SIZE_MAX, ngrpinfo, nmembers = 0;
    pmix_proc_t *members = NULL;
    pmix_byte_object_t *bo = NULL, pbo;
    pmix_nspace_caddy_t *nptr;
    pmix_list_t nslist;
    bool found;
    bool ctxid_given = false;
    pmix_regattr_input_t *p;
    uint32_t index, endptidx, infoidx;
    int32_t cnt;
    pmix_proc_t procid;
    pmix_info_t *grpinfo, *iptr;
    pmix_kval_t kp;
    pmix_value_t val;
    pmix_data_array_t darray;
    char **nspaces = NULL;
    pmix_proc_t proc;
    pmix_buffer_t pbkt;
    pmix_kval_t *kptr;
    pmix_cb_t cb;

    PMIX_ACQUIRE_OBJECT(scd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    if (NULL == trk) {
        /* give them a release if they want it - this should
         * never happen, but protect against the possibility */
        if (NULL != scd->cbfunc.relfn) {
            scd->cbfunc.relfn(scd->cbdata);
        }
        PMIX_RELEASE(scd);
        return;
    }

    pmix_output_verbose(2, pmix_server_globals.group_output,
                        "server:grpcbfunc processing WITH %d CALLBACKS",
                        (int) pmix_list_get_size(&trk->local_cbs));

    // if this is a destruct operation, there is nothing more to do
    if (PMIX_GROUP_DESTRUCT == trk->grpop) {
        goto release;
    }

    /* see if this group was assigned a context ID or collected data */
    for (n = 0; n < scd->ninfo; n++) {
        if (PMIX_CHECK_KEY(&scd->info[n], PMIX_GROUP_CONTEXT_ID)) {
            PMIX_VALUE_GET_NUMBER(ret, &scd->info[n].value, ctxid, size_t);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
            } else {
                ctxid_given = true;
            }
        } else if (PMIX_CHECK_KEY(&scd->info[n], PMIX_GROUP_ENDPT_DATA)) {
            bo = &scd->info[n].value.data.bo;
        } else if (PMIX_CHECK_KEY(&scd->info[n], PMIX_GROUP_MEMBERSHIP)) {
            members = (pmix_proc_t*)scd->info[n].value.data.darray->array;
            nmembers = scd->info[n].value.data.darray->size;
        }
    }

    /* if data was returned, then we need to have the modex cbfunc
     * store it for us before releasing the group members */
    if (NULL != bo) {
        /* get the indices of the types of data */
        p = pmix_hash_lookup_key(UINT32_MAX, PMIX_GROUP_ENDPT_DATA, NULL);
        endptidx = p->index;
        p = pmix_hash_lookup_key(UINT32_MAX, PMIX_GROUP_INFO, NULL);
        infoidx = p->index;

        PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
        PMIX_LOAD_BUFFER(pmix_globals.mypeer, &xfer, bo->bytes, bo->size);
        PMIX_CONSTRUCT(&nslist, pmix_list_t);
        /* Collect the nptr list with unique nspaces */
        PMIX_LIST_FOREACH (cd, &trk->local_cbs, pmix_server_caddy_t) {
            // see if we already have this nspace
            found = false;
            PMIX_LIST_FOREACH (nptr, &nslist, pmix_nspace_caddy_t) {
                // if we already have this nspace, ignore this entry
                if (PMIX_CHECK_NSPACE(nptr->ns->nspace, cd->peer->nptr->nspace)) {
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

        /* unpack each byte object - we get one byte object containing the
         * complete contribution from each participating server. Each byte
         * object contains up to two byte objects - one for the endpt data
         * from the local participants, and the other potentially containing
         * any group info published by those participants */
        /* unpack the index indicating the type of data in the object */
        cnt = 1;
        PMIX_BFROPS_UNPACK(ret, pmix_globals.mypeer, &xfer, &index, &cnt, PMIX_UINT32);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_DESTRUCT(&xfer);
            goto release;
        }
        while (PMIX_SUCCESS == ret) {
            /* unpack the data blob for this object */
            cnt = 1;
            PMIX_BFROPS_UNPACK(ret, pmix_globals.mypeer, &xfer, &pbo, &cnt, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PMIX_DESTRUCT(&xfer);
                goto release;
            }

            if (index == endptidx) {
                PMIX_LIST_FOREACH (nptr, &nslist, pmix_nspace_caddy_t) {
                    PMIX_CONSTRUCT(&dblob, pmix_buffer_t);
                    ret = pmix_bfrops_base_embed_payload(&dblob, &pbo);  // does NOT alter the pbo
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        break;
                    }
                    PMIX_GDS_STORE_MODEX(ret, nptr->ns, &dblob, trk);
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        break;
                    }
                    PMIX_DESTRUCT(&dblob);
                }
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);

            } else if (index == infoidx) {
                /* this is packed differently, at least for now, so we have
                 * to unpack it and process it directly */
                if (ctxid_given) {
                    /* the blob consists of a set of byte objects, each containing the ID
                     * of the contributing proc followed by the pmix_info_t they
                     * provided */
                    ret = PMIX_SUCCESS;
                    PMIX_CONSTRUCT(&dblob, pmix_buffer_t);
                    PMIX_LOAD_BUFFER(pmix_globals.mypeer, &dblob, pbo.bytes, pbo.size);
                    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                    while (PMIX_SUCCESS == ret) {
                        cnt = 1;
                        PMIX_BFROPS_UNPACK(ret, pmix_globals.mypeer, &dblob, &pbo, &cnt, PMIX_BYTE_OBJECT);
                        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == ret) {
                            /* not an error - we are simply done */
                            break;
                        }
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            PMIX_DESTRUCT(&xfer);
                            PMIX_DESTRUCT(&dblob);
                            goto release;
                        }
                        PMIX_CONSTRUCT(&rankblob, pmix_buffer_t);
                        PMIX_LOAD_BUFFER(pmix_globals.mypeer, &rankblob, pbo.bytes, pbo.size);
                        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                        cnt = 1;
                        PMIX_BFROPS_UNPACK(ret, pmix_globals.mypeer, &rankblob, &procid, &cnt, PMIX_PROC);
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            PMIX_DESTRUCT(&xfer);
                            PMIX_DESTRUCT(&dblob);
                            PMIX_DESTRUCT(&rankblob);
                            goto release;
                        }
                        cnt = 1;
                        PMIX_BFROPS_UNPACK(ret, pmix_globals.mypeer, &rankblob, &ngrpinfo, &cnt, PMIX_SIZE);
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            PMIX_DESTRUCT(&xfer);
                            PMIX_DESTRUCT(&dblob);
                            PMIX_DESTRUCT(&rankblob);
                            goto release;
                        }
                        PMIX_INFO_CREATE(grpinfo, ngrpinfo);
                        cnt = ngrpinfo;
                        PMIX_BFROPS_UNPACK(ret, pmix_globals.mypeer, &rankblob, grpinfo, &cnt, PMIX_INFO);
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            PMIX_DESTRUCT(&xfer);
                            PMIX_DESTRUCT(&dblob);
                            PMIX_DESTRUCT(&rankblob);
                            PMIX_INFO_FREE(grpinfo, ngrpinfo);
                            goto release;
                        }
                        /* reconstruct each value as a qualified one basd
                         * on the ctxid */
                        PMIX_CONSTRUCT(&kp, pmix_kval_t);
                        kp.value = &val;
                        kp.key = PMIX_QUALIFIED_VALUE;
                        val.type = PMIX_DATA_ARRAY;
                        for (n=0; n < ngrpinfo; n++) {
                            PMIX_DATA_ARRAY_CONSTRUCT(&darray, 2, PMIX_INFO);
                            iptr = (pmix_info_t*)darray.array;
                            /* the primary value is in the first position */
                            PMIX_INFO_XFER(&iptr[0], &grpinfo[n]);
                            /* add the context ID qualifier */
                            PMIX_INFO_LOAD(&iptr[1], PMIX_GROUP_CONTEXT_ID, &ctxid, PMIX_SIZE);
                            PMIX_INFO_SET_QUALIFIER(&iptr[1]);
                            /* add it to the kval */
                            val.data.darray = &darray;
                            /* store it */
                            PMIX_GDS_STORE_KV(ret, pmix_globals.mypeer, &procid, PMIX_GLOBAL, &kp);
                            PMIX_DATA_ARRAY_DESTRUCT(&darray);
                            if (PMIX_SUCCESS != ret) {
                                PMIX_ERROR_LOG(ret);
                                PMIX_DESTRUCT(&xfer);
                                PMIX_DESTRUCT(&dblob);
                                PMIX_DESTRUCT(&rankblob);
                                PMIX_INFO_FREE(grpinfo, ngrpinfo);
                                goto release;
                            }
                        }
                        PMIX_INFO_FREE(grpinfo, ngrpinfo);
                        PMIX_DESTRUCT(&rankblob);
                        ret = PMIX_SUCCESS;
                    }
                    PMIX_DESTRUCT(&dblob);
                }
            }
            /* get the next blob */
            cnt = 1;
            PMIX_BFROPS_UNPACK(ret, pmix_globals.mypeer, &xfer, &index, &cnt, PMIX_UINT32);
            if (PMIX_SUCCESS != ret && PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != ret) {
                PMIX_ERROR_LOG(ret);
                break;
            }
        }
        PMIX_DESTRUCT(&xfer);
    }

release:
    /* find the unique nspaces that are participating */
    PMIX_LIST_FOREACH (cd, &trk->local_cbs, pmix_server_caddy_t) {
        if (NULL == nspaces) {
            PMIx_Argv_append_nosize(&nspaces, cd->peer->info->pname.nspace);
        } else {
            found = false;
            for (n = 0; NULL != nspaces[n]; n++) {
                if (0 == strcmp(nspaces[n], cd->peer->info->pname.nspace)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                PMIx_Argv_append_nosize(&nspaces, cd->peer->info->pname.nspace);
            }
        }
    }

    /* loop across all procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH (cd, &trk->local_cbs, pmix_server_caddy_t) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            break;
        }
        /* setup the reply, starting with the returned status */
        PMIX_BFROPS_PACK(ret, cd->peer, reply, &scd->status, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(reply);
            break;
        }
        if (PMIX_SUCCESS == scd->status && PMIX_GROUP_CONSTRUCT == trk->grpop) {
            /* add the final membership */
            PMIX_BFROPS_PACK(ret, cd->peer, reply, &nmembers, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PMIX_RELEASE(reply);
                break;
            }
            if (0 < nmembers) {
                PMIX_BFROPS_PACK(ret, cd->peer, reply, members, nmembers, PMIX_PROC);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_RELEASE(reply);
                    break;
                }
            }
            /* if a ctxid was provided, pass it along */
            PMIX_BFROPS_PACK(ret, cd->peer, reply, &ctxid, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PMIX_RELEASE(reply);
                break;
            }
            /* loop across all participating nspaces and include their
             * job-related info */
            for (n = 0; NULL != nspaces[n]; n++) {
                /* if this is the local proc's own nspace, then
                 * ignore it - it already has this info */
                if (PMIX_CHECK_NSPACE(nspaces[n], cd->peer->info->pname.nspace)) {
                    continue;
                }

                /* this is a local request, so give the gds the option
                 * of returning a copy of the data, or a pointer to
                 * local storage */
                /* add the job-level info, if necessary */
                PMIX_LOAD_PROCID(&proc, nspaces[n], PMIX_RANK_WILDCARD);
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                /* this is for a local client, so give the gds the
                 * option of returning a complete copy of the data,
                 * or returning a pointer to local storage */
                cb.proc = &proc;
                cb.scope = PMIX_SCOPE_UNDEF;
                cb.copy = false;
                PMIX_GDS_FETCH_KV(ret, cd->peer, &cb);
                if (PMIX_SUCCESS != ret) {
                    /* try getting it from our storage */
                    PMIX_GDS_FETCH_KV(ret, pmix_globals.mypeer, &cb);
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        PMIX_RELEASE(reply);
                        PMIX_DESTRUCT(&cb);
                        goto done;
                    }
                }
                PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                /* pack the nspace name */
                PMIX_BFROPS_PACK(ret, cd->peer, &pbkt, &nspaces[n], 1, PMIX_STRING);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&pbkt);
                    PMIX_DESTRUCT(&cb);
                    goto done;
                }
                PMIX_LIST_FOREACH (kptr, &cb.kvs, pmix_kval_t) {
                    PMIX_BFROPS_PACK(ret, cd->peer, &pbkt, kptr, 1, PMIX_KVAL);
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        PMIX_RELEASE(reply);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_DESTRUCT(&cb);
                        goto done;
                    }
                }
                PMIX_DESTRUCT(&cb);


                PMIX_UNLOAD_BUFFER(&pbkt, pbo.bytes, pbo.size);
                PMIX_BFROPS_PACK(ret, cd->peer, reply, &pbo, 1, PMIX_BYTE_OBJECT);
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&pbkt);
                    PMIX_DESTRUCT(&cb);
                    goto done;
                }

                PMIX_DESTRUCT(&pbkt);
            }
        }

  done:
        pmix_output_verbose(2, pmix_server_globals.group_output,
                            "server:grp_cbfunc reply being sent to %s:%u",
                            cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
        PMIX_SERVER_QUEUE_REPLY(ret, cd->peer, cd->hdr.tag, reply);
        if (PMIX_SUCCESS != ret) {
            PMIX_RELEASE(reply);
        }
    }

    /* remove the tracker from the list */
    pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
    PMIX_RELEASE(trk);

    // cleanup
    if (NULL != nspaces) {
        PMIx_Argv_free(nspaces);
    }

    /* we are done */
    if (NULL != scd->cbfunc.relfn) {
        scd->cbfunc.relfn(scd->cbdata);
    }
    PMIX_RELEASE(scd);
}

static void grpcbfunc(pmix_status_t status,
                      pmix_info_t *info, size_t ninfo, void *cbdata,
                      pmix_release_cbfunc_t relfn, void *relcbd)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t *) cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_server_globals.group_output,
                        "server:grpcbfunc called with %d info", (int) ninfo);

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
    scd->info = info;
    scd->ninfo = ninfo;
    scd->tracker = tracker;
    scd->cbfunc.relfn = relfn;
    scd->cbdata = relcbd;
    PMIX_THREADSHIFT(scd, _grpcbfunc);
}

static void grp_timeout(int sd, short args, void *cbdata)
{
    pmix_server_trkr_t *trk = (pmix_server_trkr_t *) cbdata;
    pmix_server_caddy_t *cd;
    pmix_buffer_t *reply;
    pmix_status_t ret, rc = PMIX_ERR_TIMEOUT;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_server_globals.group_output,
                        "ALERT: grp construct timeout fired");

    /* loop across all procs in the tracker, alerting
     * them to the failure */
    PMIX_LIST_FOREACH (cd, &trk->local_cbs, pmix_server_caddy_t) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            break;
        }
        /* setup the reply, starting with the returned status */
        PMIX_BFROPS_PACK(ret, cd->peer, reply, &rc, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_RELEASE(reply);
            break;
        }
        pmix_output_verbose(2, pmix_server_globals.group_output,
                            "server:grp_cbfunc TIMEOUT being sent to %s:%u",
                            cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
        PMIX_SERVER_QUEUE_REPLY(ret, cd->peer, cd->hdr.tag, reply);
        if (PMIX_SUCCESS != ret) {
            PMIX_RELEASE(reply);
        }
    }

    trk->event_active = false;
    /* record this group as failed */
    PMIx_Argv_append_nosize(&pmix_server_globals.failedgrps, trk->id);
    /* remove the tracker from the list */
    pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
    PMIX_RELEASE(trk);
}

/* we are being called from the PMIx server's switchyard function,
 * which means we are in an event and can access global data */
pmix_status_t pmix_server_grpconstruct(pmix_server_caddy_t *cd, pmix_buffer_t *buf)
{
    pmix_peer_t *peer = (pmix_peer_t *) cd->peer;
    pmix_peer_t *pr;
    int32_t cnt, m;
    pmix_status_t rc;
    char *grpid;
    pmix_proc_t *procs;
    pmix_info_t *info = NULL, *iptr = NULL, *grpinfoptr = NULL;
    size_t n, ninfo, ninf, niptr, nprocs, n2, ngrpinfo = 0;
    pmix_server_trkr_t *trk;
    bool need_cxtid = false;
    bool match, force_local = false;
    bool embed_barrier = false;
    bool barrier_directive_included = false;
    bool locally_complete = false;
    pmix_buffer_t bucket, bkt;
    pmix_byte_object_t bo;
    pmix_grpinfo_t *g = NULL;
    pmix_regattr_input_t *p;
    struct timeval tv = {0, 0};

    pmix_output_verbose(2, pmix_server_globals.group_output,
                        "recvd grpconstruct cmd from %s",
                        PMIX_PEER_PRINT(cd->peer));

    /* unpack the group ID */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &grpid, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* is this a failed group? */
    if (NULL != pmix_server_globals.failedgrps) {
        for (m=0; NULL != pmix_server_globals.failedgrps[m]; m++) {
            if (0 == strcmp(grpid, pmix_server_globals.failedgrps[m])) {
                /* yes - reject it */
                free(grpid);
                return PMIX_ERR_TIMEOUT;
            }
        }
    }

    /* unpack the number of procs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 == nprocs) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }
    PMIX_PROC_CREATE(procs, nprocs);
    if (NULL == procs) {
        rc = PMIX_ERR_NOMEM;
        goto error;
    }
    cnt = nprocs;
    PMIX_BFROPS_UNPACK(rc, peer, buf, procs, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_PROC_FREE(procs, nprocs);
        goto error;
    }
    /* sort the procs */
    qsort(procs, nprocs, sizeof(pmix_proc_t), pmix_util_compare_proc);

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninf, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    ninfo = ninf + 2;
    PMIX_INFO_CREATE(info, ninfo);
    /* store default response */
    rc = PMIX_SUCCESS;
    PMIX_INFO_LOAD(&info[ninf], PMIX_SORTED_PROC_ARRAY, NULL, PMIX_BOOL);
    PMIX_INFO_LOAD(&info[ninf+1], PMIX_LOCAL_COLLECTIVE_STATUS, &rc, PMIX_STATUS);
    if (0 < ninf) {
        cnt = ninf;
        PMIX_BFROPS_UNPACK(rc, peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    /* check directives */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_ASSIGN_CONTEXT_ID)) {
            need_cxtid = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_LOCAL_ONLY)) {
            force_local = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_EMBED_BARRIER)) {
            embed_barrier = PMIX_INFO_TRUE(&info[n]);
            barrier_directive_included = true;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, tv.tv_sec, uint32_t);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_INFO)) {
            grpinfoptr = (pmix_info_t*)info[n].value.data.darray->array;
            ngrpinfo = info[n].value.data.darray->size;
            g = PMIX_NEW(pmix_grpinfo_t);
            PMIX_LOAD_NSPACE(g->proc.nspace, peer->nptr->nspace);
            g->proc.rank = peer->info->pname.rank;
            PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &g->proc, 1, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(g);
                PMIX_DESTRUCT(&bucket);
                goto error;
            }
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &ngrpinfo, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(g);
                PMIX_DESTRUCT(&bucket);
                goto error;
            }
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, grpinfoptr, ngrpinfo, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(g);
                PMIX_DESTRUCT(&bucket);
                goto error;
            }
            PMIX_UNLOAD_BUFFER(&bucket, g->blob.bytes, g->blob.size);
            PMIX_DESTRUCT(&bucket);
        }
    }

    /* find/create the local tracker for this operation */
    trk = get_tracker(grpid, procs, nprocs, PMIX_GROUP_CONSTRUCT_CMD);
    if (NULL == trk) {
        /* If no tracker was found - create and initialize it once */
        trk = new_tracker(grpid, procs, nprocs, PMIX_GROUP_CONSTRUCT_CMD);
        if (NULL == trk) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            rc = PMIX_ERROR;
            goto error;
        }
        /* group members must have access to all endpoint info
         * upon completion of the construct operation */
        trk->collect_type = PMIX_COLLECT_YES;
        /* mark as being a construct operation */
        trk->grpop = PMIX_GROUP_CONSTRUCT;
        /* it is possible that different participants will
         * provide different attributes, so collect the
         * aggregate of them */
        if (NULL == trk->info) {
            trk->info = info;
            trk->ninfo = ninfo;
        } else {
            niptr = trk->ninfo + ninfo;
            PMIX_INFO_CREATE(iptr, niptr);
            for (n=0; n < trk->ninfo; n++) {
                PMIX_INFO_XFER(&iptr[n], &trk->info[n]);
            }
            for (n=0; n < ninfo; n++) {
                PMIX_INFO_XFER(&iptr[n+trk->ninfo], &info[n]);
            }
            PMIX_INFO_FREE(trk->info, trk->ninfo);
            trk->info = iptr;
            trk->ninfo = niptr;
            /* cleanup */
            PMIX_INFO_FREE(info, ninfo);
            info = NULL;
        }
        if (NULL != g) {
            pmix_list_append(&trk->grpinfo, &g->super);
        }
        /* see if this constructor only references local processes and isn't
         * requesting a context ID - if both conditions are met, then we
         * can just locally process the request without bothering the host.
         * This is meant to provide an optimized path for a fairly common
         * operation */
        if (force_local) {
            trk->local = true;
        } else if (need_cxtid) {
            trk->local = false;
        } else {
            trk->local = true;
            for (n = 0; n < nprocs; n++) {
                /* if this entry references the local procs, then
                 * we can skip it */
                if (PMIX_RANK_LOCAL_PEERS == procs[n].rank ||
                    PMIX_RANK_LOCAL_NODE == procs[n].rank) {
                    continue;
                }
                /* see if it references a specific local proc */
                match = false;
                for (m = 0; m < pmix_server_globals.clients.size; m++) {
                    pr = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients, m);
                    if (NULL == pr) {
                        continue;
                    }
                    if (PMIX_CHECK_NAMES(&procs[n], &pr->info->pname)) {
                        match = true;
                        break;
                    }
                }
                if (!match) {
                    /* this requires a non_local operation */
                    trk->local = false;
                    break;
                }
            }
        }
    } else {
        /* it is possible that different participants will
         * provide different attributes, so collect the
         * aggregate of them */
        if (NULL == trk->info) {
            trk->info = info;
            trk->ninfo = ninfo;
        } else {
            niptr = trk->ninfo + ninfo;
            PMIX_INFO_CREATE(iptr, niptr);
            for (n=0; n < trk->ninfo; n++) {
                PMIX_INFO_XFER(&iptr[n], &trk->info[n]);
            }
            for (n=0; n < ninfo; n++) {
                PMIX_INFO_XFER(&iptr[n+trk->ninfo], &info[n]);
            }
            PMIX_INFO_FREE(trk->info, trk->ninfo);
            trk->info = iptr;
            trk->ninfo = niptr;
            /* cleanup */
            PMIX_INFO_FREE(info, ninfo);
            info = NULL;
        }
        /* add any grpinfo that might have been included */
        if (NULL != g) {
            pmix_list_append(&trk->grpinfo, &g->super);
        }
    }

    /* add this contributor to the tracker so they get
     * notified when we are done */
    pmix_list_append(&trk->local_cbs, &cd->super);

    /* are we locally complete? */
    if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        locally_complete = true;
    }

    /* if we are not locally complete AND this operation
     * is completely local AND someone specified a timeout,
     * then we will monitor the timeout in this library.
     * Otherwise, any timeout must be done by the host
     * to avoid a race condition whereby we release the
     * tracker object while the host is still using it */
    if (!locally_complete && trk->local &&
        0 < tv.tv_sec && !trk->event_active) {
        PMIX_THREADSHIFT_DELAY(trk, grp_timeout, tv.tv_sec);
        trk->event_active = true;
    }

    /* if we are not locally complete, then we are done */
    if (!locally_complete) {
        return PMIX_SUCCESS;
    }

    /* if all local contributions have been received,
     * shutdown the timeout event if active */
    if (trk->event_active) {
        pmix_event_del(&trk->ev);
    }

    /* let the local host's server know that we are at the
     * "fence" point - they will callback once the barrier
     * across all participants has been completed */

    pmix_output_verbose(2, pmix_server_globals.group_output,
                        "local group op complete with %d procs",
                        (int) trk->npcs);

    if (trk->local) {
        /* we have created the local group, so we technically
         * are done. However, we want to give the host a chance
         * to know about the group to support further operations.
         * For example, a tool might want to query the host to get
         * the IDs of existing groups. So if the host supports
         * group operations, pass this one up to it but indicate
         * it is strictly local */
        if (NULL != pmix_host_server.group) {
            /* we only need to pass the group ID, members, and
             * an info indicating that this is strictly a local
             * operation */
            if (!force_local) {
                /* add the local op flag to the info array */
                ninfo = trk->ninfo + 1;
                PMIX_INFO_CREATE(info, ninfo);
                for (n=0; n < trk->ninfo; n++) {
                    PMIX_INFO_XFER(&info[n], &trk->info[n]);
                }
                PMIX_INFO_LOAD(&info[trk->ninfo], PMIX_GROUP_LOCAL_ONLY, NULL, PMIX_BOOL);
                PMIX_INFO_FREE(trk->info, trk->ninfo);
                trk->info = info;
                trk->ninfo = ninfo;
                info = NULL;
                ninfo = 0;
            }
            rc = pmix_host_server.group(PMIX_GROUP_CONSTRUCT, grpid, trk->pcs, trk->npcs,
                                        trk->info, trk->ninfo, grpcbfunc, trk);
            if (PMIX_SUCCESS != rc) {
                if (PMIX_OPERATION_SUCCEEDED == rc) {
                    /* let the grpcbfunc threadshift the result */
                    grpcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
                    return PMIX_SUCCESS;
                }
                /* remove the tracker from the list */
                pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
                PMIX_RELEASE(trk);
                return rc;
            }
            /* we will take care of the rest of the process when the
             * host returns our call */
            return PMIX_SUCCESS;
        } else {
            /* let the grpcbfunc threadshift the result */
            grpcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
            return PMIX_SUCCESS;
        }
    }

    /* we don't have to worry about the timeout event being
     * active in the rest of this code because we only come
     * here if the operation is NOT completely local, and
     * we only activate the timeout if it IS local */

    /* check if our host supports group operations */
    if (NULL == pmix_host_server.group) {
        /* cannot support it */
        pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
        PMIX_RELEASE(trk);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* if they direct us to not embed a barrier, then we won't gather
     * the data for distribution */
    if (!barrier_directive_included ||
        (barrier_directive_included && embed_barrier) ||
        0 < pmix_list_get_size(&trk->grpinfo)) {
        /* collect any remote contributions provided by group members */
        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
        rc = _collect_data(trk, &bucket);
        if (PMIX_SUCCESS != rc) {
            /* remove the tracker from the list */
            pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
            PMIX_RELEASE(trk);
            PMIX_DESTRUCT(&bucket);
            return rc;
        }
        /* xfer the results to a byte object */
        PMIX_UNLOAD_BUFFER(&bucket, bo.bytes, bo.size);
        PMIX_DESTRUCT(&bucket);
        /* load any results into a data object for inclusion in the
         * fence operation */
        if (0 < bo.size ||
            0 < pmix_list_get_size(&trk->grpinfo)) {
            n2 = trk->ninfo + 1; // include space for endpt data
            PMIX_INFO_CREATE(iptr, n2);
            for (n = 0; n < trk->ninfo; n++) {
                PMIX_INFO_XFER(&iptr[n], &trk->info[n]);
            }
            /* add the endpt data */
            PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
            if (0 < bo.size) {
                p = pmix_hash_lookup_key(UINT32_MAX, PMIX_GROUP_ENDPT_DATA, NULL);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &p->index, 1, PMIX_UINT32);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&bucket);
                    goto error;
                }
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &bo, 1, PMIX_BYTE_OBJECT);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&bucket);
                    goto error;
                }
                PMIX_BYTE_OBJECT_DESTRUCT(&bo);
            }
            if (0 < pmix_list_get_size(&trk->grpinfo)) {
                p = pmix_hash_lookup_key(UINT32_MAX, PMIX_GROUP_INFO, NULL);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &p->index, 1, PMIX_UINT32);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&bucket);
                    goto error;
                }
                PMIX_CONSTRUCT(&bkt, pmix_buffer_t);
                PMIX_LIST_FOREACH(g, &trk->grpinfo, pmix_grpinfo_t) {
                    PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bkt, &g->blob, 1, PMIX_BYTE_OBJECT);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&bucket);
                        PMIX_DESTRUCT(&bkt);
                        goto error;
                    }
                }
                PMIX_UNLOAD_BUFFER(&bkt, bo.bytes, bo.size);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket, &bo, 1, PMIX_BYTE_OBJECT);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&bucket);
                    goto error;
                }
                PMIX_BYTE_OBJECT_DESTRUCT(&bo);
            }
            PMIX_UNLOAD_BUFFER(&bucket, bo.bytes, bo.size);
            PMIX_INFO_LOAD(&iptr[n2-1], PMIX_GROUP_ENDPT_DATA, &bo, PMIX_BYTE_OBJECT);
            PMIX_BYTE_OBJECT_DESTRUCT(&bo);
            /* replace the tracker's info array */
            PMIX_INFO_FREE(trk->info, trk->ninfo);
            trk->info = iptr;
            trk->ninfo = n2;
            iptr = NULL;
        }
    }
    rc = pmix_host_server.group(PMIX_GROUP_CONSTRUCT, grpid, trk->pcs, trk->npcs,
                                trk->info, trk->ninfo, grpcbfunc, trk);
    if (PMIX_SUCCESS != rc) {
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            /* let the grpcbfunc threadshift the result */
            grpcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
            return PMIX_SUCCESS;
        }
        /* remove the tracker from the list */
        pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
        PMIX_RELEASE(trk);
        return rc;
    }

    return PMIX_SUCCESS;

error:
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    if (NULL != iptr) {
        PMIX_INFO_FREE(iptr, n2);
    }
    return rc;
}

/* we are being called from the PMIx server's switchyard function,
 * which means we are in an event and can access global data */
pmix_status_t pmix_server_grpdestruct(pmix_server_caddy_t *cd, pmix_buffer_t *buf)
{
    pmix_peer_t *peer = (pmix_peer_t *) cd->peer;
    int32_t cnt, m;
    pmix_status_t rc;
    char *grpid = NULL;
    pmix_info_t *info = NULL, *iptr;
    size_t n, ninfo, ninf, niptr;
    pmix_server_trkr_t *trk;
    pmix_proc_t *members = NULL;
    size_t nmembers = 0;
    bool force_local = false;
    bool match;
    bool locally_complete = false;
    pmix_peer_t *pr;
    struct timeval tv = {0, 0};

    pmix_output_verbose(2, pmix_server_globals.group_output,
                        "recvd grpdestruct cmd");

    /* unpack the group ID */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &grpid, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* is this a failed group? */
    if (NULL != pmix_server_globals.failedgrps) {
        for (m=0; NULL != pmix_server_globals.failedgrps[m]; m++) {
            if (0 == strcmp(grpid, pmix_server_globals.failedgrps[m])) {
                /* yes - reject it */
                free(grpid);
                return PMIX_ERR_TIMEOUT;
            }
        }
    }

    /* unpack the number of members */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &nmembers, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 == nmembers) {
        /* not allowed */
        rc = PMIX_ERR_BAD_PARAM;
        goto error;
    }
    /* unpack the membership */
    PMIX_PROC_CREATE(members, nmembers);
    cnt = nmembers;
    PMIX_BFROPS_UNPACK(rc, peer, buf, members, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_PROC_FREE(members, nmembers);
        goto error;
    }

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninf, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    ninfo = ninf + 1;
    PMIX_INFO_CREATE(info, ninfo);
    /* store default response */
    rc = PMIX_SUCCESS;
    PMIX_INFO_LOAD(&info[ninf], PMIX_LOCAL_COLLECTIVE_STATUS, &rc, PMIX_STATUS);
    if (0 < ninf) {
        cnt = ninf;
        PMIX_BFROPS_UNPACK(rc, peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    /* check directives */
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_LOCAL_ONLY)) {
            force_local = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_LOCAL_ONLY)) {
            force_local = PMIX_INFO_TRUE(&info[n]);
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, tv.tv_sec, uint32_t);
            if (PMIX_SUCCESS != rc) {
                return rc;
            }
        }
    }

    /* find/create the local tracker for this operation */
    trk = get_tracker(grpid, members, nmembers, PMIX_GROUP_DESTRUCT_CMD);
    if (NULL == trk) {
        /* If no tracker was found - create and initialize it once */
        trk = new_tracker(grpid, members, nmembers, PMIX_GROUP_DESTRUCT_CMD);
        if (NULL == trk) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            rc = PMIX_ERROR;
            goto error;
        }
        trk->collect_type = PMIX_COLLECT_NO;
        /* mark as being a destruct operation */
        trk->grpop = PMIX_GROUP_DESTRUCT;
        /* see if this destructor only references local processes */
        trk->local = true;
        for (n = 0; n < nmembers; n++) {
            /* if this entry references the local procs, then
             * we can skip it */
            if (PMIX_RANK_LOCAL_PEERS == members[n].rank ||
                PMIX_RANK_LOCAL_NODE == members[n].rank) {
                continue;
            }
            /* see if it references a specific local proc - note that
             * the member name could include rank=wildcard */
            match = false;
            for (m = 0; m < pmix_server_globals.clients.size; m++) {
                pr = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients, m);
                if (NULL == pr) {
                    continue;
                }
                // cannot use PMIX_CHECK_PROCID here as pmix_peer_t includes a pname field
                // and not a pmix_proc_t
                if (PMIX_RANK_WILDCARD == members[n].rank ||
                    PMIX_RANK_WILDCARD == pr->info->pname.rank) {
                    if (PMIX_CHECK_NSPACE(members[n].nspace, pr->info->pname.nspace)) {
                        match = true;
                        break;
                    }
                }
                if (PMIX_CHECK_NAMES(&members[n], &pr->info->pname)) {
                    match = true;
                    break;
                }
            }
            if (!match) {
                /* this requires a non_local operation */
                trk->local = false;
                break;
            }
        }
    }

    /* it is possible that different participants will
     * provide different attributes, so collect the
     * aggregate of them */
    if (NULL == trk->info) {
        trk->info = info;
        trk->ninfo = ninfo;
    } else {
        niptr = trk->ninfo + ninfo;
        PMIX_INFO_CREATE(iptr, niptr);
        for (n=0; n < trk->ninfo; n++) {
            PMIX_INFO_XFER(&iptr[n], &trk->info[n]);
        }
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&iptr[n+trk->ninfo], &info[n]);
        }
        PMIX_INFO_FREE(trk->info, trk->ninfo);
        trk->info = iptr;
        trk->ninfo = niptr;
        /* cleanup */
        PMIX_INFO_FREE(info, ninfo);
        info = NULL;
    }

    /* add this contributor to the tracker so they get
     * notified when we are done */
    pmix_list_append(&trk->local_cbs, &cd->super);

    /* are we locally complete? */
    if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        locally_complete = true;
    }

    /* if we are not locally complete AND this operation
     * is completely local AND someone specified a timeout,
     * then we will monitor the timeout in this library.
     * Otherwise, any timeout must be done by the host
     * to avoid a race condition whereby we release the
     * tracker object while the host is still using it */
    if (!locally_complete && trk->local &&
        0 < tv.tv_sec && !trk->event_active) {
        PMIX_THREADSHIFT_DELAY(trk, grp_timeout, tv.tv_sec);
        trk->event_active = true;
    }

    /* if we are not locally complete, then we are done */
    if (!locally_complete) {
        return PMIX_SUCCESS;
    }

    /* if all local contributions have been received,
     * shutdown the timeout event if active */
    if (trk->event_active) {
        pmix_event_del(&trk->ev);
    }

    /* let the local host's server know that we are at the
     * "fence" point - they will callback once the barrier
     * across all participants has been completed */
    pmix_output_verbose(2, pmix_server_globals.group_output,
                        "local group destruct complete %d",
                        (int) trk->nlocal);
    if (trk->local) {
        /* we have removed the local group, so we technically
         * are done. However, we want to give the host a chance
         * to know remove the group to support further operations.
         * For example, a tool might want to query the host to get
         * the IDs of existing groups. So if the host supports
         * group operations, pass this one up to it but indicate
         * it is strictly local */
        if (NULL != pmix_host_server.group) {
            /* we only need to pass the group ID, members, and
             * an info indicating that this is strictly a local
             * operation */
            if (!force_local) {
                /* add the local op flag to the info array */
                ninfo = trk->ninfo + 1;
                PMIX_INFO_CREATE(info, ninfo);
                for (n=0; n < trk->ninfo; n++) {
                    PMIX_INFO_XFER(&info[n], &trk->info[n]);
                }
                PMIX_INFO_LOAD(&info[trk->ninfo], PMIX_GROUP_LOCAL_ONLY, NULL, PMIX_BOOL);
                PMIX_INFO_FREE(trk->info, trk->ninfo);
                trk->info = info;
                trk->ninfo = ninfo;
                info = NULL;
                ninfo = 0;
            }
            rc = pmix_host_server.group(PMIX_GROUP_DESTRUCT, grpid,
                                        members, nmembers,
                                        trk->info, trk->ninfo, grpcbfunc, trk);
            if (PMIX_SUCCESS != rc) {
                if (PMIX_OPERATION_SUCCEEDED == rc) {
                    /* let the grpcbfunc threadshift the result */
                    grpcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
                    PMIX_PROC_FREE(members, nmembers);
                    free(grpid);
                    return PMIX_SUCCESS;
                }
                /* remove the tracker from the list */
                pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
                PMIX_RELEASE(trk);
                PMIX_PROC_FREE(members, nmembers);
                free(grpid);
                return rc;
            }
            /* we will take care of the rest of the process when the
             * host returns our call */
        } else {
            /* let the grpcbfunc threadshift the result and remove
             * the group from our list */
            grpcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
            PMIX_PROC_FREE(members, nmembers);
            free(grpid);
            return PMIX_SUCCESS;
        }
    }

    /* this operation requires global support, so check if our host
     * supports group operations */
    if (NULL == pmix_host_server.group) {
        /* cannot support it */
        pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
        PMIX_RELEASE(trk);
        PMIX_PROC_FREE(members, nmembers);
        free(grpid);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    rc = pmix_host_server.group(PMIX_GROUP_DESTRUCT, grpid,
                                members, nmembers,
                                trk->info, trk->ninfo, grpcbfunc, trk);
    if (PMIX_SUCCESS != rc) {
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            /* let the grpcbfunc threadshift the result */
            grpcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
            PMIX_PROC_FREE(members, nmembers);
            free(grpid);
            return PMIX_SUCCESS;
        }
        /* remove the tracker from the list */
        pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
        PMIX_RELEASE(trk);
        PMIX_PROC_FREE(members, nmembers);
        free(grpid);
        return rc;
    }

    PMIX_PROC_FREE(members, nmembers);
    free(grpid);
    return PMIX_SUCCESS;

error:
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    if (NULL != members) {
        PMIX_PROC_FREE(members, nmembers);
    }
    if (NULL != grpid) {
        free(grpid);
    }
    return rc;
}

static void _fabric_response(int sd, short args, void *cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    qcd->cbfunc(PMIX_SUCCESS, qcd->info, qcd->ninfo, qcd->cbdata, NULL, NULL);
    PMIX_RELEASE(qcd);
}

static void frcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;

    PMIX_ACQUIRE_OBJECT(qcd);
    qcd->status = status;
    PMIX_POST_OBJECT(qcd);

    PMIX_WAKEUP_THREAD(&qcd->lock);
}
/* we are being called from the PMIx server's switchyard function,
 * which means we are in an event and can access global data */
pmix_status_t pmix_server_fabric_register(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                          pmix_info_cbfunc_t cbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_query_caddy_t *qcd = NULL;
    pmix_proc_t proc;
    pmix_fabric_t fabric;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd register_fabric request from client");

    qcd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == qcd) {
        return PMIX_ERR_NOMEM;
    }
    PMIX_RETAIN(cd);
    qcd->cbfunc = cbfunc;
    qcd->cbdata = cd;

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &qcd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(qcd);
        goto exit;
    }
    /* unpack the directives */
    if (0 < qcd->ninfo) {
        PMIX_INFO_CREATE(cd->info, qcd->ninfo);
        cnt = qcd->ninfo;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, qcd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(qcd);
            goto exit;
        }
    }

    /* see if we support this request ourselves */
    PMIX_FABRIC_CONSTRUCT(&fabric);
    rc = pmix_pnet.register_fabric(&fabric, qcd->info, qcd->ninfo, frcbfunc, qcd);
    if (PMIX_OPERATION_SUCCEEDED == rc) {
        /* we need to respond, but we want to ensure
         * that occurs _after_ the client returns from its API */
        if (NULL != qcd->info) {
            PMIX_INFO_FREE(qcd->info, qcd->ninfo);
        }
        qcd->info = fabric.info;
        qcd->ninfo = fabric.ninfo;
        PMIX_THREADSHIFT(qcd, _fabric_response);
        return PMIX_SUCCESS;
    } else if (PMIX_SUCCESS == rc) {
        PMIX_WAIT_THREAD(&qcd->lock);
        /* we need to respond, but we want to ensure
         * that occurs _after_ the client returns from its API */
        if (NULL != qcd->info) {
            PMIX_INFO_FREE(qcd->info, qcd->ninfo);
        }
        qcd->info = fabric.info;
        qcd->ninfo = fabric.ninfo;
        PMIX_THREADSHIFT(qcd, _fabric_response);
        return PMIX_SUCCESS;
    }

    /* if we don't internally support it, see if
     * our host does */
    if (NULL == pmix_host_server.fabric) {
        rc = PMIX_ERR_NOT_SUPPORTED;
        goto exit;
    }

    /* setup the requesting peer name */
    PMIX_LOAD_PROCID(&proc, cd->peer->info->pname.nspace, cd->peer->info->pname.rank);

    /* ask the host to execute the request */
    if (PMIX_SUCCESS
        != (rc = pmix_host_server.fabric(&proc, PMIX_FABRIC_REQUEST_INFO, qcd->info, qcd->ninfo,
                                         cbfunc, qcd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

exit:
    if (NULL != qcd) {
        PMIX_RELEASE(qcd);
    }
    return rc;
}

pmix_status_t pmix_server_fabric_update(pmix_server_caddy_t *cd, pmix_buffer_t *buf,
                                        pmix_info_cbfunc_t cbfunc)
{
    int32_t cnt;
    size_t index;
    pmix_status_t rc;
    pmix_query_caddy_t *qcd;
    pmix_proc_t proc;
    pmix_fabric_t fabric;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd update_fabric request from client");

    qcd = PMIX_NEW(pmix_query_caddy_t);
    if (NULL == qcd) {
        return PMIX_ERR_NOMEM;
    }
    PMIX_RETAIN(cd);
    qcd->cbdata = cd;

    /* unpack the fabric index */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &index, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* see if we support this request ourselves */
    PMIX_FABRIC_CONSTRUCT(&fabric);
    fabric.index = index;
    rc = pmix_pnet.update_fabric(&fabric);
    if (PMIX_SUCCESS == rc) {
        /* we need to respond, but we want to ensure
         * that occurs _after_ the client returns from its API */
        if (NULL != qcd->info) {
            PMIX_INFO_FREE(qcd->info, qcd->ninfo);
        }
        qcd->info = fabric.info;
        qcd->ninfo = fabric.ninfo;
        PMIX_THREADSHIFT(qcd, _fabric_response);
        return rc;
    }

    /* if we don't internally support it, see if
     * our host does */
    if (NULL == pmix_host_server.fabric) {
        rc = PMIX_ERR_NOT_SUPPORTED;
        goto exit;
    }

    /* setup the requesting peer name */
    pmix_strncpy(proc.nspace, cd->peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = cd->peer->info->pname.rank;
    /* add the index */
    qcd->ninfo = 1;
    PMIX_INFO_CREATE(qcd->info, qcd->ninfo);
    PMIX_INFO_LOAD(&qcd->info[0], PMIX_FABRIC_INDEX, &index, PMIX_SIZE);

    /* ask the host to execute the request */
    if (PMIX_SUCCESS
        != (rc = pmix_host_server.fabric(&proc, PMIX_FABRIC_UPDATE_INFO, qcd->info, qcd->ninfo,
                                         cbfunc, qcd))) {
        goto exit;
    }
    return PMIX_SUCCESS;

exit:
    return rc;
}

pmix_status_t pmix_server_device_dists(pmix_server_caddy_t *cd,
                                       pmix_buffer_t *buf,
                                       pmix_device_dist_cbfunc_t cbfunc)
{
    pmix_topology_t topo = {NULL, NULL};
    pmix_cpuset_t cpuset = {NULL, NULL};
    pmix_status_t rc;
    pmix_device_distance_t *distances;
    size_t ndist;
    int cnt;
    pmix_cb_t cb;
    pmix_kval_t *kv;
    pmix_proc_t proc;

    /* unpack the topology they want us to use */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &topo, &cnt, PMIX_TOPO);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* unpack the cpuset */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &cpuset, &cnt, PMIX_PROC_CPUSET);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    /* unpack any directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* if the provided topo is NULL, use my own */
    if (NULL == topo.topology) {
        if (NULL == pmix_globals.topology.topology) {
            /* try to get it */
            rc = pmix_hwloc_load_topology(&pmix_globals.topology);
            if (PMIX_SUCCESS != rc) {
                /* nothing we can do */
                goto cleanup;
            }
        }
        topo.topology = pmix_globals.topology.topology;
    }

    /* if the cpuset is NULL, see if we know the binding of the requesting process */
    if (NULL == cpuset.bitmap) {
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.key = strdup(PMIX_CPUSET);
        PMIX_LOAD_PROCID(&proc, cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
        cb.proc = &proc;
        cb.scope = PMIX_LOCAL;
        cb.copy = true;
        PMIX_GDS_FETCH_KV(rc, cd->peer, &cb);
        if (PMIX_SUCCESS != rc) {
            PMIX_DESTRUCT(&cb);
            goto cleanup;
        }
        kv = (pmix_kval_t*)pmix_list_get_first(&cb.kvs);
        rc = pmix_hwloc_parse_cpuset_string(kv->value->data.string, &cpuset);
        if (PMIX_SUCCESS != rc) {
            PMIX_DESTRUCT(&cb);
            goto cleanup;
        }
        PMIX_DESTRUCT(&cb);
    }
    /* compute the distances */
    rc = pmix_hwloc_compute_distances(&topo, &cpuset, cd->info, cd->ninfo, &distances, &ndist);
    if (PMIX_SUCCESS == rc) {
        /* send the reply */
        cbfunc(rc, distances, ndist, cd, NULL, NULL);
        PMIX_DEVICE_DIST_FREE(distances, ndist);
    }

cleanup:
    if (NULL != topo.topology &&
        topo.topology != pmix_globals.topology.topology) {
        pmix_hwloc_destruct_topology(&topo);
    }
    if (NULL != cpuset.bitmap) {
        pmix_hwloc_destruct_cpuset(&cpuset);
    }
    return rc;
}

pmix_status_t pmix_server_refresh_cache(pmix_server_caddy_t *cd,
                                        pmix_buffer_t *buf,
                                        pmix_op_cbfunc_t cbfunc)
{
    pmix_proc_t p;
    char *nspace;
    int cnt;
    pmix_status_t rc;
    pmix_cb_t cb;
    pmix_buffer_t *pbkt;
    pmix_kval_t *kv;
    PMIX_HIDE_UNUSED_PARAMS(cbfunc);

    // unpack the ID of the proc being requested
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &nspace, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_LOAD_NSPACE(p.nspace, nspace);
    free(nspace);

    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &p.rank, &cnt, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* retrieve the data for the specific rank they are asking about */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = &p;
    cb.scope = PMIX_REMOTE;
    cb.copy = false;
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);

    // pack it up
    pbkt = PMIX_NEW(pmix_buffer_t);
    // start with the status
    PMIX_BFROPS_PACK(rc, cd->peer, pbkt, &cb.status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(pbkt);
        return rc;
    }
    PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
        PMIX_BFROPS_PACK(rc, cd->peer, pbkt, kv, 1, PMIX_KVAL);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(pbkt);
            return rc;
        }
    }
    PMIX_DESTRUCT(&cb);

    // send it back to the requestor
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, pbkt);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(pbkt);
    }

    return PMIX_SUCCESS;
}

/*****    INSTANCE SERVER LIBRARY CLASSES    *****/
static void gcon(pmix_grpinfo_t *p)
{
    PMIX_LOAD_PROCID(&p->proc, NULL, PMIX_RANK_UNDEF);
    PMIX_BYTE_OBJECT_CONSTRUCT(&p->blob);
}
static void gdes(pmix_grpinfo_t *p)
{
    PMIX_BYTE_OBJECT_DESTRUCT(&p->blob);
}
PMIX_CLASS_INSTANCE(pmix_grpinfo_t,
                    pmix_list_item_t,
                    gcon, gdes);

static void tcon(pmix_server_trkr_t *t)
{
    t->event_active = false;
    t->host_called = false;
    t->local = true;
    t->id = NULL;
    memset(t->pname.nspace, 0, PMIX_MAX_NSLEN + 1);
    t->pname.rank = PMIX_RANK_UNDEF;
    t->pcs = NULL;
    t->npcs = 0;
    PMIX_CONSTRUCT(&t->nslist, pmix_list_t);
    PMIX_CONSTRUCT_LOCK(&t->lock);
    t->def_complete = false;
    PMIX_CONSTRUCT(&t->local_cbs, pmix_list_t);
    t->nlocal = 0;
    t->local_cnt = 0;
    t->info = NULL;
    t->ninfo = 0;
    PMIX_CONSTRUCT(&t->grpinfo, pmix_list_t);
    t->grpop = PMIX_GROUP_NONE;
    /* this needs to be set explicitly */
    t->collect_type = PMIX_COLLECT_INVALID;
    t->modexcbfunc = NULL;
    t->op_cbfunc = NULL;
    t->hybrid = false;
    t->cbdata = NULL;
}
static void tdes(pmix_server_trkr_t *t)
{
    if (NULL != t->id) {
        free(t->id);
    }
    PMIX_DESTRUCT_LOCK(&t->lock);
    if (NULL != t->pcs) {
        free(t->pcs);
    }
    PMIX_LIST_DESTRUCT(&t->local_cbs);
    if (NULL != t->info) {
        PMIX_INFO_FREE(t->info, t->ninfo);
    }
    PMIX_LIST_DESTRUCT(&t->grpinfo);
    PMIX_LIST_DESTRUCT(&t->nslist);
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
    cd->info = NULL;
    cd->ninfo = 0;
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
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
}
PMIX_CLASS_INSTANCE(pmix_server_caddy_t,
                    pmix_list_item_t,
                    cdcon, cddes);

static void scadcon(pmix_setup_caddy_t *p)
{
    p->peer = NULL;
    memset(&p->proc, 0, sizeof(pmix_proc_t));
    PMIX_CONSTRUCT_LOCK(&p->lock);
    p->nspace = NULL;
    p->codes = NULL;
    p->ncodes = 0;
    p->procs = NULL;
    p->nprocs = 0;
    p->apps = NULL;
    p->napps = 0;
    p->server_object = NULL;
    p->nlocalprocs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->copied = false;
    p->keys = NULL;
    p->channels = PMIX_FWD_NO_CHANNELS;
    memset(&p->flags, 0, sizeof(pmix_iof_flags_t));
    p->bo = NULL;
    p->nbo = 0;
    p->cbfunc = NULL;
    p->opcbfunc = NULL;
    p->setupcbfunc = NULL;
    p->lkcbfunc = NULL;
    p->spcbfunc = NULL;
    p->cbdata = NULL;
}
static void scaddes(pmix_setup_caddy_t *p)
{
    if (NULL != p->peer) {
        PMIX_RELEASE(p->peer);
    }
    PMIX_PROC_FREE(p->procs, p->nprocs);
    if (p->copied) {
        if (NULL != p->info) {
            PMIX_INFO_FREE(p->info, p->ninfo);
        }
        if (NULL != p->apps) {
            PMIX_APP_FREE(p->apps, p->napps);
        }
    }
    if (NULL != p->bo) {
        PMIX_BYTE_OBJECT_FREE(p->bo, p->nbo);
    }
    PMIX_DESTRUCT_LOCK(&p->lock);
    if (NULL != p->flags.file) {
        free(p->flags.file);
    }
    if (NULL != p->flags.directory) {
        free(p->flags.directory);
    }
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_setup_caddy_t,
                                pmix_object_t,
                                scadcon, scaddes);

PMIX_CLASS_INSTANCE(pmix_trkr_caddy_t, pmix_object_t, NULL, NULL);

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
    p->key = NULL;
}
static void dmrqdes(pmix_dmdx_request_t *p)
{
    if (p->event_active) {
        pmix_event_del(&p->ev);
    }
    if (NULL != p->lcd) {
        PMIX_RELEASE(p->lcd);
    }
    if (NULL != p->key) {
        free(p->key);
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
    p->affected = NULL;
    p->naffected = 0;
}
static void prevdes(pmix_peer_events_info_t *p)
{
    if (NULL != p->peer) {
        PMIX_RELEASE(p->peer);
    }
    if (NULL != p->affected) {
        PMIX_PROC_FREE(p->affected, p->naffected);
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

static void ilcon(pmix_inventory_rollup_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
    p->lock.active = false;
    p->status = PMIX_SUCCESS;
    p->requests = 0;
    p->replies = 0;
    PMIX_CONSTRUCT(&p->payload, pmix_list_t);
    p->info = NULL;
    p->ninfo = 0;
    p->cbfunc = NULL;
    p->infocbfunc = NULL;
    p->opcbfunc = NULL;
    p->cbdata = NULL;
}
static void ildes(pmix_inventory_rollup_t *p)
{
    PMIX_DESTRUCT_LOCK(&p->lock);
    PMIX_LIST_DESTRUCT(&p->payload);
}
PMIX_CLASS_INSTANCE(pmix_inventory_rollup_t,
                    pmix_object_t,
                    ilcon, ildes);

PMIX_CLASS_INSTANCE(pmix_group_caddy_t,
                    pmix_list_item_t,
                    NULL, NULL);

static void iocon(pmix_iof_cache_t *p)
{
    p->bo = NULL;
    p->info = NULL;
    p->ninfo = 0;
}
static void iodes(pmix_iof_cache_t *p)
{
    PMIX_BYTE_OBJECT_FREE(p->bo, 1); // macro protects against NULL
    if (0 < p->ninfo) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
}
PMIX_CLASS_INSTANCE(pmix_iof_cache_t,
                    pmix_list_item_t,
                    iocon, iodes);

static void pscon(pmix_pset_t *p)
{
    p->name = NULL;
    p->members = NULL;
    p->nmembers = 0;
}
static void psdes(pmix_pset_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->members) {
        free(p->members);
    }
}
PMIX_CLASS_INSTANCE(pmix_pset_t,
                    pmix_list_item_t,
                    pscon, psdes);
