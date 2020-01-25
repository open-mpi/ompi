/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016-2019 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <src/include/pmix_config.h>

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
#ifdef HAVE_TIME_H
#include <time.h>
#endif
#include PMIX_EVENT_HEADER

#include "src/class/pmix_hotel.h"
#include "src/class/pmix_list.h"
#include "src/common/pmix_attributes.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/plog/plog.h"
#include "src/mca/psensor/psensor.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#include "src/mca/gds/base/base.h"

#include "pmix_server_ops.h"

/* The rank_blob_t type to collect processes blobs,
 * this list afterward will form a node modex blob. */
typedef struct {
    pmix_list_item_t super;
    pmix_buffer_t *buf;
} rank_blob_t;

static void bufdes(rank_blob_t *p)
{
    PMIX_RELEASE(p);
}
static PMIX_CLASS_INSTANCE(rank_blob_t,
                           pmix_list_item_t,
                           NULL, bufdes);

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
        pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
        proc.rank = peer->info->pname.rank;
        rc = pmix_host_server.abort(&proc, peer->info->server_object, status, msg,
                                    procs, nprocs, cbfunc, cbdata);
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
static pmix_server_trkr_t* get_tracker(char *id, pmix_proc_t *procs,
                                       size_t nprocs, pmix_cmd_t type)
{
    pmix_server_trkr_t *trk;
    size_t i, j;
    size_t matches;

    pmix_output_verbose(5, pmix_server_globals.base_output,
                        "get_tracker called with %d procs", (int)nprocs);

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
    PMIX_LIST_FOREACH(trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
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
static pmix_server_trkr_t* new_tracker(char *id, pmix_proc_t *procs,
                                       size_t nprocs, pmix_cmd_t type)
{
    pmix_server_trkr_t *trk;
    size_t i;
    bool all_def;
    pmix_rank_t ns_local = 0;
    pmix_namespace_t *nptr, *ns;
    pmix_rank_info_t *info;
    pmix_nspace_caddy_t *nm;

    pmix_output_verbose(5, pmix_server_globals.base_output,
                        "new_tracker called with %d procs", (int)nprocs);

    /* bozo check - should never happen outside of programmer error */
    if (NULL == procs) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }

    pmix_output_verbose(5, pmix_server_globals.base_output,
                        "adding new tracker %s with %d procs",
                        (NULL == id) ? "NO-ID" : id, (int)nprocs);

    /* this tracker is new - create it */
    trk = PMIX_NEW(pmix_server_trkr_t);
    if (NULL == trk) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        return NULL;
    }

    if (NULL != id) {
        trk->id = strdup(id);
    }

    if (NULL != procs) {
        /* copy the procs */
        PMIX_PROC_CREATE(trk->pcs, nprocs);
        if (NULL == trk->pcs) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            PMIX_RELEASE(trk);
            return NULL;
        }
        memcpy(trk->pcs, procs, nprocs * sizeof(pmix_proc_t));
        trk->npcs = nprocs;
    }
    trk->type = type;

    all_def = true;
    for (i=0; i < nprocs; i++) {
        if (NULL == id) {
            pmix_strncpy(trk->pcs[i].nspace, procs[i].nspace, PMIX_MAX_NSLEN);
            trk->pcs[i].rank = procs[i].rank;
        }
        if (!all_def) {
            continue;
        }
        /* is this nspace known to us? */
        nptr = NULL;
        PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_namespace_t) {
            if (0 == strcmp(procs[i].nspace, ns->nspace)) {
                nptr = ns;
                break;
            }
        }
        if (NULL == nptr || nptr->nlocalprocs <= 0) {
            /* cannot be a local proc */
            pmix_output_verbose(5, pmix_server_globals.base_output,
                                "new_tracker: unknown nspace %s",
                                procs[i].nspace);
            trk->local = false;
            continue;
        }
        /* check and add uniq ns into trk nslist */
        PMIX_LIST_FOREACH(nm, &trk->nslist, pmix_nspace_caddy_t) {
            if (0 == strcmp(nptr->nspace, nm->ns->nspace)) {
                break;
            }
        }
        if ((pmix_nspace_caddy_t*)pmix_list_get_end(&trk->nslist) == nm) {
            nm = PMIX_NEW(pmix_nspace_caddy_t);
            PMIX_RETAIN(nptr);
            nm->ns = nptr;
            pmix_list_append(&trk->nslist, &nm->super);
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
        ns_local = 0;
        PMIX_LIST_FOREACH(info, &nptr->ranks, pmix_rank_info_t) {
            if (procs[i].rank == info->pname.rank ||
                PMIX_RANK_WILDCARD == procs[i].rank) {
                    pmix_output_verbose(5, pmix_server_globals.base_output,
                                        "adding local proc %s.%d to tracker",
                                        info->pname.nspace, info->pname.rank);
                /* track the count */
                ns_local++;
                if (PMIX_RANK_WILDCARD != procs[i].rank) {
                    break;
                }
            }
        }

        trk->nlocal += ns_local;
        if (0 == ns_local) {
            trk->local = false;
        } else if (PMIX_RANK_WILDCARD == procs[i].rank) {
            /* If proc is a wildcard we need to additionally check
             * that all of the processes in the namespace were
             * locally found.
             * Otherwise this tracker is not local
             */
            if (ns_local != nptr->nprocs) {
                trk->local = false;
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
    bool found;
    pmix_list_t rank_blobs;
    rank_blob_t *blob;
    uint32_t kmap_size;
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

            PMIX_LIST_FOREACH(scd, &trk->local_cbs, pmix_server_caddy_t) {
                pmix_strncpy(pcs.nspace, scd->peer->info->pname.nspace,
                             PMIX_MAX_NSLEN);
                pcs.rank = scd->peer->info->pname.rank;
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                cb.proc = &pcs;
                cb.scope = PMIX_REMOTE;
                cb.copy = true;
                PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
                if (PMIX_SUCCESS == rc) {
                    int key_idx;
                    PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
                        rc = pmix_argv_append_unique_idx(&key_idx, &kmap,
                                                         kv->key);
                        if (pmix_value_array_get_size(key_count_array) <
                                (size_t)(key_idx+1)) {
                            size_t new_size;
                            size_t old_size =
                                    pmix_value_array_get_size(key_count_array);

                            pmix_value_array_set_size(key_count_array,
                                                      key_idx+1);
                            new_size =
                                    pmix_value_array_get_size(key_count_array);
                            key_count =
                                    PMIX_VALUE_ARRAY_GET_BASE(key_count_array,
                                                              uint32_t);
                            memset(key_count + old_size, 0, sizeof(uint32_t) *
                                   (new_size - old_size));
                        }
                        key_count = PMIX_VALUE_ARRAY_GET_BASE(key_count_array,
                                                              uint32_t);
                        key_count[key_idx]++;
                    }
                }
            }

            key_count = PMIX_VALUE_ARRAY_GET_BASE(key_count_array, uint32_t);

            for (i = 0; i < pmix_argv_count(kmap); i++) {
                pmix_buffer_t tmp;
                size_t kname_size;
                size_t kidx_size;

                PMIX_CONSTRUCT(&tmp, pmix_buffer_t);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &tmp, &kmap[i], 1,
                                 PMIX_STRING);
                kname_size = tmp.bytes_used;
                PMIX_DESTRUCT(&tmp);
                PMIX_CONSTRUCT(&tmp, pmix_buffer_t);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &tmp, &i, 1,
                                 PMIX_UINT32);
                kidx_size = tmp.bytes_used;
                PMIX_DESTRUCT(&tmp);

                /* calculate the key names sizes */
                key_fmt_size[PMIX_MODEX_KEY_NATIVE_FMT] =
                        kname_size * key_count[i];
                key_fmt_size[PMIX_MODEX_KEY_KEYMAP_FMT] =
                        kname_size + key_count[i]*kidx_size;
            }
            PMIX_RELEASE(key_count_array);

            /* select the most efficient key-name pack format */
            kmap_type = key_fmt_size[PMIX_MODEX_KEY_NATIVE_FMT] >
                        key_fmt_size[PMIX_MODEX_KEY_KEYMAP_FMT] ?
                        PMIX_MODEX_KEY_KEYMAP_FMT : PMIX_MODEX_KEY_NATIVE_FMT;
            pmix_output_verbose(5, pmix_server_globals.base_output,
                                "key packing type %s",
                                kmap_type == PMIX_MODEX_KEY_KEYMAP_FMT ?
                                    "kmap" : "native");
        }
        PMIX_CONSTRUCT(&rank_blobs, pmix_list_t);
        PMIX_LIST_FOREACH(scd, &trk->local_cbs, pmix_server_caddy_t) {
            /* get any remote contribution - note that there
             * may not be a contribution */
            pmix_strncpy(pcs.nspace, scd->peer->info->pname.nspace,
                         PMIX_MAX_NSLEN);
            pcs.rank = scd->peer->info->pname.rank;
            PMIX_CONSTRUCT(&cb, pmix_cb_t);
            cb.proc = &pcs;
            cb.scope = PMIX_REMOTE;
            cb.copy = true;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
            if (PMIX_SUCCESS == rc) {
                /* calculate the throughout rank */
                rel_rank = 0;
                found = false;
                if (pmix_list_get_size(&trk->nslist) == 1) {
                    found = true;
                } else {
                    PMIX_LIST_FOREACH(nm, &trk->nslist, pmix_nspace_caddy_t) {
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
                    PMIX_DESTRUCT(&rank_blobs);
                    goto cleanup;
                }
                rel_rank += pcs.rank;

                /* pack the relative rank */
                pbkt = PMIX_NEW(pmix_buffer_t);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, pbkt,
                                 &rel_rank, 1, PMIX_PROC_RANK);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_DESTRUCT(&cb);
                    PMIX_DESTRUCT(&rank_blobs);
                    PMIX_RELEASE(pbkt);
                    goto cleanup;
                }
                /* pack the returned kval's */
                PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
                    rc = pmix_gds_base_modex_pack_kval(kmap_type, pbkt, &kmap,
                                                       kv);
                    if (rc != PMIX_SUCCESS) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        PMIX_DESTRUCT(&rank_blobs);
                        PMIX_RELEASE(pbkt);
                        goto cleanup;
                    }
                }

                /* add part of the process modex to the list */
                blob = PMIX_NEW(rank_blob_t);
                blob->buf = pbkt;
                pmix_list_append(&rank_blobs, &blob->super);
                pbkt = NULL;
            }
            PMIX_DESTRUCT(&cb);
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
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                         &blob_info_byte, 1, PMIX_BYTE);

        if (PMIX_MODEX_KEY_KEYMAP_FMT == kmap_type) {
            /* pack node part of modex to `bucket` */
            /* pack the key names map for the remote server can
             * use it to match key names by index */
            kmap_size = pmix_argv_count(kmap);
            if (0 < kmap_size) {
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                                 &kmap_size, 1, PMIX_UINT32);
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                                 kmap, kmap_size, PMIX_STRING);
            }
        }
        /* pack the collected blobs of processes */
        PMIX_LIST_FOREACH(blob, &rank_blobs, rank_blob_t) {
            /* extract the blob */
            PMIX_UNLOAD_BUFFER(blob->buf, bo.bytes, bo.size);
            /* pack the returned blob */
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                             &bo, 1, PMIX_BYTE_OBJECT);
            PMIX_BYTE_OBJECT_DESTRUCT(&bo); // releases the data
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                goto cleanup;
            }
        }
        PMIX_DESTRUCT(&rank_blobs);
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
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &bucket,
                         &blob_info_byte, 1, PMIX_BYTE);
#endif
    }
    if (!PMIX_BUFFER_IS_EMPTY(&bucket)) {
        /* because the remote servers have to unpack things
         * in chunks, we have to pack the bucket as a single
         * byte object to allow remote unpack */
        PMIX_UNLOAD_BUFFER(&bucket, bo.bytes, bo.size);
        PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, buf,
                         &bo, 1, PMIX_BYTE_OBJECT);
        PMIX_BYTE_OBJECT_DESTRUCT(&bo);  // releases the data
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

  cleanup:
    PMIX_DESTRUCT(&bucket);
    pmix_argv_free(kmap);
    return rc;
}

pmix_status_t pmix_server_fence(pmix_server_caddy_t *cd,
                                pmix_buffer_t *buf,
                                pmix_modex_cbfunc_t modexcbfunc,
                                pmix_op_cbfunc_t opcbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    size_t nprocs;
    pmix_proc_t *procs=NULL, *newprocs;
    bool collect_data = false;
    pmix_server_trkr_t *trk;
    char *data = NULL;
    size_t sz = 0;
    pmix_buffer_t bucket;
    pmix_info_t *info = NULL;
    size_t ninfo=0, n, nmbrs, idx;
    struct timeval tv = {0, 0};
    pmix_list_t expand;
    pmix_group_caddy_t *gcd;
    pmix_group_t *grp;

    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "recvd FENCE");

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

    /* cycle thru the procs and check to see if any reference
     * a PMIx group */
    nmbrs = nprocs;
    PMIX_CONSTRUCT(&expand, pmix_list_t);
    /* use groups as the outer-most loop as there will
     * usually not be any */
    PMIX_LIST_FOREACH(grp, &pmix_server_globals.groups, pmix_group_t) {
        for (n=0; n < nprocs; n++) {
            if (PMIX_CHECK_NSPACE(procs[n].nspace, grp->grpid)) {
                /* we need to replace this proc with grp members */
                gcd = PMIX_NEW(pmix_group_caddy_t);
                gcd->grp = grp;
                gcd->idx = n;
                gcd->rank = procs[n].rank;
                pmix_list_append(&expand, &gcd->super);
                /* see how many need to come across */
                if (PMIX_RANK_WILDCARD == procs[n].rank) {
                    nmbrs += grp->nmbrs - 1; // account for replacing current proc
                }
                break;
            }
        }
    }

    if (0 < pmix_list_get_size(&expand)) {
        PMIX_PROC_CREATE(newprocs, nmbrs);
        gcd = (pmix_group_caddy_t*)pmix_list_remove_first(&expand);
        n=0;
        idx = 0;
        while (n < nmbrs) {
            if (idx != gcd->idx) {
                memcpy(&newprocs[n], &procs[idx], sizeof(pmix_proc_t));
                ++n;
            } else {
                /* if we are bringing over just one, then simply replace */
                if (PMIX_RANK_WILDCARD != gcd->rank) {
                    memcpy(&newprocs[n], &gcd->grp->members[gcd->rank], sizeof(pmix_proc_t));
                    ++n;
                } else {
                    /* take them all */
                    memcpy(&newprocs[n], gcd->grp->members, gcd->grp->nmbrs * sizeof(pmix_proc_t));
                    n += gcd->grp->nmbrs;
                }
                PMIX_RELEASE(gcd);
                gcd = (pmix_group_caddy_t*)pmix_list_remove_first(&expand);
            }
            ++idx;
        }
        PMIX_PROC_FREE(procs, nprocs);
        procs = newprocs;
        nprocs = nmbrs;
    }
    PMIX_LIST_DESTRUCT(&expand);

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
        pmix_output_verbose(2, pmix_server_globals.fence_output,
                            "fence LOCALLY complete");
        /* if this is a purely local fence (i.e., all participants are local),
         * then it is done and we notify accordingly */
        if (trk->local) {
            /* the modexcbfunc thread-shifts the call prior to processing,
             * so it is okay to call it directly from here. The switchyard
             * will acknowledge successful acceptance of the fence request,
             * but the client still requires a return from the callback in
             * that scenario, so we leave this caddy on the list of local cbs */
            trk->modexcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
            rc = PMIX_SUCCESS;
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
        rc = pmix_host_server.fence_nb(trk->pcs, trk->npcs,
                                       trk->info, trk->ninfo,
                                       data, sz, trk->modexcbfunc, trk);
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
            trk->modexcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
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
    pmix_strncpy(cd->info[cd->ninfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo-1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo-1].value.data.uint32 = uid;

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
    pmix_strncpy(cd->info[cd->ninfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo-1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo-1].value.data.uint32 = uid;

    /* call the local server */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
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
    pmix_strncpy(cd->info[cd->ninfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    cd->info[cd->ninfo-1].value.type = PMIX_UINT32;
    cd->info[cd->ninfo-1].value.data.uint32 = uid;

    /* call the local server */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
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
        req->peer = cd->peer;
        req->pname.nspace = strdup(nspace);
        req->pname.rank = PMIX_RANK_WILDCARD;
        req->channels = cd->channels;
        pmix_list_append(&pmix_globals.iof_requests, &req->super);
        /* process any cached IO */
        PMIX_LIST_FOREACH_SAFE(iof, ionext, &pmix_server_globals.iof, pmix_iof_cache_t) {
            /* if the channels don't match, then ignore it */
            if (!(iof->channel & req->channels)) {
                continue;
            }
            /* if the source does not match the request, then ignore it */
            if (!PMIX_CHECK_PROCID(&iof->source, &req->pname)) {
                continue;
            }
            /* never forward back to the source! This can happen if the source
             * is a launcher */
            if (PMIX_CHECK_PROCID(&iof->source, &req->peer->info->pname)) {
                continue;
            }
            pmix_output_verbose(2, pmix_server_globals.iof_output,
                                "PMIX:SERVER:SPAWN delivering cached IOF from %s:%d to %s:%d",
                                iof->source.nspace, iof->source.rank,
                                req->pname.nspace, req->pname.rank);
            /* setup the msg */
            if (NULL == (msg = PMIX_NEW(pmix_buffer_t))) {
                PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                rc = PMIX_ERR_OUT_OF_RESOURCE;
                break;
            }
            /* provide the source */
            PMIX_BFROPS_PACK(rc, req->peer, msg, &iof->source, 1, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* provide the channel */
            PMIX_BFROPS_PACK(rc, req->peer, msg, &iof->channel, 1, PMIX_IOF_CHANNEL);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* pack the data */
            PMIX_BFROPS_PACK(rc, req->peer, msg, iof->bo, 1, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* send it to the requestor */
            PMIX_PTL_SEND_ONEWAY(rc, req->peer, msg, PMIX_PTL_TAG_IOF);
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
    /* cleanup the caddy */
    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    if (NULL != cd->apps) {
        PMIX_APP_FREE(cd->apps, cd->napps);
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
    size_t ninfo, n;
    bool stdout_found = false, stderr_found = false, stddiag_found = false;

    pmix_output_verbose(2, pmix_server_globals.spawn_output,
                        "recvd SPAWN from %s:%d", peer->info->pname.nspace, peer->info->pname.rank);

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
        /* run a quick check of the directives to see if any IOF
         * requests were included so we can set that up now - helps
         * to catch any early output - and a request for notification
         * of job termination so we can setup the event registration */
        cd->channels = PMIX_FWD_NO_CHANNELS;
        for (n=0; n < cd->ninfo; n++) {
            if (0 == strncmp(cd->info[n].key, PMIX_FWD_STDIN, PMIX_MAX_KEYLEN)) {
                if (PMIX_INFO_TRUE(&cd->info[n])) {
                    cd->channels |= PMIX_FWD_STDIN_CHANNEL;
                }
            } else if (0 == strncmp(cd->info[n].key, PMIX_FWD_STDOUT, PMIX_MAX_KEYLEN)) {
                stdout_found = true;
                if (PMIX_INFO_TRUE(&cd->info[n])) {
                    cd->channels |= PMIX_FWD_STDOUT_CHANNEL;
                }
            } else if (0 == strncmp(cd->info[n].key, PMIX_FWD_STDERR, PMIX_MAX_KEYLEN)) {
                stderr_found = true;
                if (PMIX_INFO_TRUE(&cd->info[n])) {
                    cd->channels |= PMIX_FWD_STDERR_CHANNEL;
                }
            } else if (0 == strncmp(cd->info[n].key, PMIX_FWD_STDDIAG, PMIX_MAX_KEYLEN)) {
                stddiag_found = true;
                if (PMIX_INFO_TRUE(&cd->info[n])) {
                    cd->channels |= PMIX_FWD_STDDIAG_CHANNEL;
                }
            }
        }
        /* we will construct any required iof request tracker upon completion of the spawn
         * as we need the nspace of the spawned application! */
    }
    /* add the directive to the end */
    if (PMIX_PROC_IS_TOOL(peer)) {
        PMIX_INFO_LOAD(&cd->info[ninfo], PMIX_REQUESTOR_IS_TOOL, NULL, PMIX_BOOL);
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
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
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
    size_t nprocs, ninfo;
    pmix_server_trkr_t *trk;
    pmix_proc_t *procs = NULL;

    if (NULL == pmix_host_server.disconnect) {
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
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        trk->host_called = true;
        rc = pmix_host_server.disconnect(trk->pcs, trk->npcs, trk->info, trk->ninfo, cbfunc, trk);
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
    if (NULL != cd->trk->op_cbfunc) {
        cd->trk->op_cbfunc(PMIX_ERR_TIMEOUT, cd->trk);
        return;  // the cbfunc will have cleaned up the tracker
    }
    cd->event_active = false;
    /* remove it from the list */
    pmix_list_remove_item(&cd->trk->local_cbs, &cd->super);
    PMIX_RELEASE(cd);
}

pmix_status_t pmix_server_connect(pmix_server_caddy_t *cd,
                                  pmix_buffer_t *buf,
                                  pmix_op_cbfunc_t cbfunc)
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
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
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
    } else {
        rc = PMIX_SUCCESS;
    }
    /* if a timeout was specified, set it */
    if (PMIX_SUCCESS == rc && 0 < tv.tv_sec) {
        PMIX_RETAIN(trk);
        cd->trk = trk;
        pmix_event_evtimer_set(pmix_globals.evbase, &cd->ev,
                               connect_timeout, cd);
        pmix_event_evtimer_add(&cd->ev, &tv);
        cd->event_active = true;
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
    pmix_setup_caddy_t *scd = (pmix_setup_caddy_t*)cbdata;
    pmix_notify_caddy_t *cd;
    pmix_range_trkr_t rngtrk;
    pmix_proc_t proc;
    int i;
    size_t k, n;
    bool found, matched;
    pmix_buffer_t *relay;
    pmix_status_t ret = PMIX_SUCCESS;
    pmix_cmd_t cmd = PMIX_NOTIFY_CMD;

    /* check if any matching notifications have been cached */
    rngtrk.procs = NULL;
    rngtrk.nprocs = 0;
    for (i=0; i < pmix_globals.max_events; i++) {
        pmix_hotel_knock(&pmix_globals.notifications, i, (void**)&cd);
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
            for (k=0; k < scd->ncodes; k++) {
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
        if (!pmix_notify_check_affected(cd->affected, cd->naffected,
                                        scd->procs, scd->nprocs)) {
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
            for (n=0; n < cd->ntargets; n++) {
                /* if the source of the event is the same peer just registered, then ignore it
                 * as the event notification system will have already locally
                 * processed it */
                if (PMIX_CHECK_PROCID(&cd->source, &scd->peer->info->pname)) {
                    continue;
                }
                if (PMIX_CHECK_PROCID(&scd->peer->info->pname, &cd->targets[n])) {
                    matched = true;
                    /* track the number of targets we have left to notify */
                    --cd->nleft;
                    /* if this is the last one, then evict this event
                     * from the cache */
                    if (0 == cd->nleft) {
                        pmix_hotel_checkout(&pmix_globals.notifications, cd->room);
                        found = true;  // mark that we should release cd
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
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;

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


pmix_status_t pmix_server_register_events(pmix_peer_t *peer,
                                          pmix_buffer_t *buf,
                                          pmix_op_cbfunc_t cbfunc,
                                          void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_status_t *codes = NULL;
    pmix_info_t *info = NULL;
    size_t ninfo=0, ncodes, n;
    pmix_regevents_info_t *reginfo;
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

    /* check the directives */
    for (n=0; n < ninfo; n++) {
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
    for (n=0; n < ncodes; n++) {
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
    if (0 == ncodes)  {
        PMIX_LIST_FOREACH(reginfo, &pmix_server_globals.events, pmix_regevents_info_t) {
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
    for (n=0; n < ncodes; n++) {
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
                } else if (codes[n] == reginfo->code) {
                    found = true;
                    break;
                }
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
            reginfo = PMIX_NEW(pmix_regevents_info_t);
            if (NULL == reginfo) {
                rc = PMIX_ERR_NOMEM;
                goto cleanup;
            }
            if (NULL == codes) {
                reginfo->code = PMIX_MAX_ERR_CONSTANT;
            } else {
                reginfo->code = codes[n];
            }
            pmix_list_append(&pmix_server_globals.events, &reginfo->super);
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
        if (PMIX_SUCCESS == (rc = pmix_host_server.register_events(scd->codes, scd->ncodes, scd->info, scd->ninfo, regevopcbfunc, scd))) {
            /* the host will call us back when completed */
            pmix_output_verbose(2, pmix_server_globals.event_output,
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
                                 "server register events: host server reg events returned rc =%d", rc);
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

static void intermed_step(pmix_status_t status, void *cbdata)
{
    pmix_notify_caddy_t *cd = (pmix_notify_caddy_t*)cbdata;
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

    if (NULL == pmix_host_server.notify_event) {
        rc = PMIX_ERR_NOT_SUPPORTED;
        goto complete;
    }

    /* since our host is going to send this everywhere, it may well
     * come back to us. We already processed it, so mark it here
     * to ensure we don't do it again. We previously inserted the
     * PMIX_SERVER_INTERNAL_NOTIFY key at the very end of the
     * info array - just overwrite that position */
    PMIX_INFO_LOAD(&cd->info[cd->ninfo-1], PMIX_EVENT_PROXY, &pmix_globals.myid, PMIX_PROC);

    /* pass it to our host RM for distribution */
    rc = pmix_host_server.notify_event(cd->status, &cd->source, cd->range,
                                       cd->info, cd->ninfo, local_cbfunc, cd);
    if (PMIX_SUCCESS == rc) {
        /* let the callback function respond for us */
        return;
    }
    if (PMIX_OPERATION_SUCCEEDED == rc) {
        rc = PMIX_SUCCESS;  // local_cbfunc will not be called
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
pmix_status_t pmix_server_event_recvd_from_client(pmix_peer_t *peer,
                                                  pmix_buffer_t *buf,
                                                  pmix_op_cbfunc_t cbfunc,
                                                  void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_notify_caddy_t *cd;
    size_t ninfo, n;

    pmix_output_verbose(2, pmix_server_globals.event_output,
                        "%s:%d recvd event notification from client %s:%d",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        peer->info->pname.nspace, peer->info->pname.rank);

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
    for (n=0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&cd->info[n], PMIX_SERVER_INTERNAL_NOTIFY)) {
            /* yep, we did - so don't do it again! */
            rc = PMIX_OPERATION_SUCCEEDED;
            goto exit;
        }
    }

    /* add an info object to mark that we recvd this internally */
    PMIX_INFO_LOAD(&cd->info[ninfo], PMIX_SERVER_INTERNAL_NOTIFY, NULL, PMIX_BOOL);
    /* process it */
    if (PMIX_SUCCESS != (rc = pmix_server_notify_client_of_event(cd->status,
                                                                 &cd->source,
                                                                 cd->range,
                                                                 cd->info, cd->ninfo,
                                                                 intermed_step, cd))) {
        goto exit;
    }
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cd);
    }
    return rc;

  exit:
    PMIX_RELEASE(cd);
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
    pmix_cb_t cb;
    size_t n, p;
    pmix_list_t results;
    pmix_kval_t *kv, *kvnxt;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd query from client");

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

    /* check the directives to see if they want us to refresh
     * the local cached results - if we wanted to optimize this
     * more, we would check each query and allow those that don't
     * want to be refreshed to be executed locally, and those that
     * did would be sent to the host. However, for now we simply
     * determine that if we don't have it, then ask for everything */
    memset(proc.nspace, 0, PMIX_MAX_NSLEN+1);
    proc.rank = PMIX_RANK_INVALID;
    PMIX_CONSTRUCT(&results, pmix_list_t);

    for (n=0; n < cd->nqueries; n++) {
        /* if they are asking for information on support, then go get it */
        if (0 == strcmp(cd->queries[n].keys[0], PMIX_QUERY_ATTRIBUTE_SUPPORT)) {
            /* we are already in an event, but shift it as the handler expects to */
            cd->cbfunc = cbfunc;
            PMIX_RETAIN(cd); // protect against early release
            PMIX_THREADSHIFT(cd, pmix_attrs_query_support);
            PMIX_LIST_DESTRUCT(&results);
            return PMIX_SUCCESS;
        }
        for (p=0; p < cd->queries[n].nqual; p++) {
            if (PMIX_CHECK_KEY(&cd->queries[n].qualifiers[p], PMIX_QUERY_REFRESH_CACHE)) {
                if (PMIX_INFO_TRUE(&cd->queries[n].qualifiers[p])) {
                    PMIX_LIST_DESTRUCT(&results);
                    goto query;
                }
            } else if (PMIX_CHECK_KEY(&cd->queries[n].qualifiers[p], PMIX_PROCID)) {
                PMIX_LOAD_NSPACE(proc.nspace, cd->queries[n].qualifiers[p].value.data.proc->nspace);
                proc.rank = cd->queries[n].qualifiers[p].value.data.proc->rank;
            } else if (PMIX_CHECK_KEY(&cd->queries[n].qualifiers[p], PMIX_NSPACE)) {
                PMIX_LOAD_NSPACE(proc.nspace, cd->queries[n].qualifiers[p].value.data.string);
            } else if (PMIX_CHECK_KEY(&cd->queries[n].qualifiers[p], PMIX_RANK)) {
                proc.rank = cd->queries[n].qualifiers[p].value.data.rank;
            } else if (PMIX_CHECK_KEY(&cd->queries[n].qualifiers[p], PMIX_HOSTNAME)) {
                if (0 != strcmp(cd->queries[n].qualifiers[p].value.data.string, pmix_globals.hostname)) {
                    /* asking about a different host, so ask for the info */
                    PMIX_LIST_DESTRUCT(&results);
                    goto query;
                }
            }
        }
        /* we get here if a refresh isn't required - first try a local
         * "get" on the data to see if we already have it */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.copy = false;
        /* set the proc */
        if (PMIX_RANK_INVALID == proc.rank &&
            0 == strlen(proc.nspace)) {
            /* use our id */
            cb.proc = &pmix_globals.myid;
        } else {
            if (0 == strlen(proc.nspace)) {
                /* use our nspace */
                PMIX_LOAD_NSPACE(cb.proc->nspace, pmix_globals.myid.nspace);
            }
            if (PMIX_RANK_INVALID == proc.rank) {
                /* user the wildcard rank */
                proc.rank = PMIX_RANK_WILDCARD;
            }
            cb.proc = &proc;
        }
        for (p=0; NULL != cd->queries[n].keys[p]; p++) {
            cb.key = cd->queries[n].keys[p];
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
            if (PMIX_SUCCESS != rc) {
                /* needs to be passed to the host */
                PMIX_LIST_DESTRUCT(&results);
                PMIX_DESTRUCT(&cb);
                goto query;
            }
            /* need to retain this result */
            PMIX_LIST_FOREACH_SAFE(kv, kvnxt, &cb.kvs, pmix_kval_t) {
                pmix_list_remove_item(&cb.kvs, &kv->super);
                pmix_list_append(&results, &kv->super);
            }
            PMIX_DESTRUCT(&cb);
        }
    }

    /* if we get here, then all queries were completely locally
     * resolved, so construct the results for return */
    rc = PMIX_ERR_NOT_FOUND;
    if (0 < (cd->ninfo = pmix_list_get_size(&results))) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        n = 0;
        PMIX_LIST_FOREACH_SAFE(kv, kvnxt, &results, pmix_kval_t) {
            PMIX_LOAD_KEY(cd->info[n].key, kv->key);
            rc = pmix_value_xfer(&cd->info[n].value, kv->value);
            if (PMIX_SUCCESS != rc) {
                PMIX_INFO_FREE(cd->info, cd->ninfo);
                cd->info = NULL;
                cd->ninfo = 0;
                break;
            }
            ++n;
        }
    }
    /* done with the list of results */
    PMIX_LIST_DESTRUCT(&results);
    /* we can just call the cbfunc here as we are already
     * in an event - let our internal cbfunc do a threadshift
     * if necessary */
    cbfunc(PMIX_SUCCESS, cd->info, cd->ninfo, cd, NULL, NULL);
    return PMIX_SUCCESS;

  query:
    if (NULL == pmix_host_server.query) {
        PMIX_RELEASE(cd);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* setup the requesting peer name */
    PMIX_LOAD_PROCID(&proc, peer->info->pname.nspace, peer->info->pname.rank);

    /* ask the host for the info */
    if (PMIX_SUCCESS != (rc = pmix_host_server.query(&proc, cd->queries, cd->nqueries,
                                                     cbfunc, cd))) {
        PMIX_RELEASE(cd);
    }
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
    time_t timestamp;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "recvd log from client");

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
    /* unpack the timestamp */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &timestamp, &cnt, PMIX_TIME);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto exit;
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
        PMIX_INFO_LOAD(&cd->directives[cnt+1], PMIX_LOG_TIMESTAMP, &timestamp, PMIX_TIME);
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
    rc = pmix_plog.log(&proc, cd->info, cd->ninfo,
                       cd->directives, cd->ndirs,
                       logcbfn, cd);
    return rc;

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
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
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

typedef struct {
    pmix_list_item_t super;
    pmix_epilog_t *epi;
} pmix_srvr_epi_caddy_t;
static PMIX_CLASS_INSTANCE(pmix_srvr_epi_caddy_t,
                           pmix_list_item_t,
                           NULL, NULL);

pmix_status_t pmix_server_job_ctrl(pmix_peer_t *peer,
                                   pmix_buffer_t *buf,
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
        for (n=0; n < cd->ntargets; n++) {
            /* find the nspace of this proc */
            nptr = NULL;
            PMIX_LIST_FOREACH(tmp, &pmix_globals.nspaces, pmix_namespace_t) {
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
                for (m=0; m < pmix_server_globals.clients.size; m++) {
                    if (NULL == (pr = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, m))) {
                        continue;
                    }
                    if (0 != strncmp(pr->info->pname.nspace, cd->targets[n].nspace, PMIX_MAX_NSLEN)) {
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

    cnt = 0;  // track how many infos are cleanup related
    for (n=0; n < cd->ninfo; n++) {
        if (PMIX_CHECK_KEY(&cd->info[n], PMIX_REGISTER_CLEANUP)) {
            ++cnt;
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
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_REGISTER_CLEANUP_DIR)) {
            ++cnt;
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
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_CLEANUP_RECURSIVE)) {
            recurse = PMIX_INFO_TRUE(&cd->info[n]);
            ++cnt;
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_CLEANUP_IGNORE)) {
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
            pmix_list_append(&ignorefiles, &cf->super);
            ++cnt;
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_CLEANUP_LEAVE_TOPDIR)) {
            leave_topdir = PMIX_INFO_TRUE(&cd->info[n]);
            ++cnt;
        }
    }
    if (0 < cnt) {
        /* handle any ignore directives first */
        PMIX_LIST_FOREACH(cf, &ignorefiles, pmix_cleanup_file_t) {
            PMIX_LIST_FOREACH(epicd, &epicache, pmix_srvr_epi_caddy_t) {
                /* scan the existing list of files for any duplicate */
                duplicate = false;
                PMIX_LIST_FOREACH(cf2, &epicd->epi->cleanup_files, pmix_cleanup_file_t) {
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
        PMIX_LIST_FOREACH(cdir, &cachedirs, pmix_cleanup_dir_t) {
            PMIX_LIST_FOREACH(epicd, &epicache, pmix_srvr_epi_caddy_t) {
                /* scan the existing list of directories for any duplicate */
                duplicate = false;
                PMIX_LIST_FOREACH(cdir2, &epicd->epi->cleanup_dirs, pmix_cleanup_dir_t) {
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
                    PMIX_LIST_FOREACH(cf, &epicd->epi->ignores, pmix_cleanup_file_t) {
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
        PMIX_LIST_FOREACH(cf, &cachefiles, pmix_cleanup_file_t) {
            PMIX_LIST_FOREACH(epicd, &epicache, pmix_srvr_epi_caddy_t) {
                /* scan the existing list of files for any duplicate */
                duplicate = false;
                PMIX_LIST_FOREACH(cf2, &epicd->epi->cleanup_files, pmix_cleanup_file_t) {
                    if (0 == strcmp(cf2->path, cf->path)) {
                        duplicate = true;
                        break;
                    }
                }
                if (!duplicate) {
                    /* check for conflict with ignore */
                    PMIX_LIST_FOREACH(cf2, &epicd->epi->ignores, pmix_cleanup_file_t) {
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
        if (cnt == (int)cd->ninfo) {
            /* nothing more to do */
            rc = PMIX_OPERATION_SUCCEEDED;
            goto exit;
        }
    }

    /* setup the requesting peer name */
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->pname.rank;

    /* ask the host to execute the request */
    if (PMIX_SUCCESS != (rc = pmix_host_server.job_control(&proc,
                                                           cd->targets, cd->ntargets,
                                                           cd->info, cd->ninfo,
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
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
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
    pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
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

pmix_status_t pmix_server_iofreg(pmix_peer_t *peer,
                                 pmix_buffer_t *buf,
                                 pmix_op_cbfunc_t cbfunc,
                                 void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_setup_caddy_t *cd;
    pmix_iof_req_t *req;
    bool notify, match;
    size_t n;
    pmix_buffer_t *msg;
    pmix_iof_cache_t *iof, *ionext;

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "recvd IOF PULL request from client");

    if (NULL == pmix_host_server.iof_pull) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbdata = cbdata;  // this is the pmix_server_caddy_t

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

    /* check to see if we have already registered this source/channel combination */
    notify = false;
    for (n=0; n < cd->nprocs; n++) {
        match = false;
        PMIX_LIST_FOREACH(req, &pmix_globals.iof_requests, pmix_iof_req_t) {
            /* is this request from the same peer? */
            if (peer != req->peer) {
                continue;
            }
            /* do we already have this source for this peer? */
            if (PMIX_CHECK_PROCID(&cd->procs[n], &req->pname)) {
                match = true;
                if ((req->channels & cd->channels) != cd->channels) {
                    /* this is a channel update */
                    req->channels |= cd->channels;
                    /* we need to notify the host */
                    notify = true;
                }
                break;
            }
        }
        /* if we didn't find the matching entry, then add it */
        if (!match) {
            /* record the request */
            req = PMIX_NEW(pmix_iof_req_t);
            if (NULL == req) {
                rc = PMIX_ERR_NOMEM;
                goto exit;
            }
            PMIX_RETAIN(peer);
            req->peer = peer;
            req->pname.nspace = strdup(cd->procs[n].nspace);
            req->pname.rank = cd->procs[n].rank;
            req->channels = cd->channels;
            pmix_list_append(&pmix_globals.iof_requests, &req->super);
        }
        /* process any cached IO */
        PMIX_LIST_FOREACH_SAFE(iof, ionext, &pmix_server_globals.iof, pmix_iof_cache_t) {
            /* if the channels don't match, then ignore it */
            if (!(iof->channel & req->channels)) {
                continue;
            }
            /* if the source does not match the request, then ignore it */
            if (!PMIX_CHECK_PROCID(&iof->source, &req->pname)) {
                continue;
            }
            /* never forward back to the source! This can happen if the source
             * is a launcher */
            if (PMIX_CHECK_PROCID(&iof->source, &req->peer->info->pname)) {
                continue;
            }
            pmix_output_verbose(2, pmix_server_globals.iof_output,
                                "PMIX:SERVER:IOFREQ delivering cached IOF from %s:%d to %s:%d",
                                iof->source.nspace, iof->source.rank,
                                req->peer->info->pname.nspace, req->peer->info->pname.rank);
            /* setup the msg */
            if (NULL == (msg = PMIX_NEW(pmix_buffer_t))) {
                PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                rc = PMIX_ERR_OUT_OF_RESOURCE;
                break;
            }
            /* provide the source */
            PMIX_BFROPS_PACK(rc, req->peer, msg, &iof->source, 1, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* provide the channel */
            PMIX_BFROPS_PACK(rc, req->peer, msg, &iof->channel, 1, PMIX_IOF_CHANNEL);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* pack the data */
            PMIX_BFROPS_PACK(rc, req->peer, msg, iof->bo, 1, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                break;
            }
            /* send it to the requestor */
            PMIX_PTL_SEND_ONEWAY(rc, req->peer, msg, PMIX_PTL_TAG_IOF);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
            }
            /* remove it from the list since it has now been forwarded */
            pmix_list_remove_item(&pmix_server_globals.iof, &iof->super);
            PMIX_RELEASE(iof);
        }
    }
    if (notify) {
        /* ask the host to execute the request */
        if (PMIX_SUCCESS != (rc = pmix_host_server.iof_pull(cd->procs, cd->nprocs,
                                                            cd->info, cd->ninfo,
                                                            cd->channels,
                                                            cbfunc, cd))) {
            goto exit;
        }
    }
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}

static void stdcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t*)cbdata;

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
                        "recvd stdin IOF data from tool");

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
    if (PMIX_SUCCESS != (rc = pmix_host_server.push_stdin(&source, cd->procs, cd->nprocs,
                                                          cd->info, cd->ninfo, cd->bo,
                                                          stdcbfunc, cd))) {
        if (PMIX_OPERATION_SUCCEEDED != rc) {
            goto error;
        }
    }
    return rc;

  error:
    PMIX_RELEASE(cd);
    return rc;
}

static void grp_timeout(int sd, short args, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t ret, rc = PMIX_ERR_TIMEOUT;

    pmix_output_verbose(2, pmix_server_globals.fence_output,
                        "ALERT: grp construct timeout fired");

    /* send this requestor the reply */
    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        goto error;
    }
    /* setup the reply, starting with the returned status */
    PMIX_BFROPS_PACK(ret, cd->peer, reply, &rc, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != ret) {
        PMIX_ERROR_LOG(ret);
        PMIX_RELEASE(reply);
        goto error;
    }
    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "server:grp_timeout reply being sent to %s:%u",
                        cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
    PMIX_SERVER_QUEUE_REPLY(ret, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != ret) {
        PMIX_RELEASE(reply);
    }

  error:
    cd->event_active = false;
    /* remove it from the list */
    pmix_list_remove_item(&cd->trk->local_cbs, &cd->super);
    PMIX_RELEASE(cd);
}

static void _grpcbfunc(int sd, short argc, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t*)cbdata;
    pmix_server_trkr_t *trk = scd->tracker;
    pmix_server_caddy_t *cd;
    pmix_buffer_t *reply, xfer;
    pmix_status_t ret;
    size_t n, ctxid = SIZE_MAX;
    pmix_group_t *grp = (pmix_group_t*)trk->cbdata;
    pmix_byte_object_t *bo = NULL;
    pmix_nspace_caddy_t *nptr;
    pmix_list_t nslist;
    bool found;

    PMIX_ACQUIRE_OBJECT(scd);

    if (NULL == trk) {
        /* give them a release if they want it - this should
         * never happen, but protect against the possibility */
        if (NULL != scd->cbfunc.relfn) {
            scd->cbfunc.relfn(scd->cbdata);
        }
        PMIX_RELEASE(scd);
        return;
    }

    pmix_output_verbose(2, pmix_server_globals.connect_output,
                        "server:grpcbfunc processing WITH %d MEMBERS",
                        (int)pmix_list_get_size(&trk->local_cbs));

    /* if the timer is active, clear it */
    if (trk->event_active) {
        pmix_event_del(&trk->ev);
    }

    /* the tracker's "hybrid" field is used to indicate construct
     * vs destruct */
    if (trk->hybrid) {
        /* we destructed the group */
        if (NULL != grp) {
            pmix_list_remove_item(&pmix_server_globals.groups, &grp->super);
            PMIX_RELEASE(grp);
        }
    } else {
        /* see if this group was assigned a context ID or collected data */
        for (n=0; n < scd->ninfo; n++) {
            if (PMIX_CHECK_KEY(&scd->info[n], PMIX_GROUP_CONTEXT_ID)) {
                PMIX_VALUE_GET_NUMBER(ret, &scd->info[n].value, ctxid, size_t);
            } else if (PMIX_CHECK_KEY(&scd->info[n], PMIX_GROUP_ENDPT_DATA)) {
                bo = &scd->info[n].value.data.bo;
            }
        }
    }

    /* if data was returned, then we need to have the modex cbfunc
     * store it for us before releasing the group members */
    if (NULL != bo) {
        PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
        PMIX_CONSTRUCT(&nslist, pmix_list_t);
        /* Collect the nptr list with uniq GDS components of all local
         * participants. It does not allow multiple storing to the
         * same GDS if participants have mutual GDS. */
        PMIX_LIST_FOREACH(cd, &trk->local_cbs, pmix_server_caddy_t) {
            // see if we already have this nspace
            found = false;
            PMIX_LIST_FOREACH(nptr, &nslist, pmix_nspace_caddy_t) {
                if (0 == strcmp(nptr->ns->compat.gds->name,
                            cd->peer->nptr->compat.gds->name)) {
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

        PMIX_LIST_FOREACH(nptr, &nslist, pmix_nspace_caddy_t) {
            PMIX_LOAD_BUFFER(pmix_globals.mypeer, &xfer, bo->bytes, bo->size);
            PMIX_GDS_STORE_MODEX(ret, nptr->ns, &xfer, trk);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                break;
            }
        }
    }

    /* loop across all procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH(cd, &trk->local_cbs, pmix_server_caddy_t) {
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
        if (!trk->hybrid) {
            /* if a ctxid was provided, pass it along */
            PMIX_BFROPS_PACK(ret, cd->peer, reply, &ctxid, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                PMIX_RELEASE(reply);
                break;
            }
        }
        pmix_output_verbose(2, pmix_server_globals.connect_output,
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

    /* we are done */
    if (NULL != scd->cbfunc.relfn) {
        scd->cbfunc.relfn(scd->cbdata);
    }
    PMIX_RELEASE(scd);
}


static void grpcbfunc(pmix_status_t status,
                      pmix_info_t *info, size_t ninfo,
                      void *cbdata,
                      pmix_release_cbfunc_t relfn,
                      void *relcbd)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t*)cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_server_globals.connect_output,
                        "server:grpcbfunc called with %d info", (int)ninfo);

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

/* we are being called from the PMIx server's switchyard function,
 * which means we are in an event and can access global data */
pmix_status_t pmix_server_grpconstruct(pmix_server_caddy_t *cd,
                                       pmix_buffer_t *buf)
{
    pmix_peer_t *peer = (pmix_peer_t*)cd->peer;
    pmix_peer_t *pr;
    int32_t cnt, m;
    pmix_status_t rc;
    char *grpid;
    pmix_proc_t *procs;
    pmix_group_t *grp, *pgrp;
    pmix_info_t *info = NULL, *iptr;
    size_t n, ninfo, nprocs, n2;
    pmix_server_trkr_t *trk;
    struct timeval tv = {0, 0};
    bool need_cxtid = false;
    bool match, force_local = false;
    bool embed_barrier = false;
    bool barrier_directive_included = false;
    pmix_buffer_t bucket;
    pmix_byte_object_t bo;
    pmix_list_t mbrs;
    pmix_namelist_t *nm;
    bool expanded = false;

    pmix_output_verbose(2, pmix_server_globals.connect_output,
                        "recvd grpconstruct cmd");

    /* unpack the group ID */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &grpid, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* see if we already have this group */
    grp = NULL;
    PMIX_LIST_FOREACH(pgrp, &pmix_server_globals.groups, pmix_group_t) {
        if (0 == strcmp(grpid, pgrp->grpid)) {
            grp = pgrp;
            break;
        }
    }
    if (NULL == grp) {
        /* create a new entry */
        grp = PMIX_NEW(pmix_group_t);
        if (NULL == grp) {
            rc = PMIX_ERR_NOMEM;
            goto error;
        }
        grp->grpid = grpid;
        pmix_list_append(&pmix_server_globals.groups, &grp->super);
    } else {
        free(grpid);
    }

    /* unpack the number of procs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &nprocs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 == nprocs) {
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
    if (NULL == grp->members) {
        /* see if they used a local proc or local peer
         * wildcard - if they did, then we need to expand
         * it here */
        PMIX_CONSTRUCT(&mbrs, pmix_list_t);
        for (n=0; n < nprocs; n++) {
            if (PMIX_RANK_LOCAL_PEERS == procs[n].rank) {
                expanded = true;
                /* expand to all local procs in this nspace */
                for (m=0; m < pmix_server_globals.clients.size; m++) {
                    if (NULL == (pr = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, m))) {
                        continue;
                    }
                    if (PMIX_CHECK_NSPACE(procs[n].nspace, pr->info->pname.nspace)) {
                        nm = PMIX_NEW(pmix_namelist_t);
                        nm->pname = &pr->info->pname;
                        pmix_list_append(&mbrs, &nm->super);
                    }
                }
            } else if (PMIX_RANK_LOCAL_NODE == procs[n].rank) {
                expanded = true;
                /* add in all procs on the node */
                for (m=0; m < pmix_server_globals.clients.size; m++) {
                    if (NULL == (pr = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, m))) {
                        continue;
                    }
                    nm = PMIX_NEW(pmix_namelist_t);
                    nm->pname = &pr->info->pname;
                    pmix_list_append(&mbrs, &nm->super);
                }
            } else {
                nm = PMIX_NEW(pmix_namelist_t);
                /* have to duplicate the name here */
                nm->pname = (pmix_name_t*)malloc(sizeof(pmix_name_t));
                nm->pname->nspace = strdup(procs[n].nspace);
                nm->pname->rank = procs[n].rank;
                pmix_list_append(&mbrs, &nm->super);
            }
        }
        if (expanded) {
            PMIX_PROC_FREE(procs, nprocs);
            nprocs = pmix_list_get_size(&mbrs);
            PMIX_PROC_CREATE(procs, nprocs);
            n=0;
            while (NULL != (nm = (pmix_namelist_t*)pmix_list_remove_first(&mbrs))) {
                PMIX_LOAD_PROCID(&procs[n], nm->pname->nspace, nm->pname->rank);
                PMIX_RELEASE(nm);
            }
            PMIX_DESTRUCT(&mbrs);
        }
        grp->members = procs;
        grp->nmbrs = nprocs;
    } else {
        PMIX_PROC_FREE(procs, nprocs);
    }

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(grp->grpid, grp->members, grp->nmbrs, PMIX_GROUP_CONSTRUCT_CMD))) {
        /* If no tracker was found - create and initialize it once */
        if (NULL == (trk = new_tracker(grp->grpid, grp->members, grp->nmbrs, PMIX_GROUP_CONSTRUCT_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            rc = PMIX_ERROR;
            goto error;
        }
        /* group members must have access to all endpoint info
         * upon completion of the construct operation */
        trk->collect_type = PMIX_COLLECT_YES;
        /* mark as being a construct operation */
        trk->hybrid = false;
        /* pass along the grp object */
        trk->cbdata = grp;
        /* we only save the info structs from the first caller
         * who provides them - it is a user error to provide
         * different values from different participants */
        trk->info = info;
        trk->ninfo = ninfo;
        /* see if we are to enforce a timeout or if they want
         * a context ID created - we don't internally care
         * about any other directives */
        for (n=0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
                tv.tv_sec = info[n].value.data.uint32;
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_ASSIGN_CONTEXT_ID)) {
                need_cxtid = PMIX_INFO_TRUE(&info[n]);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_LOCAL_ONLY)) {
                force_local = PMIX_INFO_TRUE(&info[n]);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_EMBED_BARRIER)) {
                embed_barrier = PMIX_INFO_TRUE(&info[n]);
                barrier_directive_included = true;
            }
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
            for (n=0; n < grp->nmbrs; n++) {
                /* if this entry references the local procs, then
                 * we can skip it */
                if (PMIX_RANK_LOCAL_PEERS == grp->members[n].rank ||
                    PMIX_RANK_LOCAL_NODE == grp->members[n].rank) {
                    continue;
                }
                /* see if it references a specific local proc */
                match = false;
                for (m=0; m < pmix_server_globals.clients.size; m++) {
                    if (NULL == (pr = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, m))) {
                        continue;
                    }
                    if (PMIX_CHECK_PROCID(&grp->members[n], &pr->info->pname)) {
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
        /* cleanup */
        PMIX_INFO_FREE(info, ninfo);
        info = NULL;
    }

    /* add this contributor to the tracker so they get
     * notified when we are done */
    pmix_list_append(&trk->local_cbs, &cd->super);

    /* if a timeout was specified, set it */
    if (0 < tv.tv_sec) {
        pmix_event_evtimer_set(pmix_globals.evbase, &trk->ev,
                               grp_timeout, trk);
        pmix_event_evtimer_add(&trk->ev, &tv);
        trk->event_active = true;
    }

    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the barrier
     * across all participants has been completed */
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        pmix_output_verbose(2, pmix_server_globals.base_output,
                            "local group op complete with %d procs", (int)trk->npcs);

        if (trk->local) {
            /* nothing further needs to be done - we have
             * created the local group. let the grpcbfunc
             * threadshift the result */
            grpcbfunc(PMIX_SUCCESS, NULL, 0, trk, NULL, NULL);
            return PMIX_SUCCESS;
        }

        /* check if our host supports group operations */
        if (NULL == pmix_host_server.group) {
            /* remove the tracker from the list */
            pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
            PMIX_RELEASE(trk);
            return PMIX_ERR_NOT_SUPPORTED;
        }

        /* if they direct us to not embed a barrier, then we won't gather
         * the data for distribution */
        if (!barrier_directive_included ||
            (barrier_directive_included && embed_barrier)) {
            /* collect any remote contributions provided by group members */
            PMIX_CONSTRUCT(&bucket, pmix_buffer_t);
            rc = _collect_data(trk, &bucket);
            if (PMIX_SUCCESS != rc) {
                if (trk->event_active) {
                    pmix_event_del(&trk->ev);
                }
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
            n2 = trk->ninfo + 1;
            PMIX_INFO_CREATE(iptr, n2);
            for (n=0; n < trk->ninfo; n++) {
                PMIX_INFO_XFER(&iptr[n], &trk->info[n]);
            }
            PMIX_INFO_LOAD(&iptr[ninfo], PMIX_GROUP_ENDPT_DATA, &bo, PMIX_BYTE_OBJECT);
            PMIX_BYTE_OBJECT_DESTRUCT(&bo);
            PMIX_INFO_FREE(trk->info, trk->ninfo);
            trk->info = iptr;
            trk->ninfo = n2;
        }
        rc = pmix_host_server.group(PMIX_GROUP_CONSTRUCT, grp->grpid,
                                    trk->pcs, trk->npcs,
                                    trk->info, trk->ninfo,
                                    grpcbfunc, trk);
        if (PMIX_SUCCESS != rc) {
            if (trk->event_active) {
                pmix_event_del(&trk->ev);
            }
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
    }

    return PMIX_SUCCESS;

  error:
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    return rc;
}

/* we are being called from the PMIx server's switchyard function,
 * which means we are in an event and can access global data */
pmix_status_t pmix_server_grpdestruct(pmix_server_caddy_t *cd,
                                      pmix_buffer_t *buf)
{
    pmix_peer_t *peer = (pmix_peer_t*)cd->peer;
    int32_t cnt;
    pmix_status_t rc;
    char *grpid;
    pmix_info_t *info = NULL;
    size_t n, ninfo;
    pmix_server_trkr_t *trk;
    pmix_group_t *grp, *pgrp;
    struct timeval tv = {0, 0};

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "recvd grpdestruct cmd");

    if (NULL == pmix_host_server.group) {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the group ID */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &grpid, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }

    /* find this group in our list */
    grp = NULL;
    PMIX_LIST_FOREACH(pgrp, &pmix_server_globals.groups, pmix_group_t) {
        if (0 == strcmp(grpid, pgrp->grpid)) {
            grp = pgrp;
            break;
        }
    }
    free(grpid);

    /* if not found, then this is an error - we cannot
     * destruct a group we don't know about */
    if (NULL == grp) {
        rc = PMIX_ERR_NOT_FOUND;
        goto error;
    }

    /* unpack the number of directives */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto error;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto error;
        }
        /* see if we are to enforce a timeout - we don't internally care
         * about any other directives */
        for (n=0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
                tv.tv_sec = info[n].value.data.uint32;
                break;
            }
        }
    }

    /* find/create the local tracker for this operation */
    if (NULL == (trk = get_tracker(grp->grpid, grp->members, grp->nmbrs, PMIX_GROUP_DESTRUCT_CMD))) {
        /* If no tracker was found - create and initialize it once */
        if (NULL == (trk = new_tracker(grp->grpid, grp->members, grp->nmbrs, PMIX_GROUP_DESTRUCT_CMD))) {
            /* only if a bozo error occurs */
            PMIX_ERROR_LOG(PMIX_ERROR);
            rc = PMIX_ERROR;
            goto error;
        }
        trk->collect_type = PMIX_COLLECT_NO;
        /* mark as being a destruct operation */
        trk->hybrid = true;
        /* pass along the group object */
        trk->cbdata = grp;
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
        pmix_event_evtimer_set(pmix_globals.evbase, &trk->ev,
                               grp_timeout, trk);
        pmix_event_evtimer_add(&trk->ev, &tv);
        trk->event_active = true;
    }

    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the barrier
     * across all participants has been completed */
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        pmix_output_verbose(2, pmix_server_globals.base_output,
                            "local group op complete %d", (int)trk->nlocal);

        rc = pmix_host_server.group(PMIX_GROUP_DESTRUCT, grp->grpid,
                                    grp->members, grp->nmbrs,
                                    trk->info, trk->ninfo,
                                    grpcbfunc, trk);
        if (PMIX_SUCCESS != rc) {
            if (trk->event_active) {
                pmix_event_del(&trk->ev);
            }
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
    }

    return PMIX_SUCCESS;

  error:
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    return rc;
}

/*****    INSTANCE SERVER LIBRARY CLASSES    *****/
static void tcon(pmix_server_trkr_t *t)
{
    t->event_active = false;
    t->host_called = false;
    t->local = true;
    t->id = NULL;
    memset(t->pname.nspace, 0, PMIX_MAX_NSLEN+1);
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
    PMIX_DESTRUCT(&t->nslist);
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
    p->keys = NULL;
    p->channels = PMIX_FWD_NO_CHANNELS;
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
    if (NULL != p->apps) {
        PMIX_APP_FREE(p->apps, p->napps);
    }
    if (NULL != p->bo) {
        PMIX_BYTE_OBJECT_FREE(p->bo, p->nbo);
    }
    PMIX_DESTRUCT_LOCK(&p->lock);
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_setup_caddy_t,
                                pmix_object_t,
                                scadcon, scaddes);

static void ncon(pmix_notify_caddy_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
#if defined(__linux__) && PMIX_HAVE_CLOCK_GETTIME
    struct timespec tp;
    (void) clock_gettime(CLOCK_MONOTONIC, &tp);
    p->ts = tp.tv_sec;
#else
    /* Fall back to gettimeofday() if we have nothing else */
    struct timeval tv;
    gettimeofday(&tv, NULL);
    p->ts = tv.tv_sec;
#endif
    p->room = -1;
    memset(p->source.nspace, 0, PMIX_MAX_NSLEN+1);
    p->source.rank = PMIX_RANK_UNDEF;
    p->range = PMIX_RANGE_UNDEF;
    p->targets = NULL;
    p->ntargets = 0;
    p->nleft = SIZE_MAX;
    p->affected = NULL;
    p->naffected = 0;
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
    PMIX_PROC_FREE(p->affected, p->naffected);
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

static void grcon(pmix_group_t *p)
{
    p->grpid = NULL;
    p->members = NULL;
    p->nmbrs = 0;
}
static void grdes(pmix_group_t *p)
{
    if (NULL != p->grpid) {
        free(p->grpid);
    }
    if (NULL != p->members) {
        PMIX_PROC_FREE(p->members, p->nmbrs);
    }
}
PMIX_CLASS_INSTANCE(pmix_group_t,
                    pmix_list_item_t,
                    grcon, grdes);

PMIX_CLASS_INSTANCE(pmix_group_caddy_t,
                    pmix_list_item_t,
                    NULL, NULL);

static void iocon(pmix_iof_cache_t *p)
{
    p->bo = NULL;
}
static void iodes(pmix_iof_cache_t *p)
{
    PMIX_BYTE_OBJECT_FREE(p->bo, 1);  // macro protects against NULL
}
PMIX_CLASS_INSTANCE(pmix_iof_cache_t,
                    pmix_list_item_t,
                    iocon, iodes);
