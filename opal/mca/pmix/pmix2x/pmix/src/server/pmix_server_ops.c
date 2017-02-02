/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
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
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"

#include "pmix_server_ops.h"

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
#include "src/dstore/pmix_dstore.h"
#endif /* PMIX_ENABLE_DSTORE */


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

    pmix_output_verbose(2, pmix_globals.debug_output, "recvd ABORT");

    /* unpack the status */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &status, &cnt, PMIX_STATUS))) {
        return rc;
    }
    /* unpack the message */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &msg, &cnt, PMIX_STRING))) {
        return rc;
    }
    /* unpack the number of procs */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nprocs, &cnt, PMIX_SIZE))) {
        return rc;
    }

    /* unpack any provided procs - these are the procs the caller
     * wants aborted */
    if (0 < nprocs) {
        PMIX_PROC_CREATE(procs, nprocs);
        cnt = nprocs;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, procs, &cnt, PMIX_PROC))) {
            return rc;
        }
    }

    /* let the local host's server execute it */
    if (NULL != pmix_host_server.abort) {
        (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
        proc.rank = peer->info->rank;
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
    pmix_buffer_t *b2;
    pmix_kval_t *kp;
    pmix_scope_t scope;
    pmix_hash_table_t *ht;
    pmix_nspace_t *nptr;
    pmix_rank_info_t *info;
    pmix_dmdx_remote_t *dcd, *dcdnext;
    pmix_buffer_t *pbkt;
    pmix_value_t *val;
    char *data;
    size_t sz;

    /* shorthand */
    info = peer->info;
    nptr = info->nptr;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s:%d EXECUTE COMMIT FOR %s:%d",
                        pmix_globals.myid.nspace,
                        pmix_globals.myid.rank,
                        nptr->nspace, info->rank);

    /* this buffer will contain one or more buffers, each
     * representing a different scope. These need to be locally
     * stored separately so we can provide required data based
     * on the requestor's location */
    cnt = 1;
    while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(buf, &scope, &cnt, PMIX_SCOPE))) {
        if (PMIX_LOCAL == scope) {
            ht = &nptr->server->mylocal;
        } else if (PMIX_REMOTE == scope) {
            ht = &nptr->server->myremote;
        } else {
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            rc = PMIX_ERR_BAD_PARAM;
            return rc;
        }
        /* unpack and store the blob */
        cnt = 1;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &b2, &cnt, PMIX_BUFFER))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }

        /* create the new data storage */
        kp = PMIX_NEW(pmix_kval_t);
        kp->key = strdup("modex");
        PMIX_VALUE_CREATE(kp->value, 1);
        kp->value->type = PMIX_BYTE_OBJECT;

#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
        /* The local buffer must go directly the dstore */
        if( PMIX_LOCAL == scope ){
            /* need to deposit this in the dstore now */
            PMIX_UNLOAD_BUFFER(b2, kp->value->data.bo.bytes, kp->value->data.bo.size);
            if (PMIX_SUCCESS != (rc = pmix_dstore_store(nptr->nspace, info->rank, kp))) {
                PMIX_ERROR_LOG(rc);
            }
            PMIX_RELEASE(kp);

            kp = PMIX_NEW(pmix_kval_t);
            kp->key = strdup("modex");
            PMIX_VALUE_CREATE(kp->value, 1);
            kp->value->type = PMIX_BYTE_OBJECT;
        }
#endif /* PMIX_ENABLE_DSTORE */

        /* see if we already have info for this proc */
        if (PMIX_SUCCESS == pmix_hash_fetch(ht, info->rank, "modex", &val) && NULL != val) {
            /* get space for the new new data blob */
            kp->value->data.bo.bytes = (char*)malloc(b2->bytes_used + val->data.bo.size);
            memcpy(kp->value->data.bo.bytes, val->data.bo.bytes, val->data.bo.size);
            memcpy(kp->value->data.bo.bytes+val->data.bo.size, b2->base_ptr, b2->bytes_used);
            kp->value->data.bo.size = val->data.bo.size + b2->bytes_used;
            /* release the storage */
            PMIX_VALUE_FREE(val, 1);
        } else {
            PMIX_UNLOAD_BUFFER(b2, kp->value->data.bo.bytes, kp->value->data.bo.size);
        }

        /* store it in the appropriate hash */
        if (PMIX_SUCCESS != (rc = pmix_hash_store(ht, info->rank, kp))) {
            PMIX_ERROR_LOG(rc);
        }
        /* maintain the accounting */
        PMIX_RELEASE(kp);
        PMIX_RELEASE(b2);

        cnt = 1;
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    rc = PMIX_SUCCESS;
    /* mark us as having successfully received a blob from this proc */
    info->modex_recvd = true;

    /* see if anyone remote is waiting on this data - could be more than one */
    PMIX_LIST_FOREACH_SAFE(dcd, dcdnext, &pmix_server_globals.remote_pnd, pmix_dmdx_remote_t) {
        if (0 != strncmp(dcd->cd->proc.nspace, nptr->nspace, PMIX_MAX_NSLEN)) {
            continue;
        }
        if (dcd->cd->proc.rank == info->rank) {
           /* we can now fulfill this request - collect the
             * remote/global data from this proc */
            pbkt = PMIX_NEW(pmix_buffer_t);
            /* get any remote contribution - note that there
             * may not be a contribution */
            if (PMIX_SUCCESS == pmix_hash_fetch(&nptr->server->myremote, info->rank, "modex", &val) &&
                NULL != val) {
                PMIX_LOAD_BUFFER(pbkt, val->data.bo.bytes, val->data.bo.size);
                PMIX_VALUE_RELEASE(val);
            }
            PMIX_UNLOAD_BUFFER(pbkt, data, sz);
            PMIX_RELEASE(pbkt);
            /* execute the callback */
            dcd->cd->cbfunc(PMIX_SUCCESS, data, sz, dcd->cd->cbdata);
            if (NULL != data) {
                free(data);
            }
            /* we have finished this request */
            pmix_list_remove_item(&pmix_server_globals.remote_pnd, &dcd->super);
            PMIX_RELEASE(dcd);
        }
    }
    /* see if anyone local is waiting on this data- could be more than one */
    return pmix_pending_resolve(nptr, info->rank, PMIX_SUCCESS, NULL);
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

    pmix_output_verbose(5, pmix_globals.debug_output,
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
    pmix_rank_info_t *iptr, *info;
    size_t i;
    bool all_def;
    pmix_nspace_t *nptr, *ns;

    pmix_output_verbose(5, pmix_globals.debug_output,
                        "new_tracker called with %d procs", (int)nprocs);

    /* bozo check - should never happen outside of programmer error */
    if (NULL == procs) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }

    assert( NULL == get_tracker(procs, nprocs, type) );

    pmix_output_verbose(5, pmix_globals.debug_output,
                        "adding new tracker with %d procs", (int)nprocs);

    /* get here if this tracker is new - create it */
    trk = PMIX_NEW(pmix_server_trkr_t);

    /* copy the procs */
    PMIX_PROC_CREATE(trk->pcs, nprocs);
    trk->npcs = nprocs;
    trk->type = type;

    all_def = true;
    for (i=0; i < nprocs; i++) {
        (void)strncpy(trk->pcs[i].nspace, procs[i].nspace, PMIX_MAX_NSLEN);
        trk->pcs[i].rank = procs[i].rank;
        /* is this nspace known to us? */
        nptr = NULL;
        PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
            if (0 == strcmp(procs[i].nspace, ns->nspace)) {
                nptr = ns;
                break;
            }
        }
        if (NULL == nptr) {
            /* cannot be a local proc */
            pmix_output_verbose(5, pmix_globals.debug_output,
                                "new_tracker: unknown nspace %s",
                                procs[i].nspace);
            continue;
        }
        /* have all the clients for this nspace been defined? */
        if (!nptr->server->all_registered) {
            /* nope, so no point in going further on this one - we'll
             * process it once all the procs are known */
            all_def = false;
            pmix_output_verbose(5, pmix_globals.debug_output,
                                "new_tracker: all clients not registered nspace %s",
                                procs[i].nspace);
            continue;
        }
        /* is this one of my local ranks? */
        PMIX_LIST_FOREACH(info, &nptr->server->ranks, pmix_rank_info_t) {
            if (procs[i].rank == info->rank ||
                PMIX_RANK_WILDCARD == procs[i].rank) {
                pmix_output_verbose(5, pmix_globals.debug_output,
                                    "adding local proc %s.%d to tracker",
                                    info->nptr->nspace, info->rank);
                /* add a tracker for this proc - don't need more than
                 * the nspace pointer and rank */
                iptr = PMIX_NEW(pmix_rank_info_t);
                PMIX_RETAIN(info->nptr);
                iptr->nptr = info->nptr;
                iptr->rank = info->rank;
                pmix_list_append(&trk->ranks, &iptr->super);
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

pmix_status_t pmix_server_fence(pmix_server_caddy_t *cd,
                                pmix_buffer_t *buf,
                                pmix_modex_cbfunc_t modexcbfunc,
                                pmix_op_cbfunc_t opcbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    size_t nprocs;
    pmix_proc_t *procs=NULL;
    bool collect_data = false;
    pmix_server_trkr_t *trk;
    char *data = NULL;
    size_t sz = 0;
    pmix_buffer_t bucket, xfer;
    pmix_rank_info_t *rkinfo;
    pmix_value_t *val;
    pmix_info_t *info = NULL;
    size_t ninfo=0, n;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd FENCE");

    if (NULL == pmix_host_server.fence_nb) {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the number of procs */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nprocs, &cnt, PMIX_SIZE))) {
        return rc;
    }
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd fence with %d procs", (int)nprocs);
    /* there must be at least one as the client has to at least provide
     * their own namespace */
    if (nprocs < 1) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* create space for the procs */
    PMIX_PROC_CREATE(procs, nprocs);
    /* unpack the procs */
    cnt = nprocs;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, procs, &cnt, PMIX_PROC))) {
        goto cleanup;
    }

    /* unpack the number of provided info structs */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        return rc;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        /* unpack the info */
        cnt = ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            goto cleanup;
        }
        /* see if we are to collect data - we don't internally care
         * about any other directives */
        for (n=0; n < ninfo; n++) {
            if (0 == strcmp(info[n].key, PMIX_COLLECT_DATA)) {
                collect_data = true;
                break;
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
    PMIX_RETAIN(cd);
    pmix_list_append(&trk->local_cbs, &cd->super);
    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the barrier
     * across all participants has been completed */
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "fence complete");
        /* if the user asked us to collect data, then we have
         * to provide any locally collected data to the host
         * server so they can circulate it - only take data
         * from the specified procs as not everyone is necessarily
         * participating! And only take data intended for remote
         * distribution */

        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);

        assert( PMIX_COLLECT_MAX < UCHAR_MAX );
        unsigned char tmp = (unsigned char)trk->collect_type;
        pmix_bfrop.pack(&bucket, &tmp, 1, PMIX_BYTE);

        if (PMIX_COLLECT_YES == trk->collect_type) {
            pmix_buffer_t databuf;
            PMIX_CONSTRUCT(&databuf, pmix_buffer_t);
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "fence - assembling data");
            PMIX_LIST_FOREACH(rkinfo, &trk->ranks, pmix_rank_info_t) {
                pmix_buffer_t rankbuf;
                PMIX_CONSTRUCT(&rankbuf, pmix_buffer_t);
                /* get any remote contribution - note that there
                 * may not be a contribution */
                if (PMIX_SUCCESS == pmix_hash_fetch(&rkinfo->nptr->server->myremote, rkinfo->rank, "modex", &val) &&
                    NULL != val) {
                    /* pack the proc so we know the source */
                    char *foobar = rkinfo->nptr->nspace;
                    pmix_bfrop.pack(&rankbuf, &foobar, 1, PMIX_STRING);
                    pmix_bfrop.pack(&rankbuf, &rkinfo->rank, 1, PMIX_PROC_RANK);
                    PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
                    PMIX_LOAD_BUFFER(&xfer, val->data.bo.bytes, val->data.bo.size);
                    PMIX_VALUE_RELEASE(val);
                    pmix_buffer_t *pxfer = &xfer;
                    pmix_bfrop.pack(&rankbuf, &pxfer, 1, PMIX_BUFFER);
                    PMIX_DESTRUCT(&xfer);
                    /* now pack this proc's contribution into the bucket */
                    pmix_buffer_t *pdatabuf = &rankbuf;
                    pmix_bfrop.pack(&databuf, &pdatabuf, 1, PMIX_BUFFER);
                }
                PMIX_DESTRUCT(&rankbuf);
            }
            // TODO: we have multiple data movings while only one is actually need
            pmix_buffer_t *pbkt = &databuf;
            pmix_bfrop.pack(&bucket, &pbkt, 1, PMIX_BUFFER);
            PMIX_DESTRUCT(&databuf);
        }

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

pmix_status_t pmix_server_publish(pmix_peer_t *peer,
                                  pmix_buffer_t *buf,
                                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    int32_t cnt;
    size_t ninfo, einfo;
    pmix_info_t *info = NULL;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd PUBLISH");

    if (NULL == pmix_host_server.publish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt=1;
    if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &uid, &cnt, PMIX_UINT32))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of info objects */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* we will be adding one for the user id */
    einfo = ninfo + 1;
    PMIX_INFO_CREATE(info, einfo);
    /* unpack the array of info objects */
    if (0 < ninfo) {
        cnt=ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    (void)strncpy(info[einfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    info[einfo-1].value.type = PMIX_UINT32;
    info[einfo-1].value.data.uint32 = uid;

    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->rank;
    rc = pmix_host_server.publish(&proc, info, einfo, cbfunc, cbdata);

 cleanup:
    PMIX_INFO_FREE(info, einfo);
    return rc;
}

pmix_status_t pmix_server_lookup(pmix_peer_t *peer,
                                 pmix_buffer_t *buf,
                                 pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    size_t nkeys, i;
    char **keys=NULL, *sptr;
    pmix_info_t *info = NULL;
    size_t ninfo, einfo;
    pmix_proc_t proc;
    uint32_t uid;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd LOOKUP");

    if (NULL == pmix_host_server.lookup) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt=1;
    if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &uid, &cnt, PMIX_UINT32))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of keys */
    cnt=1;
    if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nkeys, &cnt, PMIX_SIZE))) {
         PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of keys */
    for (i=0; i < nkeys; i++) {
        cnt=1;
        if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &sptr, &cnt, PMIX_STRING))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        pmix_argv_append_nosize(&keys, sptr);
        free(sptr);
    }
    /* unpack the number of info objects */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* we will be adding one for the user id */
    einfo = ninfo + 1;
    PMIX_INFO_CREATE(info, einfo);
    /* unpack the array of info objects */
    if (0 < ninfo) {
        cnt=ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    (void)strncpy(info[einfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    info[einfo-1].value.type = PMIX_UINT32;
    info[einfo-1].value.data.uint32 = uid;

    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->rank;
    rc = pmix_host_server.lookup(&proc, keys, info, einfo, cbfunc, cbdata);

 cleanup:
    PMIX_INFO_FREE(info, einfo);
    pmix_argv_free(keys);
    return rc;
}

pmix_status_t pmix_server_unpublish(pmix_peer_t *peer,
                                    pmix_buffer_t *buf,
                                    pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    size_t i, nkeys, ninfo, einfo;
    char **keys=NULL, *sptr;
    pmix_proc_t proc;
    uint32_t uid;
    pmix_info_t *info;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd UNPUBLISH");

    if (NULL == pmix_host_server.unpublish) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the effective user id */
    cnt=1;
    if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &uid, &cnt, PMIX_UINT32))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the number of keys */
    cnt=1;
    if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nkeys, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the keys */
    for (i=0; i < nkeys; i++) {
        cnt=1;
        if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &sptr, &cnt, PMIX_STRING))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        pmix_argv_append_nosize(&keys, sptr);
        free(sptr);
    }
    /* unpack the number of info objects */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* we will be adding one for the user id */
    einfo = ninfo + 1;
    PMIX_INFO_CREATE(info, einfo);
    /* unpack the array of info objects */
    if (0 < ninfo) {
        cnt=ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    (void)strncpy(info[einfo-1].key, PMIX_USERID, PMIX_MAX_KEYLEN);
    info[einfo-1].value.type = PMIX_UINT32;
    info[einfo-1].value.data.uint32 = uid;

    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->rank;
    rc = pmix_host_server.unpublish(&proc, keys, info, einfo, cbfunc, cbdata);

 cleanup:
    pmix_argv_free(keys);
    return rc;
}

pmix_status_t pmix_server_spawn(pmix_peer_t *peer,
                                pmix_buffer_t *buf,
                                pmix_spawn_cbfunc_t cbfunc,
                                void *cbdata)
{
    int32_t cnt;
    size_t napps, ninfo;
    pmix_info_t *info=NULL;
    pmix_app_t *apps=NULL;
    pmix_status_t rc;
    pmix_proc_t proc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd SPAWN");

    if (NULL == pmix_host_server.spawn) {
        PMIX_ERROR_LOG(PMIX_ERR_NOT_SUPPORTED);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the number of job-level directives */
    cnt=1;
    if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of directives */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt=ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* unpack the number of apps */
    cnt=1;
    if  (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &napps, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of apps */
    if (0 < napps) {
        PMIX_APP_CREATE(apps, napps);
        cnt=napps;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, apps, &cnt, PMIX_APP))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    /* call the local server */
    (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->rank;
    rc = pmix_host_server.spawn(&proc, info, ninfo, apps, napps, cbfunc, cbdata);

 cleanup:
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    if (NULL != apps) {
        PMIX_APP_FREE(apps, napps);
    }
    return rc;
}

pmix_status_t pmix_server_connect(pmix_server_caddy_t *cd,
                                  pmix_buffer_t *buf, bool disconnect,
                                  pmix_op_cbfunc_t cbfunc)
{
    int32_t cnt;
    pmix_status_t rc;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_server_trkr_t *trk;
    pmix_info_t *info = NULL;
    size_t ninfo=0;
    pmix_cmd_t type = PMIX_CONNECTNB_CMD;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd CONNECT from peer %s:%d",
                        cd->peer->info->nptr->nspace, cd->peer->info->rank);

    if ((disconnect && NULL == pmix_host_server.disconnect) ||
        (!disconnect && NULL == pmix_host_server.connect)) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* unpack the number of procs */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &nprocs, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* there must be at least one proc - we do not allow the client
     * to send us NULL proc as the server has no idea what to do
     * with that situation. Instead, the client should at least send
     * us their own namespace for the use-case where the connection
     * spans all procs in that namespace */
    if (nprocs < 1) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }

    /* unpack the procs */
    PMIX_PROC_CREATE(procs, nprocs);
    cnt = nprocs;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, procs, &cnt, PMIX_PROC))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* unpack the number of provided info structs */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        return rc;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        /* unpack the info */
        cnt = ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            goto cleanup;
        }
    }

    /* find/create the local tracker for this operation */
    if (disconnect) {
        type = PMIX_DISCONNECTNB_CMD;
    }
    if (NULL == (trk = get_tracker(procs, nprocs, type))) {
        if (NULL == (trk = new_tracker(procs, nprocs, type))) {
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

    /* add this contributor to the tracker so they get
     * notified when we are done */
    PMIX_RETAIN(cd);
    pmix_list_append(&trk->local_cbs, &cd->super);
    /* if all local contributions have been received,
     * let the local host's server know that we are at the
     * "fence" point - they will callback once the [dis]connect
     * across all participants has been completed */
    if (trk->def_complete &&
        pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
        if (disconnect) {
            rc = pmix_host_server.disconnect(procs, nprocs, info, ninfo, cbfunc, trk);
        } else {
            rc = pmix_host_server.connect(procs, nprocs, info, ninfo, cbfunc, trk);
        }
    } else {
        rc = PMIX_SUCCESS;
    }

 cleanup:
    PMIX_PROC_FREE(procs, nprocs);
    PMIX_INFO_FREE(info, ninfo);
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
    int i;
    bool enviro_events = false;
    bool found, matched;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd register events");

    /* unpack the number of codes */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ncodes, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of codes */
    if (0 < ncodes) {
        codes = (pmix_status_t*)malloc(ncodes * sizeof(pmix_status_t));
        cnt=ncodes;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, codes, &cnt, PMIX_STATUS))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* unpack the number of info objects */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* unpack the array of info objects */
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt=ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* see if they asked for enviro events */
    for (n=0; n < ninfo; n++) {
        if (0 == strcmp(info[n].key, PMIX_EVENT_ENVIRO_LEVEL)) {
            if (PMIX_UNDEF == info[n].value.type ||
                (PMIX_BOOL == info[n].value.type && info[n].value.data.flag)) {
                enviro_events = true;
            }
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
                PMIX_RETAIN(peer);
                prev->peer = peer;
                prev->enviro_events = enviro_events;
                pmix_list_append(&reginfo->peers, &prev->super);
            }
        } else {
            /* if we get here, then we didn't find an existing registration for this code */
            reginfo = PMIX_NEW(pmix_regevents_info_t);
            if (NULL == codes) {
                reginfo->code = PMIX_MAX_ERR_CONSTANT;
            } else {
                reginfo->code = codes[k];
            }
            pmix_list_append(&pmix_server_globals.events, &reginfo->super);
            prev = PMIX_NEW(pmix_peer_events_info_t);
            PMIX_RETAIN(peer);
            prev->peer = peer;
            prev->enviro_events = enviro_events;
            pmix_list_append(&reginfo->peers, &prev->super);
        }
        ++k;
    } while (k < ncodes);

    /* if they asked for enviro events, call the local server */
    if (enviro_events) {
        if (PMIX_SUCCESS != (rc = pmix_host_server.register_events(codes, ncodes, info, ninfo, cbfunc, cbdata))) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                 "server register events: host server reg events returned rc =%d", rc);
        } else {
            goto check;
        }
    }

  cleanup:
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "server register events: ninfo =%lu rc =%d", ninfo, rc);
    /* be sure to execute the callback */
    if (NULL != cbfunc) {
        cbfunc(rc, cbdata);
    }
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    if (PMIX_SUCCESS != rc) {
        if (!enviro_events) {
            if (NULL != codes) {
                free(codes);
            }
        }
        return rc;
    }

  check:
    /* check if any matching notifications have been cached */
    for (i=0; i < pmix_server_globals.notifications.size; i++) {
        if (NULL == (cd = (pmix_notify_caddy_t*)pmix_ring_buffer_poke(&pmix_server_globals.notifications, i))) {
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
                    if (0 != strncmp(peer->info->nptr->nspace, cd->targets[n].nspace, PMIX_MAX_NSLEN)) {
                        continue;
                    }
                    /* if the source of the event is the same peer just registered, then ignore it
                     * as the event notification system will have already locally
                     * processed it */
                    if (0 == strncmp(peer->info->nptr->nspace, cd->source.nspace, PMIX_MAX_NSLEN) &&
                        peer->info->rank == cd->source.rank) {
                        continue;
                    }
                    if (PMIX_RANK_WILDCARD == cd->targets[n].rank ||
                        peer->info->rank == cd->targets[n].rank) {
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
            PMIX_RETAIN(cd->buf);
            PMIX_SERVER_QUEUE_REPLY(peer, 0, cd->buf);
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
    pmix_status_t rc, *codes = NULL, *cdptr, maxcode = PMIX_MAX_ERR_CONSTANT;
    pmix_info_t *info = NULL;
    size_t ninfo=0, ncodes, ncds, n;
    pmix_regevents_info_t *reginfo = NULL;
    pmix_regevents_info_t *reginfo_next;
    pmix_peer_events_info_t *prev;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd deregister events");

    /* unpack the number of codes */
    cnt=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ncodes, &cnt, PMIX_SIZE))) {
        /* it is okay if there aren't any - equivalent to a wildcard */
        ncodes = 0;
    }
    /* unpack the array of codes */
    if (0 < ncodes) {
        codes = (pmix_status_t*)malloc(ncodes * sizeof(pmix_status_t));
        cnt=ncodes;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, codes, &cnt, PMIX_STATUS))) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* find the event registration info so we can delete them */
    if (NULL == codes) {
        cdptr = &maxcode;
        ncds = 1;
    } else {
        cdptr = codes;
        ncds = ncodes;
    }

    for (n=0; n < ncds; n++) {
        PMIX_LIST_FOREACH_SAFE(reginfo, reginfo_next, &pmix_server_globals.events, pmix_regevents_info_t) {
            if (cdptr[n] == reginfo->code) {
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
    }

cleanup:
    if (NULL != codes) {
        free(codes);
    }
    if (NULL != info) {
        PMIX_INFO_FREE(info, ninfo);
    }
    return;
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

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd event notification from client");

    if (NULL == pmix_host_server.notify_event) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_notify_caddy_t);
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* set the source */
    (void)strncpy(cd->source.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
    cd->source.rank = peer->info->rank;

    /* unpack status */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cd->status, &cnt, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the range */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cd->range, &cnt, PMIX_DATA_RANGE))) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }

    /* unpack the info keys */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cd->ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, cd->info, &cnt, PMIX_INFO))) {
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

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd query from client");

    if (NULL == pmix_host_server.query) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_query_caddy_t);
    cd->cbdata = cbdata;
    /* unpack the number of queries */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cd->nqueries, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the queries */
    if (0 < cd->nqueries) {
        PMIX_QUERY_CREATE(cd->queries, cd->nqueries);
        cnt = cd->nqueries;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, cd->queries, &cnt, PMIX_QUERY))) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->rank;

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

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd log from client");

    if (NULL == pmix_host_server.log) {
        return PMIX_ERR_NOT_SUPPORTED;
    }

    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;
    /* unpack the number of data */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cd->ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the data */
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        cnt = cd->ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, cd->info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }
    /* unpack the number of directives */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cd->ndirs, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto exit;
    }
    /* unpack the directives */
    if (0 < cd->ndirs) {
        PMIX_INFO_CREATE(cd->directives, cd->ndirs);
        cnt = cd->ndirs;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, cd->directives, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto exit;
        }
    }

    /* setup the requesting peer name */
    (void)strncpy(proc.nspace, peer->info->nptr->nspace, PMIX_MAX_NSLEN);
    proc.rank = peer->info->rank;

    /* ask the host to log the info */
    pmix_host_server.log(&proc, cd->info, cd->ninfo,
                         cd->directives, cd->ndirs,
                         logcbfn, cd);
    return PMIX_SUCCESS;

  exit:
    PMIX_RELEASE(cd);
    return rc;
}


/*****    INSTANCE SERVER LIBRARY CLASSES    *****/
static void tcon(pmix_server_trkr_t *t)
{
    t->pcs = NULL;
    t->npcs = 0;
    t->active = true;
    t->def_complete = false;
    PMIX_CONSTRUCT(&t->ranks, pmix_list_t);
    PMIX_CONSTRUCT(&t->local_cbs, pmix_list_t);
    t->nlocal = 0;
    t->local_cnt = 0;
    t->info = NULL;
    t->ninfo = 0;
    /* this needs to be set explicitly */
    t->collect_type = PMIX_COLLECT_INVALID;
    t->modexcbfunc = NULL;
    t->op_cbfunc = NULL;
}
static void tdes(pmix_server_trkr_t *t)
{
    if (NULL != t->pcs) {
        free(t->pcs);
    }
    PMIX_LIST_DESTRUCT(&t->ranks);
    PMIX_LIST_DESTRUCT(&t->local_cbs);
    PMIX_INFO_FREE(t->info, t->ninfo);
}
PMIX_CLASS_INSTANCE(pmix_server_trkr_t,
                   pmix_list_item_t,
                   tcon, tdes);

static void cdcon(pmix_server_caddy_t *cd)
{
    cd->peer = NULL;
    PMIX_CONSTRUCT(&cd->snd, pmix_snd_caddy_t);
}
static void cddes(pmix_server_caddy_t *cd)
{
    if (NULL != cd->peer) {
        PMIX_RELEASE(cd->peer);
    }
    PMIX_DESTRUCT(&cd->snd);
}
PMIX_CLASS_INSTANCE(pmix_server_caddy_t,
                   pmix_list_item_t,
                   cdcon, cddes);


PMIX_CLASS_INSTANCE(pmix_snd_caddy_t,
                   pmix_object_t,
                   NULL, NULL);

static void scadcon(pmix_setup_caddy_t *p)
{
    memset(&p->proc, 0, sizeof(pmix_proc_t));
    p->active = true;
    p->nspace = NULL;
    p->server_object = NULL;
    p->nlocalprocs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->cbfunc = NULL;
    p->opcbfunc = NULL;
    p->setupcbfunc = NULL;
    p->cbdata = NULL;
}
static void scaddes(pmix_setup_caddy_t *p)
{
}
PMIX_EXPORT PMIX_CLASS_INSTANCE(pmix_setup_caddy_t,
                                pmix_object_t,
                                scadcon, scaddes);

static void ncon(pmix_notify_caddy_t *p)
{
    p->active = true;
    memset(p->source.nspace, 0, PMIX_MAX_NSLEN+1);
    p->source.rank = PMIX_RANK_UNDEF;
    p->range = PMIX_RANGE_UNDEF;
    p->targets = NULL;
    p->ntargets = 0;
    p->nondefault = false;
    p->info = NULL;
    p->ninfo = 0;
    p->buf = PMIX_NEW(pmix_buffer_t);
}
static void ndes(pmix_notify_caddy_t *p)
{
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
    if (NULL != p->targets) {
        free(p->targets);
    }
    if (NULL != p->buf) {
        PMIX_RELEASE(p->buf);
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

PMIX_CLASS_INSTANCE(pmix_dmdx_request_t,
                    pmix_list_item_t,
                    NULL, NULL);

static void lmcon(pmix_dmdx_local_t *p)
{
    memset(&p->proc, 0, sizeof(pmix_proc_t));
    PMIX_CONSTRUCT(&p->loc_reqs, pmix_list_t);
    p->info = NULL;
    p->ninfo = 0;
}
static void lmdes(pmix_dmdx_local_t *p)
{
    PMIX_INFO_FREE(p->info, p->ninfo);
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
