/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
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
#include PMIX_EVENT_HEADER

#include "src/class/pmix_list.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
#include "src/dstore/pmix_dstore.h"
#endif /* PMIX_ENABLE_DSTORE */

#include "pmix_server_ops.h"

extern pmix_server_module_t pmix_host_server;

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    volatile bool active;
    pmix_status_t status;
    const char *data;
    size_t ndata;
    pmix_dmdx_local_t *lcd;
    pmix_release_cbfunc_t relcbfunc;
    void *cbdata;
} pmix_dmdx_reply_caddy_t;
static void dcd_con(pmix_dmdx_reply_caddy_t *p)
{
    p->status = PMIX_ERROR;
    p->ndata = 0;
    p->lcd = NULL;
    p->relcbfunc = NULL;
    p->cbdata = NULL;
}
PMIX_CLASS_INSTANCE(pmix_dmdx_reply_caddy_t,
                   pmix_object_t, dcd_con, NULL);


static void dmdx_cbfunc(pmix_status_t status, const char *data,
                        size_t ndata, void *cbdata,
                        pmix_release_cbfunc_t relfn, void *relcbdata);
static pmix_status_t _satisfy_request(pmix_nspace_t *ns, pmix_rank_t rank,
                                      pmix_server_caddy_t *cd,
                                      pmix_modex_cbfunc_t cbfunc, void *cbdata, bool *scope);
static pmix_status_t create_local_tracker(char nspace[], pmix_rank_t rank,
                                          pmix_info_t info[], size_t ninfo,
                                          pmix_modex_cbfunc_t cbfunc,
                                          void *cbdata,
                                          pmix_dmdx_local_t **lcd);


/* declare a function whose sole purpose is to
 * free data that we provided to our host server
 * when servicing dmodex requests */
static void relfn(void *cbdata)
{
    char *data = (char*)cbdata;
    if (NULL != data) {
        free(data);
    }
}


pmix_status_t pmix_server_get(pmix_buffer_t *buf,
                              pmix_modex_cbfunc_t cbfunc,
                              void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    int32_t cnt;
    pmix_status_t rc;
    pmix_rank_t rank;
    char *cptr;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_nspace_t *ns, *nptr;
    pmix_info_t *info=NULL;
    size_t ninfo=0;
    pmix_dmdx_local_t *lcd;
    bool local;
    bool localonly = false;
    pmix_buffer_t pbkt;
    char *data;
    size_t sz, n;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "recvd GET");

    /* setup */
    memset(nspace, 0, sizeof(nspace));

    /* retrieve the nspace and rank of the requested proc */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &cptr, &cnt, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    (void)strncpy(nspace, cptr, PMIX_MAX_NSLEN);
    free(cptr);
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &rank, &cnt, PMIX_PROC_RANK))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* retrieve any provided info structs */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_INFO_FREE(info, ninfo);
            return rc;
        }
    }

    /* search for directives we can deal with here */
    for (n=0; n < ninfo; n++) {
        if (0 == strcmp(info[n].key, PMIX_IMMEDIATE)) {
            if (PMIX_UNDEF == info[n].value.type || info[n].value.data.flag) {
                /* just check our own data - don't wait
                 * or request it from someone else */
                localonly = true;
            }
        }
    }

    /* find the nspace object for this client */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s:%d EXECUTE GET FOR %s:%d ON BEHALF OF %s:%d",
                        pmix_globals.myid.nspace,
                        pmix_globals.myid.rank, nspace, rank,
                        cd->peer->info->nptr->nspace,
                        cd->peer->info->rank);

    if (NULL == nptr || NULL == nptr->server) {
        if (localonly) {
            return PMIX_ERR_NOT_FOUND;
        }
        /* this is for an nspace we don't know about yet, so
         * record the request for data from this process and
         * give the host server a chance to tell us about it */
        rc = create_local_tracker(nspace, rank, info, ninfo,
                                  cbfunc, cbdata, &lcd);
        if (PMIX_ERR_NOMEM == rc) {
            PMIX_INFO_FREE(info, ninfo);
            return rc;
        }

        /*
         * Its possible there are no local processes on this
         * host, so lets ask for this explicitly.  There can
         * be a timing issue here if this information shows
         * up on its own, but I believe we handle it ok.  */
        if( NULL != pmix_host_server.direct_modex ){
                pmix_host_server.direct_modex(&lcd->proc, info, ninfo, dmdx_cbfunc, lcd);
        }
        return (rc == PMIX_ERR_NOT_FOUND ? PMIX_SUCCESS : rc);
    }

    /* if the rank is wildcard, then they are asking for the job-level
     * info for this nspace - provide it */
    if (PMIX_RANK_WILDCARD == rank) {
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        pmix_bfrop.pack(&pbkt, &rank, 1, PMIX_PROC_RANK);
        /* the client is expecting this to arrive as a byte object
         * containing a buffer, so package it accordingly */
        pmix_bfrop.pack(&pbkt, &nptr->server->job_info, 1, PMIX_BUFFER);
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
        cbfunc(PMIX_SUCCESS, data, sz, cbdata, relfn, data);
        return PMIX_SUCCESS;
    }

    /* We have to wait for all local clients to be registered before
     * we can know whether this request is for data from a local or a
     * remote client because one client might ask for data about another
     * client that the host RM hasn't told us about yet. Fortunately,
     * we do know how many clients to expect, so first check to see if
     * all clients have been registered with us */
     if (!nptr->server->all_registered) {
        if (localonly) {
            return PMIX_ERR_NOT_FOUND;
        }
        /* we cannot do anything further, so just track this request
         * for now */
        rc = create_local_tracker(nspace, rank, info, ninfo,
                                  cbfunc, cbdata, &lcd);
        if (PMIX_ERR_NOMEM == rc) {
            PMIX_INFO_FREE(info, ninfo);
        }
        return rc;
    }

    /* see if we already have this data */
    rc = _satisfy_request(nptr, rank, cd, cbfunc, cbdata, &local);
    if( PMIX_SUCCESS == rc ){
        /* request was successfully satisfied */
        PMIX_INFO_FREE(info, ninfo);
        return rc;
    }

    /* If we get here, then we don't have the data at this time. If
     * the user doesn't want to look for it, then we are done */
    if (localonly) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* Check to see if we already have a pending request for the data - if
     * we do, then we can just wait for it to arrive */
    rc = create_local_tracker(nspace, rank, info, ninfo,
                              cbfunc, cbdata, &lcd);
    if (PMIX_SUCCESS == rc) {
       /* we are already waiting for the data - nothing more
        * for us to do as the function added the new request
        * to the tracker for us */
       return PMIX_SUCCESS;
    }
    if (PMIX_ERR_NOT_FOUND != rc || NULL == lcd) {
        /* we have a problem - e.g., out of memory */
        PMIX_INFO_FREE(info, ninfo);
        return rc;
    }

    /* Getting here means that we didn't already have a request for
     * for data pending, and so we created a new tracker for this
     * request. We know the identity of all our local clients, so
     * if this is one, then we have nothing further to do - we will
     * fulfill the request once the process commits its data */
    if (local) {
        return PMIX_SUCCESS;
    }

    /* this isn't a local client of ours, so we need to ask the host
     * resource manager server to please get the info for us from
     * whomever is hosting the target process */
    if (NULL != pmix_host_server.direct_modex) {
        rc = pmix_host_server.direct_modex(&lcd->proc, info, ninfo, dmdx_cbfunc, lcd);
    } else {
        /* if we don't have direct modex feature, just respond with "not found" */
        cbfunc(PMIX_ERR_NOT_FOUND, NULL, 0, cbdata, NULL, NULL);
        PMIX_INFO_FREE(info, ninfo);
        pmix_list_remove_item(&pmix_server_globals.local_reqs, &lcd->super);
        PMIX_RELEASE(lcd);
        rc = PMIX_ERR_NOT_FOUND;
    }

    return rc;
}

static pmix_status_t create_local_tracker(char nspace[], pmix_rank_t rank,
                                          pmix_info_t info[], size_t ninfo,
                                          pmix_modex_cbfunc_t cbfunc,
                                          void *cbdata,
                                          pmix_dmdx_local_t **ld)
{
    pmix_dmdx_local_t *lcd, *cd;
    pmix_dmdx_request_t *req;
    pmix_status_t rc;

    /* define default */
    *ld = NULL;

    /* see if we already have an existing request for data
     * from this namespace/rank */
    lcd = NULL;
    PMIX_LIST_FOREACH(cd, &pmix_server_globals.local_reqs, pmix_dmdx_local_t) {
        if (0 != strncmp(nspace, cd->proc.nspace, PMIX_MAX_NSLEN) ||
                rank != cd->proc.rank ) {
            continue;
        }
        lcd = cd;
        break;
    }
    if (NULL != lcd) {
        /* we already have a request, so just track that someone
         * else wants data from the same target */
        rc = PMIX_SUCCESS; // indicates we found an existing request
        goto complete;
    }
    /* we do not have an existing request, so let's create
     * one and add it to our list */
    lcd = PMIX_NEW(pmix_dmdx_local_t);
    if (NULL == lcd){
        return PMIX_ERR_NOMEM;
    }
    strncpy(lcd->proc.nspace, nspace, PMIX_MAX_NSLEN);
    lcd->proc.rank = rank;
    lcd->info = info;
    lcd->ninfo = ninfo;
    pmix_list_append(&pmix_server_globals.local_reqs, &lcd->super);
    rc = PMIX_ERR_NOT_FOUND;  // indicates that we created a new request tracker

  complete:
    /* track this specific requestor so we return the
     * data to them */
    req = PMIX_NEW(pmix_dmdx_request_t);
    req->cbfunc = cbfunc;
    req->cbdata = cbdata;
    pmix_list_append(&lcd->loc_reqs, &req->super);
    *ld = lcd;
    return rc;
}

void pmix_pending_nspace_requests(pmix_nspace_t *nptr)
{
    pmix_dmdx_local_t *cd, *cd_next;

    /* Now that we know all local ranks, go along request list and ask for remote data
     * for the non-local ranks, and resolve all pending requests for local procs
     * that were waiting for registration to complete
     */
    PMIX_LIST_FOREACH_SAFE(cd, cd_next, &pmix_server_globals.local_reqs, pmix_dmdx_local_t) {
        pmix_rank_info_t *info;
        bool found = false;

        if (0 != strncmp(nptr->nspace, cd->proc.nspace, PMIX_MAX_NSLEN) ) {
            continue;
        }

        PMIX_LIST_FOREACH(info, &nptr->server->ranks, pmix_rank_info_t) {
            if (info->rank == cd->proc.rank) {
                found = true;  // we will satisy this request upon commit from new proc
                break;
            }
        }

        /* if not found - this is remote process and we need to send
         * corresponding direct modex request */
        if( !found ){
            if( NULL != pmix_host_server.direct_modex ){
                pmix_host_server.direct_modex(&cd->proc, cd->info, cd->ninfo, dmdx_cbfunc, cd);
            } else {
                pmix_dmdx_request_t *req, *req_next;
                PMIX_LIST_FOREACH_SAFE(req, req_next, &cd->loc_reqs, pmix_dmdx_request_t) {
                    req->cbfunc(PMIX_ERR_NOT_FOUND, NULL, 0, req->cbdata, NULL, NULL);
                    pmix_list_remove_item(&cd->loc_reqs, &req->super);
                    PMIX_RELEASE(req);
                }
                pmix_list_remove_item(&pmix_server_globals.local_reqs, &cd->super);
                PMIX_RELEASE(cd);
            }
        }
    }
}

static pmix_status_t _satisfy_request(pmix_nspace_t *nptr, pmix_rank_t rank,
                                      pmix_server_caddy_t *cd,
                                      pmix_modex_cbfunc_t cbfunc,
                                      void *cbdata, bool *scope)
{
    pmix_status_t rc;
    pmix_value_t *val;
    char *data;
    size_t sz;
    pmix_rank_t cur_rank;
    int found = 0;
    pmix_buffer_t pbkt, *pbptr;
    void *last;
    pmix_hash_table_t *hts[3];
    pmix_hash_table_t **htptr;
    pmix_rank_info_t *iptr;
    bool local;

    /* Since we know about all the local clients in this nspace,
     * let's first try to satisfy the request with any available data.
     * By default, we assume we are looking for data from a remote
     * client, and then check to see if this is one of my local
     * clients - if so, then we look in that hash table */
    memset(hts, 0, sizeof(hts));
    if (PMIX_RANK_UNDEF == rank) {
        local = true;
        hts[0] = &nptr->server->remote;
        hts[1] = &nptr->server->mylocal;
    } else if (PMIX_RANK_WILDCARD == rank) {
        local = true;
        hts[0] = NULL;
    } else {
        local = false;
        hts[0] = &nptr->server->remote;
        PMIX_LIST_FOREACH(iptr, &nptr->server->ranks, pmix_rank_info_t) {
            if (iptr->rank == rank) {
                /* it is known local client - check the local table */
                hts[0] = &nptr->server->mylocal;
                local = true;
                break;
            }
        }
    }

    if (NULL != scope) {
        *scope = local;
    }

    /* check to see if this data already has been
     * obtained as a result of a prior direct modex request from
     * a remote peer, or due to data from a local client
     * having been committed */
    htptr = hts;
    PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);

    /* if they are asking about a rank from an nspace different
     * from their own, then include a copy of the job-level info */
    if (rank == PMIX_RANK_WILDCARD || (NULL != cd &&
        0 != strncmp(nptr->nspace, cd->peer->info->nptr->nspace, PMIX_MAX_NSLEN))) {
        cur_rank = PMIX_RANK_WILDCARD;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&pbkt, &cur_rank, 1, PMIX_PROC_RANK))) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&pbkt);
            cbfunc(rc, NULL, 0, cbdata, NULL, NULL);
            return rc;
        }
        /* the client is expecting this to arrive as a byte object
         * containing a buffer, so package it accordingly */
        pbptr = &nptr->server->job_info;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&pbkt, &pbptr, 1, PMIX_BUFFER))) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&pbkt);
            cbfunc(rc, NULL, 0, cbdata, NULL, NULL);
            return rc;
        }
        if (rank == PMIX_RANK_WILDCARD) {
            found++;
        }
    }

    while (NULL != *htptr) {
        cur_rank = rank;
        if (PMIX_RANK_UNDEF == rank) {
            rc = pmix_hash_fetch_by_key(*htptr, "modex", &cur_rank, &val, &last);
        } else {
            rc = pmix_hash_fetch(*htptr, cur_rank, "modex", &val);
        }
        while (PMIX_SUCCESS == rc) {
            if (NULL != val) {
#if defined(PMIX_ENABLE_DSTORE) && (PMIX_ENABLE_DSTORE == 1)
                pmix_kval_t *kv;

                /* setup to xfer the data */
                kv = PMIX_NEW(pmix_kval_t);
                kv->key = strdup("modex");
                kv->value = (pmix_value_t *)malloc(sizeof(pmix_value_t));
                rc = pmix_value_xfer(kv->value, val);
                if (PMIX_SUCCESS != (rc = pmix_dstore_store(nptr->nspace, cur_rank, kv))) {
                        PMIX_ERROR_LOG(rc);
                }
                PMIX_RELEASE(kv);
#else
                pmix_buffer_t xfer, *xptr;
                pmix_bfrop.pack(&pbkt, &cur_rank, 1, PMIX_PROC_RANK);
                /* the client is expecting this to arrive as a byte object
                 * containing a buffer, so package it accordingly */
                PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
                xptr = &xfer;
                PMIX_LOAD_BUFFER(&xfer, val->data.bo.bytes, val->data.bo.size);
                pmix_bfrop.pack(&pbkt, &xptr, 1, PMIX_BUFFER);
                xfer.base_ptr = NULL; // protect the passed data
                xfer.bytes_used = 0;
                PMIX_DESTRUCT(&xfer);
#endif /* PMIX_ENABLE_DSTORE */
                PMIX_VALUE_RELEASE(val);
                found++;
            }
            if (PMIX_RANK_UNDEF == rank) {
                rc = pmix_hash_fetch_by_key(*htptr, NULL, &cur_rank, &val, &last);
            } else {
                break;
            }
        }
        htptr++;
    }
    PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
    PMIX_DESTRUCT(&pbkt);

    if (found) {
        /* pass it back */
        cbfunc(PMIX_SUCCESS, data, sz, cbdata, relfn, data);
        return PMIX_SUCCESS;
    }

    return PMIX_ERR_NOT_FOUND;
}

/* Resolve pending requests to this namespace/rank */
pmix_status_t pmix_pending_resolve(pmix_nspace_t *nptr, pmix_rank_t rank,
                                   pmix_status_t status, pmix_dmdx_local_t *lcd)
{
    pmix_dmdx_local_t *cd, *ptr;
    pmix_dmdx_request_t *req;

    /* find corresponding request (if exists) */
    if (NULL == lcd) {
        ptr = NULL;
        if (NULL != nptr) {
            PMIX_LIST_FOREACH(cd, &pmix_server_globals.local_reqs, pmix_dmdx_local_t) {
                if (0 != strncmp(nptr->nspace, cd->proc.nspace, PMIX_MAX_NSLEN) ||
                        rank != cd->proc.rank) {
                    continue;
                }
                ptr = cd;
                break;
            }
        }
        if (NULL == ptr) {
            return PMIX_SUCCESS;
        }
    } else {
        ptr = lcd;
    }

    /* somebody was interested in this rank */
    if (PMIX_SUCCESS != status){
        /* if we've got an error for this request - just forward it*/
        PMIX_LIST_FOREACH(req, &ptr->loc_reqs, pmix_dmdx_request_t) {
            req->cbfunc(status, NULL, 0, req->cbdata, NULL, NULL);
        }
    } else if (NULL != nptr) {
        /* if we've got the blob - try to satisfy requests */
        /* run through all the requests to this rank */
        PMIX_LIST_FOREACH(req, &ptr->loc_reqs, pmix_dmdx_request_t) {
            pmix_status_t rc;
            rc = _satisfy_request(nptr, rank, NULL, req->cbfunc, req->cbdata, NULL);
            if( PMIX_SUCCESS != rc ){
                /* if we can't satisfy this particular request (missing key?) */
                req->cbfunc(rc, NULL, 0, req->cbdata, NULL, NULL);
            }
        }
    }
    /* remove all requests to this rank and cleanup the corresponding structure */
    pmix_list_remove_item(&pmix_server_globals.local_reqs, (pmix_list_item_t*)ptr);
    PMIX_RELEASE(ptr);

    return PMIX_SUCCESS;
}

/* process the returned data from the host RM server */
static void _process_dmdx_reply(int fd, short args, void *cbdata)
{
    pmix_dmdx_reply_caddy_t *caddy = (pmix_dmdx_reply_caddy_t *)cbdata;
    pmix_kval_t *kp;
    pmix_nspace_t *ns, *nptr;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(caddy);

    pmix_output_verbose(2, pmix_globals.debug_output,
                    "[%s:%d] process dmdx reply from %s:%u",
                    __FILE__, __LINE__,
                    caddy->lcd->proc.nspace, caddy->lcd->proc.rank);

    /* find the nspace object for this client */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(caddy->lcd->proc.nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }

    if (NULL == nptr) {
        /*
         * We may not have this namespace because someone asked about this namespace
         * but there are not processses from it running on this host
         */
        nptr = PMIX_NEW(pmix_nspace_t);
        (void)strncpy(nptr->nspace, caddy->lcd->proc.nspace, PMIX_MAX_NSLEN);
        nptr->server = PMIX_NEW(pmix_server_nspace_t);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }

    /* if the request was successfully satisfied, then store the data
     * in our hash table for remote procs. Although we could immediately
     * resolve any outstanding requests on our tracking list, we instead
     * store the data first so we can immediately satisfy any future
     * requests. Then, rather than duplicate the resolve code here, we
     * will let the pmix_pending_resolve function go ahead and retrieve
     * it from the hash table.
     *
     * NOTE: A NULL data pointer indicates that the data has already
     * been returned via completion of a background fence_nb operation.
     * In this case, all we need to do is resolve the request */
    if (PMIX_SUCCESS == caddy->status && NULL != caddy->data) {
        if (caddy->lcd->proc.rank == PMIX_RANK_WILDCARD) {
            void * where = malloc(caddy->ndata);
            if (where) {
               memcpy(where, caddy->data, caddy->ndata);
               PMIX_LOAD_BUFFER(&nptr->server->job_info, where, caddy->ndata);
            } else {
               /* The data was stored, so hate to change caddy->status just because
                * we could not store it locally.
                */
               PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            }
        } else {
            kp = PMIX_NEW(pmix_kval_t);
            kp->key = strdup("modex");
            PMIX_VALUE_CREATE(kp->value, 1);
            kp->value->type = PMIX_BYTE_OBJECT;
            /* we don't know if the host is going to save this data
             * or not, so we have to copy it - the client is expecting
             * this to arrive as a byte object containing a buffer, so
             * package it accordingly */
            kp->value->data.bo.bytes = malloc(caddy->ndata);
            if (kp->value->data.bo.bytes) {
                memcpy(kp->value->data.bo.bytes, caddy->data, caddy->ndata);
                kp->value->data.bo.size = caddy->ndata;
                /* store it in the appropriate hash */
                if (PMIX_SUCCESS != (rc = pmix_hash_store(&nptr->server->remote, caddy->lcd->proc.rank, kp))) {
                    PMIX_ERROR_LOG(rc);
                }
            } else {
               /* The data was stored, so hate to change caddy->status just because
                * we could not store it locally.
                */
               PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            }
            PMIX_RELEASE(kp);  // maintain acctg
        }
    }

    /* always execute the callback to avoid having the client hang */
    pmix_pending_resolve(nptr, caddy->lcd->proc.rank, caddy->status, caddy->lcd);

    /* now call the release function so the host server
     * knows it can release the data */
    if (NULL != caddy->relcbfunc) {
        caddy->relcbfunc(caddy->cbdata);
    }
    PMIX_RELEASE(caddy);
}

/* this is the callback function that the host RM server will call
 * when it gets requested info back from a remote server */
static void dmdx_cbfunc(pmix_status_t status,
                        const char *data, size_t ndata, void *cbdata,
                        pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_dmdx_reply_caddy_t *caddy;

    /* because the host RM is calling us from their own thread, we
     * need to thread-shift into our local progress thread before
     * accessing any global info */
    caddy = PMIX_NEW(pmix_dmdx_reply_caddy_t);
    caddy->status = status;
    /* point to the callers cbfunc */
    caddy->relcbfunc = release_fn;
    caddy->cbdata = release_cbdata;

    /* point to the returned data and our own internal
     * tracker */
    caddy->data   = data;
    caddy->ndata  = ndata;
    caddy->lcd    = (pmix_dmdx_local_t *)cbdata;
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "[%s:%d] queue dmdx reply for %s:%u",
                        __FILE__, __LINE__,
                        caddy->lcd->proc.nspace, caddy->lcd->proc.rank);
    PMIX_THREADSHIFT(caddy, _process_dmdx_reply);
}
