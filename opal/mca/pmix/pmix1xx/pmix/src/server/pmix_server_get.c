/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>
#include <private/pmix_socket_errno.h>

#include <pmix_server.h>
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
#include "src/util/progress_threads.h"
#include "src/usock/usock.h"
#include "src/sec/pmix_sec.h"

#include "pmix_server_ops.h"

extern pmix_server_module_t pmix_host_server;

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
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
static pmix_status_t _satisfy_request(pmix_hash_table_t *ht, int rank,
                                      pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t create_local_tracker(char nspace[], int rank,
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
    free(data);
}


pmix_status_t pmix_server_get(pmix_buffer_t *buf,
                              pmix_modex_cbfunc_t cbfunc,
                              void *cbdata)
{
    int32_t cnt;
    pmix_status_t rc;
    int rank;
    char *cptr;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_nspace_t *ns, *nptr;
    pmix_info_t *info=NULL;
    size_t ninfo=0;
    pmix_dmdx_local_t *lcd;
    pmix_rank_info_t *iptr;
    pmix_hash_table_t *ht;
    bool local;

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
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &rank, &cnt, PMIX_INT))) {
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

    /* find the nspace object for this client */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s:%d EXECUTE GET FOR %s:%d",
                        pmix_globals.myid.nspace,
                        pmix_globals.myid.rank, nspace, rank);

    if (NULL == nptr || NULL == nptr->server) {
        /* this is for an nspace we don't know about yet, so
         * record the request for data from this process and
         * give the host server a chance to tell us about it */
        rc = create_local_tracker(nspace, rank, info, ninfo,
                                  cbfunc, cbdata, &lcd);
        return rc;
    }

    /* We have to wait for all local clients to be registered before
     * we can know whether this request is for data from a local or a
     * remote client because one client might ask for data about another
     * client that the host RM hasn't told us about yet. Fortunately,
     * we do know how many clients to expect, so first check to see if
     * all clients have been registered with us */
     if (!nptr->server->all_registered) {
        /* we cannot do anything further, so just track this request
         * for now */
        rc = create_local_tracker(nspace, rank, info, ninfo,
                                  cbfunc, cbdata, &lcd);
        return rc;
    }

    /* Since we know about all the local clients in this nspace,
     * let's first try to satisfy the request with any available data.
     * By default, we assume we are looking for data from a remote
     * client, and then check to see if this is one of my local
     * clients - if so, then we look in that hash table */
    ht = &nptr->server->remote;
    local = false;
    PMIX_LIST_FOREACH(iptr, &nptr->server->ranks, pmix_rank_info_t) {
        if (iptr->rank == rank) {
            /* it is known local client - check the local table */
            ht = &nptr->server->mylocal;
            local = true;
            break;
        }
    }

    /* see if we already have this data */
    rc = _satisfy_request(ht, rank, cbfunc, cbdata);
    if( PMIX_SUCCESS == rc ){
        /* request was successfully satisfied */
        PMIX_INFO_FREE(info, ninfo);
        return rc;
    }

    /* If we get here, then we don't have the data at this time. Check
     * to see if we already have a pending request for the data - if
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
        PMIX_LIST_DESTRUCT(&lcd->loc_reqs);
        PMIX_RELEASE(lcd);
        rc = PMIX_ERR_NOT_FOUND;
    }

    return rc;
}

static pmix_status_t create_local_tracker(char nspace[], int rank,
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
        PMIX_INFO_FREE(info, ninfo);
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

static pmix_status_t _satisfy_request(pmix_hash_table_t *ht, int rank,
                                      pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    pmix_value_t *val;
    char *data;
    size_t sz;
    pmix_buffer_t xfer, pbkt, *xptr;

    /* check to see if this data already has been
     * obtained as a result of a prior direct modex request from
     * a remote peer, or due to data from a local client
     * having been committed */
    rc = pmix_hash_fetch(ht, rank, "modex", &val);
    if (PMIX_SUCCESS == rc && NULL != val) {
        /* the client is expecting this to arrive as a byte object
         * containing a buffer, so package it accordingly */
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
        xptr = &xfer;
        PMIX_LOAD_BUFFER(&xfer, val->data.bo.bytes, val->data.bo.size);
        pmix_bfrop.pack(&pbkt, &xptr, 1, PMIX_BUFFER);
        xfer.base_ptr = NULL; // protect the passed data
        xfer.bytes_used = 0;
        PMIX_DESTRUCT(&xfer);
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
        PMIX_VALUE_RELEASE(val);
        /* pass it back */
        cbfunc(rc, data, sz, cbdata, relfn, data);
        return rc;
    }
    return PMIX_ERR_NOT_FOUND;
}

/* Resolve pending requests to this namespace/rank */
pmix_status_t pmix_pending_resolve(pmix_nspace_t *nptr, int rank,
                                   pmix_status_t status, pmix_dmdx_local_t *lcd)
{
    pmix_dmdx_local_t *cd;

    /* find corresponding request (if exists) */
    if (NULL == lcd && NULL != nptr) {
        PMIX_LIST_FOREACH(cd, &pmix_server_globals.local_reqs, pmix_dmdx_local_t) {
            if (0 != strncmp(nptr->nspace, cd->proc.nspace, PMIX_MAX_NSLEN) ||
                    rank != cd->proc.rank) {
                continue;
            }
            lcd = cd;
            break;
        }
    }

    /* If somebody was interested in this rank */
    if (NULL != lcd) {
        pmix_dmdx_request_t *req;

        if (PMIX_SUCCESS != status){
            /* if we've got an error for this request - just forward it*/
            PMIX_LIST_FOREACH(req, &lcd->loc_reqs, pmix_dmdx_request_t) {
                /* if we can't satisfy this request - respond with error */
                req->cbfunc(status, NULL, 0, req->cbdata, NULL, NULL);
            }
        } else if (NULL != nptr) {
            /* if we've got the blob - try to satisfy requests */
            pmix_hash_table_t *ht;
            pmix_rank_info_t *iptr;

            /* by default we are looking for the remote data */
            ht = &nptr->server->remote;
            /* check if this rank is local */
            PMIX_LIST_FOREACH(iptr, &nptr->server->ranks, pmix_rank_info_t) {
                if (iptr->rank == rank) {
                    ht = &nptr->server->mylocal;
                    break;
                }
            }

            /* run through all the requests to this rank */
            PMIX_LIST_FOREACH(req, &lcd->loc_reqs, pmix_dmdx_request_t) {
                pmix_status_t rc;
                rc = _satisfy_request(ht, rank, req->cbfunc, req->cbdata);
                if( PMIX_SUCCESS != rc ){
                    /* if we can't satisfy this particular request (missing key?) */
                    req->cbfunc(rc, NULL, 0, req->cbdata, NULL, NULL);
                }
            }
        }
        /* remove all requests to this rank and cleanup the corresponding structure */
        pmix_list_remove_item(&pmix_server_globals.local_reqs, (pmix_list_item_t*)lcd);
        PMIX_RELEASE(lcd);
    }
    return PMIX_SUCCESS;
}

/* process the returned data from the host RM server */
static void _process_dmdx_reply(int fd, short args, void *cbdata)
{
    pmix_dmdx_reply_caddy_t *caddy = (pmix_dmdx_reply_caddy_t *)cbdata;
    pmix_kval_t *kp;
    pmix_nspace_t *ns, *nptr;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                    "[%s:%d] process dmdx reply from %s:%d",
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
        /* should be impossible */
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        caddy->status = PMIX_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* if the request was successfully satisfied, then store the data
     * in our hash table for remote procs. Although we could immediately
     * resolve any outstanding requests on our tracking list, we instead
     * store the data first so we can immediately satisfy any future
     * requests. Then, rather than duplicate the resolve code here, we
     * will let the pmix_pending_resolve function go ahead and retrieve
     * it from the hash table */
    if (PMIX_SUCCESS == caddy->status) {
        kp = PMIX_NEW(pmix_kval_t);
        kp->key = strdup("modex");
        PMIX_VALUE_CREATE(kp->value, 1);
        kp->value->type = PMIX_BYTE_OBJECT;
        /* we don't know if the host is going to save this data
         * or not, so we have to copy it - the client is expecting
         * this to arrive as a byte object containing a buffer, so
         * package it accordingly */
        kp->value->data.bo.bytes = malloc(caddy->ndata);
        memcpy(kp->value->data.bo.bytes, caddy->data, caddy->ndata);
        kp->value->data.bo.size = caddy->ndata;
        /* store it in the appropriate hash */
        if (PMIX_SUCCESS != (rc = pmix_hash_store(&nptr->server->remote, caddy->lcd->proc.rank, kp))) {
            PMIX_ERROR_LOG(rc);
        }
        PMIX_RELEASE(kp);  // maintain acctg
    }

    /* always execute the callback to avoid having the client hang */
    pmix_pending_resolve(nptr, caddy->lcd->proc.rank, caddy->status, caddy->lcd);

cleanup:
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
                        "[%s:%d] queue dmdx reply for %s:%d",
                        __FILE__, __LINE__,
                        caddy->lcd->proc.nspace, caddy->lcd->proc.rank);
    event_assign(&caddy->ev, pmix_globals.evbase, -1, EV_WRITE,
                 _process_dmdx_reply, caddy);
    event_priority_set(&caddy->ev, 0);
    event_active(&caddy->ev, EV_WRITE, 1);
}

