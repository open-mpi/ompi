/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

#include "src/include/pmix_config.h"

#include "src/include/pmix_stdint.h"
#include "src/include/pmix_socket_errno.h"

#include "include/pmix_server.h"
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
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/gds/gds.h"
#include "src/mca/ptl/base/base.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/name_fns.h"
#include "src/util/output.h"
#include "src/util/pmix_environ.h"

#include "src/client/pmix_client_ops.h"
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
static pmix_status_t _satisfy_request(pmix_namespace_t *nptr, pmix_rank_t rank,
                                      pmix_server_caddy_t *cd,
                                      bool diffnspace, pmix_scope_t scope,
                                      pmix_modex_cbfunc_t cbfunc, void *cbdata);
static pmix_status_t create_local_tracker(char nspace[], pmix_rank_t rank,
                                          pmix_info_t info[], size_t ninfo,
                                          pmix_modex_cbfunc_t cbfunc,
                                          void *cbdata,
                                          pmix_dmdx_local_t **lcd,
                                          pmix_dmdx_request_t **rq);
static pmix_status_t get_job_data(char *nspace,
                                  pmix_server_caddy_t *cd,
                                  pmix_buffer_t *pbkt);
static void get_timeout(int sd, short args, void *cbdata);


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

static pmix_status_t defer_response(char *nspace, pmix_rank_t rank,
                                    pmix_server_caddy_t *cd,
                                    bool localonly,
                                    pmix_modex_cbfunc_t cbfunc,
                                    void *cbdata,
                                    struct timeval *tv,
                                    pmix_dmdx_local_t **locald)
{
    pmix_status_t rc;
    pmix_dmdx_request_t *req;
    pmix_dmdx_local_t *lcd;

    *locald = NULL;

    if (localonly) {
        /* the client asked that we not wait, so return now */
        pmix_output_verbose(2, pmix_server_globals.get_output,
                            "%s:%d CLIENT REQUESTED IMMEDIATE",
                            pmix_globals.myid.nspace,
                            pmix_globals.myid.rank);
        return PMIX_ERR_NOT_AVAILABLE;
    }
    /* we cannot do anything further, so just track this request
     * for now */
    rc = create_local_tracker(nspace, rank, cd->info, cd->ninfo,
                              cbfunc, cbdata, &lcd, &req);
    if (PMIX_ERR_NOMEM == rc || NULL == lcd) {
        return rc;
    }
    pmix_output_verbose(2, pmix_server_globals.get_output,
                        "%s:%d TRACKER CREATED - WAITING",
                        pmix_globals.myid.nspace,
                        pmix_globals.myid.rank);
    /* if they specified a timeout, set it up now */
    if (NULL != tv && 0 < tv->tv_sec) {
        pmix_event_evtimer_set(pmix_globals.evbase, &req->ev,
                               get_timeout, req);
        pmix_event_evtimer_add(&req->ev, tv);
        req->event_active = true;
    }
    /* the peer object has been added to the new lcd tracker,
     * so return success here */
    *locald = lcd;
    return rc;

}
pmix_status_t pmix_server_get(pmix_buffer_t *buf,
                              pmix_modex_cbfunc_t cbfunc,
                              void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t*)cbdata;
    int32_t cnt;
    pmix_status_t rc;
    pmix_rank_t rank;
    char *cptr, *key=NULL;
    char nspace[PMIX_MAX_NSLEN+1];
    pmix_namespace_t *ns, *nptr;
    pmix_dmdx_local_t *lcd;
    bool local = false;
    bool localonly = false;
    bool diffnspace = false;
    bool refresh_cache = false;
    bool scope_given = false;
    struct timeval tv = {0, 0};
    pmix_buffer_t pbkt;
    pmix_cb_t cb;
    pmix_proc_t proc;
    char *data;
    size_t sz, n;
    pmix_info_t *info;
    pmix_scope_t scope = PMIX_SCOPE_UNDEF;
    pmix_rank_info_t *iptr;

    pmix_output_verbose(2, pmix_server_globals.get_output,
                        "%s recvd GET",
                        PMIX_NAME_PRINT(&pmix_globals.myid));

    /* setup */
    PMIX_LOAD_NSPACE(nspace, NULL);

    /* retrieve the nspace and rank of the requested proc */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &cptr, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_LOAD_NSPACE(nspace, cptr);
    free(cptr);
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &rank, &cnt, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* retrieve any provided info structs */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &cd->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    if (0 < cd->ninfo) {
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        if (NULL == cd->info) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }
        cnt = cd->ninfo;
        PMIX_BFROPS_UNPACK(rc, cd->peer, buf, cd->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    }
    /* if they provided a specific key, get it */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, cd->peer, buf, &key, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc && PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* search for directives we can deal with here */
    for (n=0; n < cd->ninfo; n++) {
        if (PMIX_CHECK_KEY(&cd->info[n], PMIX_IMMEDIATE)) {
            /* just check our own data - don't wait
             * or request it from someone else */
            localonly = PMIX_INFO_TRUE(&cd->info[n]);
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_TIMEOUT)) {
            tv.tv_sec = cd->info[n].value.data.uint32;
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_GET_REFRESH_CACHE)) {
            refresh_cache = PMIX_INFO_TRUE(&cd->info[n]);
        } else if (PMIX_CHECK_KEY(&cd->info[n], PMIX_DATA_SCOPE)) {
            scope = cd->info[n].value.data.scope;
            scope_given = true;
        }
    }

    /* find the nspace object for the target proc */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }

    pmix_output_verbose(2, pmix_server_globals.get_output,
                        "%s EXECUTE GET FOR %s:%d WITH KEY %s ON BEHALF OF %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid),
                        nspace, rank, (NULL == key) ? "NULL" : key,
                        PMIX_PNAME_PRINT(&cd->peer->info->pname));

    /* This call flows upward from a local client. If we don't
     * know about this nspace, then it cannot refer to the
     * nspace of the requestor - i.e., they aren't asking
     * about one of their peers. There are two reasons why we
     * might not know about this nspace at this time:
     *
     * (a) we don't host any local procs from this nspace, and
     *     so the local RM didn't tell us about it. We will have
     *     to request the information from it.
     *
     * (b) a race condition where the other job hasn't registered
     *     its nspace yet. This begs the question as to how the
     *     requestor got the nspace name in the first place!
     *     However, there _may_ be some path whereby that could
     *     happen, so we try to deal with it here.
     *
     * Either way, we are going to have to request the info from
     * the host RM. Since we are hopeful of getting an answer,
     * we add the nspace to our list of known nspaces so the
     * info has a "landing zone" upon return */

    if (NULL == nptr) {
        if (localonly) {
            /* the user doesn't want us to look for the info,
             * so we simply return at this point */
            pmix_output_verbose(5, pmix_server_globals.get_output,
                                "%s UNKNOWN NSPACE: LOCAL ONLY - NOT FOUND",
                                PMIX_NAME_PRINT(&pmix_globals.myid));
            return PMIX_ERR_NOT_FOUND;
        }
        /* this is for an nspace we don't know about yet, so
         * give the host server a chance to tell us about it.
         * The cbdata passed here is the pmix_server_caddy_t
         * we were passed - it contains the pmix_peer_t of
         * the original requestor so they will get the data
         * back when we receive it */
        goto request;
    }

    /* the target nspace is known, so we can process the request.
     * if the rank is wildcard, then they are asking for the job-level
     * info for this nspace - provide it */
    if (PMIX_RANK_WILDCARD == rank) {
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        rc = get_job_data(nptr->nspace, cd, &pbkt);
        if (PMIX_SUCCESS != rc) {
            PMIX_DESTRUCT(&pbkt);
            return rc;
        }
        /* unload the resulting payload */
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
        /* call the internal callback function - it will
         * release the cbdata */
        cbfunc(PMIX_SUCCESS, data, sz, cbdata, relfn, data);
        /* return success so the server doesn't duplicate
         * the release of cbdata */
        return PMIX_SUCCESS;
    }

    /* We have to wait for all local clients to be registered before
     * we can know whether this request is for data from a local or a
     * remote client because one client might ask for data about another
     * client that the host RM hasn't told us about yet. Fortunately,
     * we do know how many clients to expect, so first check to see if
     * all clients have been registered with us */
     if (!nptr->all_registered) {
        pmix_output_verbose(2, pmix_server_globals.get_output,
                            "%s:%d NSPACE %s not all registered",
                            pmix_globals.myid.nspace,
                            pmix_globals.myid.rank, nspace);
        rc = defer_response(nspace, rank, cd, localonly, cbfunc, cbdata, &tv, &lcd);
        if (PMIX_ERR_NOT_FOUND == rc) {
            /* just means we created a tracker */
            rc = PMIX_SUCCESS;
        } else if (PMIX_ERR_NOT_AVAILABLE == rc) {
            /* means they requested "immediate" */
            rc = PMIX_ERR_NOT_FOUND;
        }
        return rc;
    }

    /* everyone has been registered, so we know who our local procs
     * are - see if this target is one of them. Note that we cannot reach
     * this point if rank = WILDCARD */
    if (0 < nptr->nlocalprocs) {
        /* if all the procs are local, then this must be a local proc */
        if (nptr->nprocs == nptr->nlocalprocs) {
            local = true;
        } else {
            /* see if this proc is one of our local ones */
            PMIX_LIST_FOREACH(iptr, &nptr->ranks, pmix_rank_info_t) {
                if (rank == iptr->pname.rank) {
                    if (0 > iptr->peerid) {
                        /* this rank has not connected yet, so this request needs to be held */
                        rc = defer_response(nspace, rank, cd, localonly, cbfunc, cbdata, &tv, &lcd);
                        if (PMIX_ERR_NOT_FOUND == rc) {
                            /* just means we created a tracker */
                            rc = PMIX_SUCCESS;
                        } else if (PMIX_ERR_NOT_AVAILABLE == rc) {
                            /* means they requested "immediate" */
                            rc = PMIX_ERR_NOT_FOUND;
                        }
                        return rc;
                    }
                    local = true;
                    break;
                }
            }
            if (NULL == pmix_pointer_array_get_item(&pmix_server_globals.clients, iptr->peerid)) {
                /* this must be a remote rank */
                local = false;
            }
        }
    } else {
        local = false;
    }

    /* if the proc is local, then we assume that the host/server maintains
     * updated info - there is no need to ask the host to refresh a cache */
    if (local && refresh_cache) {
        return PMIX_OPERATION_SUCCEEDED;
    } else if (refresh_cache) {
        if (NULL != key) {
            free(key);
            key = NULL;
        }
        goto request;
    }

    /* the target nspace is known - if they asked us to wait for a specific
     * key to be available, check if it is present. NOTE: key is only
     * NULL if the request came from an older version */
    if (NULL != key) {
        PMIX_LOAD_PROCID(&proc, nspace, rank);
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &proc;
        if (scope_given) {
            cb.scope = scope;
        } else if (local) {
            cb.scope = PMIX_LOCAL;
        } else {
            cb.scope = PMIX_REMOTE;
        }
        cb.copy = false;
        cb.info = cd->info;
        cb.ninfo = cd->ninfo;
        cb.key = key;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        /* A local client may send a get request concurrently with
         * a commit request from another client, but the server may
         * have processed the commit request earlyer than the get
         * request. In this case, we create a local tracker for
         * possibly existing keys that are added with the completed
         * commit request. Thus, the get request will be pended in
         * tracker and will be deffered. This scenario is possible
         * when the non-fence commit-get scheme is used and when
         * the peer GDS component is `dstore`.
         * Checking the peer storage for local keys to avoid creating
         * a local tracker for existing keys. */
        if ((PMIX_SUCCESS != rc) && local) {
            PMIX_GDS_FETCH_KV(rc, cd->peer, &cb);
            if (PMIX_SUCCESS == rc) {
                cbfunc(rc, NULL, 0, cbdata, NULL, NULL);
                PMIX_DESTRUCT(&cb);
                return rc;
            }
        }
        PMIX_DESTRUCT(&cb);  // does not release info or key
        if (PMIX_SUCCESS != rc) {
            /* if the target proc is local, then we just need to wait */
            if (local) {
                /* if they provided a timeout, we need to execute it here
                 * as we are not going to pass it upwards for the host
                 * to perform - we default it to 2 sec */
                if (0 == tv.tv_sec) {
                    tv.tv_sec = 2;
                }
                rc = defer_response(nspace, rank, cd, localonly, cbfunc, cbdata, &tv, &lcd);
                if (PMIX_ERR_NOT_FOUND == rc) {
                    /* just means we created a tracker */
                    rc = PMIX_SUCCESS;
                } else if (PMIX_ERR_NOT_AVAILABLE == rc) {
                    /* means they requested "immediate" */
                    rc = PMIX_ERR_NOT_FOUND;
                }
                return rc;
            }
            /* otherwise, we need to request the info */
            goto request;
        }
        /* we did find it, so go ahead and collect the payload */
    } else if (PMIX_PEER_IS_EARLIER(pmix_client_globals.myserver, 4, 0, 0)) {
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        rc = get_job_data(nspace, cd, &pbkt);
        if (PMIX_SUCCESS != rc) {
            PMIX_DESTRUCT(&pbkt);
            return rc;
        }
        /* pass it back */
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
        cbfunc(rc, data, sz, cbdata, relfn, data);
        return rc;
    }

    /* check if the nspace of the requestor is different from
     * the nspace of the target process */
    diffnspace = !PMIX_CHECK_NSPACE(nptr->nspace, cd->peer->info->pname.nspace);

    if (!scope_given) {
        if (PMIX_RANK_UNDEF == rank || diffnspace) {
            scope = PMIX_GLOBAL;
        } else if (local) {
            scope = PMIX_LOCAL;
        } else {
            scope = PMIX_REMOTE;
        }
    }

    /* since everyone has registered, see if we already have this data */
    rc = _satisfy_request(nptr, rank, cd, diffnspace, scope, cbfunc, cbdata);
    if (PMIX_SUCCESS == rc) {
        /* return success as the satisfy_request function
         * calls the cbfunc for us, and it will have
         * released the cbdata object */
        return PMIX_SUCCESS;
    }

    pmix_output_verbose(2, pmix_server_globals.get_output,
                        "%s:%d DATA NOT FOUND",
                        pmix_globals.myid.nspace,
                        pmix_globals.myid.rank);

  request:
    /* setup to handle this remote request, but don't set any timeout as
     * this might create a race condition with our host if they also
     * support the timeout */
    rc = defer_response(nspace, rank, cd, localonly, cbfunc, cbdata, NULL, &lcd);
    if (PMIX_SUCCESS == rc) {
       /* we are already waiting for the data - nothing more
        * for us to do as the function added the new request
        * to the tracker for us */
       return PMIX_SUCCESS;
    } else if (PMIX_ERR_NOT_AVAILABLE == rc) {
        /* means they requested "immediate" */
        return PMIX_ERR_NOT_FOUND;
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
        if (NULL != key) {
            sz = cd->ninfo;
            PMIX_INFO_CREATE(info, sz+1);
            for (n=0; n < sz; n++) {
                PMIX_INFO_XFER(&info[n], &cd->info[n]);
            }
            PMIX_INFO_LOAD(&info[sz], PMIX_REQUIRED_KEY, key, PMIX_STRING);
            if (NULL != cd->info) {
                PMIX_INFO_FREE(cd->info, cd->ninfo);
            }
            cd->info = info;
            cd->ninfo = sz + 1;
        }
        rc = pmix_host_server.direct_modex(&lcd->proc, cd->info, cd->ninfo, dmdx_cbfunc, lcd);
        if (PMIX_SUCCESS != rc) {
            /* may have a function entry but not support the request */
            pmix_list_remove_item(&pmix_server_globals.local_reqs, &lcd->super);
            PMIX_RELEASE(lcd);
        }
    } else {
        pmix_output_verbose(2, pmix_server_globals.get_output,
                            "%s:%d NO SERVER SUPPORT",
                            pmix_globals.myid.nspace,
                            pmix_globals.myid.rank);
        /* if we don't have direct modex feature, just respond with "not found" */
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
                                          pmix_dmdx_local_t **ld,
                                          pmix_dmdx_request_t **rq)
{
    pmix_dmdx_local_t *lcd, *cd;
    pmix_dmdx_request_t *req;
    pmix_status_t rc;
    size_t n;

    /* define default */
    *ld = NULL;
    *rq = NULL;

    /* see if we already have an existing request for data
     * from this namespace/rank */
    lcd = NULL;
    PMIX_LIST_FOREACH(cd, &pmix_server_globals.local_reqs, pmix_dmdx_local_t) {
        if (!PMIX_CHECK_NSPACE(nspace, cd->proc.nspace) || rank != cd->proc.rank ) {
            continue;
        }
        lcd = cd;
        break;
    }
    if (NULL != lcd) {
        /* we already have a request, so just track that someone
         * else wants data from the same target */
        rc = PMIX_SUCCESS; // indicates we found an existing request
        PMIX_RETAIN(lcd);
        goto complete;
    }
    /* we do not have an existing request, so let's create
     * one and add it to our list */
    lcd = PMIX_NEW(pmix_dmdx_local_t);
    if (NULL == lcd){
        return PMIX_ERR_NOMEM;
    }
    PMIX_LOAD_PROCID(&lcd->proc, nspace, rank);
    if (0 < ninfo) {
        lcd->ninfo = ninfo;
        PMIX_INFO_CREATE(lcd->info, lcd->ninfo);
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&lcd->info[n], &info[n]);
        }
    }
    pmix_list_append(&pmix_server_globals.local_reqs, &lcd->super);
    rc = PMIX_ERR_NOT_FOUND;  // indicates that we created a new request tracker

  complete:
    /* track this specific requester so we return the
     * data to them */
    req = PMIX_NEW(pmix_dmdx_request_t);
    if (NULL == req) {
        *ld = lcd;
        return PMIX_ERR_NOMEM;
    }
    PMIX_RETAIN(lcd);
    req->lcd = lcd;
    req->cbfunc = cbfunc;
    pmix_list_append(&lcd->loc_reqs, &req->super);
    /* if provided, the cbdata is always a pmix_server_caddy_t. Since
     * it will be released by every req when it completes, we have to
     * up the refcount on it to avoid multiple free's of its contents */
    if (NULL != cbdata && 1 < pmix_list_get_size(&lcd->loc_reqs)) {
        PMIX_RETAIN(cbdata);
    }
    req->cbdata = cbdata;
    *ld = lcd;
    *rq = req;
    return rc;
}

void pmix_pending_nspace_requests(pmix_namespace_t *nptr)
{
    pmix_dmdx_local_t *cd, *cd_next;
    pmix_status_t rc;

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

        PMIX_LIST_FOREACH(info, &nptr->ranks, pmix_rank_info_t) {
            if (info->pname.rank == cd->proc.rank) {
                found = true;  // we will satisy this request upon commit from new proc
                break;
            }
        }

        /* if not found - this is remote process and we need to send
         * corresponding direct modex request */
        if (!found){
            rc = PMIX_ERR_NOT_SUPPORTED;
            if (NULL != pmix_host_server.direct_modex){
                rc = pmix_host_server.direct_modex(&cd->proc, cd->info, cd->ninfo, dmdx_cbfunc, cd);
            }
            if (PMIX_SUCCESS != rc) {
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

static pmix_status_t get_job_data(char *nspace,
                                  pmix_server_caddy_t *cd,
                                  pmix_buffer_t *pbkt)
{
    pmix_status_t rc;
    pmix_buffer_t pkt;
    pmix_proc_t proc;
    pmix_cb_t cb;
    pmix_byte_object_t bo;

    PMIX_LOAD_PROCID(&proc, nspace, PMIX_RANK_WILDCARD);
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    /* this data is requested by a local client, so give the gds the option
     * of returning a copy of the data, or a pointer to
     * local storage */
    cb.proc = &proc;
    cb.scope = PMIX_INTERNAL;
    cb.copy = false;
    cb.info = cd->info;
    cb.ninfo = cd->ninfo;
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
    cb.info = NULL;
    cb.ninfo = 0;
    if (PMIX_SUCCESS == rc) {
        PMIX_CONSTRUCT(&pkt, pmix_buffer_t);
        /* assemble the provided data into a byte object */
        PMIX_GDS_ASSEMB_KVS_REQ(rc, pmix_globals.mypeer, &proc, &cb.kvs, &pkt, cd);
        if (rc != PMIX_SUCCESS) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&pkt);
            PMIX_DESTRUCT(&pbkt);
            PMIX_DESTRUCT(&cb);
            return rc;
        }
        if (PMIX_PEER_IS_V1(cd->peer)) {
            /* if the client is using v1, then it expects the
             * data returned to it as the rank followed by abyte object containing
             * a buffer - so we have to do a little gyration */
            pmix_buffer_t xfer;
            PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
            PMIX_BFROPS_PACK(rc, cd->peer, &xfer, &pkt, 1, PMIX_BUFFER);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&pkt);
                PMIX_DESTRUCT(&xfer);
                PMIX_DESTRUCT(&cb);
                return rc;
            }
            PMIX_UNLOAD_BUFFER(&xfer, bo.bytes, bo.size);
            PMIX_DESTRUCT(&xfer);
        } else {
            PMIX_UNLOAD_BUFFER(&pkt, bo.bytes, bo.size);
        }
        PMIX_DESTRUCT(&pkt);
        /* pack it for transmission */
        PMIX_BFROPS_PACK(rc, cd->peer, pbkt, &bo, 1, PMIX_BYTE_OBJECT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&cb);
            return rc;
        }
    }
    PMIX_DESTRUCT(&cb);

    return PMIX_SUCCESS;
}

static pmix_status_t _satisfy_request(pmix_namespace_t *nptr, pmix_rank_t rank,
                                      pmix_server_caddy_t *cd,
                                      bool diffnspace, pmix_scope_t scope,
                                      pmix_modex_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    bool found = false;
    pmix_buffer_t pbkt, pkt;
    pmix_proc_t proc;
    pmix_cb_t cb;
    pmix_byte_object_t bo;
    char *data = NULL;
    size_t sz = 0;

    pmix_output_verbose(2, pmix_server_globals.get_output,
                        "%s:%d SATISFY REQUEST CALLED FOR %s:%d",
                        pmix_globals.myid.nspace,
                        pmix_globals.myid.rank,
                        nptr->nspace, rank);

    PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
    PMIX_LOAD_NSPACE(proc.nspace, nptr->nspace);

    /* if the rank is WILDCARD or the target is in an nspace different
     * from the requester, include a copy of the job-level data */
    if (PMIX_RANK_WILDCARD == rank || diffnspace) {
        rc = get_job_data(nptr->nspace, cd, &pbkt);
        if (PMIX_SUCCESS != rc) {
            PMIX_DESTRUCT(&pbkt);
            return rc;
        }
    }

    /* retrieve the data for the specific rank they are asking about */
    proc.rank = rank;
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    /* this is a local request, so give the gds the option
     * of returning a copy of the data, or a pointer to
     * local storage */
    cb.proc = &proc;
    cb.scope = scope;
    cb.copy = false;
    cb.info = cd->info;
    cb.ninfo = cd->ninfo;
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
    cb.info = NULL;
    cb.ninfo = 0;
    if (PMIX_SUCCESS == rc) {
        found = true;
        PMIX_CONSTRUCT(&pkt, pmix_buffer_t);
        /* assemble the provided data into a byte object */
        if (PMIX_RANK_UNDEF == rank || diffnspace) {
            PMIX_GDS_ASSEMB_KVS_REQ(rc, pmix_globals.mypeer, &proc, &cb.kvs, &pkt, cd);
        } else {
            PMIX_GDS_ASSEMB_KVS_REQ(rc, cd->peer, &proc, &cb.kvs, &pkt, cd);
        }
        if (rc != PMIX_SUCCESS) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&pkt);
            PMIX_DESTRUCT(&pbkt);
            PMIX_DESTRUCT(&cb);
            return rc;
        }
        if (PMIX_PEER_IS_V1(cd->peer)) {
            /* if the client is using v1, then it expects the
             * data returned to it in a different order than v2
             * - so we have to do a little gyration */
            /* pack the rank */
            PMIX_BFROPS_PACK(rc, cd->peer, &pbkt, &rank, 1, PMIX_PROC_RANK);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&pkt);
                PMIX_DESTRUCT(&pbkt);
                PMIX_DESTRUCT(&cb);
                return rc;
            }
            /* now pack the data itself as a buffer */
            PMIX_BFROPS_PACK(rc, cd->peer, &pbkt, &pkt, 1, PMIX_BUFFER);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&pkt);
                PMIX_DESTRUCT(&pbkt);
                PMIX_DESTRUCT(&cb);
                return rc;
            }
            PMIX_DESTRUCT(&pkt);
        } else {
            PMIX_UNLOAD_BUFFER(&pkt, bo.bytes, bo.size);
            PMIX_DESTRUCT(&pkt);
            /* pack it for transmission */
            PMIX_BFROPS_PACK(rc, cd->peer, &pbkt, &bo, 1, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DESTRUCT(&pbkt);
                PMIX_DESTRUCT(&cb);
                return rc;
            }
        }
    }
    PMIX_DESTRUCT(&cb);

    PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
    PMIX_DESTRUCT(&pbkt);

    if (found) {
        /* pass it back */
        cbfunc(rc, data, sz, cbdata, relfn, data);
        return rc;
    }

    return PMIX_ERR_NOT_FOUND;
}

/* Resolve pending requests to this namespace/rank */
pmix_status_t pmix_pending_resolve(pmix_namespace_t *nptr, pmix_rank_t rank,
                                   pmix_status_t status, pmix_dmdx_local_t *lcd)
{
    pmix_dmdx_local_t *cd, *ptr;
    pmix_dmdx_request_t *req, *rnext;
    pmix_server_caddy_t scd;

    /* find corresponding request (if exists) */
    if (NULL == lcd) {
        ptr = NULL;
        if (NULL != nptr) {
            PMIX_LIST_FOREACH(cd, &pmix_server_globals.local_reqs, pmix_dmdx_local_t) {
                if (!PMIX_CHECK_NSPACE(nptr->nspace, cd->proc.nspace) ||
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

    /* if there are no local reqs on this request (e.g., only
     * one proc requested it and that proc has died), then
     * just remove the request */
    if (0 == pmix_list_get_size(&ptr->loc_reqs)) {
        goto cleanup;
    }

    /* somebody was interested in this rank */
    if (PMIX_SUCCESS != status){
        /* if we've got an error for this request - just forward it*/
        PMIX_LIST_FOREACH(req, &ptr->loc_reqs, pmix_dmdx_request_t) {
            req->cbfunc(status, NULL, 0, req->cbdata, NULL, NULL);
        }
    } else if (NULL != nptr) {
        /* if we've got the blob - try to satisfy requests */
        /* run through all the requests for this rank */
        /* this info is going back to one of our peers, so provide a server
         * caddy with our peer in it so the data gets packed correctly */
        PMIX_CONSTRUCT(&scd, pmix_server_caddy_t);
        PMIX_RETAIN(pmix_globals.mypeer);
        scd.peer = pmix_globals.mypeer;
        PMIX_LIST_FOREACH(req, &ptr->loc_reqs, pmix_dmdx_request_t) {
            pmix_status_t rc;
            bool diffnspace = !PMIX_CHECK_NSPACE(nptr->nspace, req->lcd->proc.nspace);
            rc = _satisfy_request(nptr, rank, &scd, diffnspace, PMIX_REMOTE, req->cbfunc, req->cbdata);
            if( PMIX_SUCCESS != rc ){
                /* if we can't satisfy this particular request (missing key?) */
                req->cbfunc(rc, NULL, 0, req->cbdata, NULL, NULL);
            }
        }
        PMIX_DESTRUCT(&scd);
    }

  cleanup:
    /* remove all requests to this rank and cleanup the corresponding structure */
    pmix_list_remove_item(&pmix_server_globals.local_reqs, &ptr->super);
    /* the dmdx request is linked back to its local request for ease
     * of lookup upon return from the server. However, this means that
     * the refcount of the local request has been increased by the number
     * dmdx requests attached to it. In order to release the local request's
     * storage, we first have to drive the refcount down by releasing all
     * of the associated dmdx requests */
    PMIX_LIST_FOREACH_SAFE(req, rnext, &ptr->loc_reqs, pmix_dmdx_request_t) {
        pmix_list_remove_item(&ptr->loc_reqs, &req->super);
        PMIX_RELEASE(req);  // decrements refcount of ptr
    }
    PMIX_RELEASE(ptr);

    return PMIX_SUCCESS;
}

/* process the returned data from the host RM server */
static void _process_dmdx_reply(int fd, short args, void *cbdata)
{
    pmix_dmdx_reply_caddy_t *caddy = (pmix_dmdx_reply_caddy_t *)cbdata;
    pmix_server_caddy_t *cd;
    pmix_peer_t *peer;
    pmix_rank_info_t *rinfo;
    int32_t cnt;
    pmix_kval_t *kv;
    pmix_namespace_t *ns, *nptr;
    pmix_status_t rc;
    pmix_list_t nspaces;
    pmix_nspace_caddy_t *nm;
    pmix_dmdx_request_t *dm;
    bool found;
    pmix_buffer_t pbkt;
    pmix_cb_t cb;

    PMIX_ACQUIRE_OBJECT(caddy);

    pmix_output_verbose(2, pmix_server_globals.get_output,
                    "[%s:%d] process dmdx reply from %s:%u",
                    __FILE__, __LINE__,
                    caddy->lcd->proc.nspace, caddy->lcd->proc.rank);

    /* find the nspace object for the proc whose data is being received */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_namespace_t) {
        if (PMIX_CHECK_NSPACE(caddy->lcd->proc.nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }

    if (NULL == nptr) {
        /* We may not have this namespace because there are no local
         * processes from it running on this host - so just record it
         * so we know we have the data for any future requests */
        nptr = PMIX_NEW(pmix_namespace_t);
        nptr->nspace = strdup(caddy->lcd->proc.nspace);
        /* add to the list */
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }

    /* if the request was successfully satisfied, then store the data.
     * Although we could immediately
     * resolve any outstanding requests on our tracking list, we instead
     * store the data first so we can immediately satisfy any future
     * requests. Then, rather than duplicate the resolve code here, we
     * will let the pmix_pending_resolve function go ahead and retrieve
     * it from the GDS
     *
     * NOTE: if the data returned is NULL, then it has already been
     * stored (e.g., via a register_nspace call in response to a request
     * for job-level data). For now, we will retrieve it so it can
     * be stored for each peer */
    if (PMIX_SUCCESS == caddy->status) {
        /* cycle across all outstanding local requests and collect their
         * unique nspaces so we can store this for each one */
        PMIX_CONSTRUCT(&nspaces, pmix_list_t);
        PMIX_LIST_FOREACH(dm, &caddy->lcd->loc_reqs, pmix_dmdx_request_t) {
            /* this is a local proc that has requested this data - search
             * the list of nspace's and see if we already have it */
            cd = (pmix_server_caddy_t*)dm->cbdata;
            found = false;
            PMIX_LIST_FOREACH(nm, &nspaces, pmix_nspace_caddy_t) {
                if (PMIX_CHECK_NSPACE(nm->ns->nspace, cd->peer->nptr->nspace)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                /* add it */
                nm = PMIX_NEW(pmix_nspace_caddy_t);
                PMIX_RETAIN(cd->peer->nptr);
                nm->ns = cd->peer->nptr;
                pmix_list_append(&nspaces, &nm->super);
            }
        }
        /* now go thru each unique nspace and store the data using its
         * assigned GDS component - note that if the nspace of the requesting
         * proc is different from the nspace of the proc whose data is being
         * returned, then we have to store it into our hash tables */
        PMIX_LIST_FOREACH(nm, &nspaces, pmix_nspace_caddy_t) {
            if (NULL == nm->ns->compat.gds || 0 == nm->ns->nlocalprocs ||
                !PMIX_CHECK_NSPACE(nptr->nspace, nm->ns->nspace)) {
                peer = pmix_globals.mypeer;
            } else {
                /* there must be at least one local proc */
                rinfo = (pmix_rank_info_t*)pmix_list_get_first(&nm->ns->ranks);
                peer = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, rinfo->peerid);
            }
            PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
            if (NULL == caddy->data) {
                if (peer != pmix_globals.mypeer) {
                    /* we assume that the data was provided via a call to
                     * register_nspace, so what we need to do now is simply
                     * transfer it across to the individual nspace storage
                     * components */
                    PMIX_CONSTRUCT(&cb, pmix_cb_t);
                    PMIX_PROC_CREATE(cb.proc, 1);
                    if (NULL == cb.proc) {
                        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                        PMIX_DESTRUCT(&cb);
                        goto complete;
                    }
                    PMIX_LOAD_PROCID(cb.proc, nm->ns->nspace, PMIX_RANK_WILDCARD);
                    cb.scope = PMIX_INTERNAL;
                    cb.copy = false;
                    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        goto complete;
                    }
                    PMIX_LIST_FOREACH(kv, &cb.kvs, pmix_kval_t) {
                        PMIX_GDS_STORE_KV(rc, peer, &caddy->lcd->proc, PMIX_INTERNAL, kv);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_ERROR_LOG(rc);
                            break;
                        }
                    }
                    PMIX_DESTRUCT(&cb);
                }
            } else {
                PMIX_LOAD_BUFFER(pmix_globals.mypeer, &pbkt, caddy->data, caddy->ndata);
                /* unpack and store it*/
                kv = PMIX_NEW(pmix_kval_t);
                cnt = 1;
                PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &pbkt, kv, &cnt, PMIX_KVAL);
                while (PMIX_SUCCESS == rc) {
                    if (caddy->lcd->proc.rank == PMIX_RANK_WILDCARD) {
                        PMIX_GDS_STORE_KV(rc, peer, &caddy->lcd->proc, PMIX_INTERNAL, kv);
                    } else {
                        PMIX_GDS_STORE_KV(rc, peer, &caddy->lcd->proc, PMIX_REMOTE, kv);
                    }
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        caddy->status = rc;
                        goto complete;
                    }
                    PMIX_RELEASE(kv);
                    kv = PMIX_NEW(pmix_kval_t);
                    cnt = 1;
                    PMIX_BFROPS_UNPACK(rc, pmix_globals.mypeer, &pbkt, kv, &cnt, PMIX_KVAL);
                }
                PMIX_RELEASE(kv);
                pbkt.base_ptr = NULL;  // protect the data
                PMIX_DESTRUCT(&pbkt);
                if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
                    PMIX_ERROR_LOG(rc);
                    caddy->status = rc;
                    goto complete;
                }
            }
        }
        PMIX_LIST_DESTRUCT(&nspaces);
    }

  complete:
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
    pmix_output_verbose(2, pmix_server_globals.get_output,
                        "[%s:%d] queue dmdx reply for %s:%u",
                        __FILE__, __LINE__,
                        caddy->lcd->proc.nspace, caddy->lcd->proc.rank);
    PMIX_THREADSHIFT(caddy, _process_dmdx_reply);
}

static void get_timeout(int sd, short args, void *cbdata)
{
    pmix_dmdx_request_t *req = (pmix_dmdx_request_t*)cbdata;

    pmix_output_verbose(2, pmix_server_globals.get_output,
                        "ALERT: get timeout fired");
    /* execute the provided callback function with the error */
    if (NULL != req->cbfunc) {
        req->cbfunc(PMIX_ERR_TIMEOUT, NULL, 0, req->cbdata, NULL, NULL);
    }
    req->event_active = false;
    pmix_list_remove_item(&req->lcd->loc_reqs, &req->super);
    PMIX_RELEASE(req);
}
