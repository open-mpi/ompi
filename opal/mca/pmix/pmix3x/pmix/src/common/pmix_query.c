/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "src/include/pmix_config.h"

#include "src/include/pmix_stdint.h"
#include "src/include/pmix_socket_errno.h"

#include "include/pmix.h"
#include "include/pmix_common.h"
#include "include/pmix_server.h"

#include "src/threads/threads.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/name_fns.h"
#include "src/util/output.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/ptl/base/base.h"

#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

static void relcbfunc(void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query release callback");

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}
static void query_cbfunc(struct pmix_peer_t *peer,
                         pmix_ptl_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    pmix_status_t rc;
    pmix_shift_caddy_t *results;
    int cnt;
    size_t n;
    pmix_kval_t *kv;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server");

    results = PMIX_NEW(pmix_shift_caddy_t);

    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &results->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        results->status = rc;
        goto complete;
    }
    if (PMIX_SUCCESS != results->status) {
        goto complete;
    }

    /* unpack any returned data */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &results->ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        results->status = rc;
        goto complete;
    }
    if (0 < results->ninfo) {
        PMIX_INFO_CREATE(results->info, results->ninfo);
        cnt = results->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, results->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            results->status = rc;
            goto complete;
        }
        /* locally cache the results */
        for (n=0; n < results->ninfo; n++) {
            kv = PMIX_NEW(pmix_kval_t);
            kv->key = strdup(results->info[n].key);
            PMIX_VALUE_CREATE(kv->value, 1);
            PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer,
                                   kv->value, &results->info[n].value);

            PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer,
                              &pmix_globals.myid, PMIX_INTERNAL,
                              kv);
            PMIX_RELEASE(kv);  // maintain accounting
        }
    }

  complete:
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server releasing with status %s", PMIx_Error_string(results->status));
    /* release the caller */
    if (NULL != cd->cbfunc) {
        cd->cbfunc(results->status, results->info, results->ninfo, cd->cbdata, relcbfunc, results);
    }
    PMIX_RELEASE(cd);
}

static pmix_status_t request_help(pmix_query_t queries[], size_t nqueries,
                                  pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_query_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_QUERY_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc;

    /* if we are the server, then we just issue the query and
     * return the response */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        if (NULL == pmix_host_server.query) {
            /* nothing we can do */
            return PMIX_ERR_NOT_SUPPORTED;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query handed to RM");
        rc = pmix_host_server.query(&pmix_globals.myid,
                                    queries, nqueries,
                                    cbfunc, cbdata);
        return rc;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* if we are a client, then relay this request to the server */
    cd = PMIX_NEW(pmix_query_caddy_t);
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;
    msg = PMIX_NEW(pmix_buffer_t);
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd);
        return rc;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &nqueries, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd);
        return rc;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, queries, nqueries, PMIX_QUERY);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd);
        return rc;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query sending to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, query_cbfunc, (void*)cd);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cd);
    }
    return rc;

}

static void _local_relcb(void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}

static void localquery(int sd, short args, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    pmix_query_t *queries = cd->queries;
    size_t nqueries = cd->nqueries;
    pmix_status_t rc;
    pmix_cb_t cb;
    size_t n, p;
    pmix_list_t results;
    pmix_kval_t *kv, *kvnxt;
    pmix_proc_t proc;
    bool rank_given = false;

    /* setup the list of local results */
    PMIX_CONSTRUCT(&results, pmix_list_t);

    for (n=0; n < nqueries; n++) {
        PMIX_LOAD_PROCID(&proc, NULL, PMIX_RANK_INVALID);
        for (p=0; p < queries[n].nqual; p++) {
            if (PMIX_CHECK_KEY(&queries[n].qualifiers[p], PMIX_PROCID)) {
                PMIX_LOAD_NSPACE(proc.nspace, queries[n].qualifiers[p].value.data.proc->nspace);
                proc.rank = queries[n].qualifiers[p].value.data.proc->rank;
                rank_given = true;
            } else if (PMIX_CHECK_KEY(&queries[n].qualifiers[p], PMIX_NSPACE)) {
                PMIX_LOAD_NSPACE(proc.nspace, queries[n].qualifiers[p].value.data.string);
            } else if (PMIX_CHECK_KEY(&queries[n].qualifiers[p], PMIX_RANK)) {
                proc.rank = queries[n].qualifiers[p].value.data.rank;
                rank_given = true;
            }
        }

        /* first try a local "get" on the data to see if we already have it */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.copy = false;
        /* if they are querying about node or app values not directly
         * associated with a proc (i.e., they didn't specify the proc),
         * then we obtain those by leaving the proc info as undefined */
        if (!rank_given) {
            proc.rank = PMIX_RANK_UNDEF;
            cb.proc = &proc;
        } else {
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
        }

        /* first see if we already have this info */
        for (p=0; NULL != queries[n].keys[p]; p++) {
            cb.key = queries[n].keys[p];
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
            if (PMIX_SUCCESS != rc) {
                /* not in our gds */
                PMIX_DESTRUCT(&cb);
                goto nextstep;
            }
            /* need to retain this result */
            PMIX_LIST_FOREACH_SAFE(kv, kvnxt, &cb.kvs, pmix_kval_t) {
                pmix_list_remove_item(&cb.kvs, &kv->super);
                pmix_list_append(&results, &kv->super);
            }
            PMIX_DESTRUCT(&cb);
        }
    }

  nextstep:
    if (PMIX_OPERATION_SUCCEEDED == rc) {
        /* if we get here, then all queries were locally
         * resolved, so construct the results for return */
        cd->status = PMIX_SUCCESS;
        cd->ninfo = pmix_list_get_size(&results);
        if (0 < cd->ninfo) {
            PMIX_INFO_CREATE(cd->info, cd->ninfo);
            n = 0;
            PMIX_LIST_FOREACH_SAFE(kv, kvnxt, &results, pmix_kval_t) {
                PMIX_LOAD_KEY(cd->info[n].key, kv->key);
                rc = pmix_value_xfer(&cd->info[n].value, kv->value);
                if (PMIX_SUCCESS != rc) {
                    cd->status = rc;
                    PMIX_INFO_FREE(cd->info, cd->ninfo);
                    break;
                }
                ++n;
            }
        }
        /* done with the list of results */
        PMIX_LIST_DESTRUCT(&results);

        if (NULL != cd->cbfunc) {
            cd->cbfunc(cd->status, cd->info, cd->ninfo, cd->cbdata, _local_relcb, cd);
        }
    } else if (PMIX_SUCCESS != rc) {
        /* need to ask our host */
        rc = request_help(cd->queries, cd->nqueries, cd->cbfunc, cd->cbdata);
        if (PMIX_SUCCESS != rc) {
            /* we have to return the error to the caller */
            if (NULL != cd->cbfunc) {
                cd->cbfunc(rc, NULL, 0, cd->cbdata, NULL, NULL);
            }
        }
        PMIX_RELEASE(cd);
        return;
    }

    /* get here if the query returned PMIX_SUCCESS, which means
     * that the query is being processed and will call the cbfunc
     * when complete */
}

PMIX_EXPORT pmix_status_t PMIx_Query_info_nb(pmix_query_t queries[], size_t nqueries,
                                             pmix_info_cbfunc_t cbfunc, void *cbdata)

{
    pmix_query_caddy_t *cd;
    pmix_status_t rc;
    size_t n, p;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query non-blocking");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    if (0 == nqueries || NULL == queries) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* do a quick check of the qualifiers arrays to ensure
     * the nqual field has been set */
    for (n=0; n < nqueries; n++) {
        if (NULL != queries[n].qualifiers && 0 == queries[n].nqual) {
            /* look for the info marked as "end" */
            p = 0;
            while (!(PMIX_INFO_IS_END(&queries[n].qualifiers[p])) && p < SIZE_MAX) {
                ++p;
            }
            if (SIZE_MAX == p) {
                /* nothing we can do */
                return PMIX_ERR_BAD_PARAM;
            }
            queries[n].nqual = p;
        }
    }

    /* check the directives to see if they want us to refresh
     * the local cached results - if we wanted to optimize this
     * more, we would check each query and allow those that don't
     * want to be refreshed to be executed locally, and those that
     * did would be sent to the host. However, for now we simply
     * assume that any requirement to refresh will force all to
     * do so */
    for (n=0; n < nqueries; n++) {
        for (p=0; p < queries[n].nqual; p++) {
            if (PMIX_CHECK_KEY(&queries[n].qualifiers[p], PMIX_QUERY_REFRESH_CACHE)) {
                if (PMIX_INFO_TRUE(&queries[n].qualifiers[p])) {
                    /* need to refresh the cache from our host */
                    rc = request_help(queries, nqueries, cbfunc, cbdata);
                    return rc;
                }
            }
        }
    }

    /* we get here if a refresh isn't required - need to
     * threadshift this to access our internal data */
    cd = PMIX_NEW(pmix_query_caddy_t);
    cd->queries = queries;
    cd->nqueries = nqueries;
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, localquery);
    /* regardless of the result of the query, we return
     * PMIX_SUCCESS here to indicate that the operation
     * was accepted for processing */

    return PMIX_SUCCESS;
}

static void acb(pmix_status_t status,
                pmix_info_t *info, size_t ninfo,
                void *cbdata,
                pmix_release_cbfunc_t release_fn,
                void *release_cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    size_t n;

    cb->status = status;
    if (NULL != info) {
        PMIX_INFO_CREATE(cb->info, ninfo);
        if (NULL == cb->info) {
            cb->status = PMIX_ERR_NOMEM;
            goto done;
        }
        cb->ninfo = ninfo;
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&cb->info[n], &info[n]);
        }
    }

  done:
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    PMIX_WAKEUP_THREAD(&cb->lock);
}

PMIX_EXPORT pmix_status_t PMIx_Allocation_request(pmix_alloc_directive_t directive,
                                                  pmix_info_t *info, size_t ninfo,
                                                  pmix_info_t **results, size_t *nresults)
{
    pmix_cb_t cb;
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s pmix:allocate", PMIX_NAME_PRINT(&pmix_globals.myid));

    /* set the default response */
    *results = NULL;
    *nresults = 0;

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    if (PMIX_SUCCESS != (rc = PMIx_Allocation_request_nb(directive, info, ninfo,
                                                         acb, &cb))) {
        PMIX_DESTRUCT(&cb);
        return rc;
    }

    /* wait for the operation to complete */
    PMIX_WAIT_THREAD(&cb.lock);
    rc = cb.status;
    if (NULL != cb.info) {
        *results = cb.info;
        *nresults = cb.ninfo;
        /* protect the data */
        cb.info = NULL;
        cb.ninfo = 0;
    }
    PMIX_DESTRUCT(&cb);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:allocate completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Allocation_request_nb(pmix_alloc_directive_t directive,
                                                     pmix_info_t *info, size_t ninfo,
                                                     pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_ALLOC_CMD;
    pmix_status_t rc;
    pmix_query_caddy_t *cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: allocate called");

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we are the server, then we just issue the request and
     * return the response */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        if (NULL == pmix_host_server.allocate) {
            /* nothing we can do */
            return PMIX_ERR_NOT_SUPPORTED;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:allocate handed to RM");
        rc = pmix_host_server.allocate(&pmix_globals.myid,
                                       directive,
                                       info, ninfo,
                                       cbfunc, cbdata);
        return rc;
    }

    /* if we are a client, then relay this request to the server */

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }

    /* pack the directive */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &directive, 1, PMIX_ALLOC_DIRECTIVE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }

    /* pack the info */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_query_caddy_t);
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, query_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}
