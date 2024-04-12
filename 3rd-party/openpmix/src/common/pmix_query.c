/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2022 IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "src/include/pmix_config.h"

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_stdint.h"

#include "include/pmix.h"
#include "pmix_common.h"
#include "include/pmix_server.h"

#include "src/common/pmix_attributes.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/pstrg/pstrg.h"
#include "src/mca/ptl/base/base.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"

#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/server/pmix_server_ops.h"

/*
 * Support structure for locally resolving some keys
 * outside of what we send to the server
 */
typedef struct {
    pmix_query_caddy_t super;

    /** Number of locally resolved keys */
    size_t num_local;
    /** Original query list */
    pmix_query_t *orig_queries;
    size_t orig_nqueries;
    /** Original cbfunc/data */
    pmix_info_cbfunc_t orig_cbfunc;
    void *orig_cbdata;
    /** Info returned to the user **/
    pmix_info_t *info;
    size_t num_info;
} pmix_local_query_caddy_t;
PMIX_CLASS_DECLARATION(pmix_local_query_caddy_t);

static void qlcon(pmix_local_query_caddy_t *p) {
    p->num_local = 0;
    p->orig_cbfunc = NULL;
    p->orig_cbdata = NULL;
    p->orig_queries = NULL;
    p->orig_nqueries = 0;
    p->info = NULL;
    p->num_info = 0;
}
static void qldes(pmix_local_query_caddy_t *p) {
    if (NULL != p->super.queries) {
        PMIX_QUERY_RELEASE(p->super.queries);
        p->super.queries = NULL;
    }

    p->num_local = 0;
    p->orig_cbfunc = NULL;
    p->orig_cbdata = NULL;
    p->orig_queries = NULL;
    p->orig_nqueries = 0;
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->num_info);
    }
    p->info = NULL;
    p->num_info = 0;
}
PMIX_CLASS_INSTANCE(pmix_local_query_caddy_t, pmix_query_caddy_t, qlcon, qldes);

static bool pmix_query_check_is_local_resolve(const char *key);
static size_t pmix_query_get_num_local_resolve(pmix_query_t queries[], size_t nqueries);
static int pmix_query_resolve_all_pre_init(pmix_query_t queries[], size_t nqueries,
                                           pmix_info_t **results, size_t *nresults);
void pmix_query_local_resolve_cbfunc(pmix_status_t status,
                                     pmix_info_t *cb_info, size_t ninfo,
                                     void *cbdata,
                                     pmix_release_cbfunc_t release_fn,
                                     void *release_cbdata);
static pmix_query_t * pmix_query_strip_local_keys(pmix_query_t orig_queries[],
                                                  size_t orig_nqueries,
                                                  size_t nqueries);


static void relcbfunc(void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                "pmix:query release callback");

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}
static void query_cbfunc(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;
    pmix_status_t rc;
    pmix_shift_caddy_t *results;
    int cnt;
    size_t n;
    pmix_kval_t *kv;
    PMIX_HIDE_UNUSED_PARAMS(hdr);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server");

    /* a zero-byte buffer indicates that this recv is being
     * completed due to a lost connection */
    if (PMIX_BUFFER_IS_EMPTY(buf)) {
        return;
    }

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
        for (n = 0; n < results->ninfo; n++) {
            kv = PMIX_NEW(pmix_kval_t);
            kv->key = strdup(results->info[n].key);
            PMIX_VALUE_CREATE(kv->value, 1);
            PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer, kv->value, &results->info[n].value);

            PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kv);
            PMIX_RELEASE(kv); // maintain accounting
        }
    }

complete:
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server releasing with status %s",
                        PMIx_Error_string(results->status));
    /* release the caller */
    if (NULL != cd->cbfunc) {
        cd->cbfunc(results->status, results->info, results->ninfo, cd->cbdata, relcbfunc, results);
    }
    PMIX_RELEASE(cd);
}

static void qinfocb(pmix_status_t status, pmix_info_t info[], size_t ninfo, void *cbdata,
                    pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_query_caddy_t *cb = (pmix_query_caddy_t *) cbdata;
    size_t n;

    cb->status = status;
    if (NULL != info) {
        cb->ninfo = ninfo;
        PMIX_INFO_CREATE(cb->info, cb->ninfo);
        for (n = 0; n < ninfo; n++) {
            PMIX_INFO_XFER(&cb->info[n], &info[n]);
        }
    }
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    PMIX_WAKEUP_THREAD(&cb->lock);
}

static pmix_status_t send_for_help(pmix_query_t queries[], size_t nqueries,
                                   pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_query_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_QUERY_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc;

    cd = PMIX_NEW(pmix_query_caddy_t);
    cd->cbfunc = cbfunc;  // the final callback function
    cd->cbdata = cbdata;  // the user's cbdata
    msg = PMIX_NEW(pmix_buffer_t);
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd);
        return rc;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &nqueries, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd);
        return rc;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, queries, nqueries, PMIX_QUERY);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd);
        return rc;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query sending to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, query_cbfunc, (void *) cd);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cd);
    }
    return rc;
}

static void finalstep(pmix_status_t status, pmix_info_t info[], size_t ninfo, void *cbdata,
                      pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    pmix_status_t rc;

    /* if the host satisfied the request, then we are done */
    if (PMIX_SUCCESS == status) {
        if (NULL != cd->cbfunc) {
            cd->cbfunc(status, info, ninfo, cd->cbdata, release_fn, release_cbdata);
            PMIX_RELEASE(cd);
        }
        return;
    } else {
        /* if we are connected, let our server have a try */
        PMIX_ACQUIRE_THREAD(&pmix_global_lock);
        if (!pmix_globals.connected) {
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            /* nothing more we can do */
            if (NULL != cd->cbfunc) {
                cd->cbfunc(status, info, ninfo, cd->cbdata, release_fn, release_cbdata);
                PMIX_RELEASE(cd);
            } else {
                rc = send_for_help(cd->queries, cd->nqueries, cd->cbfunc, cd->cbdata);
                if (PMIX_SUCCESS != rc) {
                    if (NULL != cd->cbfunc) {
                        cd->cbfunc(rc, NULL, 0, cd->cbdata, release_fn, release_cbdata);
                        PMIX_RELEASE(cd);
                    }
                }
            }
            return;
        }
        PMIX_RELEASE_THREAD(&pmix_global_lock);
    }
}

static pmix_status_t request_help(pmix_query_caddy_t *cd)
{
    pmix_local_query_caddy_t *local_cd;
    pmix_status_t rc;
    size_t num_local;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    /* if our host has support, then we just issue the query and
     * return the response - but don't pass it back to the host
     * is the host is a server as that would be a loopback */
    if (!cd->host_called && NULL != pmix_host_server.query) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query handed to RM");
        rc = pmix_host_server.query(&pmix_globals.myid,
                                    cd->queries, cd->nqueries,
                                    finalstep, (void*)cd);
        return rc;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    num_local = pmix_query_get_num_local_resolve(cd->queries, cd->nqueries);
    if( 0 == num_local ) {
        // No locally resolved keys, so send directly to the server
        rc = send_for_help(cd->queries,cd->nqueries, cd->cbfunc, cd->cbdata);
    } else {
        // Some locally resolved keys, so send the subset of non-locally resolved
        // keys to the server. We will patch up the results with the locally
        // resolved keys when they come back from the server in our callback.
        local_cd = PMIX_NEW(pmix_local_query_caddy_t);
        // Save original values
        local_cd->orig_cbfunc = cd->cbfunc;
        local_cd->orig_cbdata = cd->cbdata;
        local_cd->orig_queries = cd->queries;
        local_cd->orig_nqueries = cd->nqueries;
        local_cd->num_local = num_local;
        // Values we are going to send to the server
        local_cd->super.nqueries = cd->nqueries - num_local;
        if (0 < local_cd->super.nqueries) {
            local_cd->super.queries = pmix_query_strip_local_keys(cd->queries, cd->nqueries, cd->nqueries - num_local);
        } else {
            local_cd->super.queries = NULL;
        }
        local_cd->super.cbfunc = pmix_query_local_resolve_cbfunc;
        local_cd->super.cbdata = &local_cd;

        if (0 == local_cd->super.nqueries) {
            // Skip calling the server and just resolve locally
            rc = PMIX_SUCCESS;
            pmix_query_local_resolve_cbfunc(PMIX_SUCCESS, NULL, 0, local_cd, NULL, NULL);
        } else {
            rc = send_for_help(local_cd->super.queries, local_cd->super.nqueries, pmix_query_local_resolve_cbfunc, local_cd);
        }
    }

    return rc;
}

static void _local_relcb(void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}

static void nxtcbfunc(pmix_status_t status, pmix_list_t *results, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;
    size_t n;
    pmix_kval_t *kv, *kvnxt;
    pmix_status_t rc;

    /* if they return success, then all queries were locally
     * resolved, so construct the results for return */
    if (PMIX_SUCCESS == status) {
        cd->status = status;
        cd->ninfo = pmix_list_get_size(results);
        PMIX_INFO_CREATE(cd->info, cd->ninfo);
        n = 0;
        PMIX_LIST_FOREACH_SAFE (kv, kvnxt, results, pmix_kval_t) {
            PMIX_LOAD_KEY(cd->info[n].key, kv->key);
            rc = PMIx_Value_xfer(&cd->info[n].value, kv->value);
            if (PMIX_SUCCESS != rc) {
                cd->status = rc;
                PMIX_INFO_FREE(cd->info, cd->ninfo);
                break;
            }
            ++n;
        }

        if (NULL != cd->cbfunc) {
            cd->cbfunc(cd->status, cd->info, cd->ninfo, cd->cbdata, _local_relcb, cd);
        }
    } else {
        /* need to ask our host */
        rc = request_help(cd);
        if (PMIX_SUCCESS != rc) {
            /* we have to return the error to the caller */
            if (NULL != cd->cbfunc) {
                cd->cbfunc(rc, NULL, 0, cd->cbdata, NULL, NULL);
            }
        }
        cd->queries = NULL;
        cd->nqueries = 0;
        PMIX_RELEASE(cd);
        return;
    }
}

void pmix_parse_localquery(int sd, short args, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t *) cbdata;
    pmix_query_t *queries = cd->queries;
    size_t nqueries = cd->nqueries;
    pmix_status_t rc;
    pmix_cb_t cb;
    size_t n, p;
    pmix_list_t results;
    pmix_kval_t *kv, *kvnxt;
    pmix_proc_t proc;
    bool rank_given = false;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* setup the list of local results */
    PMIX_CONSTRUCT(&results, pmix_list_t);

    for (n = 0; n < nqueries; n++) {
        PMIX_LOAD_PROCID(&proc, NULL, PMIX_RANK_INVALID);
        for (p = 0; p < queries[n].nqual; p++) {
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
            if (PMIX_RANK_INVALID == proc.rank && 0 == strlen(proc.nspace)) {
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
        for (p = 0; NULL != queries[n].keys[p]; p++) {
            cb.key = queries[n].keys[p];
            // Locally resolvable keys
            if (0 == strcmp(queries[n].keys[p], PMIX_QUERY_STABLE_ABI_VERSION)) {
                PMIX_KVAL_NEW(kv, cb.key);
                PMIx_Value_load(kv->value, PMIX_STD_ABI_STABLE_VERSION, PMIX_STRING);
                pmix_list_append(&cb.kvs, &kv->super);
            } else if (0 == strcmp(queries[n].keys[p], PMIX_QUERY_PROVISIONAL_ABI_VERSION)) {
                PMIX_KVAL_NEW(kv, cb.key);
                PMIx_Value_load(kv->value, PMIX_STD_ABI_PROVISIONAL_VERSION, PMIX_STRING);
                pmix_list_append(&cb.kvs, &kv->super);
            } else if (0 == strcmp(queries[n].keys[p], PMIX_QUERY_ATTRIBUTE_SUPPORT)) {
                PMIX_THREADSHIFT(cd, pmix_attrs_query_support);
                return;
            /* check for request to scan the local node for available
             * servers the caller could connect to */
            } else if (0 == strcmp(queries[n].keys[p], PMIX_QUERY_AVAIL_SERVERS)) {
                PMIX_THREADSHIFT(cd, pmix_ptl_base_query_servers);
                return;
            } else {
                PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
                if (PMIX_SUCCESS != rc) {
                    /* not in our gds */
                    PMIX_DESTRUCT(&cb);
                    goto nextstep;
                }
            }
            /* need to retain this result */
            PMIX_LIST_FOREACH_SAFE (kv, kvnxt, &cb.kvs, pmix_kval_t) {
                pmix_list_remove_item(&cb.kvs, &kv->super);
                pmix_list_append(&results, &kv->super);
            }
            PMIX_DESTRUCT(&cb);
            goto complete;
        }
    }

nextstep:
    /* pass the queries thru our active plugins with query
     * interfaces to see if someone can resolve it */
    rc = pmix_pstrg.query(queries, nqueries, &results, nxtcbfunc, cd);
    if (PMIX_OPERATION_SUCCEEDED == rc) {
complete:
        /* if we get here, then all queries were locally
         * resolved, so construct the results for return */
        cd->status = PMIX_SUCCESS;
        cd->ninfo = pmix_list_get_size(&results);
        if (0 < cd->ninfo) {
            PMIX_INFO_CREATE(cd->info, cd->ninfo);
            n = 0;
            PMIX_LIST_FOREACH_SAFE (kv, kvnxt, &results, pmix_kval_t) {
                PMIX_LOAD_KEY(cd->info[n].key, kv->key);
                rc = PMIx_Value_xfer(&cd->info[n].value, kv->value);
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
        /* ask for help */
        rc = request_help(cd);
        if (PMIX_SUCCESS != rc) {
            /* we have to return the error to the caller */
            if (NULL != cd->cbfunc) {
                cd->cbfunc(rc, NULL, 0, cd->cbdata, NULL, NULL);
            }
            cd->queries = NULL;
            cd->nqueries = 0;
        }
        return;
    }

    /* get here if the query returned PMIX_SUCCESS, which means
     * that the query is being processed and will call the cbfunc
     * when complete */
}

PMIX_EXPORT pmix_status_t PMIx_Query_info(pmix_query_t queries[], size_t nqueries,
                                          pmix_info_t **results, size_t *nresults)
{
    pmix_query_caddy_t *cd;
    pmix_status_t rc;
    size_t n, p;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        rc = pmix_query_resolve_all_pre_init(queries, nqueries, results, nresults);
        if (PMIX_SUCCESS == rc) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix:query completed - locally, pre-init");
            return rc;
        } else {
            return PMIX_ERR_INIT;
        }
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "%s pmix:query",
                        PMIX_NAME_PRINT(&pmix_globals.myid));

    if (0 == nqueries || NULL == queries) {
        return PMIX_ERR_BAD_PARAM;
    }
    /* do a quick check of the qualifiers arrays to ensure
     * the nqual field has been set */
    for (n = 0; n < nqueries; n++) {
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

    /* we get here if a refresh isn't required - need to
     * threadshift this to access our internal data */
    cd = PMIX_NEW(pmix_query_caddy_t);
    cd->host_called = true;
    cd->queries = queries;
    cd->nqueries = nqueries;
    cd->cbfunc = qinfocb;
    cd->cbdata = cd;
    PMIX_THREADSHIFT(cd, pmix_parse_localquery);

    /* wait for the operation to complete */
    PMIX_WAIT_THREAD(&cd->lock);
    rc = cd->status;
    if (NULL != cd->info) {
        *results = cd->info;
        *nresults = cd->ninfo;
        cd->info = NULL;
        cd->ninfo = 0;
    }
    PMIX_RELEASE(cd);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Query_info_nb(pmix_query_t queries[], size_t nqueries,
                                             pmix_info_cbfunc_t cbfunc, void *cbdata)

{
    pmix_query_caddy_t *cd;
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
    for (n = 0; n < nqueries; n++) {
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

    /* we get here if a refresh isn't required - need to
     * threadshift this to access our internal data */
    cd = PMIX_NEW(pmix_query_caddy_t);
    cd->host_called = true;
    cd->queries = queries;
    cd->nqueries = nqueries;
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, pmix_parse_localquery);
    /* regardless of the result of the query, we return
     * PMIX_SUCCESS here to indicate that the operation
     * was accepted for processing */

    return PMIX_SUCCESS;
}

static void acb(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    size_t n;

    cb->status = status;
    if (NULL != info) {
        PMIX_INFO_CREATE(cb->info, ninfo);
        if (NULL == cb->info) {
            cb->status = PMIX_ERR_NOMEM;
            goto done;
        }
        cb->ninfo = ninfo;
        for (n = 0; n < ninfo; n++) {
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

    pmix_output_verbose(2, pmix_globals.debug_output, "%s pmix:allocate",
                        PMIX_NAME_PRINT(&pmix_globals.myid));

    /* set the default response */
    *results = NULL;
    *nresults = 0;

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    if (PMIX_SUCCESS != (rc = PMIx_Allocation_request_nb(directive, info, ninfo, acb, &cb))) {
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

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:allocate completed");

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

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix: allocate called");

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we are the server, then we just issue the request and
     * return the response */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) && !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        if (NULL == pmix_host_server.allocate) {
            /* nothing we can do */
            return PMIX_ERR_NOT_SUPPORTED;
        }
        pmix_output_verbose(2, pmix_globals.debug_output, "pmix:allocate handed to RM");
        rc = pmix_host_server.allocate(&pmix_globals.myid, directive, info, ninfo, cbfunc, cbdata);
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
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }

    /* pack the directive */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &directive, 1, PMIX_ALLOC_DIRECTIVE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }

    /* pack the info */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, info, ninfo, PMIX_INFO);
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
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, query_cbfunc, (void *) cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}


/*
 * Determine if the keys is meant to be locally resolved
 */
static bool pmix_query_check_is_local_resolve(const char *key)
{
    if (0 == strcmp(key, PMIX_QUERY_STABLE_ABI_VERSION)) {
        return true;
    }
    else if (0 == strcmp(key, PMIX_QUERY_PROVISIONAL_ABI_VERSION)) {
        return true;
    }
    return false;
}

/*
 * Count the number of keys that are locally resolvable
 */
static size_t pmix_query_get_num_local_resolve(pmix_query_t queries[], size_t nqueries)
{
    size_t num_local = 0;
    size_t n, p;

    for (n = 0; n < nqueries; n++) {
        for (p = 0; NULL != queries[n].keys[p]; p++) {
            if (pmix_query_check_is_local_resolve(queries[n].keys[p])) {
                ++num_local;
            }
        }
    }

    return num_local;
}

/*
 * Called outside of init/finalize. Check to see if we are allowed to resolve
 * the query entirely locally.
 * Query must contain only those keys allowed to be called before init.
 */
static int pmix_query_resolve_all_pre_init(pmix_query_t queries[], size_t nqueries,
                                           pmix_info_t **results, size_t *nresults)
{
    size_t n, p;
    size_t cur_info = 0, num_info = 0;

    // Check to see if this query qualifies.
    num_info = pmix_query_get_num_local_resolve(queries, nqueries);

    // If it does not qualify then reject
    if( num_info != nqueries ) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query Found %d queries of %d queries that cannot be handled before init.",
                            (int)(nqueries - num_info), (int)nqueries);
        return PMIX_ERROR;
    }

    // If it does qualify then fill in the results
    *nresults = num_info;
    PMIX_INFO_CREATE((*results), (*nresults));
    cur_info = 0;
    for (n = 0; n < nqueries; n++) {
        for (p = 0; NULL != queries[n].keys[p]; p++) {
            if (0 == strcmp(queries[n].keys[p], PMIX_QUERY_STABLE_ABI_VERSION)) {
                PMIx_Info_load(&((*results)[cur_info]), PMIX_QUERY_STABLE_ABI_VERSION, PMIX_STD_ABI_STABLE_VERSION, PMIX_STRING);
                ++cur_info;
            }
            else if (0 == strcmp(queries[n].keys[p], PMIX_QUERY_PROVISIONAL_ABI_VERSION)) {
                PMIx_Info_load(&((*results)[cur_info]), PMIX_QUERY_PROVISIONAL_ABI_VERSION, PMIX_STD_ABI_PROVISIONAL_VERSION, PMIX_STRING);
                ++cur_info;
            }
        }
    }

    return PMIX_SUCCESS;
}

static void local_resolve_release_cbfunc(void *cbdata)
{
    pmix_local_query_caddy_t *local_cd = (pmix_local_query_caddy_t*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query local release callback");

    if (NULL != local_cd) {
        PMIX_RELEASE(local_cd);
    }
}

void pmix_query_local_resolve_cbfunc(pmix_status_t status,
                                     pmix_info_t *info, size_t ninfo,
                                     void *cbdata,
                                     pmix_release_cbfunc_t release_fn,
                                     void *release_cbdata)
{
    pmix_local_query_caddy_t *local_cd = (pmix_local_query_caddy_t*)cbdata;
    size_t n, p, n_idx, p_idx;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query local resolve callback (ninfo %d, local %d)",
                        (int)ninfo, (int)local_cd->num_local);

    local_cd->num_info = ninfo + local_cd->num_local;
    PMIX_INFO_CREATE(local_cd->info, local_cd->num_info);

    // Copy values from the server
    for (n_idx = 0; n_idx < ninfo; ++n_idx) {
        PMIx_Info_xfer(&local_cd->info[n_idx], &info[n_idx]);
    }

    // Append the locally resolved values
    for (n = 0; n < local_cd->orig_nqueries; n++) {
        p_idx = 0;
        for (p = 0; NULL != local_cd->orig_queries[n].keys[p]; p++) {
            if (0 == strcmp(local_cd->orig_queries[n].keys[p], PMIX_QUERY_STABLE_ABI_VERSION)) {
                PMIx_Info_load(&local_cd->info[n_idx], local_cd->orig_queries[n].keys[p],
                               PMIX_STD_ABI_STABLE_VERSION, PMIX_STRING);
                ++p_idx;
            }
            else if (0 == strcmp(local_cd->orig_queries[n].keys[p], PMIX_QUERY_PROVISIONAL_ABI_VERSION)) {
                PMIx_Info_load(&local_cd->info[n_idx], local_cd->orig_queries[n].keys[p],
                               PMIX_STD_ABI_PROVISIONAL_VERSION, PMIX_STRING);
                ++p_idx;
            }
        }
        if( p_idx > 0 ) {
            ++n_idx;
        }
    }

    // We copied the server contents into a new info list, so
    // release what was passed to us.
    if( NULL != release_fn ) {
        release_fn(release_cbdata);
    }

    local_cd->orig_cbfunc(status, local_cd->info, local_cd->num_info,
                          local_cd->orig_cbdata, local_resolve_release_cbfunc, cbdata);
}

static pmix_query_t * pmix_query_strip_local_keys(pmix_query_t orig_queries[],
                                                  size_t orig_nqueries,
                                                  size_t nqueries)
{
    int rc = PMIX_SUCCESS;
    size_t n, p, n_idx, p_idx;
    pmix_query_t *queries;

    PMIX_QUERY_CREATE(queries, nqueries);
    n_idx = 0;
    for (n = 0; n < orig_nqueries; n++) {
        p_idx = 0;
        for (p = 0; NULL != orig_queries[n].keys[p]; p++) {
            if (!pmix_query_check_is_local_resolve(orig_queries[n].keys[p])) {
                PMIX_ARGV_APPEND(rc, queries[n_idx].keys, orig_queries[n].keys[p]);
                if (PMIX_SUCCESS != rc) {
                    goto out;
                }
                ++p_idx;
            }
        }
        if( p_idx > 0 ) {
            ++n_idx;
        }
    }
out:
    if (PMIX_SUCCESS != rc) {
        PMIX_QUERY_RELEASE(queries);
        // Note that queries is set to NULL by PMIX_QUERY_RELEASE.
    }
    return queries;
}
