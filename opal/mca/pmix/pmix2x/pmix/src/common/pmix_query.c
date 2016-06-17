/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2016 Intel, Inc.  All rights reserved.
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
#include <pmix/autogen/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#include <pmix.h>
#include <pmix/pmix_common.h>
#include <pmix_server.h>

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/usock/usock.h"

#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

static void wait_cbfunc(pmix_status_t status,
                        pmix_info_t *info, size_t ninfo,
                        void *cbdata,
                        pmix_release_cbfunc_t release_fn,
                        void *release_cbdata);

PMIX_EXPORT pmix_status_t PMIx_Query_info(pmix_info_t *info, size_t ninfo)
{
    pmix_query_caddy_t *cd;
    pmix_status_t rc, ret;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query blocking version");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* prep the caddy */
    cd = PMIX_NEW(pmix_query_caddy_t);
    cd->cbfunc = wait_cbfunc;
    cd->cbdata = cd;

    /* Use the non-blocking form as our engine */
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query requesting %d values",
                        (int)ninfo);
    cd->info = info;
    cd->ninfo = ninfo;
    cd->active = true;
    if (PMIX_SUCCESS != (rc = PMIx_Query_info_nb(cd->info, cd->ninfo, NULL, 0, wait_cbfunc, cd))) {
        PMIX_RELEASE(cd);
        return rc;
    }
    PMIX_WAIT_FOR_COMPLETION(cd->active);
    if (PMIX_ERR_NOT_FOUND == cd->status) {
        PMIX_RELEASE(cd);
        return PMIX_ERR_NOT_FOUND;
    }
    /* the RM always returns the data in the info array*/
    ret = cd->status;
    PMIX_RELEASE(cd);
    return ret;
}

static void relcbfunc(void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query release callback");

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}
static void query_cbfunc(struct pmix_peer_t *peer,
                         pmix_usock_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    pmix_status_t rc;
    pmix_query_caddy_t *results;
    int cnt;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server");

    results = PMIX_NEW(pmix_query_caddy_t);

    /* unpack the status */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &results->status, &cnt, PMIX_STATUS))) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (PMIX_SUCCESS != results->status) {
        goto complete;
    }

    /* unpack any returned data */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &results->ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < results->ninfo) {
        PMIX_INFO_CREATE(results->info, results->ninfo);
        cnt = results->ninfo;
        if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, results->info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            goto complete;
        }
    }

  complete:
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server releasing");
    /* release the caller */
    if (NULL != cd->cbfunc) {
        cd->cbfunc(results->status, results->info, results->ninfo, cd->cbdata, relcbfunc, results);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_Query_info_nb(pmix_info_t info[], size_t ninfo,
                                             pmix_info_t *directives, size_t ndirectives,
                                             pmix_info_cbfunc_t cbfunc, void *cbdata)

{
    pmix_query_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_QUERY_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query non-blocking");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    if (0 == ninfo || NULL == info) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* if we are the server, then we just issue the query and
     * return the response */
    if (pmix_globals.server) {
            if (NULL == pmix_host_server.query) {
                /* nothing we can do */
                return PMIX_ERR_NOT_SUPPORTED;
            }
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix:query handed to RM");
            pmix_host_server.query(&pmix_globals.myid,
                                   info, ninfo,
                                   directives, ndirectives,
                                   cbfunc, cbdata);
    } else {
        /* if we are a client, then relay this request to the server */
        cd = PMIX_NEW(pmix_query_caddy_t);
        cd->cbfunc = cbfunc;
        cd->cbdata = cbdata;
        msg = PMIX_NEW(pmix_buffer_t);
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ninfo, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ndirectives, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (0 < ndirectives) {
            if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, directives, ndirectives, PMIX_INFO))) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                PMIX_RELEASE(cd);
                return rc;
            }
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query sending to server");
        PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msg, query_cbfunc, cd);
    }
    return PMIX_SUCCESS;
}

static void wait_cbfunc(pmix_status_t status,
                        pmix_info_t *results, size_t nresults,
                        void *cbdata,
                        pmix_release_cbfunc_t release_fn,
                        void *release_cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    size_t n, m;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query wait callback");

    cd->status = status;
    /* transfer the results across to our query - while these _should_
     * be in the same order as our query, there is no _guarantee_ that
     * this is true, so we have to do a search */
    for (n=0; n < nresults; n++) {
        for (m=0; m < cd->ninfo; m++) {
            if (0 == strncmp(results[n].key, cd->info[m].key, PMIX_MAX_KEYLEN)) {
                if (PMIX_SUCCESS != (rc = pmix_value_xfer(&cd->info[m].value, &results[n].value))) {
                    cd->status = rc;
                    goto complete;
                }
                break;
            }
        }
    }

  complete:
    cd->relcbfunc = release_fn;
    cd->cbdata = release_cbdata;
    cd->active = false;
}
