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
#include <src/include/pmix_stdint.h>
#include <src/include/pmix_socket_errno.h>

#include <pmix.h>
#include <pmix_common.h>
#include <pmix_server.h>

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/usock/usock.h"

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
                         pmix_usock_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata)
{
    pmix_query_caddy_t *cd = (pmix_query_caddy_t*)cbdata;
    pmix_status_t rc;
    pmix_shift_caddy_t *results;
    int cnt;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server");

    results = PMIX_NEW(pmix_shift_caddy_t);

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

PMIX_EXPORT pmix_status_t PMIx_Query_info_nb(pmix_query_t queries[], size_t nqueries,
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

    if (0 == nqueries || NULL == queries) {
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
                                   queries, nqueries,
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
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &nqueries, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, queries, nqueries, PMIX_QUERY))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query sending to server");
        PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msg, query_cbfunc, cd);
    }
    return PMIX_SUCCESS;
}
