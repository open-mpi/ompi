/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
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
#include <pmix_rename.h>

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/ptl/ptl.h"

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

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:query cback from server");

    results = PMIX_NEW(pmix_shift_caddy_t);

    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &results->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
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
        goto complete;
    }
    if (0 < results->ninfo) {
        PMIX_INFO_CREATE(results->info, results->ninfo);
        cnt = results->ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, results->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
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
    if (PMIX_PROC_SERVER == pmix_globals.proc_type) {
        if (NULL == pmix_host_server.query) {
            /* nothing we can do */
            return PMIX_ERR_NOT_SUPPORTED;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query handed to RM");
        pmix_host_server.query(&pmix_globals.myid,
                               queries, nqueries,
                               cbfunc, cbdata);
        rc = PMIX_SUCCESS;
    } else {
        /* if we are a client, then relay this request to the server */

        /* if we aren't connected, don't attempt to send */
        if (!pmix_globals.connected) {
            return PMIX_ERR_UNREACH;
        }

        cd = PMIX_NEW(pmix_query_caddy_t);
        cd->cbfunc = cbfunc;
        cd->cbdata = cbdata;
        msg = PMIX_NEW(pmix_buffer_t);
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, &cmd, 1, PMIX_CMD);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, &nqueries, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, queries, nqueries, PMIX_QUERY);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query sending to server");
        PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                           msg, query_cbfunc, (void*)cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(cd);
        }
    }
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

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we are the server, then we just issue the request and
     * return the response */
    if (PMIX_PROC_SERVER == pmix_globals.proc_type) {
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
        return PMIX_ERR_UNREACH;
    }

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }

    /* pack the directive */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &directive, 1, PMIX_ALLOC_DIRECTIVE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }

    /* pack the info */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
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
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                       msg, query_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}
