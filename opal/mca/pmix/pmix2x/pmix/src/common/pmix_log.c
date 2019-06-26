/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
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

#include "src/threads/threads.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/mca/bfrops/bfrops.h"
#include "src/mca/ptl/ptl.h"

#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

static void log_cbfunc(struct pmix_peer_t *peer,
                       pmix_ptl_hdr_t *hdr,
                       pmix_buffer_t *buf, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    int32_t m;
    pmix_status_t rc, status;

    /* unpack the return status */
    m=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &status, &m, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        status = rc;
    }

    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_Log_nb(const pmix_info_t data[], size_t ndata,
                                      const pmix_info_t directives[], size_t ndirs,
                                      pmix_op_cbfunc_t cbfunc, void *cbdata)

{
    pmix_shift_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_LOG_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:log non-blocking");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!PMIX_PROC_IS_SERVER(pmix_globals.mypeer) && !pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    if (0 == ndata || NULL == data) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* if we are the server, then we just log and
     * return the response */
    if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
            if (NULL == pmix_host_server.log) {
                /* nothing we can do */
                return PMIX_ERR_NOT_SUPPORTED;
            }
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix:log handed to RM");
            pmix_host_server.log(&pmix_globals.myid,
                                 data, ndata, directives, ndirs,
                                 cbfunc, cbdata);
            rc = PMIX_SUCCESS;
    } else {
        /* if we are a client, then relay this request to the server */
        cd = PMIX_NEW(pmix_shift_caddy_t);
        cd->cbfunc.opcbfn = cbfunc;
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
                         msg, &ndata, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, data, ndata, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, &ndirs, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (0 < ndirs) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                             msg, directives, ndirs, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                PMIX_RELEASE(cd);
                return rc;
            }
        }

        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:log sending to server");
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                           msg, log_cbfunc, (void*)cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
    }
    return rc;
}
