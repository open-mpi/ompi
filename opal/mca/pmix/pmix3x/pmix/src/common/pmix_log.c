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
#include <pmix_rename.h>

#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/buffer_ops/buffer_ops.h"
#include "src/usock/usock.h"

#include "src/client/pmix_client_ops.h"
#include "src/server/pmix_server_ops.h"
#include "src/include/pmix_globals.h"

static void log_cbfunc(struct pmix_peer_t *peer,
                       pmix_usock_hdr_t *hdr,
                       pmix_buffer_t *buf, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    int32_t m;
    pmix_status_t rc, status;

    /* unpack the return status */
    m=1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &status, &m, PMIX_STATUS))) {
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

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:log non-blocking");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    if (0 == ndata || NULL == data) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* if we are the server, then we just log and
     * return the response */
    if (PMIX_PROC_SERVER == pmix_globals.proc_type) {
            if (NULL == pmix_host_server.log) {
                /* nothing we can do */
                return PMIX_ERR_NOT_SUPPORTED;
            }
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix:query handed to RM");
            pmix_host_server.log(&pmix_globals.myid,
                                 data, ndata, directives, ndirs,
                                 cbfunc, cbdata);
    } else {
        /* if we are a client, then relay this request to the server */
        cd = PMIX_NEW(pmix_shift_caddy_t);
        cd->cbfunc.opcbfn = cbfunc;
        cd->cbdata = cbdata;
        msg = PMIX_NEW(pmix_buffer_t);
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ndata, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, data, ndata, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ndirs, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (0 < ndirs) {
            if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, data, ndata, PMIX_INFO))) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                PMIX_RELEASE(cd);
                return rc;
            }
        }

        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:query sending to server");
        PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msg, log_cbfunc, cd);
    }
    return PMIX_SUCCESS;
}
