/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2018 Intel, Inc. All rights reserved.
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

static void msgcbfunc(struct pmix_peer_t *peer,
                       pmix_ptl_hdr_t *hdr,
                       pmix_buffer_t *buf, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    int32_t m;
    pmix_status_t rc, status;

    /* unpack the return status */
    m=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &status, &m, PMIX_STATUS);
    if (PMIX_SUCCESS == rc && PMIX_SUCCESS == status) {
        /* store the request on our list - we are in an event, and
         * so this is safe */
        pmix_list_append(&pmix_globals.iof_requests, &cd->iofreq->super);
    } else if (PMIX_SUCCESS != rc) {
        status = rc;
        PMIX_RELEASE(cd->iofreq);
    }

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof_register returned status %s", PMIx_Error_string(status));

    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_IOF_register(const pmix_proc_t procs[], size_t nprocs,
                                            const pmix_info_t directives[], size_t ndirs,
                                            pmix_iof_channel_t channel, pmix_iof_cbfunc_t cbfunc,
                                            pmix_hdlr_reg_cbfunc_t regcbfunc, void *regcbdata)
{
    pmix_shift_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_IOF_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof_register");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we are a server, we cannot do this */
    if (PMIX_PROC_IS_SERVER(pmix_globals.mypeer)) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* send this request to the server */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbfunc.hdlrregcbfn = regcbfunc;
    cd->cbdata = regcbdata;
    /* setup the request item */
    cd->iofreq = PMIX_NEW(pmix_iof_req_t);
    if (NULL == cd->iofreq) {
        PMIX_RELEASE(cd);
        return PMIX_ERR_NOMEM;
    }
    /* retain the channels and cbfunc */
    cd->iofreq->channels = channel;
    cd->iofreq->cbfunc = cbfunc;
    /* we don't need the source specifications - only the
    * server cares as it will filter against them */

    /* setup the registration cmd */
    msg = PMIX_NEW(pmix_buffer_t);
    if (NULL == msg) {
        PMIX_RELEASE(cd->iofreq);
        PMIX_RELEASE(cd);
        return PMIX_ERR_NOMEM;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &nprocs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, procs, nprocs, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &ndirs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    if (0 < ndirs) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, directives, ndirs, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &channel, 1, PMIX_IOF_CHANNEL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof_request sending to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, msgcbfunc, (void*)cd);

  cleanup:
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd->iofreq);
        PMIX_RELEASE(cd);
    }
    return rc;
}
