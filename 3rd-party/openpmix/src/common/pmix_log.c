/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
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

#include "src/mca/bfrops/bfrops.h"
#include "src/mca/plog/base/base.h"
#include "src/mca/ptl/base/base.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"

#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/server/pmix_server_ops.h"

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    cb->status = status;
    PMIX_WAKEUP_THREAD(&cb->lock);
}

static void log_cbfunc(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                       pmix_buffer_t *buf, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;
    int32_t m;
    pmix_status_t rc, status;
    PMIX_HIDE_UNUSED_PARAMS(hdr);

    /* unpack the return status */
    m = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &status, &m, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        status = rc;
    }

    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_Log(const pmix_info_t data[], size_t ndata,
                                   const pmix_info_t directives[], size_t ndirs)
{
    pmix_cb_t cb;
    pmix_status_t rc;

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    pmix_output_verbose(2, pmix_plog_base_framework.framework_output, "%s pmix:log",
                        PMIX_NAME_PRINT(&pmix_globals.myid));

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    rc = PMIx_Log_nb(data, ndata, directives, ndirs, opcbfunc, &cb);
    if (PMIX_SUCCESS == rc) {
        /* wait for the operation to complete */
        PMIX_WAIT_THREAD(&cb.lock);
    } else {
        PMIX_DESTRUCT(&cb);
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            rc = PMIX_SUCCESS;
        }
        return rc;
    }

    rc = cb.status;
    PMIX_DESTRUCT(&cb);

    pmix_output_verbose(2, pmix_plog_base_framework.framework_output, "pmix:log completed");

    return rc;
}

static void localcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;

    PMIX_INFO_FREE(cd->directives, cd->ndirs);
    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(status, cd->cbdata);
    }
    PMIX_RELEASE(cd);
}

void pmix_log_local_op(int sd, short args, void *cbdata_)
{
    pmix_shift_caddy_t *qd = (pmix_shift_caddy_t *) cbdata_;
    pmix_info_t *data = qd->info;
    size_t ndata = qd->ninfo;
    pmix_info_t *directives = qd->directives;
    size_t ndirs = qd->ndirs;
    pmix_op_cbfunc_t cbfunc = qd->cbfunc.opcbfn;
    void *cbdata = qd->cbdata;
    pmix_proc_t *source = qd->proc;
    pmix_status_t rc;
    pmix_shift_caddy_t *cd;
    size_t n;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* if no recorded source was found, then we must be it */
    if (NULL == source) {
        source = &pmix_globals.myid;
        cd = PMIX_NEW(pmix_shift_caddy_t);
        cd->cbfunc.opcbfn = cbfunc;
        cd->cbdata = cbdata;
        cd->ndirs = ndirs + 1;
        PMIX_INFO_CREATE(cd->directives, cd->ndirs);
        for (n = 0; n < ndirs; n++) {
            PMIX_INFO_XFER(&cd->directives[n], (pmix_info_t *) &directives[n]);
        }
        PMIX_INFO_LOAD(&cd->directives[ndirs], PMIX_LOG_SOURCE, source, PMIX_PROC);
        rc = pmix_plog.log(source, data, ndata, cd->directives, cd->ndirs, localcbfunc, cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_INFO_FREE(cd->directives, cd->ndirs);
            PMIX_RELEASE(cd);
        }
    } else if (PMIX_CHECK_PROCID(source, &pmix_globals.myid)) {
        /* if I am the recorded source, then this is a re-submission of
         * something that got "upcalled" by a prior call. In this case,
         * we return a "not supported" error as clearly we couldn't
         * handle it, and neither could our host */
        rc = PMIX_ERR_NOT_SUPPORTED;
    } else {
        /* call down to process the request - the various components
         * will thread shift as required */
        rc = pmix_plog.log(source, data, ndata, directives, ndirs, cbfunc, cbdata);
    }
}

PMIX_EXPORT pmix_status_t PMIx_Log_nb(const pmix_info_t data[], size_t ndata,
                                      const pmix_info_t directives[], size_t ndirs,
                                      pmix_op_cbfunc_t cbfunc, void *cbdata)

{
    pmix_cmd_t cmd = PMIX_LOG_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc = PMIX_SUCCESS;
    time_t timestamp = 0;
    pmix_proc_t *source = NULL;
    pmix_shift_caddy_t *cd;

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:log non-blocking");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    if (0 == ndata || NULL == data) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* check the directives - if they requested a timestamp, then
     * get the time, also look for a source */
    if (NULL != directives) {
        for (size_t n = 0; n < ndirs; n++) {
            if (0 == strncmp(directives[n].key, PMIX_LOG_GENERATE_TIMESTAMP, PMIX_MAX_KEYLEN)) {
                if (PMIX_INFO_TRUE(&directives[n])) {
                    /* pickup the timestamp */
                    timestamp = time(NULL);
                }
            } else if (0 == strncmp(directives[n].key, PMIX_LOG_SOURCE, PMIX_MAX_KEYLEN)) {
                source = directives[n].value.data.proc;
            }
        }
    }

    /* if we are a client or tool, we never do this ourselves - we
     * always pass this request to our server for execution */
    if (!PMIX_PEER_IS_SERVER(pmix_globals.mypeer) && !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        /* if we aren't connected, don't attempt to send */
        if (!pmix_globals.connected) {
            return PMIX_ERR_UNREACH;
        }

        /* if we are not a server, then relay this request to the server */
        cd = PMIX_NEW(pmix_shift_caddy_t);
        cd->cbfunc.opcbfn = cbfunc;
        cd->cbdata = cbdata;
        msg = PMIX_NEW(pmix_buffer_t);
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (!PMIX_PEER_IS_EARLIER(pmix_client_globals.myserver, 3, PMIX_MINOR_WILDCARD,
                                  PMIX_RELEASE_WILDCARD)) {
            /* provide the timestamp - zero will indicate
             * that it wasn't taken */
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &timestamp, 1, PMIX_TIME);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                PMIX_RELEASE(cd);
                return rc;
            }
        }
        /* pack the number of data entries */
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ndata, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (0 < ndata) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, data, ndata, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                PMIX_RELEASE(cd);
                return rc;
            }
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ndirs, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            PMIX_RELEASE(cd);
            return rc;
        }
        if (0 < ndirs) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, directives, ndirs, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                PMIX_RELEASE(cd);
                return rc;
            }
        }

        pmix_output_verbose(2, pmix_plog_base_framework.framework_output,
                            "pmix:log sending to server");
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, log_cbfunc, (void *) cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->info = (pmix_info_t *) data;
    cd->ninfo = ndata;
    cd->directives = (pmix_info_t *) directives;
    cd->ndirs = ndirs;
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;
    cd->proc = source;
    PMIX_THREADSHIFT(cd, pmix_log_local_op);

    return rc;
}
