/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2017 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
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

#include <src/include/pmix_config.h>

#include <src/include/types.h>
#include <src/include/pmix_stdint.h>

#include <pmix.h>
#include <pmix_rename.h>

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
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/output.h"
#include "src/mca/gds/gds.h"
#include "src/mca/ptl/ptl.h"

#include "pmix_client_ops.h"

static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata);
static void spawn_cbfunc(pmix_status_t status, char nspace[], void *cbdata);

PMIX_EXPORT pmix_status_t PMIx_Spawn(const pmix_info_t job_info[], size_t ninfo,
                           const pmix_app_t apps[], size_t napps,
                           char nspace[])
{
    pmix_status_t rc;
    pmix_cb_t *cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: spawn called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* ensure the nspace (if provided) is initialized */
    if (NULL != nspace) {
        memset(nspace, 0, PMIX_MAX_NSLEN+1);
    }

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    if (PMIX_SUCCESS != (rc = PMIx_Spawn_nb(job_info, ninfo, apps, napps, spawn_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the result */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->status;
    if (NULL != nspace) {
        (void)strncpy(nspace, cb->pname.nspace, PMIX_MAX_NSLEN);
    }
    PMIX_RELEASE(cb);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Spawn_nb(const pmix_info_t job_info[], size_t ninfo,
                                        const pmix_app_t apps[], size_t napps,
                                        pmix_spawn_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_SPAWNNB_CMD;
    pmix_status_t rc;
    pmix_cb_t *cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: spawn called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

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

    /* pack the job-level directives */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, job_info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
    }

    /* pack the apps */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &napps, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < napps) {
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, apps, napps, PMIX_APP);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->cbfunc.spawnfn = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                       msg, wait_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}

/* callback for wait completion */
static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    char nspace[PMIX_MAX_NSLEN+1];
    char *n2 = NULL;
    pmix_status_t rc, ret;
    int32_t cnt;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client recv callback activated with %d bytes",
                        (NULL == buf) ? -1 : (int)buf->bytes_used);

    /* init */
    memset(nspace, 0, PMIX_MAX_NSLEN+1);

    /* unpack the returned status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    }
    if (PMIX_SUCCESS == ret) {
        /* unpack the namespace */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                           buf, &n2, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            ret = rc;
        }
        pmix_output_verbose(1, pmix_globals.debug_output,
                        "pmix:client recv '%s'", n2);

        if (NULL != n2) {
            /* protect length */
            (void)strncpy(nspace, n2, PMIX_MAX_NSLEN);
            free(n2);
            PMIX_GDS_STORE_JOB_INFO(rc, pmix_globals.mypeer, nspace, buf);
            /* extract and process any job-related info for this nspace */
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
        }
    }

    if (NULL != cb->cbfunc.spawnfn) {
        cb->cbfunc.spawnfn(ret, nspace, cb->cbdata);
    }
    cb->cbdata = NULL;
    PMIX_RELEASE(cb);
}

static void spawn_cbfunc(pmix_status_t status, char nspace[], void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    cb->status = status;
    if (NULL != nspace) {
        cb->pname.nspace = strdup(nspace);
    }
    cb->active = false;
}
