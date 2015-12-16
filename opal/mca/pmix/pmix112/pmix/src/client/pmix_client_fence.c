/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2015      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include <private/autogen/config.h>
#include <pmix/rename.h>
#include <private/types.h>
#include <private/pmix_stdint.h>

#include <pmix.h>

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
#include "src/buffer_ops/buffer_ops.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/hash.h"
#include "src/util/output.h"
#include "src/util/progress_threads.h"
#include "src/usock/usock.h"
#include "src/sec/pmix_sec.h"

#include "pmix_client_ops.h"

static int unpack_return(pmix_buffer_t *data);
static int pack_fence(pmix_buffer_t *msg, pmix_cmd_t cmd,
                      const pmix_proc_t *procs, size_t nprocs,
                      const pmix_info_t *info, size_t ninfo);
static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_usock_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata);
static void op_cbfunc(int status, void *cbdata);

int PMIx_Fence(const pmix_proc_t procs[], size_t nprocs,
               const pmix_info_t info[], size_t ninfo)
{
    pmix_cb_t *cb;
    int rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: executing fence");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    /* push the message into our event base to send to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Fence_nb(procs, nprocs, info, ninfo,
                                            op_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the fence to complete */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->status;
    PMIX_RELEASE(cb);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: fence released");

    return rc;
}

int PMIx_Fence_nb(const pmix_proc_t procs[], size_t nprocs,
                  const pmix_info_t info[], size_t ninfo,
                  pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FENCENB_CMD;
    int rc;
    pmix_cb_t *cb;
    pmix_proc_t rg, *rgs;
    size_t nrg;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: fence_nb called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* check for bozo input */
    if (NULL == procs && 0 != nprocs) {
        return PMIX_ERR_BAD_PARAM;
    }
    /* if we are given a NULL proc, then the caller is referencing
     * all procs within our own nspace */
    if (NULL == procs) {
        (void)strncpy(rg.nspace, pmix_globals.myid.nspace, PMIX_MAX_NSLEN);
        rg.rank = PMIX_RANK_WILDCARD;
        rgs = &rg;
        nrg = 1;
    } else {
        rgs = (pmix_proc_t*)procs;
        nrg = nprocs;
    }

    msg = PMIX_NEW(pmix_buffer_t);
    if (PMIX_SUCCESS != (rc = pack_fence(msg, cmd, rgs, nrg, info, ninfo))) {
        PMIX_RELEASE(msg);
        return rc;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->op_cbfunc = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msg, wait_cbfunc, cb);

    return PMIX_SUCCESS;
}

static pmix_status_t unpack_return(pmix_buffer_t *data)
{
    pmix_status_t rc;
    int ret;
    int32_t cnt;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "client:unpack fence called");

    /* unpack the status code */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(data, &ret, &cnt, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    pmix_output_verbose(2, pmix_globals.debug_output,
                        "client:unpack fence received status %d", ret);
    return PMIX_SUCCESS;
}

static pmix_status_t pack_fence(pmix_buffer_t *msg, pmix_cmd_t cmd,
                                const pmix_proc_t *procs, size_t nprocs,
                                const pmix_info_t *info, size_t ninfo)
{
    pmix_status_t rc;

    /* pack the cmd */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* pack the number of procs */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &nprocs, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack any provided procs - must always be at least one (our own) */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, procs, nprocs, PMIX_PROC))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack the number of info */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack any provided info - may be NULL */
    if (NULL != info && 0 < ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
    }

    return PMIX_SUCCESS;
}

static void wait_cbfunc(struct pmix_peer_t *pr, pmix_usock_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: fence_nb callback recvd");

    if (NULL == cb) {
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return;
    }
    rc = unpack_return(buf);

    /* if a callback was provided, execute it */
    if (NULL != cb->op_cbfunc) {
        cb->op_cbfunc(rc, cb->cbdata);
    }
    PMIX_RELEASE(cb);
}

static void op_cbfunc(int status, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    cb->status = status;
    cb->active = false;
}

