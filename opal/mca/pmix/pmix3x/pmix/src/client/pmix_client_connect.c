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
#include "src/mca/gds/base/base.h"

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
#include "src/threads/threads.h"
#include "src/mca/gds/gds.h"
#include "src/mca/ptl/ptl.h"

#include "pmix_client_ops.h"

/* callback for wait completion */
static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata);
static void discbfunc(struct pmix_peer_t *pr,
                      pmix_ptl_hdr_t *hdr,
                      pmix_buffer_t *buf, void *cbdata);
static void cnct_cbfunc(pmix_status_t status,
                        char nspace[], int rank,
                        void *cbdata);

static void op_cbfunc(pmix_status_t status, void *cbdata);

PMIX_EXPORT pmix_status_t PMIx_Connect(const pmix_proc_t procs[], size_t nprocs,
                                       const pmix_info_t info[], size_t ninfo,
                                       char nspace[], pmix_rank_t *newrank)
{
    pmix_status_t rc;
    pmix_cb_t *cb;
    size_t n;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: connect called");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    /* see if this connect request was to return a new nspace/rank, or
     * was just an exchange of info */
    cb->checked = true;
    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_CONNECT_XCHG_ONLY, PMIX_MAX_KEYLEN)) {
            cb->checked = false;
            break;
        }
    }

    /* push the message into our event base to send to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Connect_nb(procs, nprocs, info, ninfo, cnct_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the connect to complete */
    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;

    if (cb->checked && PMIX_SUCCESS == rc) {
        if (NULL != nspace) {
            (void)strncpy(nspace, cb->pname.nspace, PMIX_MAX_NSLEN);
        }
        if (NULL != newrank) {
            *newrank = cb->pname.rank;
        }
    }
    PMIX_RELEASE(cb);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: connect completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Connect_nb(const pmix_proc_t procs[], size_t nprocs,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_connect_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_CONNECTNB_CMD;
    pmix_status_t rc;
    pmix_cb_t *cb;
    size_t n;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: connect called");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* check for bozo input */
    if (NULL == procs || 0 >= nprocs) {
        return PMIX_ERR_BAD_PARAM;
    }

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* pack the number of procs */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &nprocs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, procs, nprocs, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* pack the info structs */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
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
    cb = PMIX_NEW(pmix_cb_t);
    cb->cbfunc.cnctfn = cbfunc;
    cb->cbdata = cbdata;

    /* see if this connect request was to return a new nspace/rank, or
     * was just an exchange of info */
    cb->checked = true;
    for (n=0; n < ninfo; n++) {
        if (0 == strncmp(info[n].key, PMIX_CONNECT_XCHG_ONLY, PMIX_MAX_KEYLEN)) {
            cb->checked = false;
            break;
        }
    }

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, wait_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Disconnect(const char nspace[],
                                          const pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    pmix_cb_t *cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);

    if (PMIX_SUCCESS != (rc = PMIx_Disconnect_nb(nspace, info, ninfo, op_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the connect to complete */
    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;
    PMIX_RELEASE(cb);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: disconnect completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Disconnect_nb(const char nspace[],
                                             const pmix_info_t info[], size_t ninfo,
                                             pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_DISCONNECTNB_CMD;
    pmix_status_t rc;
    pmix_cb_t *cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: disconnect called");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* check for bozo input */
    if (NULL == nspace) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* release our internal resources */
    if (0 != strncmp(pmix_globals.myid.nspace, nspace, PMIX_MAX_NSLEN)) {
        PMIX_GDS_DEL_NSPACE(rc, nspace);
    }

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* pack the nspace */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &nspace, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* pack the info structs */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
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
    cb = PMIX_NEW(pmix_cb_t);
    cb->cbfunc.opfn = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, discbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: disconnect completed");

    return rc;
}

static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    pmix_status_t ret;
    int32_t cnt;
    char *nspace;
    pmix_buffer_t bkt;
    pmix_byte_object_t bo;
    pmix_proc_t pname;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client recv callback activated with %d bytes",
                        (NULL == buf) ? -1 : (int)buf->bytes_used);

    if (NULL == buf) {
        ret = PMIX_ERR_BAD_PARAM;
        goto report;
    }

    /* a zero-byte buffer indicates that this recv is being
     * completed due to a lost connection */
    if (PMIX_BUFFER_IS_EMPTY(buf)) {
        ret = PMIX_ERR_UNREACH;
        goto report;
    }

    /* set the default nspace/rank */
    memset(pname.nspace, 0, PMIX_MAX_NSLEN+1);
    pname.rank = PMIX_RANK_UNDEF;

    /* unpack the returned status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver,
                       buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    }

    if (PMIX_SUCCESS != ret) {
        goto report;
    }

    if (cb->checked) {
        /* unpack the returned nspace/rank */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver,
                           buf, &pname, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            ret = rc;
            goto report;
        }
    }

    /* connect has to also pass back data from all nspace's involved in
     * the operation, including our own. Each will come as a byte object */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver,
                       buf, &bo, &cnt, PMIX_BYTE_OBJECT);
    while (PMIX_SUCCESS == rc) {
        /* load it for unpacking */
        PMIX_CONSTRUCT(&bkt, pmix_buffer_t);
        PMIX_LOAD_BUFFER(pmix_client_globals.myserver, &bkt, bo.bytes, bo.size);

        /* unpack the nspace for this blob */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver,
                           &bkt, &nspace, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DESTRUCT(&bkt);
            continue;
        }
        /* extract and process any proc-related info for this nspace */
        PMIX_GDS_STORE_JOB_INFO(rc, pmix_globals.mypeer, nspace, &bkt);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        free(nspace);
        PMIX_DESTRUCT(&bkt);
        /* get the next one */
        cnt = 1;
        PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver,
                           buf, &bo, &cnt, PMIX_BYTE_OBJECT);
        }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    }

  report:
    if (NULL != cb->cbfunc.cnctfn) {
        cb->cbfunc.cnctfn(ret, pname.nspace, pname.rank, cb->cbdata);
    }
    PMIX_RELEASE(cb);
}

static void discbfunc(struct pmix_peer_t *pr,
                      pmix_ptl_hdr_t *hdr,
                      pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    pmix_status_t ret;
    int32_t cnt;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client recv callback activated with %d bytes",
                        (NULL == buf) ? -1 : (int)buf->bytes_used);

    if (NULL == buf) {
        ret = PMIX_ERR_BAD_PARAM;
        goto report;
    }

    /* a zero-byte buffer indicates that this recv is being
     * completed due to a lost connection */
    if (PMIX_BUFFER_IS_EMPTY(buf)) {
        ret = PMIX_ERR_UNREACH;
        goto report;
    }

    /* unpack the returned status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver,
                       buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    }

  report:
    if (NULL != cb->cbfunc.opfn) {
        cb->cbfunc.opfn(ret, cb->cbdata);
    }
    PMIX_RELEASE(cb);
}

static void op_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    cb->status = status;
    PMIX_POST_OBJECT(cb);
    PMIX_WAKEUP_THREAD(&cb->lock);
}

static void cnct_cbfunc(pmix_status_t status,
                        char nspace[], int rank,
                        void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    cb->status = status;
    if (NULL != nspace) {
        cb->pname.nspace = strdup(nspace);
    }
    cb->pname.rank = rank;
    PMIX_POST_OBJECT(cb);
    PMIX_WAKEUP_THREAD(&cb->lock);
}
