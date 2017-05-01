/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2015 Research Organization for Information Science
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
#include "src/mca/ptl/ptl.h"

#include "pmix_client_ops.h"

static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata);
static void op_cbfunc(pmix_status_t status, void *cbdata);
static void wait_lookup_cbfunc(struct pmix_peer_t *pr,
                               pmix_ptl_hdr_t *hdr,
                               pmix_buffer_t *buf, void *cbdata);
static void lookup_cbfunc(pmix_status_t status, pmix_pdata_t pdata[], size_t ndata,
                          void *cbdata);

PMIX_EXPORT pmix_status_t PMIx_Publish(const pmix_info_t info[],
                                       size_t ninfo)
{
    pmix_status_t rc;
    pmix_cb_t *cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: publish called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* create a callback object to let us know when it is done */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    if (PMIX_SUCCESS != (rc = PMIx_Publish_nb(info, ninfo, op_cbfunc, cb))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the server to ack our request */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = (pmix_status_t)cb->status;
    PMIX_RELEASE(cb);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Publish_nb(const pmix_info_t info[], size_t ninfo,
                                          pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_PUBLISHNB_CMD;
    pmix_status_t rc;
    pmix_cb_t *cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: publish called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        return PMIX_ERR_UNREACH;
    }

    /* check for bozo cases */
    if (NULL == info) {
        /* nothing to publish */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return PMIX_ERR_BAD_PARAM;
    }

    /* create the publish cmd */
    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* pack our effective userid - will be used to constrain lookup */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &pmix_globals.uid, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* pass the number of info structs - needed on remote end so
     * space can be malloc'd for the values */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        /* pack the info structs */
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
    cb = PMIX_NEW(pmix_cb_t);
    cb->cbfunc.opfn = cbfunc;
    cb->cbdata = cbdata;
    cb->active = true;

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                       msg, wait_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Lookup(pmix_pdata_t pdata[], size_t ndata,
                                      const pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    pmix_cb_t *cb;
    char **keys = NULL;
    size_t i;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: lookup called");

    /* bozo protection */
    if (NULL == pdata) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* transfer the pdata keys to the keys argv array */
    for (i=0; i < ndata; i++) {
        if ('\0' != pdata[i].key[0]) {
            pmix_argv_append_nosize(&keys, pdata[i].key);
        }
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->cbdata = (void*)pdata;
    cb->nvals = ndata;
    cb->active = true;

    if (PMIX_SUCCESS != (rc = PMIx_Lookup_nb(keys, info, ninfo,
                                             lookup_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        pmix_argv_free(keys);
        return rc;
    }

    /* wait for the server to ack our request */
    PMIX_WAIT_FOR_COMPLETION(cb->active);

    /* the data has been stored in the info array by lookup_cbfunc, so
     * nothing more for us to do */
    rc = cb->status;
    PMIX_RELEASE(cb);
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Lookup_nb(char **keys,
                                         const pmix_info_t info[], size_t ninfo,
                                         pmix_lookup_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_LOOKUPNB_CMD;
    pmix_status_t rc;
    pmix_cb_t *cb;
    size_t nkeys, n;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: lookup_nb called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* check for bozo cases */
    if (NULL == keys) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* create the lookup cmd */
    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* pack our effective userid - will be used to constrain lookup */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &pmix_globals.uid, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* pack the keys */
    nkeys = pmix_argv_count(keys);
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &nkeys, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < nkeys) {
        for (n=0; n < nkeys; n++) {
            PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                             msg, &keys[n], 1, PMIX_STRING);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return rc;
            }
        }
    }
    /* pass the number of info structs - needed on remote end so
     * space can be malloc'd for the values */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        /* pack the info structs */
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
    cb = PMIX_NEW(pmix_cb_t);
    cb->cbfunc.lookupfn = cbfunc;
    cb->cbdata = cbdata;

    /* send to the server */
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                       msg, wait_lookup_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Unpublish(char **keys,
                               const pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    pmix_cb_t *cb;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: unpublish called");

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    /* push the message into our event base to send to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Unpublish_nb(keys, info, ninfo, op_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the server to ack our request */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->status;
    PMIX_RELEASE(cb);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Unpublish_nb(char **keys,
                                            const pmix_info_t info[], size_t ninfo,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_UNPUBLISHNB_CMD;
    pmix_status_t rc;
    pmix_cb_t *cb;
    size_t i, j;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: unpublish called");

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* create the unpublish cmd */
    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_CMD);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* pack our effective userid - will be used to constrain lookup */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &pmix_globals.uid, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* pack the number of keys */
    i = pmix_argv_count(keys);
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &i, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < i) {
        for (j=0; j < i; j++) {
            PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                             msg, &keys[j], 1, PMIX_STRING);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return rc;
            }
        }
    }
    /* pass the number of info structs - needed on remote end so
     * space can be malloc'd for the values */
    PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    if (0 < ninfo) {
        /* pack the info structs */
        PMIX_BFROPS_PACK(rc, &pmix_client_globals.myserver,
                         msg, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
    }

    /* create a callback object */
    cb = PMIX_NEW(pmix_cb_t);
    cb->cbfunc.opfn = cbfunc;
    cb->cbdata = cbdata;
    cb->active = true;

    /* send to the server */
    PMIX_PTL_SEND_RECV(rc, &pmix_client_globals.myserver,
                       msg, wait_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cb);
    }

    return rc;
}

static void wait_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;
    int ret;
    int32_t cnt;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client recv callback activated with %d bytes",
                        (NULL == buf) ? -1 : (int)buf->bytes_used);

    /* unpack the returned status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    if (NULL != cb->cbfunc.opfn) {
        cb->cbfunc.opfn(rc, cb->cbdata);
    }
    PMIX_RELEASE(cb);
}

static void op_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    cb->status = status;
    cb->active = false;
}

static void wait_lookup_cbfunc(struct pmix_peer_t *pr,
                               pmix_ptl_hdr_t *hdr,
                               pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc, ret;
    int32_t cnt;
    pmix_pdata_t *pdata;
    size_t ndata;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client recv callback activated with %d bytes",
                        (NULL == buf) ? -1 : (int)buf->bytes_used);

    if (NULL == cb->cbfunc.lookupfn) {
        /* nothing we can do with this */
        PMIX_RELEASE(cb);
        return;
    }

    /* set the defaults */
    pdata = NULL;
    ndata = 0;

    /* unpack the returned status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    }
    if (PMIX_SUCCESS != ret) {
        if (NULL != cb->cbfunc.lookupfn) {
            cb->cbfunc.lookupfn(ret, NULL, 0, cb->cbdata);
        }
        PMIX_RELEASE(cb);
        return;
    }

    /* unpack the number of returned values */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                       buf, &ndata, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cb);
        return;
    }
    if (0 < ndata) {
        /* create the array storage */
        PMIX_PDATA_CREATE(pdata, ndata);
        cnt = ndata;
        /* unpack the returned values into the pdata array */
        PMIX_BFROPS_UNPACK(rc, &pmix_client_globals.myserver,
                           buf, pdata, &cnt, PMIX_PDATA);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    if (NULL != cb->cbfunc.lookupfn) {
        cb->cbfunc.lookupfn(rc, pdata, ndata, cb->cbdata);
    }

 cleanup:
    /* cleanup */
    PMIX_PDATA_FREE(pdata, ndata);

    PMIX_RELEASE(cb);
}

static void lookup_cbfunc(pmix_status_t status, pmix_pdata_t pdata[], size_t ndata,
                          void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_pdata_t *tgt = (pmix_pdata_t*)cb->cbdata;
    size_t i, j;

    cb->status = status;
    if (PMIX_SUCCESS == status) {
        /* find the matching key in the provided info array - error if not found */
        for (i=0; i < ndata; i++) {
            for (j=0; j < cb->nvals; j++) {
                if (0 == strcmp(pdata[i].key, tgt[j].key)) {
                    /* transfer the publishing proc id */
                    (void)strncpy(tgt[j].proc.nspace, pdata[i].proc.nspace, PMIX_MAX_NSLEN);
                    tgt[j].proc.rank = pdata[i].proc.rank;
                    /* transfer the value to the pmix_info_t */
                    PMIX_BFROPS_VALUE_XFER(cb->status, &pmix_client_globals.myserver, &tgt[j].value, &pdata[i].value);
                    break;
                }
            }
        }
    }

    cb->active = false;
}
