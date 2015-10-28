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

static pmix_buffer_t* pack_get(char *nspace, int rank,
                               const pmix_info_t info[], size_t ninfo,
                               pmix_cmd_t cmd);
static void getnb_cbfunc(struct pmix_peer_t *pr, pmix_usock_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata);
static void getnb_shortcut(int fd, short flags, void *cbdata);
static void value_cbfunc(int status, pmix_value_t *kv, void *cbdata);

int PMIx_Get(const pmix_proc_t *proc, const char key[],
             const pmix_info_t info[], size_t ninfo,
             pmix_value_t **val)
{
    pmix_cb_t *cb;
    int rc;

    if (NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: %s:%d getting value for proc %s:%d key %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        proc->nspace, proc->rank,
                        (NULL == key) ? "NULL" : key);

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    cb->active = true;

    if (PMIX_SUCCESS != (rc = PMIx_Get_nb(proc, key, info, ninfo, value_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        *val = NULL;
        return rc;
    }

    /* wait for the data to return */
    PMIX_WAIT_FOR_COMPLETION(cb->active);
    rc = cb->status;
    *val = cb->value;
    PMIX_RELEASE(cb);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:client get completed");

    return rc;
}

pmix_status_t PMIx_Get_nb(const pmix_proc_t *proc, const char *key,
                          const pmix_info_t info[], size_t ninfo,
                          pmix_value_cbfunc_t cbfunc, void *cbdata)
{
    pmix_value_t *val;
    pmix_buffer_t *msg;
    pmix_cb_t *cb;
    pmix_status_t rc;
    char *nm;
    pmix_nspace_t *ns, *nptr;
    size_t n;

    if (NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: get_nb value for proc %s:%d key %s",
                        proc->nspace, proc->rank,
                        (NULL == key) ? "NULL" : key);

    if (pmix_globals.init_cntr <= 0) {
        return PMIX_ERR_INIT;
    }

    /* protect against bozo input */
    if (NULL == key) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* if the nspace is empty, then the caller is referencing
     * our own nspace */
    if (0 == strlen(proc->nspace)) {
        nm = pmix_globals.myid.nspace;
    } else {
        nm = (char*)proc->nspace;
    }

    /* find the nspace object */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(nm, ns->nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* we are asking for info about a new nspace - give us
         * a chance to learn about it from the server. If the
         * server has never heard of it, the server will return
         * an error */
         nptr = PMIX_NEW(pmix_nspace_t);
         (void)strncpy(nptr->nspace, nm, PMIX_MAX_NSLEN);
         pmix_list_append(&pmix_globals.nspaces, &nptr->super);
         /* there is no point in looking for data in this nspace
          * object, so let's just go generate the request */
         goto request;
    }

    /* the requested data could be in the job-data table, so let's
     * just check there first.  */
    if (PMIX_SUCCESS == (rc = pmix_hash_fetch(&nptr->internal, PMIX_RANK_WILDCARD, key, &val))) {
        /* found it - return it via appropriate channel */
        cb = PMIX_NEW(pmix_cb_t);
        (void)strncpy(cb->nspace, nm, PMIX_MAX_NSLEN);
        cb->rank = proc->rank;
        cb->key = strdup(key);
        cb->value_cbfunc = cbfunc;
        cb->cbdata = cbdata;
        /* pack the return data so the unpack routine can get it */
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&cb->data, val, 1, PMIX_VALUE))) {
            PMIX_ERROR_LOG(rc);
        }
        /* cleanup */
        if (NULL != val) {
            PMIX_VALUE_RELEASE(val);
        }
        /* activate the event */
        event_assign(&(cb->ev), pmix_globals.evbase, -1,
                     EV_WRITE, getnb_shortcut, cb);
        event_active(&(cb->ev), EV_WRITE, 1);
        return PMIX_SUCCESS;
    }
    if (PMIX_RANK_WILDCARD == proc->rank) {
        /* can't be anywhere else */
        return PMIX_ERR_NOT_FOUND;
    }

    /* it could still be in the job-data table, only stored under its own
     * rank and not WILDCARD - e.g., this is true of data returned about
     * ourselves during startup */
    if (PMIX_SUCCESS == (rc = pmix_hash_fetch(&nptr->internal, proc->rank, key, &val))) {
        /* found it - return it via appropriate channel */
        cb = PMIX_NEW(pmix_cb_t);
        (void)strncpy(cb->nspace, nm, PMIX_MAX_NSLEN);
        cb->rank = proc->rank;
        cb->key = strdup(key);
        cb->value_cbfunc = cbfunc;
        cb->cbdata = cbdata;
        /* pack the return data so the unpack routine can get it */
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&cb->data, val, 1, PMIX_VALUE))) {
            PMIX_ERROR_LOG(rc);
        }
        /* cleanup */
        if (NULL != val) {
            PMIX_VALUE_RELEASE(val);
        }
        /* activate the event */
        event_assign(&(cb->ev), pmix_globals.evbase, -1,
                     EV_WRITE, getnb_shortcut, cb);
        event_active(&(cb->ev), EV_WRITE, 1);
        return PMIX_SUCCESS;
    }

    /* not finding it is not an error - it could be in the
     * modex hash table, so check it */
    if (PMIX_SUCCESS == (rc = pmix_hash_fetch(&nptr->modex, proc->rank, key, &val))) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix: value retrieved from dstore");
        /* need to push this into the event library to ensure
         * the callback occurs within an event */
        cb = PMIX_NEW(pmix_cb_t);
        (void)strncpy(cb->nspace, nm, PMIX_MAX_NSLEN);
        cb->rank = proc->rank;
        cb->key = strdup(key);
        cb->value_cbfunc = cbfunc;
        cb->cbdata = cbdata;
        /* pack the return data so the unpack routine can get it */
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(&cb->data, val, 1, PMIX_VALUE))) {
            PMIX_ERROR_LOG(rc);
        }
        /* cleanup */
        if (NULL != val) {
            PMIX_VALUE_RELEASE(val);
        }
        /* activate the event */
        event_assign(&(cb->ev), pmix_globals.evbase, -1,
                     EV_WRITE, getnb_shortcut, cb);
        event_active(&(cb->ev), EV_WRITE, 1);
        return PMIX_SUCCESS;
    } else if (PMIX_ERR_NOT_FOUND == rc) {
        /* we have the modex data from this proc, but didn't find the key
         * the user requested. At this time, there is no way for the
         * key to eventually be found, so all we can do is return
         * the error */
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "Error requesting key=%s for rank = %d, namespace = %s",
                            key, proc->rank, nm);
        return rc;
    }

  request:
    /* if we got here, then we don't have the data for this proc. If we
     * are a server, or we are a client and not connected, then there is
     * nothing more we can do */
    if (pmix_globals.server || (!pmix_globals.server && !pmix_globals.connected)) {
        return PMIX_ERR_NOT_FOUND;
    }

    /* we also have to check the user's directives to see if they do not want
     * us to attempt to retrieve it from the server */
    for (n=0; n < ninfo; n++) {
        if (0 == strcmp(info[n].key, PMIX_OPTIONAL) &&
            info[n].value.data.flag) {
            /* they don't want us to try and retrieve it */
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "PMIx_Get key=%s for rank = %d, namespace = %s was not found - request was optional",
                                key, proc->rank, nm);
            return PMIX_ERR_NOT_FOUND;
        }
    }
    /* see if we already have a request in place with the server for data from
     * this nspace:rank. If we do, then no need to ask again as the
     * request will return _all_ data from that proc */
    PMIX_LIST_FOREACH(cb, &pmix_client_globals.pending_requests, pmix_cb_t) {
        if (0 == strncmp(nm, cb->nspace, PMIX_MAX_NSLEN) && cb->rank == proc->rank) {
            /* we do have a pending request, but we still need to track this
             * outstanding request so we can satisfy it once the data is returned */
            cb = PMIX_NEW(pmix_cb_t);
            (void)strncpy(cb->nspace, nm, PMIX_MAX_NSLEN);
            cb->rank = proc->rank;
            cb->key = strdup(key);
            cb->value_cbfunc = cbfunc;
            cb->cbdata = cbdata;
            pmix_list_append(&pmix_client_globals.pending_requests, &cb->super);
            return PMIX_SUCCESS;
        }
    }

    /* we don't have a pending request, so let's create one - don't worry
     * about packing the key as we return everything from that proc */
    if (NULL == (msg = pack_get(nm, proc->rank, info, ninfo, PMIX_GETNB_CMD))) {
        return PMIX_ERROR;
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_cb_t);
    (void)strncpy(cb->nspace, nm, PMIX_MAX_NSLEN);
    cb->rank = proc->rank;
    cb->key = strdup(key);
    cb->value_cbfunc = cbfunc;
    cb->cbdata = cbdata;
    pmix_list_append(&pmix_client_globals.pending_requests, &cb->super);

    /* push the message into our event base to send to the server */
    PMIX_ACTIVATE_SEND_RECV(&pmix_client_globals.myserver, msg, getnb_cbfunc, cb);
    return PMIX_SUCCESS;
}

static void value_cbfunc(int status, pmix_value_t *kv, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_status_t rc;

    cb->status = status;
    if (PMIX_SUCCESS == status) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.copy((void**)&cb->value, kv, PMIX_VALUE))) {
            PMIX_ERROR_LOG(rc);
        }
    }
    cb->active = false;
}

static pmix_buffer_t* pack_get(char *nspace, int rank,
                               const pmix_info_t info[], size_t ninfo,
                               pmix_cmd_t cmd)
{
    pmix_buffer_t *msg;
    pmix_status_t rc;

    /* nope - see if we can get it */
    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the get cmd */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &cmd, 1, PMIX_CMD))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    /* pack the request information - we'll get the entire blob
     * for this proc, so we don't need to pass the key */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &nspace, 1, PMIX_STRING))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &rank, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    /* pack the number of info structs */
    if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, &ninfo, 1, PMIX_SIZE))) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return NULL;
    }
    if (0 < ninfo) {
        if (PMIX_SUCCESS != (rc = pmix_bfrop.pack(msg, info, ninfo, PMIX_INFO))) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return NULL;
        }
    }
    return msg;
}

static void getnb_cbfunc(struct pmix_peer_t *pr, pmix_usock_hdr_t *hdr,
                         pmix_buffer_t *buf, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_cb_t *cb2;
    pmix_status_t rc, ret;
    pmix_value_t *val = NULL;
    int32_t cnt;
    pmix_buffer_t *bptr;
    pmix_kval_t *kp;
    pmix_nspace_t *ns, *nptr;
    int rank;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: get_nb callback recvd");
    if (NULL == cb) {
        /* nothing we can do */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return;
    }
    // cache the rank
    rank = cb->rank;

    /* unpack the status */
    cnt = 1;
    if (PMIX_SUCCESS != (rc = pmix_bfrop.unpack(buf, &ret, &cnt, PMIX_INT))) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* look up the nspace object for this proc */
    nptr = NULL;
    PMIX_LIST_FOREACH(ns, &pmix_globals.nspaces, pmix_nspace_t) {
        if (0 == strcmp(cb->nspace, ns->nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* new nspace - setup a record for it */
        nptr = PMIX_NEW(pmix_nspace_t);
        (void)strncpy(nptr->nspace, cb->nspace, PMIX_MAX_NSLEN);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }

    if (PMIX_SUCCESS != ret) {
        goto done;
    }

    /* we received the entire blob for this process, so
     * unpack and store it in the modex - this could consist
     * of buffers from multiple scopes */
    cnt = 1;
    while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(buf, &bptr, &cnt, PMIX_BUFFER))) {
        cnt = 1;
        kp = PMIX_NEW(pmix_kval_t);
        while (PMIX_SUCCESS == (rc = pmix_bfrop.unpack(bptr, kp, &cnt, PMIX_KVAL))) {
            pmix_output_verbose(2, pmix_globals.debug_output,
                                "pmix: unpacked key %s", kp->key);
            if (PMIX_SUCCESS != (rc = pmix_hash_store(&nptr->modex, cb->rank, kp))) {
                PMIX_ERROR_LOG(rc);
            }
            if (NULL != cb->key && 0 == strcmp(cb->key, kp->key)) {
                pmix_output_verbose(2, pmix_globals.debug_output,
                                    "pmix: found requested value");
                if (PMIX_SUCCESS != (rc = pmix_bfrop.copy((void**)&val, kp->value, PMIX_VALUE))) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(kp);
                    val = NULL;
                    goto done;
                }
            }
            PMIX_RELEASE(kp); // maintain acctg - hash_store does a retain
            cnt = 1;
            kp = PMIX_NEW(pmix_kval_t);
        }
        cnt = 1;
        PMIX_RELEASE(kp);
        PMIX_RELEASE(bptr);  // free's the data region
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
            rc = PMIX_ERR_SILENT; // avoid error-logging twice
            break;
        }
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
    } else {
        rc = PMIX_SUCCESS;
    }

 done:
    /* if a callback was provided, execute it */
    if (NULL != cb && NULL != cb->value_cbfunc) {
        if (NULL == val) {
            rc = PMIX_ERR_NOT_FOUND;
        }
        cb->value_cbfunc(rc, val, cb->cbdata);
    }
    if (NULL != val) {
        PMIX_VALUE_RELEASE(val);
    }
    /* we obviously processed this one, so remove it from the
     * list of pending requests */
    pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
    PMIX_RELEASE(cb);

    /* now search any pending requests to see if they can be met */
    PMIX_LIST_FOREACH_SAFE(cb, cb2, &pmix_client_globals.pending_requests, pmix_cb_t) {
        if (0 == strncmp(nptr->nspace, cb->nspace, PMIX_MAX_NSLEN) && cb->rank == rank) {
           /* we have the data - see if we can find the key */
            val = NULL;
            rc = pmix_hash_fetch(&nptr->modex, rank, cb->key, &val);
            cb->value_cbfunc(rc, val, cb->cbdata);
            if (NULL != val) {
                PMIX_VALUE_RELEASE(val);
            }
            pmix_list_remove_item(&pmix_client_globals.pending_requests, &cb->super);
            PMIX_RELEASE(cb);
        }
    }
}

static void getnb_shortcut(int fd, short flags, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;
    pmix_value_t val;
    pmix_status_t rc;
    int32_t m;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "getnb_shortcut called with %s cbfunc",
                        (NULL == cb->value_cbfunc) ? "NULL" : "NON-NULL");

    PMIX_VALUE_CONSTRUCT(&val);
    if (NULL != cb->value_cbfunc) {
        m=1;
        rc = pmix_bfrop.unpack(&cb->data, &val, &m, PMIX_VALUE);
        cb->value_cbfunc(rc, &val, cb->cbdata);
    }
    PMIX_VALUE_DESTRUCT(&val);
    PMIX_RELEASE(cb);
}
