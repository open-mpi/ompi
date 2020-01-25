/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
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

/* define a tracking object for group operations */
typedef struct {
    pmix_object_t super;
    pmix_lock_t lock;
    pmix_status_t status;
    size_t ref;
    size_t accepted;
    pmix_proc_t *members;
    size_t nmembers;
    pmix_info_t *info;
    size_t ninfo;
    pmix_info_t *results;
    size_t nresults;
    pmix_op_cbfunc_t opcbfunc;
    pmix_info_cbfunc_t cbfunc;
    void *cbdata;
} pmix_group_tracker_t;

static void gtcon(pmix_group_tracker_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
    p->status = PMIX_SUCCESS;
    p->ref = 0;
    p->accepted = 0;
    p->members = NULL;
    p->nmembers = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->results = NULL;
    p->nresults = 0;
    p->cbfunc = NULL;
    p->opcbfunc = NULL;
    p->cbdata = NULL;
}
static void gtdes(pmix_group_tracker_t *p)
{
    PMIX_DESTRUCT_LOCK(&p->lock);
    if (NULL != p->members) {
        PMIX_PROC_FREE(p->members, p->nmembers);
    }
    if (NULL != p->info) {
        PMIX_INFO_FREE(p->info, p->ninfo);
    }
}
PMIX_CLASS_INSTANCE(pmix_group_tracker_t,
                    pmix_object_t,
                    gtcon, gtdes);

/* callback for wait completion */
static void grp_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata);
static void op_cbfunc(pmix_status_t status, void *cbdata);

static void info_cbfunc(pmix_status_t status,
                        pmix_info_t *info, size_t ninfo,
                        void *cbdata,
                        pmix_release_cbfunc_t release_fn,
                        void *release_cbdata);


PMIX_EXPORT pmix_status_t PMIx_Group_construct(const char grp[],
                                               const pmix_proc_t procs[], size_t nprocs,
                                               const pmix_info_t info[], size_t ninfo,
                                               pmix_info_t **results, size_t *nresults)
{
    pmix_status_t rc;
    pmix_group_tracker_t *cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix: group_construct called");

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
    cb = PMIX_NEW(pmix_group_tracker_t);

    /* push the message into our event base to send to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Group_construct_nb(grp, procs, nprocs, info, ninfo, info_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the connect to complete */
    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;
    /* user takes responsibility for releasing any results */
    *results = cb->results;
    *nresults =  cb->nresults;
    PMIX_RELEASE(cb);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix: group construct completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_construct_nb(const char grp[],
                                                  const pmix_proc_t procs[], size_t nprocs,
                                                  const pmix_info_t info[], size_t ninfo,
                                                  pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg = NULL;
    pmix_cmd_t cmd = PMIX_GROUP_CONSTRUCT_CMD;
    pmix_status_t rc;
    pmix_group_tracker_t *cb = NULL;
    size_t n, num;
    bool embed = true;
    pmix_info_t *iptr = NULL;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix:group_construct_nb called");

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

    /* need to add the fence request to the provided info
     * structs as this is a blocking operation - only add
     * it if the user didn't specify this themselves */
    for (n=0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_EMBED_BARRIER)) {
            embed = PMIX_INFO_TRUE(&info[n]);
            break;
        }
    }
    if (embed) {
        PMIX_INFO_CREATE(iptr, ninfo + 1);
        num = ninfo + 1;
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&iptr[n], &info[n]);
        }
        PMIX_INFO_LOAD(&iptr[ninfo], PMIX_EMBED_BARRIER, &embed, PMIX_BOOL);
    } else {
        iptr = (pmix_info_t*)info;
        num = ninfo;
    }

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* pack the group ID */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &grp, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* pack the number of procs */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &nprocs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, procs, nprocs, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* pack the info structs */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &num, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        goto done;
    }
    if (0 < num) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, iptr, num, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            goto done;
        }
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_group_tracker_t);
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, grp_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cb);
    }

  done:
    if (embed && NULL != iptr) {
        PMIX_INFO_FREE(iptr, num);
    }
    if (PMIX_SUCCESS != rc && NULL != msg) {
        PMIX_RELEASE(msg);
    }
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_destruct(const char grp[],
                                              const pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    pmix_group_tracker_t cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix: group_destruct called");

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
    PMIX_CONSTRUCT(&cb, pmix_group_tracker_t);

    /* push the message into our event base to send to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Group_destruct_nb(grp, info, ninfo, op_cbfunc, (void*)&cb))) {
        PMIX_ERROR_LOG(rc);
        PMIX_DESTRUCT(&cb);
        return rc;
    }

    /* wait for the connect to complete */
    PMIX_WAIT_THREAD(&cb.lock);
    rc = cb.status;
    PMIX_DESTRUCT(&cb);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix: group destruct completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_destruct_nb(const char grp[],
                                                 const pmix_info_t info[], size_t ninfo,
                                                 pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg = NULL;
    pmix_cmd_t cmd = PMIX_GROUP_DESTRUCT_CMD;
    pmix_status_t rc;
    pmix_group_tracker_t *cb = NULL;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix:group_destruct_nb called");

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
    if (NULL == grp) {
        return PMIX_ERR_BAD_PARAM;
    }

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* pack the group ID */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &grp, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* pack the info structs */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        goto done;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            goto done;
        }
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_group_tracker_t);
    cb->opcbfunc = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, grp_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cb);
    }

  done:
    if (PMIX_SUCCESS != rc && NULL != msg) {
        PMIX_RELEASE(msg);
    }
    return rc;
}

static void chaincbfunc(pmix_status_t status, void *cbdata)
{
    pmix_group_tracker_t *cb = (pmix_group_tracker_t*)cbdata;

    if (NULL != cb) {
        PMIX_RELEASE(cb);
    }
}

static void relcbfunc(void *cbdata)
{
    pmix_group_tracker_t *cb = (pmix_group_tracker_t*)cbdata;

    PMIX_RELEASE(cb);
}

static void invite_handler(size_t evhdlr_registration_id,
                           pmix_status_t status,
                           const pmix_proc_t *source,
                           pmix_info_t info[], size_t ninfo,
                           pmix_info_t *results, size_t nresults,
                           pmix_event_notification_cbfunc_fn_t cbfunc,
                           void *cbdata)
{
    pmix_group_tracker_t *cb = NULL;
    pmix_proc_t *affected = NULL;
    size_t n;
    pmix_status_t rc = PMIX_GROUP_INVITE_DECLINED;
    size_t contextid = SIZE_MAX;

    /* find the object we asked to be returned with event */
    for (n=0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_EVENT_RETURN_OBJECT)) {
            if (PMIX_POINTER != info[n].value.type) {
                /* this is an unrecoverable error - need to abort */
            }
            cb = (pmix_group_tracker_t*)info[n].value.data.ptr;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_EVENT_AFFECTED_PROC)) {
            if (PMIX_PROC != info[n].value.type) {
                /* this is an unrecoverable error - need to abort */
            }
            affected = info[n].value.data.proc;
        }  else if (PMIX_CHECK_KEY(&info[n], PMIX_GROUP_CONTEXT_ID)) {
            PMIX_VALUE_GET_NUMBER(rc, &info[n].value, contextid, size_t);
        }
    }
    if (NULL == cb) {
        pmix_output(0, "[%s:%d] INVITE HANDLER NULL OBJECT", pmix_globals.myid.nspace, pmix_globals.myid.rank);
        /* this is an unrecoverable error - need to abort */
        /* always must continue the chain */
        cbfunc(rc, NULL, 0, chaincbfunc, NULL, cbdata);
        return;
    }

    /* if the status is accepted, then record it */
    if (PMIX_GROUP_INVITE_ACCEPTED == status) {
        cb->accepted++;
        /* allow the chain to continue */
        rc = PMIX_SUCCESS;
        goto complete;
    }

    /* if the reporting process terminated, then issue the corresponding
     * group event - it only goes to this process */
    if (PMIX_PROC_TERMINATED == status) {
        cb->ninfo = 2;
        PMIX_INFO_CREATE(cb->info, cb->ninfo);
        PMIX_INFO_LOAD(&cb->info[0], PMIX_EVENT_AFFECTED_PROC, affected, PMIX_PROC);
        PMIX_INFO_LOAD(&cb->info[1], PMIX_GROUP_CONTEXT_ID, &contextid, PMIX_SIZE);
        rc = PMIx_Notify_event(PMIX_GROUP_INVITE_FAILED,
                               &pmix_globals.myid,
                               PMIX_RANGE_PROC_LOCAL,
                               cb->info, cb->ninfo,
                               chaincbfunc, (void*)cb);
        if (PMIX_SUCCESS != rc) {
            /* this is an unrecoverable error - need to abort */
            pmix_output(0, "[%s:%d] INVITE HANDLER ERROR", pmix_globals.myid.nspace, pmix_globals.myid.rank);
        }
        PMIX_INFO_FREE(cb->info, cb->ninfo);
        goto complete;
    }

    /* otherwise, we consider the process as having "declined" and
     * ignore it here - if the user registered a handler for that
     * case, then the chain will eventually call it
     */

  complete:
    /* check for complete */
    if (cb->accepted == cb->nmembers) {
        /* execute the completion callback */
        if (NULL != cb->cbfunc) {
            cb->cbfunc(PMIX_SUCCESS, NULL, 0, cb->cbdata, relcbfunc, cb);
            rc = PMIX_EVENT_ACTION_COMPLETE;
        }
    }

    /* always must continue the chain */
    cbfunc(rc, cb->results, cb->nresults, chaincbfunc, cb, cbdata);
    return;
}

static void regcbfunc(pmix_status_t status,
                      size_t refid,
                      void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t*)cbdata;

    cb->status = status;
    cb->errhandler_ref = refid;
    PMIX_WAKEUP_THREAD(&cb->lock);
}

PMIX_EXPORT pmix_status_t PMIx_Group_invite(const char grp[],
                                            const pmix_proc_t procs[], size_t nprocs,
                                            const pmix_info_t info[], size_t ninfo,
                                            pmix_info_t **results, size_t *nresults)
{
    pmix_group_tracker_t cb;
    pmix_status_t rc;
    size_t n;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, then we cannot notify */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* check for bozo input */
    if (NULL == grp || NULL == procs) {
        return PMIX_ERR_BAD_PARAM;
    }

    PMIX_CONSTRUCT(&cb, pmix_group_tracker_t);

    rc = PMIx_Group_invite_nb(grp, procs, nprocs, info, ninfo, info_cbfunc, (void*)&cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_DESTRUCT(&cb);
        return rc;
    }

    /* wait for the group construction to complete or fail */
    PMIX_WAIT_THREAD(&cb.lock);
    rc = cb.status;
    *results = cb.results;
    *nresults = cb.nresults;
    PMIX_DESTRUCT(&cb);

    /* announce group completion, including list of final
     * members - only sent to members. Servers will intercept
     * and update their membership lists */
    PMIX_CONSTRUCT(&cb, pmix_group_tracker_t);
    PMIX_INFO_CREATE(cb.info, 3);
    if (NULL == cb.info) {
        PMIX_DESTRUCT(&cb);
        return PMIX_ERR_NOMEM;
    }
    cb.ninfo = 3;
    n = 0;
    (void)strncpy(cb.info[n].key, PMIX_EVENT_CUSTOM_RANGE, PMIX_MAX_KEYLEN);
    cb.info[n].value.type = PMIX_DATA_ARRAY;
    PMIX_DATA_ARRAY_CREATE(cb.info[n].value.data.darray, nprocs, PMIX_PROC);
    if (NULL == cb.info[n].value.data.darray ||
        NULL == cb.info[n].value.data.darray->array) {
        PMIX_DESTRUCT(&cb);
        return PMIX_ERR_NOMEM;
    }
    memcpy(cb.info[n].value.data.darray->array, procs, nprocs * sizeof(pmix_proc_t));
    ++n;
    /* mark that this only goes to non-default handlers */
    PMIX_INFO_LOAD(&cb.info[n], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    ++n;
    /* provide the group ID */
    PMIX_INFO_LOAD(&cb.info[n], PMIX_GROUP_ID, grp, PMIX_STRING);

    /* notify members */
    rc = PMIx_Notify_event(PMIX_GROUP_CONSTRUCT_COMPLETE,
                           &pmix_globals.myid,
                           PMIX_RANGE_CUSTOM,
                           cb.info, cb.ninfo,
                           op_cbfunc, (void*)&cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_DESTRUCT(&cb);
        return rc;
    }

    /* wait for the notify to get out */
    PMIX_WAIT_THREAD(&cb.lock);
    rc = cb.status;
    PMIX_DESTRUCT(&cb);
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_invite_nb(const char grp[],
                                               const pmix_proc_t procs[], size_t nprocs,
                                               const pmix_info_t info[], size_t ninfo,
                                               pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_group_tracker_t *cb;
    pmix_cb_t lock;
    pmix_status_t codes[] = {
        PMIX_GROUP_INVITE_ACCEPTED,
        PMIX_GROUP_INVITE_DECLINED,
        PMIX_PROC_TERMINATED
    };
    size_t ncodes, n;
    pmix_info_t myinfo[2];
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, then we cannot notify */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* check for bozo input */
    if (NULL == grp || NULL == procs) {
        return PMIX_ERR_BAD_PARAM;
    }

    cb = PMIX_NEW(pmix_group_tracker_t);
    if (NULL == cb) {
        return PMIX_ERR_NOMEM;
    }
    cb->cbfunc = cbfunc;
    cb->cbdata = cbdata;
    cb->accepted = 1;  // obviously, we accept

    /* compute the number of proposed members */
    for (n=0; n < nprocs; n++) {
        if (PMIX_RANK_WILDCARD == procs[n].rank) {
            /* must get the number of procs in this nspace */
        } else {
            cb->nmembers++;
        }
    }

    /* register an event handler specifically to respond
     * to accept responses */
    PMIX_INFO_LOAD(&myinfo[0], PMIX_EVENT_RETURN_OBJECT, cb, PMIX_POINTER);
    PMIX_INFO_LOAD(&myinfo[1], PMIX_EVENT_HDLR_PREPEND, NULL, PMIX_BOOL);
    ncodes = sizeof(codes)/sizeof(pmix_status_t);
    PMIX_CONSTRUCT(&lock, pmix_cb_t);
    PMIx_Register_event_handler(codes, ncodes, myinfo, 2,
                                invite_handler, regcbfunc, &lock);
    PMIX_WAIT_THREAD(&lock.lock);
    rc = lock.status;
    cb->ref = lock.errhandler_ref;
    PMIX_DESTRUCT(&lock);
    PMIX_INFO_DESTRUCT(&myinfo[0]);
    PMIX_INFO_DESTRUCT(&myinfo[1]);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* check for directives */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
                /* setup a timer */
                break;
            }
        }
    }

    /* limit the range to just the procs we are inviting */
    PMIX_INFO_CREATE(cb->info, 3);
    if (NULL == cb->info) {
        PMIX_CONSTRUCT(&lock, pmix_cb_t);
        PMIx_Deregister_event_handler(cb->ref,
                                      op_cbfunc, &lock);
        PMIX_WAIT_THREAD(&lock.lock);
        PMIX_DESTRUCT(&lock);
        PMIX_RELEASE(cb);
        return PMIX_ERR_NOMEM;
    }
    cb->ninfo = 3;
    n = 0;
    (void)strncpy(cb->info[n].key, PMIX_EVENT_CUSTOM_RANGE, PMIX_MAX_KEYLEN);
    cb->info[n].value.type = PMIX_DATA_ARRAY;
    PMIX_DATA_ARRAY_CREATE(cb->info[n].value.data.darray, nprocs, PMIX_PROC);
    if (NULL == cb->info[n].value.data.darray ||
        NULL == cb->info[n].value.data.darray->array) {
        PMIX_CONSTRUCT(&lock, pmix_cb_t);
        PMIx_Deregister_event_handler(cb->ref,
                                      op_cbfunc, &lock);
        PMIX_WAIT_THREAD(&lock.lock);
        PMIX_DESTRUCT(&lock);
        PMIX_RELEASE(cb);
        return PMIX_ERR_NOMEM;
    }
    memcpy(cb->info[n].value.data.darray->array, procs, nprocs * sizeof(pmix_proc_t));
    ++n;
    /* mark that this only goes to non-default handlers */
    PMIX_INFO_LOAD(&cb->info[n], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    ++n;
    /* provide the group ID */
    PMIX_INFO_LOAD(&cb->info[n], PMIX_GROUP_ID, grp, PMIX_STRING);

    /* notify everyone of the invitation */
    PMIX_CONSTRUCT(&lock, pmix_cb_t);
    rc = PMIx_Notify_event(PMIX_GROUP_INVITED,
                           &pmix_globals.myid,
                           PMIX_RANGE_CUSTOM,
                           cb->info, cb->ninfo,
                           op_cbfunc, (void*)&lock);
    PMIX_WAIT_THREAD(&lock.lock);
    rc = lock.status;
    PMIX_DESTRUCT(&lock);
    if (PMIX_SUCCESS != rc) {
        PMIX_CONSTRUCT(&lock, pmix_cb_t);
        PMIx_Deregister_event_handler(cb->ref,
                                      op_cbfunc, &lock);
        PMIX_WAIT_THREAD(&lock.lock);
        PMIX_DESTRUCT(&lock);
        PMIX_RELEASE(cb);
    }

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_join(const char grp[],
                                          const pmix_proc_t *leader,
                                          pmix_group_opt_t opt,
                                          const pmix_info_t info[], size_t ninfo,
                                          pmix_info_t **results, size_t *nresults)
{
    pmix_status_t rc;
    pmix_group_tracker_t *cb;

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
     * recv routine so we know which lock to release when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_group_tracker_t);

    if (PMIX_SUCCESS != (rc = PMIx_Group_join_nb(grp, leader, opt, info, ninfo, info_cbfunc, cb))) {
        PMIX_RELEASE(cb);
        return rc;
    }

    /* wait for the group construction to complete */
    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;
    PMIX_RELEASE(cb);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix: group construction completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_join_nb(const char grp[],
                                             const pmix_proc_t *leader,
                                             pmix_group_opt_t opt,
                                             const pmix_info_t info[], size_t ninfo,
                                             pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_status_t rc;
    pmix_group_tracker_t *cb;
    pmix_status_t code;
    size_t n;
    pmix_data_range_t range;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "[%s:%d] pmix: join nb called",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank);

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we aren't connected, then we cannot notify */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* create a callback object as we need to pass it to the
     * recv routine so we know which lock to release when
     * the notification is done */
    cb = PMIX_NEW(pmix_group_tracker_t);
    cb->cbdata = cbdata;

    /* check for directives */
    if (NULL != info) {
        for (n=0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
                /* setup a timer */
                break;
            }
        }
    }

    /* set the code according to their request */
    if (PMIX_GROUP_ACCEPT == opt) {
        code = PMIX_GROUP_INVITE_ACCEPTED;
    } else {
        code = PMIX_GROUP_INVITE_DECLINED;
    }

    /* only notify the leader so we don't hit all procs */
    if (NULL != leader) {
        range = PMIX_RANGE_CUSTOM;
        PMIX_INFO_CREATE(cb->info, 1);
        if (NULL == cb->info) {
            PMIX_RELEASE(cb);
            return PMIX_ERR_NOMEM;
        }
        PMIX_INFO_LOAD(&cb->info[0], PMIX_EVENT_CUSTOM_RANGE, leader, PMIX_PROC);
        cb->ninfo = 1;
    } else {
        range = PMIX_RANGE_SESSION;
    }

    rc = PMIx_Notify_event(code,
                           &pmix_globals.myid,
                           range,
                           cb->info, cb->ninfo,
                           op_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cb);
    }
    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "[%s:%d] pmix: group invite %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank,
                        (PMIX_GROUP_INVITE_ACCEPTED == code) ? "ACCEPTED" : "DECLINED");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_leave(const char grp[],
                                           const pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    pmix_group_tracker_t cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix: group_leave called");

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
    PMIX_CONSTRUCT(&cb, pmix_group_tracker_t);

    /* push the message into our event base to send to the server */
    if (PMIX_SUCCESS != (rc = PMIx_Group_leave_nb(grp, info, ninfo, op_cbfunc, (void*)&cb))) {
        PMIX_ERROR_LOG(rc);
        PMIX_DESTRUCT(&cb);
        return rc;
    }

    /* wait for the connect to complete */
    PMIX_WAIT_THREAD(&cb.lock);
    rc = cb.status;
    PMIX_DESTRUCT(&cb);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix: group leave completed");

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_Group_leave_nb(const char grp[],
                                              const pmix_info_t info[], size_t ninfo,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg = NULL;
    pmix_cmd_t cmd = PMIX_GROUP_LEAVE_CMD;
    pmix_status_t rc;
    pmix_group_tracker_t *cb = NULL;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.connect_output,
                        "pmix:group_leave_nb called");

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
    if (NULL == grp) {
        return PMIX_ERR_BAD_PARAM;
    }

    msg = PMIX_NEW(pmix_buffer_t);
    /* pack the cmd */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* pack the group ID */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &grp, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto done;
    }

    /* pack the info structs */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        goto done;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            goto done;
        }
    }

    /* create a callback object as we need to pass it to the
     * recv routine so we know which callback to use when
     * the return message is recvd */
    cb = PMIX_NEW(pmix_group_tracker_t);
    cb->opcbfunc = cbfunc;
    cb->cbdata = cbdata;

    /* push the message into our event base to send to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, grp_cbfunc, (void*)cb);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(cb);
    }

  done:
    if (PMIX_SUCCESS != rc && NULL != msg) {
        PMIX_RELEASE(msg);
    }
    return rc;
}

static void op_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_group_tracker_t *cb = (pmix_group_tracker_t*)cbdata;

    cb->status = status;
    if (NULL != cb->opcbfunc) {
        cb->opcbfunc(status, cb->cbdata);
    }
    PMIX_POST_OBJECT(cb);
    PMIX_WAKEUP_THREAD(&cb->lock);
}

static void relfn(void *cbdata)
{
    pmix_group_tracker_t *cb = (pmix_group_tracker_t*)cbdata;
    PMIX_RELEASE(cb);
}
static void grp_cbfunc(struct pmix_peer_t *pr,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_group_tracker_t *cb = (pmix_group_tracker_t*)cbdata;
    pmix_status_t rc;
    pmix_status_t ret;
    int32_t cnt;
    size_t ctxid, ninfo=0;
    pmix_info_t info, *iptr=NULL;

    pmix_output_verbose(2, pmix_client_globals.connect_output,
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
    /* unpack any ctxid that was provided - it is okay if
     * this attempts to unpack past end of buffer */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver,
                       buf, &ctxid, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc && PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        ret = rc;
    } else {
        PMIX_INFO_LOAD(&info, PMIX_GROUP_CONTEXT_ID, &ctxid, PMIX_SIZE);
        iptr = &info;
        ninfo = 1;
    }

  report:
    if (NULL != cb->cbfunc) {
        cb->cbfunc(ret, iptr, ninfo, cb->cbdata, relfn, cb);
        return;
    } else if (NULL != cb->opcbfunc) {
        cb->opcbfunc(ret, cb->cbdata);
        return;
    }
    PMIX_RELEASE(cb);
}

static void info_cbfunc(pmix_status_t status,
                        pmix_info_t *info, size_t ninfo,
                        void *cbdata,
                        pmix_release_cbfunc_t release_fn,
                        void *release_cbdata)
{
    pmix_group_tracker_t *cb = (pmix_group_tracker_t*)cbdata;
    size_t n;

    /* see if anything was returned - e.g., a context id */
    cb->status = status;
    /* copy/save any returned info */
    if (NULL != info) {
        cb->nresults = ninfo;
        PMIX_INFO_CREATE(cb->results, cb->nresults);
        for (n=0; n < ninfo; n++) {
            PMIX_INFO_XFER(&cb->results[n], &info[n]);
        }
    }
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    PMIX_POST_OBJECT(cb);
    PMIX_WAKEUP_THREAD(&cb->lock);
}
