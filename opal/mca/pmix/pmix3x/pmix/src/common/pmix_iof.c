/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      IBM Corporation.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "src/include/pmix_config.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#else
#ifdef HAVE_SYS_FCNTL_H
#include <sys/fcntl.h>
#endif
#endif

#include "src/include/pmix_stdint.h"
#include "src/include/pmix_socket_errno.h"

#include "include/pmix.h"
#include "include/pmix_common.h"
#include "include/pmix_server.h"

#include "src/threads/threads.h"
#include "src/util/argv.h"
#include "src/util/error.h"
#include "src/util/name_fns.h"
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
    size_t refid = 0;

    PMIX_ACQUIRE_OBJECT(cd);

    /* unpack the return status */
    m=1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &status, &m, PMIX_STATUS);
    if (NULL != cd->iofreq && PMIX_SUCCESS == rc && PMIX_SUCCESS == status) {
        /* get the reference ID */
        m=1;
        PMIX_BFROPS_UNPACK(rc, peer, buf, &refid, &m, PMIX_SIZE);
        /* store the remote reference id */
        cd->iofreq->remote_id = refid;
        if (NULL != cd->cbfunc.hdlrregcbfn) {
            cd->cbfunc.hdlrregcbfn(PMIX_SUCCESS, cd->iofreq->local_id, cd->cbdata);
        }
    } else if (PMIX_SUCCESS != rc) {
        status = rc;
    }

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof_register/deregister returned status %s", PMIx_Error_string(status));

    if (NULL == cd->iofreq) {
        /* this was a deregistration request */
        if (NULL == cd->cbfunc.opcbfn) {
            cd->status = status;
            PMIX_WAKEUP_THREAD(&cd->lock);
        } else {
            cd->cbfunc.opcbfn(status, cd->cbdata);
        }
    } else if (PMIX_SUCCESS != status) {
        pmix_pointer_array_set_item(&pmix_globals.iof_requests, cd->iofreq->local_id, NULL);
        PMIX_RELEASE(cd->iofreq);
    } else if (NULL == cd->cbfunc.hdlrregcbfn) {
        cd->status = status;
        cd->iofreq->remote_id = refid;
        PMIX_WAKEUP_THREAD(&cd->lock);
    } else {
        cd->iofreq->remote_id = refid;
        cd->cbfunc.hdlrregcbfn(PMIX_SUCCESS, cd->iofreq->local_id, cd->cbdata);
    }

    PMIX_RELEASE(cd);
}

static void mycbfn(pmix_status_t status,
                   size_t refid,
                   void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;

    PMIX_ACQUIRE_OBJECT(cd);
    if (PMIX_SUCCESS == status) {
        cd->status = refid;
    } else {
        cd->status = status;
    }

    PMIX_WAKEUP_THREAD(&cd->lock);
}

PMIX_EXPORT pmix_status_t PMIx_IOF_pull(const pmix_proc_t procs[], size_t nprocs,
                                        const pmix_info_t directives[], size_t ndirs,
                                        pmix_iof_channel_t channel, pmix_iof_cbfunc_t cbfunc,
                                        pmix_hdlr_reg_cbfunc_t regcbfunc, void *regcbdata)
{
    pmix_shift_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_IOF_PULL_CMD;
    pmix_buffer_t *msg = NULL;
    pmix_status_t rc;
    pmix_iof_req_t *req;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof_register");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we are a server, we cannot do this */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* we don't allow stdin to flow thru this path */
    if (PMIX_FWD_STDIN_CHANNEL & channel) {
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
    if (NULL == regcbfunc) {
        cd->cbfunc.hdlrregcbfn = mycbfn;
        PMIX_RETAIN(cd);
        cd->cbdata = cd;
    } else {
        cd->cbfunc.hdlrregcbfn = regcbfunc;
        cd->cbdata = regcbdata;
    }

    /* setup the request item */
    req = PMIX_NEW(pmix_iof_req_t);
    if (NULL == req) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* retain the channels and cbfunc */
    req->channels = channel;
    req->cbfunc = cbfunc;
    req->local_id = pmix_pointer_array_add(&pmix_globals.iof_requests, req);
    cd->iofreq = req;
    /* we don't need the source specifications - only the
    * server cares as it will filter against them */

    /* setup the registration cmd */
    msg = PMIX_NEW(pmix_buffer_t);
    if (NULL == msg) {
        PMIX_RELEASE(req);
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
        if (NULL != msg) {
            PMIX_RELEASE(msg);
        }
        PMIX_RELEASE(req);
        PMIX_RELEASE(cd);
    } else if (NULL == regcbfunc) {
        PMIX_WAIT_THREAD(&cd->lock);
        rc = cd->status;
        if (0 > rc) {
            /* the request failed */
            pmix_pointer_array_set_item(&pmix_globals.iof_requests, req->local_id, NULL);
            PMIX_RELEASE(req);
        }
        PMIX_RELEASE(cd);
    }
    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_IOF_deregister(size_t iofhdlr,
                                              const pmix_info_t directives[], size_t ndirs,
                                              pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_shift_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_IOF_DEREG_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc;
    pmix_iof_req_t *req;
    size_t remote_id;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof_deregister");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we are a server, we cannot do this */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) &&
        !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    req = (pmix_iof_req_t*)pmix_pointer_array_get_item(&pmix_globals.iof_requests, iofhdlr);
    if (NULL == req) {
        /* bad value */
        return PMIX_ERR_BAD_PARAM;
    }
    remote_id = req->remote_id;
    pmix_pointer_array_set_item(&pmix_globals.iof_requests, iofhdlr, NULL);
    PMIX_RELEASE(req);

    /* send this request to the server */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;

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

    /* pack the remote handler ID */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &remote_id, 1, PMIX_SIZE);

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof_dereg sending to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, msgcbfunc, (void*)cd);

  cleanup:
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        PMIX_RELEASE(cd);
    } else if (NULL == cbfunc) {
        PMIX_WAIT_THREAD(&cd->lock);
        rc = cd->status;
        PMIX_RELEASE(cd);
    }
    return rc;
}

typedef struct {
    pmix_object_t super;
    pmix_event_t ev;
    pmix_lock_t lock;
    pmix_status_t status;
    pmix_op_cbfunc_t cbfunc;
    void *cbdata;
} pmix_ltcaddy_t;

static void ltcon(pmix_ltcaddy_t *p)
{
    PMIX_CONSTRUCT_LOCK(&p->lock);
}
static void ltdes(pmix_ltcaddy_t *p)
{
    PMIX_DESTRUCT_LOCK(&p->lock);
}
static PMIX_CLASS_INSTANCE(pmix_ltcaddy_t,
                           pmix_object_t,
                           ltcon, ltdes);

static pmix_event_t stdinsig_ev;
static pmix_iof_read_event_t *stdinev = NULL;

static void stdincbfunc(struct pmix_peer_t *peer,
                        pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_ltcaddy_t *cd = (pmix_ltcaddy_t*)cbdata;
    int cnt;
    pmix_status_t rc, status;

    /* a zero-byte buffer indicates that this recv is being
     * completed due to a lost connection */
    if (PMIX_BUFFER_IS_EMPTY(buf)) {
        /* release the caller */
        if (NULL != cd->cbfunc) {
            cd->cbfunc(PMIX_ERR_COMM_FAILURE, cd->cbdata);
        }
        free(cd);
        return;
    }

    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        status = rc;
    }
    if (NULL != cd->cbfunc) {
        cd->cbfunc(status, cd->cbdata);
    }
    free(cd);
}

static void myopcb(pmix_status_t status, void *cbdata)
{
    pmix_ltcaddy_t *cd = (pmix_ltcaddy_t*)cbdata;

    cd->status = status;
    PMIX_WAKEUP_THREAD(&cd->lock);
}

pmix_status_t PMIx_IOF_push(const pmix_proc_t targets[], size_t ntargets,
                            pmix_byte_object_t *bo,
                            const pmix_info_t directives[], size_t ndirs,
                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_IOF_PUSH_CMD;
    pmix_status_t rc = PMIX_SUCCESS;
    pmix_ltcaddy_t *cd;
    size_t n;
    bool begincollecting, stopcollecting;
    int flags, fd = fileno(stdin);

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    if (NULL == bo) {
        /* check the directives */
        for (n=0; n < ndirs; n++) {
            if (PMIX_CHECK_KEY(&directives[n], PMIX_IOF_PUSH_STDIN)) {
                /* we are to start collecting our stdin and pushing
                 * it to the specified targets */
                begincollecting = PMIX_INFO_TRUE(&directives[n]);
                if (begincollecting) {
                    /* add these targets to our list */
                    if (!pmix_globals.pushstdin) {
                        /* not already collecting, so start */
                        pmix_globals.pushstdin = true;
                        /* We don't want to set nonblocking on our
                         * stdio stream.  If we do so, we set the file descriptor to
                         * non-blocking for everyone that has that file descriptor, which
                         * includes everyone else in our shell pipeline chain.  (See
                         * http://lists.freebsd.org/pipermail/freebsd-hackers/2005-January/009742.html).
                         * This causes things like "prun -np 1 big_app | cat" to lose
                         * output, because cat's stdout is then ALSO non-blocking and cat
                         * isn't built to deal with that case (same with almost all other
                         * unix text utils).
                         */
                        if (0 != fd) {
                            if((flags = fcntl(fd, F_GETFL, 0)) < 0) {
                                pmix_output(pmix_client_globals.iof_output,
                                            "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n",
                                            __FILE__, __LINE__, errno);
                            } else {
                                flags |= O_NONBLOCK;
                                fcntl(fd, F_SETFL, flags);
                            }
                        }
                        if (isatty(fd)) {
                            /* We should avoid trying to read from stdin if we
                             * have a terminal, but are backgrounded.  Catch the
                             * signals that are commonly used when we switch
                             * between being backgrounded and not.  If the
                             * filedescriptor is not a tty, don't worry about it
                             * and always stay connected.
                             */
                            pmix_event_signal_set(pmix_globals.evbase, &stdinsig_ev,
                                                  SIGCONT, pmix_iof_stdin_cb,
                                                  NULL);

                            /* setup a read event to read stdin, but don't activate it yet. The
                             * dst_name indicates who should receive the stdin. If that recipient
                             * doesn't do a corresponding pull, however, then the stdin will
                             * be dropped upon receipt at the local daemon
                             */
                            PMIX_IOF_READ_EVENT(&stdinev,
                                                targets, ntargets,
                                                directives, ndirs, fd,
                                                pmix_iof_read_local_handler, false);

                            /* check to see if we want the stdin read event to be
                             * active - we will always at least define the event,
                             * but may delay its activation
                             */
                            if (pmix_iof_stdin_check(fd)) {
                                PMIX_IOF_READ_ACTIVATE(stdinev);
                            }
                        } else {
                            /* if we are not looking at a tty, just setup a read event
                             * and activate it
                             */
                            PMIX_IOF_READ_EVENT(&stdinev, targets, ntargets,
                                                directives, ndirs, fd,
                                                pmix_iof_read_local_handler, true);
                        }
                    }
                } else {
                    if (pmix_globals.pushstdin) {
                        /* remove these targets from the list of
                         * recipients - if the list is then empty,
                         * stop collecting. If the targets param
                         * is NULL, then remove all targets and stop.
                         * Flush any cached input before calling
                         * the cbfunc */
                    }
                }
            } else if (PMIX_CHECK_KEY(&directives[n], PMIX_IOF_COMPLETE)) {
                /* if we are collecting our stdin for the specified
                 * targets, then stop - a NULL for targets indicates
                 * stop for everyone. Flush any remaining cached input
                 * before calling the cbfunc */
                stopcollecting = PMIX_INFO_TRUE(&directives[n]);
                if (stopcollecting) {
                    if (pmix_globals.pushstdin) {
                        /* remove these targets from the list of
                         * recipients - if the list is then empty,
                         * stop collecting */
                    }
                }
            }
        }
        return PMIX_OPERATION_SUCCEEDED;
    }

    /* if we are not a server, then we send the provided
     * data to our server for processing */
    if (!PMIX_PEER_IS_SERVER(pmix_globals.mypeer) ||
        PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        msg = PMIX_NEW(pmix_buffer_t);
        if (NULL == msg) {
            return PMIX_ERR_NOMEM;
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, &ntargets, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
        if (0 < ntargets) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                             msg, targets, ntargets, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return rc;
            }
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, &ndirs, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
        if (0 < ndirs) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                             msg, directives, ndirs, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return rc;
            }
        }
        if (NULL != bo) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                             msg, bo, 1, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return rc;
            }
        }

        cd = PMIX_NEW(pmix_ltcaddy_t);
        if (NULL == cd) {
            PMIX_RELEASE(msg);
            rc = PMIX_ERR_NOMEM;
            return rc;
        }
        if (NULL == cbfunc) {
            cd->cbfunc = myopcb;
            PMIX_RETAIN(cd);
            cd->cbdata = cd;
        } else {
            cd->cbfunc = cbfunc;
            cd->cbdata = cbdata;
        }
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                           msg, stdincbfunc, cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            free(cd);
        } else if (NULL == cbfunc) {
            PMIX_WAIT_THREAD(&cd->lock);
            rc = cd->status;
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    /* if we are a server, just pass the data up to our host */
    if (NULL == pmix_host_server.push_stdin) {
        return PMIX_ERR_NOT_SUPPORTED;
    }
    rc = pmix_host_server.push_stdin(&pmix_globals.myid,
                                     targets, ntargets,
                                     directives, ndirs,
                                     bo, cbfunc, cbdata);
    return rc;
}

pmix_status_t pmix_iof_process_iof(pmix_iof_channel_t channels,
                                   const pmix_proc_t *source,
                                   const pmix_byte_object_t *bo,
                                   const pmix_info_t *info, size_t ninfo,
                                   const pmix_iof_req_t *req)
{
    bool match;
    size_t m;
    pmix_buffer_t *msg;
    pmix_status_t rc;

    /* if the channel wasn't included, then ignore it */
    if (!(channels & req->channels)) {
        return PMIX_SUCCESS;
    }
    /* see if the source matches the request */
    match = false;
    for (m=0; m < req->nprocs; m++) {
        if (PMIX_CHECK_PROCID(source, &req->procs[m])) {
            match = true;
            break;
        }
    }
    if (!match) {
        return PMIX_SUCCESS;
    }
    /* never forward back to the source! This can happen if the source
     * is a launcher - also, never forward to a peer that is no
     * longer with us */
    if (NULL == req->requestor->info || req->requestor->finalized) {
        return PMIX_SUCCESS;
    }
    if (PMIX_CHECK_PROCID(source, &req->requestor->info->pname)) {
        return PMIX_SUCCESS;
    }
    /* setup the msg */
    if (NULL == (msg = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        return PMIX_ERR_OUT_OF_RESOURCE;
    }
    /* provide the source */
    PMIX_BFROPS_PACK(rc, req->requestor, msg, source, 1, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* provide the channel */
    PMIX_BFROPS_PACK(rc, req->requestor, msg, &channels, 1, PMIX_IOF_CHANNEL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* provide their local handler ID so they know which cbfunc to use */
    PMIX_BFROPS_PACK(rc, req->requestor, msg, &req->remote_id, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* pack the number of info's provided */
    PMIX_BFROPS_PACK(rc, req->requestor, msg, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* if some were provided, then pack them too */
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, req->requestor, msg, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
    }
    /* pack the data */
    PMIX_BFROPS_PACK(rc, req->requestor, msg, bo, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return rc;
    }
    /* send it to the requestor */
    PMIX_PTL_SEND_ONEWAY(rc, req->requestor, msg, PMIX_PTL_TAG_IOF);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
    }
    return PMIX_OPERATION_SUCCEEDED;
}

pmix_status_t pmix_iof_write_output(const pmix_proc_t *name,
                                    pmix_iof_channel_t stream,
                                    const pmix_byte_object_t *bo,
                                    pmix_iof_flags_t *flags)
{
    char starttag[PMIX_IOF_BASE_TAG_MAX], endtag[PMIX_IOF_BASE_TAG_MAX], *suffix;
    pmix_iof_write_output_t *output;
    size_t i;
    int j, k, starttaglen, endtaglen, num_buffered;
    bool endtagged;
    char qprint[10];
    pmix_iof_write_event_t *channel;
    pmix_iof_flags_t myflags;

    if (PMIX_FWD_STDOUT_CHANNEL & stream) {
        channel = &pmix_client_globals.iof_stdout.wev;
    } else {
        channel = &pmix_client_globals.iof_stderr.wev;
    }
    if (NULL == flags) {
        myflags.xml = pmix_globals.xml_output;
        if (pmix_globals.timestamp_output) {
            time(&myflags.timestamp);
        } else {
            myflags.timestamp = 0;
        }
        myflags.tag = pmix_globals.tag_output;
    } else {
        myflags = *flags;
    }

    PMIX_OUTPUT_VERBOSE((1, pmix_client_globals.iof_output,
                         "%s write:output setting up to write %lu bytes to %s for %s on fd %d",
                         PMIX_NAME_PRINT(&pmix_globals.myid),
                         (unsigned long)bo->size,
                         PMIx_IOF_channel_string(stream),
                         PMIX_NAME_PRINT(name),
                         (NULL == channel) ? -1 : channel->fd));

    /* setup output object */
    output = PMIX_NEW(pmix_iof_write_output_t);
    memset(starttag, 0, PMIX_IOF_BASE_TAG_MAX);
    memset(endtag, 0, PMIX_IOF_BASE_TAG_MAX);

    /* write output data to the corresponding tag */
    if (PMIX_FWD_STDIN_CHANNEL & stream) {
        /* copy over the data to be written */
        if (0 < bo->size) {
            /* don't copy 0 bytes - we just need to pass
             * the zero bytes so the fd can be closed
             * after it writes everything out
             */
            memcpy(output->data, bo->bytes, bo->size);
        }
        output->numbytes = bo->size;
        goto process;
    } else if (PMIX_FWD_STDOUT_CHANNEL & stream) {
        /* write the bytes to stdout */
        suffix = "stdout";
    } else if (PMIX_FWD_STDERR_CHANNEL & stream) {
        /* write the bytes to stderr */
        suffix = "stderr";
    } else if (PMIX_FWD_STDDIAG_CHANNEL & stream) {
        /* write the bytes to stderr */
        suffix = "stddiag";
    } else {
        /* error - this should never happen */
        PMIX_ERROR_LOG(PMIX_ERR_VALUE_OUT_OF_BOUNDS);
        PMIX_OUTPUT_VERBOSE((1, pmix_client_globals.iof_output,
                             "%s stream %0x", PMIX_NAME_PRINT(&pmix_globals.myid), stream));
        return PMIX_ERR_VALUE_OUT_OF_BOUNDS;
    }

    /* if this is to be xml tagged, create a tag with the correct syntax - we do not allow
     * timestamping of xml output
     */
    if (myflags.xml) {
        snprintf(starttag, PMIX_IOF_BASE_TAG_MAX, "<%s rank=\"%s\">", suffix, PMIX_RANK_PRINT(name->rank));
        snprintf(endtag, PMIX_IOF_BASE_TAG_MAX, "</%s>", suffix);
        goto construct;
    }

    /* if we are to timestamp output, start the tag with that */
    if (0 < myflags.timestamp) {
        char *cptr;
        /* get the timestamp */
        cptr = ctime(&myflags.timestamp);
        cptr[strlen(cptr)-1] = '\0';  /* remove trailing newline */

        if (myflags.tag) {
            /* if we want it tagged as well, use both */
            snprintf(starttag, PMIX_IOF_BASE_TAG_MAX, "%s[%s]<%s>:",
                     cptr, PMIX_NAME_PRINT(name), suffix);
        } else {
            /* only use timestamp */
            snprintf(starttag, PMIX_IOF_BASE_TAG_MAX, "%s<%s>:", cptr, suffix);
        }
        /* no endtag for this option */
        memset(endtag, '\0', PMIX_IOF_BASE_TAG_MAX);
        goto construct;
    }

    if (myflags.tag) {
        snprintf(starttag, PMIX_IOF_BASE_TAG_MAX, "[%s]<%s>:",
                 PMIX_NAME_PRINT(name), suffix);
        /* no endtag for this option */
        memset(endtag, '\0', PMIX_IOF_BASE_TAG_MAX);
        goto construct;
    }

    /* if we get here, then the data is not to be tagged - just copy it
     * and move on to processing
     */
    if (0 < bo->size) {
        /* don't copy 0 bytes - we just need to pass
         * the zero bytes so the fd can be closed
         * after it writes everything out
         */
        memcpy(output->data, bo->bytes, bo->size);
    }
    output->numbytes = bo->size;
    goto process;

  construct:
    starttaglen = strlen(starttag);
    endtaglen = strlen(endtag);
    endtagged = false;
    /* start with the tag */
    for (j=0, k=0; j < starttaglen && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; j++) {
        output->data[k++] = starttag[j];
    }
    /* cycle through the data looking for <cr>
     * and replace those with the tag
     */
    for (i=0; i < bo->size && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; i++) {
        if (myflags.xml) {
            if ('&' == bo->bytes[i]) {
                if (k+5 >= PMIX_IOF_BASE_TAGGED_OUT_MAX) {
                    PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&amp;");
                for (j=0; j < (int)strlen(qprint) && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
            } else if ('<' == bo->bytes[i]) {
                if (k+4 >= PMIX_IOF_BASE_TAGGED_OUT_MAX) {
                    PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&lt;");
                for (j=0; j < (int)strlen(qprint) && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
            } else if ('>' == bo->bytes[i]) {
                if (k+4 >= PMIX_IOF_BASE_TAGGED_OUT_MAX) {
                    PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&gt;");
                for (j=0; j < (int)strlen(qprint) && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
            } else if (bo->bytes[i] < 32 || bo->bytes[i] > 127) {
                /* this is a non-printable character, so escape it too */
                if (k+7 >= PMIX_IOF_BASE_TAGGED_OUT_MAX) {
                    PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                    goto process;
                }
                snprintf(qprint, 10, "&#%03d;", (int)bo->bytes[i]);
                for (j=0; j < (int)strlen(qprint) && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; j++) {
                    output->data[k++] = qprint[j];
                }
                /* if this was a \n, then we also need to break the line with the end tag */
                if ('\n' == bo->bytes[i] && (k+endtaglen+1) < PMIX_IOF_BASE_TAGGED_OUT_MAX) {
                    /* we need to break the line with the end tag */
                    for (j=0; j < endtaglen && k < PMIX_IOF_BASE_TAGGED_OUT_MAX-1; j++) {
                        output->data[k++] = endtag[j];
                    }
                    if (k == PMIX_IOF_BASE_TAGGED_OUT_MAX) {
                        /* out of space */
                        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                        goto process;
                    }
                    /* move the <cr> over */
                    output->data[k++] = '\n';
                    /* if this isn't the end of the data buffer, add a new start tag */
                    if (i < bo->size-1 && (k+starttaglen) < PMIX_IOF_BASE_TAGGED_OUT_MAX) {
                        for (j=0; j < starttaglen && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; j++) {
                            output->data[k++] = starttag[j];
                            endtagged = false;
                        }
                    } else {
                        endtagged = true;
                    }
                }
            } else {
                output->data[k++] = bo->bytes[i];
            }
        } else {
            if ('\n' == bo->bytes[i]) {
                /* we need to break the line with the end tag */
                for (j=0; j < endtaglen && k < PMIX_IOF_BASE_TAGGED_OUT_MAX-1; j++) {
                    output->data[k++] = endtag[j];
                }
                /* move the <cr> over */
                output->data[k++] = '\n';
                /* if this isn't the end of the data buffer, add a new start tag */
                if (i < bo->size-1) {
                    for (j=0; j < starttaglen && k < PMIX_IOF_BASE_TAGGED_OUT_MAX; j++) {
                        output->data[k++] = starttag[j];
                        endtagged = false;
                    }
                } else {
                    endtagged = true;
                }
            } else {
                output->data[k++] = bo->bytes[i];
            }
        }
    }
    if (!endtagged && k < PMIX_IOF_BASE_TAGGED_OUT_MAX) {
        /* need to add an endtag */
        for (j=0; j < endtaglen && k < PMIX_IOF_BASE_TAGGED_OUT_MAX-1; j++) {
            output->data[k++] = endtag[j];
        }
        output->data[k] = '\n';
    }
    output->numbytes = k;

  process:
    /* add this data to the write list for this fd */
    pmix_list_append(&channel->outputs, &output->super);

    /* record how big the buffer is */
    num_buffered = pmix_list_get_size(&channel->outputs);

    /* is the write event issued? */
    if (!channel->pending) {
        /* issue it */
        PMIX_OUTPUT_VERBOSE((1, pmix_client_globals.iof_output,
                             "%s write:output adding write event",
                             PMIX_NAME_PRINT(&pmix_globals.myid)));
        PMIX_IOF_SINK_ACTIVATE(channel);
    }

    return num_buffered;
}

void pmix_iof_static_dump_output(pmix_iof_sink_t *sink)
{
    bool dump;
    int num_written;
    pmix_iof_write_event_t *wev = &sink->wev;
    pmix_iof_write_output_t *output;

    if (!pmix_list_is_empty(&wev->outputs)) {
        dump = false;
        /* make one last attempt to write this out */
        while (NULL != (output = (pmix_iof_write_output_t*)pmix_list_remove_first(&wev->outputs))) {
            if (!dump && 0 < output->numbytes) {
                num_written = write(wev->fd, output->data, output->numbytes);
                if (num_written < output->numbytes) {
                    /* don't retry - just cleanout the list and dump it */
                    dump = true;
                }
            }
            PMIX_RELEASE(output);
        }
    }
}

void pmix_iof_write_handler(int _fd, short event, void *cbdata)
{
    pmix_iof_sink_t *sink = (pmix_iof_sink_t*)cbdata;
    pmix_iof_write_event_t *wev = &sink->wev;
    pmix_list_item_t *item;
    pmix_iof_write_output_t *output;
    int num_written, total_written = 0;

    PMIX_ACQUIRE_OBJECT(sink);

    PMIX_OUTPUT_VERBOSE((1, pmix_client_globals.iof_output,
                         "%s write:handler writing data to %d",
                         PMIX_NAME_PRINT(&pmix_globals.myid),
                         wev->fd));

    while (NULL != (item = pmix_list_remove_first(&wev->outputs))) {
        output = (pmix_iof_write_output_t*)item;
        if (0 == output->numbytes) {
            /* indicates we are to close this stream */
            PMIX_DESTRUCT(sink);
            return;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                pmix_list_prepend(&wev->outputs, item);
                /* if the list is getting too large, abort */
                if (pmix_globals.output_limit < pmix_list_get_size(&wev->outputs)) {
                    pmix_output(0, "IO Forwarding is running too far behind - something is blocking us from writing");
                    goto ABORT;
                }
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                goto NEXT_CALL;
            }
            /* otherwise, something bad happened so all we can do is abort
             * this attempt
             */
            PMIX_RELEASE(output);
            goto ABORT;
        } else if (num_written < output->numbytes) {
            /* incomplete write - adjust data to avoid duplicate output */
            memmove(output->data, &output->data[num_written], output->numbytes - num_written);
            /* adjust the number of bytes remaining to be written */
            output->numbytes -= num_written;
            /* push this item back on the front of the list */
            pmix_list_prepend(&wev->outputs, item);
            /* if the list is getting too large, abort */
            if (pmix_globals.output_limit < pmix_list_get_size(&wev->outputs)) {
                pmix_output(0, "IO Forwarding is running too far behind - something is blocking us from writing");
                goto ABORT;
            }
            /* leave the write event running so it will call us again
             * when the fd is ready
             */
            goto NEXT_CALL;
        }
        PMIX_RELEASE(output);

        total_written += num_written;
        if(wev->always_writable && (PMIX_IOF_SINK_BLOCKSIZE <= total_written)){
            /* If this is a regular file it will never tell us it will block
             * Write no more than PMIX_IOF_REGULARF_BLOCK at a time allowing
             * other fds to progress
             */
            goto NEXT_CALL;
        }
    }
  ABORT:
    wev->pending = false;
    PMIX_POST_OBJECT(wev);
    return;
NEXT_CALL:
    PMIX_IOF_SINK_ACTIVATE(wev);
}

/* return true if we should read stdin from fd, false otherwise */
bool pmix_iof_stdin_check(int fd)
{
#if defined(HAVE_TCGETPGRP)
    if( isatty(fd) && (getpgrp() != tcgetpgrp(fd)) ) {
        return false;
    }
#endif
    return true;
}

void pmix_iof_stdin_cb(int fd, short event, void *cbdata)
{
    bool should_process;
    pmix_iof_read_event_t *stdinev = (pmix_iof_read_event_t*)cbdata;

    PMIX_ACQUIRE_OBJECT(stdinev);

    should_process = pmix_iof_stdin_check(0);

    if (should_process) {
        PMIX_IOF_READ_ACTIVATE(stdinev);
    } else {
        pmix_event_del(&stdinev->ev);
        stdinev->active = false;
        PMIX_POST_OBJECT(stdinev);
    }
}

static void iof_stdin_cbfunc(struct pmix_peer_t *peer,
                             pmix_ptl_hdr_t *hdr,
                             pmix_buffer_t *buf, void *cbdata)
{
    pmix_iof_read_event_t *stdinev = (pmix_iof_read_event_t*)cbdata;
    int cnt;
    pmix_status_t rc, ret;

    PMIX_ACQUIRE_OBJECT(stdinev);

    /* check the return status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ret, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        pmix_event_del(&stdinev->ev);
        stdinev->active = false;
        PMIX_POST_OBJECT(stdinev);
        return;
    }
    /* if the status wasn't success, then terminate the forward */
    if (PMIX_SUCCESS != ret) {
        pmix_event_del(&stdinev->ev);
        stdinev->active = false;
        PMIX_POST_OBJECT(stdinev);
        return;
    }

    pmix_iof_stdin_cb(0, 0, stdinev);
}

/* this is the read handler for stdin */
void pmix_iof_read_local_handler(int unusedfd, short event, void *cbdata)
{
    pmix_iof_read_event_t *rev = (pmix_iof_read_event_t*)cbdata;
    unsigned char data[PMIX_IOF_BASE_MSG_MAX];
    int32_t numbytes;
    pmix_status_t rc;
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_IOF_PUSH_CMD;
    pmix_byte_object_t bo;
    int fd;

    PMIX_ACQUIRE_OBJECT(rev);

    if (0 > rev->fd) {
        fd = fileno(stdin);
    } else {
        fd = rev->fd;
    }
    /* read up to the fragment size */
    memset(data, 0, PMIX_IOF_BASE_MSG_MAX);
    numbytes = read(fd, data, sizeof(data));

    if (numbytes < 0) {
        /* either we have a connection error or it was a non-blocking read */

        /* non-blocking, retry */
        if (EAGAIN == errno || EINTR == errno) {
            PMIX_IOF_READ_ACTIVATE(rev);
            return;
        }

        PMIX_OUTPUT_VERBOSE((1, pmix_client_globals.iof_output,
                             "%s iof:read handler Error on stdin",
                             PMIX_NAME_PRINT(&pmix_globals.myid)));
        /* Un-recoverable error. Allow the code to flow as usual in order to
         * to send the zero bytes message up the stream, and then close the
         * file descriptor and delete the event.
         */
        numbytes = 0;
    }

    /* The event has fired, so it's no longer active until we
       re-add it */
    rev->active = false;

    /* pass the data to our PMIx server so it can relay it
     * to the host RM for distribution */
    msg = PMIX_NEW(pmix_buffer_t);
    if (NULL == msg) {
        /* don't restart the event - just return */
        return;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }
    /* pack the number of targets */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &rev->ntargets, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }
    /* and the targets */
    if (0 < rev->ntargets) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, rev->targets, rev->ntargets, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return;
        }
    }
    /* pack the number of directives */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &rev->ndirs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }
    /* and the directives */
    if (0 < rev->ndirs) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                         msg, rev->directives, rev->ndirs, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return;
        }
    }

    /* pack the data */
    bo.bytes = (char*)data;
    bo.size = numbytes;
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver,
                     msg, &bo, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }

    /* send it to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver,
                       msg, iof_stdin_cbfunc, rev);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
    }

    if (0 < numbytes) {
        PMIX_IOF_READ_ACTIVATE(rev);
    }

    /* nothing more to do */
    return;
}

/* class instances */
static void iof_sink_construct(pmix_iof_sink_t* ptr)
{
    PMIX_CONSTRUCT(&ptr->wev, pmix_iof_write_event_t);
    ptr->xoff = false;
    ptr->exclusive = false;
    ptr->closed = false;
}
static void iof_sink_destruct(pmix_iof_sink_t* ptr)
{
    if (0 <= ptr->wev.fd) {
        PMIX_OUTPUT_VERBOSE((20, pmix_client_globals.iof_output,
                             "%s iof: closing sink for process %s on fd %d",
                             PMIX_NAME_PRINT(&pmix_globals.myid),
                             PMIX_NAME_PRINT(&ptr->name), ptr->wev.fd));
        PMIX_DESTRUCT(&ptr->wev);
    }
}
PMIX_CLASS_INSTANCE(pmix_iof_sink_t,
                    pmix_list_item_t,
                    iof_sink_construct,
                    iof_sink_destruct);


static void iof_read_event_construct(pmix_iof_read_event_t* rev)
{
    rev->fd = -1;
    rev->active = false;
    rev->childproc = NULL;
    rev->tv.tv_sec = 0;
    rev->tv.tv_usec = 0;
    rev->targets = NULL;
    rev->ntargets = 0;
    rev->directives = NULL;
    rev->ndirs = 0;
}
static void iof_read_event_destruct(pmix_iof_read_event_t* rev)
{
    if (rev->active) {
        pmix_event_del(&rev->ev);
    }
    if (0 <= rev->fd) {
        PMIX_OUTPUT_VERBOSE((20, pmix_client_globals.iof_output,
                             "%s iof: closing fd %d",
                             PMIX_NAME_PRINT(&pmix_globals.myid), rev->fd));
        close(rev->fd);
        rev->fd = -1;
    }
    if (NULL != rev->targets) {
        PMIX_PROC_FREE(rev->targets, rev->ntargets);
    }
    if (NULL != rev->directives) {
        PMIX_INFO_FREE(rev->directives, rev->ndirs);
    }
}
PMIX_CLASS_INSTANCE(pmix_iof_read_event_t,
                    pmix_object_t,
                    iof_read_event_construct,
                    iof_read_event_destruct);

static void iof_write_event_construct(pmix_iof_write_event_t* wev)
{
    wev->pending = false;
    wev->always_writable = false;
    wev->fd = -1;
    PMIX_CONSTRUCT(&wev->outputs, pmix_list_t);
    wev->tv.tv_sec = 0;
    wev->tv.tv_usec = 0;
}
static void iof_write_event_destruct(pmix_iof_write_event_t* wev)
{
    if (wev->pending) {
        pmix_event_del(&wev->ev);
    }
    if (2 < wev->fd) {
        PMIX_OUTPUT_VERBOSE((20, pmix_client_globals.iof_output,
                             "%s iof: closing fd %d for write event",
                             PMIX_NAME_PRINT(&pmix_globals.myid), wev->fd));
        close(wev->fd);
    }
    PMIX_LIST_DESTRUCT(&wev->outputs);
}
PMIX_CLASS_INSTANCE(pmix_iof_write_event_t,
                    pmix_list_item_t,
                    iof_write_event_construct,
                    iof_write_event_destruct);

PMIX_CLASS_INSTANCE(pmix_iof_write_output_t,
                    pmix_list_item_t,
                    NULL, NULL);
