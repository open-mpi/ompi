/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2021 IBM Corporation.  All rights reserved.
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

#ifdef HAVE_FCNTL_H
#    include <fcntl.h>
#else
#    ifdef HAVE_SYS_FCNTL_H
#        include <sys/fcntl.h>
#    endif
#endif
#include <ctype.h>

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_stdint.h"

#include "include/pmix.h"
#include "pmix_common.h"
#include "include/pmix_server.h"

#include "src/mca/bfrops/bfrops.h"
#include "src/mca/pfexec/base/base.h"
#include "src/mca/ptl/ptl.h"
#include "src/threads/pmix_threads.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_basename.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_printf.h"

#include "src/client/pmix_client_ops.h"
#include "src/include/pmix_globals.h"
#include "src/server/pmix_server_ops.h"

static void msgcbfunc(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                      pmix_buffer_t *buf, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;
    int32_t m;
    pmix_status_t rc, status;
    size_t refid = SIZE_MAX;
    size_t localid = SIZE_MAX;
    PMIX_HIDE_UNUSED_PARAMS(hdr);

    PMIX_ACQUIRE_OBJECT(cd);

    /* unpack the return status */
    m = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &status, &m, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        /* Ignore short buffer/premature connection disconnect */
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
            status = PMIX_SUCCESS;
        }
        else {
            status = rc;
        }
    }
    if (NULL != cd->iofreq) {
        pmix_output_verbose(2, pmix_client_globals.iof_output,
                            "pmix:iof_register returned status %s", PMIx_Error_string(status));
        /* this was a registration request */
        if (PMIX_SUCCESS == status) {
            /* get the reference ID */
            m = 1;
            PMIX_BFROPS_UNPACK(rc, peer, buf, &refid, &m, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                status = rc;
            } else {
                /* store the remote reference id */
                cd->iofreq->remote_id = refid;
                localid = cd->iofreq->local_id;
            }
        }
        if (NULL == cd->cbfunc.hdlrregcbfn) {
            cd->status = status;
            cd->iofreq->remote_id = refid;
            PMIX_WAKEUP_THREAD(&cd->lock);
        } else {
            cd->cbfunc.hdlrregcbfn(status, localid, cd->cbdata);
        }
        return;
    }

    pmix_output_verbose(2, pmix_client_globals.iof_output, "pmix:iof_deregister returned status %s",
                        PMIx_Error_string(status));

    /* this was a deregistration request */
    if (NULL == cd->cbfunc.opcbfn) {
        cd->status = status;
        PMIX_WAKEUP_THREAD(&cd->lock);
    } else {
        cd->cbfunc.opcbfn(status, cd->cbdata);
    }

    PMIX_RELEASE(cd);
}

static void mycbfn(pmix_status_t status, size_t refid, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;

    PMIX_ACQUIRE_OBJECT(cd);
    if (PMIX_SUCCESS == status) {
        cd->status = refid;
    } else {
        cd->status = status;
    }

    PMIX_WAKEUP_THREAD(&cd->lock);
}

static void process_cache(int sd, short args, void *cbdata)
{
    pmix_iof_req_t *req = (pmix_iof_req_t *) cbdata;
    pmix_iof_cache_t *iof, *ionext;
    bool found;
    size_t n;
    pmix_status_t rc;
    pmix_buffer_t *msg;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_LIST_FOREACH_SAFE (iof, ionext, &pmix_server_globals.iof, pmix_iof_cache_t) {
        /* if the channels don't match, then ignore it */
        if (!(iof->channel & req->channels)) {
            continue;
        }
        /* never forward back to the source! This can happen if the source
         * is a launcher */
        if (PMIX_CHECK_NAMES(&iof->source, &req->requestor->info->pname)) {
            continue;
        }
        /* never forward to myself */
        if (PMIX_CHECK_NAMES(&req->requestor->info->pname, &pmix_globals.myid)) {
            continue;
        }
        /* if the source does not match the request, then ignore it */
        found = false;
        for (n = 0; n < req->nprocs; n++) {
            if (PMIX_CHECK_PROCID(&iof->source, &req->procs[n])) {
                found = true;
                break;
            }
        }
        if (found) {
            /* setup the msg */
            if (NULL == (msg = PMIX_NEW(pmix_buffer_t))) {
                PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
                return;
            }
            /* provide the source */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &iof->source, 1, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return;
            }
            /* provide the channel */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &iof->channel, 1, PMIX_IOF_CHANNEL);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return;
            }
            /* provide the local handler ID */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &req->local_id, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return;
            }
            /* pack the number of info's provided */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, &iof->ninfo, 1, PMIX_SIZE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return;
            }
            /* if some were provided, then pack them too */
            if (0 < iof->ninfo) {
                PMIX_BFROPS_PACK(rc, req->requestor, msg, iof->info, iof->ninfo, PMIX_INFO);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(msg);
                    return;
                }
            }
            /* pack the data */
            PMIX_BFROPS_PACK(rc, req->requestor, msg, iof->bo, 1, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return;
            }
            /* send it to the requestor */
            PMIX_PTL_SEND_ONEWAY(rc, req->requestor, msg, PMIX_PTL_TAG_IOF);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
            }
        }
    }
}

static void myreg(int sd, short args, void *cbdata)
{
    pmix_iof_req_t *req = (pmix_iof_req_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    if (NULL != req->regcbfunc) {
        req->regcbfunc(PMIX_SUCCESS, req->local_id, req->cbdata);
    }
    process_cache(0, 0, req);
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

    pmix_output_verbose(2, pmix_client_globals.iof_output, "pmix:iof:PULL");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* we don't allow stdin to flow thru this path */
    if (PMIX_FWD_STDIN_CHANNEL & channel) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* if I am my own active server, then just register
     * the request */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) &&
        pmix_client_globals.myserver == pmix_globals.mypeer) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        req = PMIX_NEW(pmix_iof_req_t);
        if (NULL == req) {
            return PMIX_ERR_NOMEM;
        }
        PMIX_RETAIN(pmix_globals.mypeer);
        req->requestor = pmix_globals.mypeer;
        req->nprocs = nprocs;
        PMIX_PROC_CREATE(req->procs, req->nprocs);
        memcpy(req->procs, procs, nprocs * sizeof(pmix_proc_t));
        req->channels = channel;
        req->local_id = pmix_pointer_array_add(&pmix_globals.iof_requests, req);
        /* if there is a regsitration callback function, threadshift
         * to call it - we cannot call it before returning from here */
        if (NULL != regcbfunc) {
            req->regcbfunc = regcbfunc;
            req->cbdata = regcbdata;
            PMIX_THREADSHIFT(req, myreg);
            return PMIX_SUCCESS;
        }
        /* if there isn't a registration callback, then we can return "succeeded"
         * as we atomically performed the request. threadshift to process any
         * cached IO as we must return from this function before we do so */
        PMIX_THREADSHIFT(req, process_cache);
        return PMIX_OPERATION_SUCCEEDED;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        PMIX_ERROR_LOG(PMIX_ERR_UNREACH);
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
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &nprocs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, procs, nprocs, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ndirs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    if (0 < ndirs) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, directives, ndirs, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &channel, 1, PMIX_IOF_CHANNEL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &req->local_id, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "pmix:iof:PULL sending request to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, msgcbfunc, (void *) cd);

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

PMIX_EXPORT pmix_status_t PMIx_IOF_deregister(size_t iofhdlr, const pmix_info_t directives[],
                                              size_t ndirs, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_shift_caddy_t *cd;
    pmix_cmd_t cmd = PMIX_IOF_DEREG_CMD;
    pmix_buffer_t *msg;
    pmix_status_t rc;
    pmix_iof_req_t *req;
    size_t remote_id;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_client_globals.iof_output, "pmix:iof_deregister");

    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    /* if we are a server, we cannot do this */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer) && !PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOT_SUPPORTED;
    }

    /* if we aren't connected, don't attempt to send */
    if (!pmix_globals.connected) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_UNREACH;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    req = (pmix_iof_req_t *) pmix_pointer_array_get_item(&pmix_globals.iof_requests, iofhdlr);
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
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ndirs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    if (0 < ndirs) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, directives, ndirs, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }

    /* pack the remote handler ID */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &remote_id, 1, PMIX_SIZE);

    pmix_output_verbose(2, pmix_client_globals.iof_output, "pmix:iof_dereg sending to server");
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, msgcbfunc, (void *) cd);

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
static PMIX_CLASS_INSTANCE(pmix_ltcaddy_t, pmix_object_t, ltcon, ltdes);

static pmix_event_t stdinsig_ev;
static pmix_iof_read_event_t *stdinev_global = NULL;

static void stdincbfunc(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                        pmix_buffer_t *buf, void *cbdata)
{
    pmix_ltcaddy_t *cd = (pmix_ltcaddy_t *) cbdata;
    int cnt;
    pmix_status_t rc, status;
    PMIX_HIDE_UNUSED_PARAMS(hdr);

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
    pmix_ltcaddy_t *cd = (pmix_ltcaddy_t *) cbdata;

    cd->status = status;
    PMIX_WAKEUP_THREAD(&cd->lock);
}

pmix_status_t PMIx_IOF_push(const pmix_proc_t targets[], size_t ntargets, pmix_byte_object_t *bo,
                            const pmix_info_t directives[], size_t ndirs, pmix_op_cbfunc_t cbfunc,
                            void *cbdata)
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
        for (n = 0; n < ndirs; n++) {
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
                            if ((flags = fcntl(fd, F_GETFL, 0)) < 0) {
                                pmix_output(pmix_client_globals.iof_output,
                                            "[%s:%d]: fcntl(F_GETFL) failed with errno=%d\n",
                                            __FILE__, __LINE__, errno);
                            } else {
                                flags |= O_NONBLOCK;
                                if (0 != fcntl(fd, F_SETFL, flags)) {
                                    pmix_output(pmix_client_globals.iof_output,
                                                "[%s:%d]: fcntl(F_SETFL) failed with errno=%d\n",
                                                __FILE__, __LINE__, errno);
                                }
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
                            pmix_event_signal_set(pmix_globals.evauxbase, &stdinsig_ev, SIGCONT,
                                                  pmix_iof_stdin_cb, NULL);

                            /* setup a read event to read stdin, but don't activate it yet. The
                             * dst_name indicates who should receive the stdin. If that recipient
                             * doesn't do a corresponding pull, however, then the stdin will
                             * be dropped upon receipt at the local daemon
                             */
                            PMIX_IOF_READ_EVENT(&stdinev_global, targets, ntargets, directives,
                                                ndirs, fd, pmix_iof_read_local_handler, false);

                            /* check to see if we want the stdin read event to be
                             * active - we will always at least define the event,
                             * but may delay its activation
                             */
                            if (pmix_iof_stdin_check(fd)) {
                                PMIX_IOF_READ_ACTIVATE(stdinev_global);
                            }
                        } else {
                            /* if we are not looking at a tty, just setup a read event
                             * and activate it
                             */
                            PMIX_IOF_READ_EVENT(&stdinev_global, targets, ntargets, directives,
                                                ndirs, fd, pmix_iof_read_local_handler, true);
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
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ntargets, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
        if (0 < ntargets) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, targets, ntargets, PMIX_PROC);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return rc;
            }
        }
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &ndirs, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
        if (0 < ndirs) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, directives, ndirs, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_RELEASE(msg);
                return rc;
            }
        }
        if (NULL != bo) {
            PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, bo, 1, PMIX_BYTE_OBJECT);
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
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, stdincbfunc, cd);
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
    rc = pmix_host_server.push_stdin(&pmix_globals.myid, targets, ntargets, directives, ndirs, bo,
                                     cbfunc, cbdata);
    return rc;
}

static pmix_iof_write_event_t* pmix_iof_setup(pmix_namespace_t *nptr,
                                              pmix_rank_t rank,
                                              pmix_iof_channel_t stream)
{
    int rc;
    char *outdir, *outfile;
    int np, numdigs, fdout;
    pmix_iof_sink_t *snk;
    pmix_proc_t src;

    pmix_output_verbose(5, pmix_server_globals.iof_output,
                        "IOF SETUP %s %u",
                        nptr->nspace, rank);
    PMIX_LOAD_PROCID(&src, nptr->nspace, rank);

    np = nptr->nprocs / 10;
    /* determine the number of digits required for max vpid */
    numdigs = 1;
    while (np > 0) {
        numdigs++;
        np = np / 10;
    }

    /* see if we are to output to a directory */
    if (NULL != nptr->iof_flags.directory) {
        /* construct the directory where the output files will go */
        pmix_asprintf(&outdir, "%s/%s/rank.%0*u", nptr->iof_flags.directory,
                      nptr->nspace, numdigs, rank);
        /* ensure the directory exists */
        rc = pmix_os_dirpath_create(outdir, S_IRWXU | S_IRGRP | S_IXGRP);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            free(outdir);
            return NULL;
        }
        if (PMIX_FWD_STDOUT_CHANNEL & stream ||
            nptr->iof_flags.merge) {
            /* setup the stdout sink */
            pmix_asprintf(&outfile, "%s/stdout", outdir);
            fdout = open(outfile, O_CREAT | O_RDWR | O_TRUNC, 0644);
            free(outfile);
            if (fdout < 0) {
                /* couldn't be opened */
                PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
                free(outdir);
                return NULL;
            }
            /* define a sink to that file descriptor */
            snk = PMIX_NEW(pmix_iof_sink_t);
            if (nptr->iof_flags.merge) {
                PMIX_IOF_SINK_DEFINE(snk, &src, fdout,
                                     PMIX_FWD_ALL_CHANNELS, pmix_iof_write_handler);
            } else {
                PMIX_IOF_SINK_DEFINE(snk, &src, fdout,
                                     PMIX_FWD_STDOUT_CHANNEL, pmix_iof_write_handler);
            }
            pmix_list_append(&nptr->sinks, &snk->super);
            free(outdir);
            return &snk->wev;
        } else {
            /* setup the stderr sink */
            pmix_asprintf(&outfile, "%s/stderr", outdir);
            fdout = open(outfile, O_CREAT | O_RDWR | O_TRUNC, 0644);
            free(outfile);
            if (fdout < 0) {
                /* couldn't be opened */
                PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
                free(outdir);
                return NULL;
            }
            /* define a sink to that file descriptor */
            snk = PMIX_NEW(pmix_iof_sink_t);
            PMIX_IOF_SINK_DEFINE(snk, &src, fdout,
                                 PMIX_FWD_STDERR_CHANNEL, pmix_iof_write_handler);
            pmix_list_append(&nptr->sinks, &snk->super);
            free(outdir);
            return &snk->wev;
        }
    }

    /* see if we are to output to a file */
    if (NULL != nptr->iof_flags.file) {
        /* construct the directory where the output files will go */
        outdir = pmix_dirname(nptr->iof_flags.file);
        /* ensure the directory exists */
        rc = pmix_os_dirpath_create(outdir, S_IRWXU | S_IRGRP | S_IXGRP);
        free(outdir);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return NULL;
        }
        if (PMIX_FWD_STDOUT_CHANNEL & stream ||
            nptr->iof_flags.merge) {
            /* setup the stdout sink */
            if (nptr->iof_flags.pattern) {
                /* if there are no '%' signs in the pattern, then it is just a literal string */
                if (NULL == strchr(nptr->iof_flags.file, '%')) {
                    /* setup the file */
                    pmix_asprintf(&outfile, "%s.out", nptr->iof_flags.file);
                } else {
                    /* must process the pattern - for now, just take it literally */
                    pmix_asprintf(&outfile, "%s.pattern.out", nptr->iof_flags.file);
                }
            } else {
                /* setup the file */
                pmix_asprintf(&outfile, "%s.%s.%0*u.out", nptr->iof_flags.file,
                              nptr->nspace, numdigs, rank);
            }
            fdout = open(outfile, O_CREAT | O_RDWR | O_TRUNC, 0644);
            free(outfile);
            if (fdout < 0) {
                /* couldn't be opened */
                PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
                return NULL;
            }
            /* define a sink to that file descriptor */
            snk = PMIX_NEW(pmix_iof_sink_t);
            if (nptr->iof_flags.merge) {
                PMIX_IOF_SINK_DEFINE(snk, &src, fdout,
                                     PMIX_FWD_ALL_CHANNELS, pmix_iof_write_handler);
            } else {
                PMIX_IOF_SINK_DEFINE(snk, &src, fdout,
                                     PMIX_FWD_STDOUT_CHANNEL, pmix_iof_write_handler);
            }
            pmix_list_append(&nptr->sinks, &snk->super);
            return &snk->wev;
        } else {
            /* setup the stderr sink */
            if (nptr->iof_flags.pattern) {
                /* if there are no '%' signs in the pattern, then it is just a literal string */
                if (NULL == strchr(nptr->iof_flags.file, '%')) {
                    /* setup the file */
                    pmix_asprintf(&outfile, "%s.err", nptr->iof_flags.file);
                } else {
                    /* must process the pattern - for now, just take it literally */
                    pmix_asprintf(&outfile, "%s.pattern.err", nptr->iof_flags.file);
                }
            } else {
                /* setup the file */
                pmix_asprintf(&outfile, "%s.%s.%0*u.err", nptr->iof_flags.file,
                              nptr->nspace, numdigs, rank);
            }
            fdout = open(outfile, O_CREAT | O_RDWR | O_TRUNC, 0644);
            free(outfile);
            if (fdout < 0) {
                /* couldn't be opened */
                PMIX_ERROR_LOG(PMIX_ERR_FILE_OPEN_FAILURE);
                return NULL;
            }
            /* define a sink to that file descriptor */
            snk = PMIX_NEW(pmix_iof_sink_t);
            PMIX_IOF_SINK_DEFINE(snk, &src, fdout,
                                 PMIX_FWD_STDERR_CHANNEL, pmix_iof_write_handler);
            pmix_list_append(&nptr->sinks, &snk->super);
            return &snk->wev;
        }
    }

    return NULL;
}

void pmix_iof_check_flags(pmix_info_t *info, pmix_iof_flags_t *flags)
{
    if (PMIX_CHECK_KEY(info, PMIX_IOF_TAG_OUTPUT) ||
        PMIX_CHECK_KEY(info, PMIX_TAG_OUTPUT)) {
        flags->tag = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_TAG_DETAILED_OUTPUT)) {
        flags->tag_detailed = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_TAG_FULLNAME_OUTPUT)) {
        flags->tag_fullname = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_RANK_OUTPUT)) {
        flags->rank = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_TIMESTAMP_OUTPUT) ||
               PMIX_CHECK_KEY(info, PMIX_TIMESTAMP_OUTPUT)) {
        flags->timestamp = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_XML_OUTPUT)) {
        flags->xml = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_OUTPUT_TO_FILE) ||
               PMIX_CHECK_KEY(info, PMIX_OUTPUT_TO_FILE)) {
        flags->file = strdup(info->value.data.string);
        flags->set = true;
        flags->local_output = true;
        flags->local_output_given = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_OUTPUT_TO_DIRECTORY) ||
               PMIX_CHECK_KEY(info, PMIX_OUTPUT_TO_DIRECTORY)) {
        flags->directory = strdup(info->value.data.string);
        flags->set = true;
        flags->local_output = true;
        flags->local_output_given = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_FILE_ONLY) ||
               PMIX_CHECK_KEY(info, PMIX_OUTPUT_NOCOPY)) {
        flags->nocopy = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_MERGE_STDERR_STDOUT) ||
               PMIX_CHECK_KEY(info, PMIX_MERGE_STDERR_STDOUT)) {
        flags->merge = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_LOCAL_OUTPUT)) {
        flags->local_output = PMIX_INFO_TRUE(info);
        flags->set = true;
        flags->local_output_given = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_OUTPUT_RAW)) {
        flags->raw = PMIX_INFO_TRUE(info);
        flags->set = true;
    } else if (PMIX_CHECK_KEY(info, PMIX_IOF_FILE_PATTERN)) {
        flags->pattern = PMIX_INFO_TRUE(info);
        /* don't mark as set here as this is just a qualifier */
    }
}

pmix_status_t pmix_iof_process_iof(pmix_iof_channel_t channels, const pmix_proc_t *source,
                                   const pmix_byte_object_t *bo, const pmix_info_t *info,
                                   size_t ninfo, const pmix_iof_req_t *req)
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
    for (m = 0; m < req->nprocs; m++) {
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
    if (PMIX_CHECK_NAMES(source, &req->requestor->info->pname)) {
        return PMIX_SUCCESS;
    }
    /* never forward to myself */
    if (PMIX_CHECK_NAMES(&req->requestor->info->pname, &pmix_globals.myid)) {
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

static pmix_status_t write_output_line(const pmix_proc_t *name,
                                       pmix_iof_write_event_t *channel,
                                       pmix_iof_flags_t *myflags,
                                       pmix_iof_channel_t stream,
                                       bool copystdout, bool copystderr,
                                       const pmix_byte_object_t *bo)
{
    char starttag[PMIX_IOF_BASE_TAG_MAX], endtag[PMIX_IOF_BASE_TAG_MAX], *suffix;
    char timestamp[PMIX_IOF_BASE_TAG_MAX], outtag[PMIX_IOF_BASE_TAG_MAX];
    char begintag[PMIX_IOF_BASE_TAG_MAX];
    char **segments = NULL;
    pmix_iof_write_output_t *output, *copy;
    size_t offset, j, n, m, bufsize;
    char *buffer, qprint[15], *cptr;
    const char *usestring;
    bool bufcopy;
    pmix_cb_t cb2;
    pmix_info_t optional;
    pmix_kval_t *kv;
    pid_t pid;
    char *pidstring;
    pmix_status_t rc;

    /* setup output object */
    output = PMIX_NEW(pmix_iof_write_output_t);
    memset(begintag, 0, PMIX_IOF_BASE_TAG_MAX);
    memset(starttag, 0, PMIX_IOF_BASE_TAG_MAX);
    memset(endtag, 0, PMIX_IOF_BASE_TAG_MAX);
    memset(timestamp, 0, PMIX_IOF_BASE_TAG_MAX);
    memset(outtag, 0, PMIX_IOF_BASE_TAG_MAX);
    PMIX_INFO_LOAD(&optional, PMIX_OPTIONAL, NULL, PMIX_BOOL);

    /* write output data to the corresponding tag */
    if (PMIX_FWD_STDIN_CHANNEL & stream) {
        /* copy over the data to be written */
        if (0 < bo->size) {
            /* don't copy 0 bytes - we just need to pass
             * the zero bytes so the fd can be closed
             * after it writes everything out
             */
            output->data = (char*)malloc(bo->size);
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
        pmix_output_verbose(1, pmix_client_globals.iof_output, "%s stream %0x",
                             PMIX_NAME_PRINT(&pmix_globals.myid), stream);
        return PMIX_ERR_VALUE_OUT_OF_BOUNDS;
    }

    /* if 0 bytes, then just pass it so the fd can be closed
     * after it writes everything out
     */
    if (0 == bo->size) {
        output->numbytes = 0;
        goto process;
    }

    if (!myflags->set) {
        /* the data is not to be tagged - just copy it
         * and move on to processing
         */
        output->data = (char*)malloc(bo->size);
        memcpy(output->data, bo->bytes, bo->size);
        output->numbytes = bo->size;
        goto process;
    }

    /* if this is to be xml tagged, create a tag with the correct syntax - we do not allow
     * timestamping of xml output
     */
    if (myflags->xml) {
        if (myflags->tag) {
            /* find the '@' delimiter in the nspace */
            cptr = strrchr(name->nspace, '@');
            if (NULL == cptr) {
                usestring = name->nspace;  // just use the whole thing
            } else {
                ++cptr;
                usestring = cptr; // use the jobid portion
            }
            pmix_snprintf(begintag, PMIX_IOF_BASE_TAG_MAX,
                          "<%s %s=\"%s\" rank=\"%s\"", suffix,
                          (usestring == name->nspace) ? "nspace" : "jobid",
                          usestring, PMIX_RANK_PRINT(name->rank));
        } else if (myflags->tag_fullname) {
            pmix_snprintf(begintag, PMIX_IOF_BASE_TAG_MAX,
                          "<%s nspace=\"%s\" rank=\"%s\"", suffix,
                          name->nspace, PMIX_RANK_PRINT(name->rank));
        } else if (myflags->tag_detailed) {
            /* we need the hostname and pid of the source */
            PMIX_CONSTRUCT(&cb2, pmix_cb_t);
            cb2.proc = (pmix_proc_t*)name;
            cb2.key = PMIX_HOSTNAME;
            cb2.info = &optional;
            cb2.ninfo = 1;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb2);
            if (PMIX_SUCCESS == rc || PMIX_OPERATION_SUCCEEDED == rc) {
                kv = (pmix_kval_t*)pmix_list_remove_first(&cb2.kvs);
                if (NULL != kv) {  // should never be NULL
                    cptr = strdup(kv->value->data.string);
                    PMIX_RELEASE(kv);
                } else {
                    cptr = strdup("unknown");
                }
            } else {
                cptr = strdup("unknown");
            }
            PMIX_DESTRUCT(&cb2);
            /* get the pid */
            PMIX_CONSTRUCT(&cb2, pmix_cb_t);
            cb2.proc = (pmix_proc_t*)name;
            cb2.key = PMIX_PROC_PID;
            cb2.info = &optional;
            cb2.ninfo = 1;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb2);
            if (PMIX_SUCCESS == rc || PMIX_OPERATION_SUCCEEDED == rc) {
                kv = (pmix_kval_t*)pmix_list_remove_first(&cb2.kvs);
                if (NULL != kv) { // should never be NULL
                    PMIX_VALUE_GET_NUMBER(rc, kv->value, pid, pid_t);
                    PMIX_RELEASE(kv);
                    if (PMIX_SUCCESS != rc) {
                        pidstring = strdup("unknown");
                    } else {
                        pmix_asprintf(&pidstring, "%u", pid);
                    }
                } else {
                    pidstring = strdup("unknown");
                }
            } else {
                pidstring = strdup("unknown");
            }
            PMIX_DESTRUCT(&cb2);

            pmix_snprintf(begintag, PMIX_IOF_BASE_TAG_MAX,
                          "<%s nspace=\"%s\" rank=\"%s\"[\"%s\":\"%s\"",
                          suffix, name->nspace,
                          PMIX_RANK_PRINT(name->rank),
                          cptr, pidstring);
            free(cptr);
            free(pidstring);
        } else if (myflags->rank) {
            pmix_snprintf(begintag, PMIX_IOF_BASE_TAG_MAX,
                     "<%s rank=\"%s\"", suffix,
                     PMIX_RANK_PRINT(name->rank));
        } else if (myflags->timestamp) {
            pmix_snprintf(begintag, PMIX_IOF_BASE_TAG_MAX,
                     "<%s rank=\"%s\"", suffix,
                     PMIX_RANK_PRINT(name->rank));
        } else {
            pmix_snprintf(begintag, PMIX_IOF_BASE_TAG_MAX,
                     "<%s rank=\"%s\"", suffix,
                     PMIX_RANK_PRINT(name->rank));
        }
        pmix_snprintf(endtag, PMIX_IOF_BASE_TAG_MAX,
                 "</%s>", suffix);
    } else {
        if (myflags->tag) {
            /* find the '@' delimiter in the nspace */
            cptr = strrchr(name->nspace, '@');
            if (NULL == cptr) {
                usestring = name->nspace;  // just use the whole thing
            } else {
                ++cptr;
                usestring = cptr; // use the jobid portion
            }
            pmix_snprintf(outtag, PMIX_IOF_BASE_TAG_MAX,
                          "[%s,%s]<%s>: ",
                          usestring,
                          PMIX_RANK_PRINT(name->rank),
                          suffix);
        } else if (myflags->tag_detailed) {
            if (myflags->tag_fullname) {
                usestring = name->nspace;
            } else {
                /* find the '@' delimiter in the nspace */
                cptr = strrchr(name->nspace, '@');
                if (NULL == cptr) {
                    usestring = name->nspace;  // just use the whole thing
                } else {
                    ++cptr;
                    usestring = cptr; // use the jobid portion
                }
            }
            /* we need the hostname and pid of the source */
            PMIX_CONSTRUCT(&cb2, pmix_cb_t);
            cb2.proc = (pmix_proc_t*)name;
            cb2.key = PMIX_HOSTNAME;
            cb2.info = &optional;
            cb2.ninfo = 1;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb2);
            if (PMIX_SUCCESS == rc || PMIX_OPERATION_SUCCEEDED == rc) {
                kv = (pmix_kval_t*)pmix_list_remove_first(&cb2.kvs);
                cptr = strdup(kv->value->data.string);
                PMIX_RELEASE(kv);
            } else {
                cptr = strdup("unknown");
            }
            PMIX_DESTRUCT(&cb2);
            /* get the pid */
            PMIX_CONSTRUCT(&cb2, pmix_cb_t);
            cb2.proc = (pmix_proc_t*)name;
            cb2.key = PMIX_PROC_PID;
            cb2.info = &optional;
            cb2.ninfo = 1;
            PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb2);
            if (PMIX_SUCCESS == rc || PMIX_OPERATION_SUCCEEDED == rc) {
                kv = (pmix_kval_t*)pmix_list_remove_first(&cb2.kvs);
                PMIX_VALUE_GET_NUMBER(rc, kv->value, pid, pid_t);
                PMIX_RELEASE(kv);
                if (PMIX_SUCCESS != rc) {
                    pidstring = strdup("unknown");
                } else {
                    pmix_asprintf(&pidstring, "%u", pid);
                }
            } else {
                pidstring = strdup("unknown");
            }
            PMIX_DESTRUCT(&cb2);
            pmix_snprintf(outtag, PMIX_IOF_BASE_TAG_MAX,
                          "[%s,%s][%s:%s]<%s>: ",
                          usestring,
                          PMIX_RANK_PRINT(name->rank),
                          cptr, pidstring,
                          suffix);
            free(cptr);
            free(pidstring);
        } else if (myflags->tag_fullname) {
            pmix_snprintf(outtag, PMIX_IOF_BASE_TAG_MAX,
                          "[%s,%s]<%s>: ",
                          name->nspace,
                          PMIX_RANK_PRINT(name->rank),
                          suffix);
        } else if (myflags->rank) {
            pmix_snprintf(outtag, PMIX_IOF_BASE_TAG_MAX,
                     "[%s]<%s>: ",
                     PMIX_RANK_PRINT(name->rank), suffix);
        }
    }

    /* if we are to timestamp output, start the tag with that */
    if (myflags->timestamp) {
        time_t mytime;
        /* get the timestamp */
        time(&mytime);
        cptr = ctime(&mytime);
        cptr[strlen(cptr) - 1] = '\0'; /* remove trailing newline */

        if (myflags->xml && !myflags->tag && !myflags->rank) {
            pmix_snprintf(timestamp, PMIX_IOF_BASE_TAG_MAX,
                     " timestamp=\"%s\"", cptr);
        } else if (myflags->xml && (myflags->tag || myflags->rank)) {
            pmix_snprintf(timestamp, PMIX_IOF_BASE_TAG_MAX,
                     " timestamp=\"%s\"", cptr);
        } else if (myflags->tag || myflags->rank) {
            pmix_snprintf(timestamp, PMIX_IOF_BASE_TAG_MAX, "[%s]", cptr);
        } else {
            pmix_snprintf(timestamp, PMIX_IOF_BASE_TAG_MAX, "[%s]<%s>: ", cptr, suffix);
        }
    }

    /* start with the starttag */
    if (0 < strlen(begintag)) {
        PMIx_Argv_append_nosize(&segments, begintag);
    }
    /* add the timestamp */
    if (0 < strlen(timestamp)) {
        PMIx_Argv_append_nosize(&segments, timestamp);
    }
    /* add the output tag */
    if (0 < strlen(outtag)) {
        PMIx_Argv_append_nosize(&segments, outtag);
    }
    /* if xml, end the starttag with a '>' */
    if (myflags->xml) {
        PMIx_Argv_append_nosize(&segments, ">");
    }

    /* if we are doing XML, then we need to replace key characters */
    if (myflags->xml) {
        bufsize = bo->size;
        for (n = 0; n < bo->size; n++) {
            if ('&' == bo->bytes[n]) {
                bufsize += 5;
            } else if ('<' == bo->bytes[n] || '>' == bo->bytes[n]) {
                bufsize += 4;
            } else if (!isprint(bo->bytes[n])) {
                pmix_snprintf(qprint, 10, "&#%03d;", (int) bo->bytes[n]);
                bufsize += strlen(qprint);
            }
        }
        if (bo->size < bufsize) {
            /* we need to increase the size of our buffer to handle the
             * extra characters we need to add to represent these special
             * cases */
            buffer = malloc(bufsize);
            memset(buffer, 0, bufsize);
            bufcopy = true;
            m = 0;
            for (n = 0; n < bo->size; n++) {
                if ('&' == bo->bytes[n]) {
                    buffer[m++] = '&';
                    buffer[m++] = 'a';
                    buffer[m++] = 'p';
                    buffer[m++] = ';';
                } else if ('<' == bo->bytes[n]) {
                    buffer[m++] = '&';
                    buffer[m++] = 'l';
                    buffer[m++] = 't';
                    buffer[m++] = ';';
                } else if ('>' == bo->bytes[n]) {
                    buffer[m++] = '&';
                    buffer[m++] = 'g';
                    buffer[m++] = 't';
                    buffer[m++] = ';';
                } else if (!isprint(bo->bytes[n])) {
                    pmix_snprintf(qprint, 10, "&#%03d;", (int) bo->bytes[n]);
                    for (j = 0; j < strlen(qprint); j++) {
                        buffer[m++] = qprint[j];
                    }
                } else {
                    buffer[m++] = bo->bytes[n];
                }
            }
        } else {
            buffer = bo->bytes;
            bufsize = bo->size;
            bufcopy = false;
        }
    } else {
        buffer = bo->bytes;
        bufsize = bo->size;
        bufcopy = false;
    }

    /* assemble the output line */
    if (NULL != segments) {
        for (n=0; NULL != segments[n]; n++) {
            output->numbytes += strlen(segments[n]);
        }
    }
    output->numbytes += bufsize;
    output->numbytes += strlen(endtag);
    if (myflags->xml) {
        // add a spot for a trailing newline
        output->numbytes++;
    }

    output->data = (char*)malloc(output->numbytes);
    offset = 0;
    if (NULL != segments) {
        for (n=0; NULL != segments[n]; n++) {
            memcpy(&output->data[offset], segments[n], strlen(segments[n]));
            offset += strlen(segments[n]);
        }
    }
    memcpy(&output->data[offset], buffer, bufsize);
    offset += bufsize;
    if (0 < strlen(endtag)) {
        memcpy(&output->data[offset], endtag, strlen(endtag));
    }
    if (myflags->xml) {
        output->data[output->numbytes-1] = '\n';
    }
    if (bufcopy) {
        free(buffer);
    }

process:
    /* add this data to the write list for this fd */
    pmix_list_append(&channel->outputs, &output->super);

    if (copystdout){
        copy = PMIX_NEW(pmix_iof_write_output_t);
        copy->data = (char *) malloc(output->numbytes);
        memcpy(copy->data, output->data, output->numbytes);
        copy->numbytes = output->numbytes;
        pmix_list_append(&pmix_client_globals.iof_stdout.wev.outputs, &copy->super);
        if (!pmix_client_globals.iof_stdout.wev.pending) {
            PMIX_IOF_SINK_ACTIVATE(&pmix_client_globals.iof_stdout.wev);
        }
    }
    if (copystderr){
        copy = PMIX_NEW(pmix_iof_write_output_t);
        copy->data = (char *) malloc(output->numbytes);
        memcpy(copy->data, output->data, output->numbytes);
        copy->numbytes = output->numbytes;
        pmix_list_append(&pmix_client_globals.iof_stderr.wev.outputs, &copy->super);
        if (!pmix_client_globals.iof_stderr.wev.pending) {
            PMIX_IOF_SINK_ACTIVATE(&pmix_client_globals.iof_stderr.wev);
        }
    }

    /* is the write event issued? */
    if (!channel->pending) {
        /* issue it */
        pmix_output_verbose(1, pmix_client_globals.iof_output,
                             "%s write:output adding write event",
                             PMIX_NAME_PRINT(&pmix_globals.myid));
        PMIX_IOF_SINK_ACTIVATE(channel);
    }

    return PMIX_SUCCESS;
}

pmix_status_t pmix_iof_write_output(const pmix_proc_t *name, pmix_iof_channel_t stream,
                                    const pmix_byte_object_t *bo)
{
    pmix_status_t rc;
    size_t n, start;
    pmix_byte_object_t bopass;
    pmix_iof_write_event_t *channel;
    pmix_iof_flags_t myflags;
    pmix_namespace_t *nptr, *ns;
    bool outputio;
    bool copystdout = false;
    bool copystderr = false;
    pmix_iof_sink_t *sink;
    pmix_iof_residual_t *res;
    char *inputdata;
    size_t inputsize;
    bool copied;

    /* stdin doesn't come thru here*/
    if (PMIX_FWD_STDIN_CHANNEL & stream) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* find the nspace for this source */
    nptr = NULL;
    PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t)
    {
        if (0 == strcmp(ns->nspace, name->nspace)) {
            nptr = ns;
            break;
        }
    }

    channel = NULL;
    /* default outputio to our flag */
    outputio = pmix_globals.iof_flags.local_output;

    if (NULL != nptr) {
        if (nptr->iof_flags.set) {
            if (nptr->iof_flags.local_output_given) {
                outputio = nptr->iof_flags.local_output;
            }
            if (!outputio) {
                return PMIX_SUCCESS;
            }
            /* do we need an IOF channel for this source? */
            if (NULL != nptr->iof_flags.directory) {
                /* see if we already have one */
                PMIX_LIST_FOREACH(sink, &nptr->sinks, pmix_iof_sink_t) {
                    if (sink->name.rank == name->rank &&
                        ((stream & sink->tag) || nptr->iof_flags.merge)) {
                        channel = &sink->wev;
                        break;
                    }
                }
                if (NULL == channel) {
                    /* need to set this one up */
                    channel = pmix_iof_setup(nptr, name->rank, stream);
                    if (NULL == channel) {
                        return PMIX_ERR_IOF_FAILURE;
                    }
                }
                if (!nptr->iof_flags.nocopy && pmix_globals.iof_flags.local_output) {
                    if (PMIX_FWD_STDOUT_CHANNEL & stream) {
                        copystdout = true;
                    } else {
                        copystderr = true;
                    }
                }
            } else if (NULL != nptr->iof_flags.file) {
                /* see if we already have one - we reuse the same sink for
                 * all streams */
                PMIX_LIST_FOREACH(sink, &nptr->sinks, pmix_iof_sink_t) {
                    if (sink->name.rank == name->rank &&
                        ((stream & sink->tag) || nptr->iof_flags.merge)) {
                        channel = &sink->wev;
                        break;
                    }
                }
                if (NULL == channel) {
                    /* need to set this one up */
                    channel = pmix_iof_setup(nptr, name->rank, stream);
                    if (NULL == channel) {
                        return PMIX_ERR_IOF_FAILURE;
                    }
                }
                if (!nptr->iof_flags.nocopy && pmix_globals.iof_flags.local_output) {
                    if (PMIX_FWD_STDOUT_CHANNEL & stream) {
                        copystdout = true;
                    } else {
                        copystderr = true;
                    }
                }
            }
            myflags = nptr->iof_flags;
        } else {
            myflags = pmix_globals.iof_flags;
        }
    } else {
        myflags = pmix_globals.iof_flags;
    }

    if (!outputio) {
        return PMIX_SUCCESS;
    }

    if (NULL == channel) {
        if (PMIX_FWD_STDOUT_CHANNEL & stream) {
            channel = &pmix_client_globals.iof_stdout.wev;
        } else {
            if(!myflags.merge) {
                channel = &pmix_client_globals.iof_stderr.wev;
            }
            else {
                channel = &pmix_client_globals.iof_stdout.wev;
            }
        }
    }

    pmix_output_verbose(1, pmix_client_globals.iof_output,
                         "%s write:output setting up to write %lu bytes to %s for %s on fd %d",
                         PMIX_NAME_PRINT(&pmix_globals.myid), (unsigned long) bo->size,
                         PMIx_IOF_channel_string(stream), PMIX_NAME_PRINT(name),
                         (NULL == channel) ? -1 : channel->fd);

    /* zero bytes can just be passed along */
    if (0 == bo->size) {
        rc = write_output_line(name, channel, &myflags, stream,
                               false, false, bo);
        return rc;
    }

    /* see if we have some residual for this name/stream */
    inputdata = bo->bytes;
    inputsize = bo->size;
    copied = false;
    PMIX_LIST_FOREACH(res, &pmix_server_globals.iof_residuals, pmix_iof_residual_t) {
        if (PMIX_CHECK_PROCID(name, &res->name) || (stream & res->stream)) {
            /* we need to pre-pend the residual data to the new
             * data so any lines can be completed */
            inputdata = (char*)malloc(inputsize + res->bo.size);
            memcpy(inputdata, res->bo.bytes, res->bo.size);
            memcpy(&inputdata[res->bo.size], bo->bytes, bo->size);
            inputsize += res->bo.size;
            copied = true;
            pmix_list_remove_item(&pmix_server_globals.iof_residuals, &res->super);
            PMIX_RELEASE(res);
            break;
        }
    }

    /* search the input data stream for '\n' */
    start = 0;
    for (n=0; n < inputsize; n++) {
        if ('\n' == inputdata[n]) {
            bopass.bytes = &inputdata[start];
            bopass.size = n - start + 1;
            rc = write_output_line(name, channel, &myflags, stream,
                                   copystdout, copystderr, &bopass);
            if (PMIX_SUCCESS != rc) {
                if (copied) {
                    free(inputdata);
                }
                return rc;
            }
            start = n + 1;
        }
    }

    if (start < inputsize) {
        if (myflags.raw) {
            bopass.bytes = &inputdata[start];
            bopass.size = inputsize - start;
            rc = write_output_line(name, channel, &myflags, stream,
                                   copystdout, copystderr, &bopass);
            if (PMIX_SUCCESS != rc) {
                if (copied) {
                    free(inputdata);
                }
                return rc;
            }
        } else {
            /* we have some residual that needs to be cached until
             * the rest of the line is seen */
            res = PMIX_NEW(pmix_iof_residual_t);
            PMIX_XFER_PROCID(&res->name, name);
            res->channel = channel;
            memcpy(&res->flags, &myflags, sizeof(pmix_iof_flags_t));
            res->stream = stream;
            res->copystdout = copystdout;
            res->copystderr = copystderr;
            res->bo.bytes = (char*)malloc(inputsize - start);
            memcpy(res->bo.bytes, &inputdata[start], inputsize - start);
            res->bo.size = inputsize - start;
            pmix_list_append(&pmix_server_globals.iof_residuals, &res->super);
        }
    }
    if (copied) {
        free(inputdata);
    }
    return PMIX_SUCCESS;
}

void pmix_iof_flush_residuals(void)
{
    pmix_status_t rc;
    pmix_iof_residual_t *res;

    PMIX_LIST_FOREACH(res, &pmix_server_globals.iof_residuals, pmix_iof_residual_t) {
        rc = write_output_line(&res->name, res->channel, &res->flags,
                               res->stream, res->copystdout, res->copystderr, &res->bo);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return;
        }
    }
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
        while (NULL != (output = (pmix_iof_write_output_t *) pmix_list_remove_first(&wev->outputs))) {
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

void pmix_iof_write_handler(int sd, short args, void *cbdata)
{
    pmix_iof_sink_t *sink = (pmix_iof_sink_t *) cbdata;
    pmix_iof_write_event_t *wev = &sink->wev;
    pmix_list_item_t *item;
    pmix_iof_write_output_t *output;
    int num_written, total_written = 0;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(sink);

    pmix_output_verbose(1, pmix_client_globals.iof_output,
                         "%s write:handler writing data to %d",
                         PMIX_NAME_PRINT(&pmix_globals.myid), wev->fd);

    while (NULL != (item = pmix_list_remove_first(&wev->outputs))) {
        output = (pmix_iof_write_output_t *) item;
        if (0 == output->numbytes) {
            /* don't reactivate the event */
            PMIX_RELEASE(output);
            if (2 < wev->fd) {  // close the channel
                close(wev->fd);
                wev->fd = -1;
            }
            return;
        }
        num_written = write(wev->fd, output->data, output->numbytes);
        if (num_written < 0) {
            if (EAGAIN == errno || EINTR == errno) {
                /* push this item back on the front of the list */
                pmix_list_prepend(&wev->outputs, item);
                /* if the list is getting too large, abort */
                if (pmix_globals.output_limit < pmix_list_get_size(&wev->outputs)) {
                    pmix_output(0, "IO Forwarding is running too far behind - something is "
                                   "blocking us from writing");
                    goto ABORT;
                }
                /* leave the write event running so it will call us again
                 * when the fd is ready.
                 */
                wev->numtries++;
                if (PMIX_IOF_MAX_RETRIES < wev->numtries) {
                    /* give up */
                    pmix_output(0, "IO Forwarding is unable to output - something is "
                                "blocking us from writing");
                    goto ABORT;
                }
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
                pmix_output(0, "IO Forwarding is running too far behind - something is blocking us "
                               "from writing");
                goto ABORT;
            }
            /* leave the write event running so it will call us again
             * when the fd is ready
             */
            wev->numtries = 0;
            goto NEXT_CALL;
        }
        PMIX_RELEASE(output);
        wev->numtries = 0;

        total_written += num_written;
        if (wev->always_writable && (PMIX_IOF_SINK_BLOCKSIZE <= total_written)) {
            /* If this is a regular file it will never tell us it will block
             * Write no more than PMIX_IOF_SINK_BLOCKSIZE at a time to allow
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
    if (isatty(fd) && (getpgrp() != tcgetpgrp(fd))) {
        return false;
    }
#endif
    return true;
}

void pmix_iof_stdin_cb(int sd, short args, void *cbdata)
{
    bool should_process;
    pmix_iof_read_event_t *stdinev = (pmix_iof_read_event_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

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

static void iof_stdin_cbfunc(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                             pmix_buffer_t *buf, void *cbdata)
{
    pmix_iof_read_event_t *stdinev = (pmix_iof_read_event_t *) cbdata;
    int cnt;
    pmix_status_t rc, ret;
    PMIX_HIDE_UNUSED_PARAMS(hdr);

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
        if (PMIX_ERR_IOF_COMPLETE != ret) {
            /* generate an IOF-failed event so the tool knows */
            PMIx_Notify_event(PMIX_ERR_IOF_FAILURE, &pmix_globals.myid, PMIX_RANGE_PROC_LOCAL, NULL,
                              0, NULL, NULL);
        }
        return;
    }

    pmix_iof_stdin_cb(0, 0, stdinev);
}

static void opcbfn(pmix_status_t status, void *cbdata)
{
    pmix_byte_object_t *boptr = (pmix_byte_object_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(status);

    PMIX_ACQUIRE_OBJECT(boptr);
    PMIX_BYTE_OBJECT_FREE(boptr, 1);
}

/* this is the read handler for stdin */
void pmix_iof_read_local_handler(int sd, short args, void *cbdata)
{
    pmix_iof_read_event_t *rev = (pmix_iof_read_event_t *) cbdata;
    unsigned char data[PMIX_IOF_BASE_MSG_MAX];
    int32_t numbytes;
    pmix_status_t rc;
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_IOF_PUSH_CMD;
    pmix_byte_object_t bo, *boptr;
    int fd;
    pmix_pfexec_child_t *child = (pmix_pfexec_child_t *) rev->childproc;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(rev);

    if (0 > rev->fd) {
        fd = fileno(stdin);
    } else {
        fd = rev->fd;
    }
    /* read up to the fragment size */
    memset(data, 0, PMIX_IOF_BASE_MSG_MAX);
    numbytes = read(fd, data, sizeof(data));

    /* The event has fired, so it's no longer active until we
     re-add it */
    rev->active = false;

    if (numbytes < 0) {
        /* either we have a connection error or it was a non-blocking read */

        /* non-blocking, retry */
        if (EAGAIN == errno || EINTR == errno) {
            PMIX_IOF_READ_ACTIVATE(rev);
            return;
        }

        pmix_output_verbose(1, pmix_client_globals.iof_output,
                             "%s iof:read handler Error on %s",
                             PMIX_NAME_PRINT(&pmix_globals.myid),
                             PMIx_IOF_channel_string(rev->channel));
        /* Un-recoverable error */
        bo.bytes = NULL;
        bo.size = 0;
        numbytes = 0;
    } else {
        bo.bytes = (char *) data;
        bo.size = numbytes;
    }

    /* if this is stdout or stderr of a child, then just output it */
    if (NULL != child &&
        (PMIX_FWD_STDOUT_CHANNEL == rev->channel ||
         PMIX_FWD_STDERR_CHANNEL == rev->channel)) {
        if (PMIX_FWD_STDOUT_CHANNEL == rev->channel) {
            rc = pmix_iof_write_output(&child->stdoutev->name, PMIX_FWD_STDOUT_CHANNEL, &bo);
        } else if (PMIX_FWD_STDERR_CHANNEL == rev->channel) {
            rc = pmix_iof_write_output(&child->stderrev->name, PMIX_FWD_STDERR_CHANNEL, &bo);
        } else {
            rc = PMIX_ERR_BAD_PARAM;
        }
        if (0 > rc) {
            PMIX_ERROR_LOG(rc);
        }
        /* if the number of bytes is zero, then we just delete the event - there
         * is no need to pass it upstream as WE are the ones holding the event
         * and associated file descriptor */
        if (0 == numbytes) {
            if (NULL != child && child->completed &&
                (NULL == child->stdoutev || !child->stdoutev->active) &&
                (NULL == child->stderrev || !child->stderrev->active)) {
                PMIX_PFEXEC_CHK_COMPLETE(child);
            }
            return;
        }
        goto reactivate;
    }

    /* if it is stdin that was read, then see if we have a sink
     * for a child of ours that matches this target - this has precedence over
     * anything else */
    if (PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        if (rev == stdinev_global && NULL != rev->targets) {
            PMIX_LIST_FOREACH(child, &pmix_pfexec_globals.children, pmix_pfexec_child_t) {
                if (PMIX_CHECK_PROCID(&child->proc, &rev->targets[0])) {
                    /* send the input to that target */
                    rc = write_output_line(&child->proc, &child->stdinsink.wev, NULL,
                                           PMIX_FWD_STDIN_CHANNEL, false, false, &bo);
                    goto reactivate;
                }
            }
        }
    }

    /* if I am a launcher or a tool and connected to a server, then
     * we want to send things to our server for relay */
    if ((PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer) ||
         PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) &&
        pmix_globals.connected) {
        goto forward;
    }

    /* if I am a server, then push this up to my host */
    if (PMIX_PEER_IS_SERVER(pmix_globals.mypeer)) {
        if (NULL == pmix_host_server.push_stdin) {
            /* nothing we can do with this info - no point in reactivating it */
            return;
        }
        PMIX_BYTE_OBJECT_CREATE(boptr, 1);
        if (0 < bo.size) {
            boptr->bytes = (char*)malloc(bo.size);
            memcpy(boptr->bytes, bo.bytes, bo.size);
            boptr->size = bo.size;
        }
        rc = pmix_host_server.push_stdin(&pmix_globals.myid, rev->targets, rev->ntargets,
                                         rev->directives, rev->ndirs, boptr, opcbfn, (void*)boptr);
        goto reactivate;
    }

forward:
    /* pass the data to our PMIx server so it can relay it
     * to the host RM for distribution */
    msg = PMIX_NEW(pmix_buffer_t);
    if (NULL == msg) {
        /* don't restart the event - just return */
        return;
    }
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }
    /* pack the number of targets */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &rev->ntargets, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }
    /* and the targets */
    if (0 < rev->ntargets) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, rev->targets, rev->ntargets,
                         PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return;
        }
    }
    /* pack the number of directives */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &rev->ndirs, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }
    /* and the directives */
    if (0 < rev->ndirs) {
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, rev->directives, rev->ndirs,
                         PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return;
        }
    }

    /* pack the data */
    PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &bo, 1, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
        return;
    }

    /* send it to the server */
    PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, iof_stdin_cbfunc, rev);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(msg);
    }

reactivate:
    if (0 < numbytes) {
        PMIX_IOF_READ_ACTIVATE(rev);
    }

    /* nothing more to do */
    return;
}

/* class instances */
static void iof_sink_construct(pmix_iof_sink_t *ptr)
{
    PMIX_CONSTRUCT(&ptr->wev, pmix_iof_write_event_t);
    ptr->xoff = false;
    ptr->exclusive = false;
    ptr->closed = false;
}
static void iof_sink_destruct(pmix_iof_sink_t *ptr)
{
    if (0 <= ptr->wev.fd) {
        pmix_output_verbose
            (20, pmix_client_globals.iof_output, "%s iof: closing sink for process %s on fd %d",
             PMIX_NAME_PRINT(&pmix_globals.myid), PMIX_NAME_PRINT(&ptr->name), ptr->wev.fd);
        PMIX_DESTRUCT(&ptr->wev);
    }
}
PMIX_CLASS_INSTANCE(pmix_iof_sink_t, pmix_list_item_t, iof_sink_construct, iof_sink_destruct);

static void iof_read_event_construct(pmix_iof_read_event_t *rev)
{
    rev->tv.tv_sec = 0;
    rev->tv.tv_usec = 0;
    rev->fd = -1;
    rev->channel = PMIX_FWD_NO_CHANNELS;
    rev->active = false;
    rev->childproc = NULL;
    rev->always_readable = false;
    rev->targets = NULL;
    rev->ntargets = 0;
    rev->directives = NULL;
    rev->ndirs = 0;
}
static void iof_read_event_destruct(pmix_iof_read_event_t *rev)
{
    if (rev->active) {
        pmix_event_del(&rev->ev);
    }
    if (0 <= rev->fd) {
        pmix_output_verbose(20, pmix_client_globals.iof_output, "%s iof: closing fd %d",
                             PMIX_NAME_PRINT(&pmix_globals.myid), rev->fd);
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
PMIX_CLASS_INSTANCE(pmix_iof_read_event_t, pmix_object_t, iof_read_event_construct,
                    iof_read_event_destruct);

static void iof_write_event_construct(pmix_iof_write_event_t *wev)
{
    wev->pending = false;
    wev->always_writable = false;
    wev->numtries = 0;
    wev->ev = (pmix_event_t*)malloc(sizeof(pmix_event_t));
    wev->fd = -1;
    PMIX_CONSTRUCT(&wev->outputs, pmix_list_t);
    wev->tv.tv_sec = 0;
    wev->tv.tv_usec = 0;
}
static void iof_write_event_destruct(pmix_iof_write_event_t *wev)
{
    if (wev->pending) {
        pmix_event_del(wev->ev);
    }
    free(wev->ev);
    if (2 < wev->fd) {
        pmix_output_verbose(20, pmix_client_globals.iof_output,
                             "%s iof: closing fd %d for write event",
                             PMIX_NAME_PRINT(&pmix_globals.myid), wev->fd);
        close(wev->fd);
    }
    PMIX_LIST_DESTRUCT(&wev->outputs);
}
PMIX_CLASS_INSTANCE(pmix_iof_write_event_t, pmix_list_item_t, iof_write_event_construct,
                    iof_write_event_destruct);

static void wocon(pmix_iof_write_output_t *p)
{
    p->data = NULL;
    p->numbytes = 0;
}
static void wodes(pmix_iof_write_output_t *p)
{
    if (NULL != p->data) {
        free(p->data);
    }
}
PMIX_CLASS_INSTANCE(pmix_iof_write_output_t,
                    pmix_list_item_t,
                    wocon, wodes);

static void iofrescon(pmix_iof_residual_t *p)
{
    PMIX_BYTE_OBJECT_CONSTRUCT(&p->bo);
}
static void iofresdes(pmix_iof_residual_t *p)
{
    if (NULL != p->bo.bytes) {
        free(p->bo.bytes);
    }
}
PMIX_CLASS_INSTANCE(pmix_iof_residual_t,
                    pmix_list_item_t,
                    iofrescon, iofresdes);
