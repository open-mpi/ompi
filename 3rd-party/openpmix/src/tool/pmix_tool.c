/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2014      Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2021 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "src/include/pmix_socket_errno.h"

#include "include/pmix_server.h"
#include "include/pmix_tool.h"
#include "src/client/pmix_client_ops.h"

#include "src/include/pmix_globals.h"

#ifdef HAVE_STRING_H
#    include <string.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#    include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#    include <sys/un.h>
#endif
#ifdef HAVE_SYS_UIO_H
#    include <sys/uio.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#ifdef HAVE_DIRENT_H
#    include <dirent.h>
#endif /* HAVE_DIRENT_H */

#include "src/class/pmix_list.h"
#include "src/client/pmix_client_ops.h"
#include "src/common/pmix_attributes.h"
#include "src/common/pmix_iof.h"
#include "src/hwloc/pmix_hwloc.h"
#include "src/include/pmix_globals.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/pfexec/base/base.h"
#include "src/mca/pmdl/base/base.h"
#include "src/mca/pnet/base/base.h"
#include "src/mca/psec/psec.h"
#include "src/mca/pstrg/base/base.h"
#include "src/mca/ptl/base/base.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/server/pmix_server_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

#define PMIX_MAX_RETRIES 10

static pmix_event_t stdinsig, parentdied;
static pmix_iof_read_event_t stdinev;
static pmix_proc_t myparent;

static void pdiedfn(int sd, short args, void *cbdata)
{
    pmix_info_t info[2];
    pmix_proc_t keepalive;
    PMIX_HIDE_UNUSED_PARAMS(sd, args, cbdata);

    PMIX_LOAD_PROCID(&keepalive, "PMIX_KEEPALIVE_PIPE", PMIX_RANK_UNDEF);

    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    PMIX_INFO_LOAD(&info[1], PMIX_EVENT_AFFECTED_PROC, &keepalive, PMIX_PROC);

    /* generate a job-terminated event */
    PMIx_Notify_event(PMIX_ERR_JOB_TERMINATED, &pmix_globals.myid,
                      PMIX_RANGE_PROC_LOCAL, info, 2,
                      NULL, NULL);
}

static void _notify_complete(pmix_status_t status, void *cbdata)
{
    pmix_event_chain_t *chain = (pmix_event_chain_t *) cbdata;
    pmix_notify_caddy_t *cd;
    size_t n;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(chain);

    /* if the event wasn't found, then cache it as it might
     * be registered later */
    if (PMIX_ERR_NOT_FOUND == status && !chain->cached) {
        cd = PMIX_NEW(pmix_notify_caddy_t);
        cd->status = chain->status;
        PMIX_LOAD_PROCID(&cd->source, chain->source.nspace, chain->source.rank);
        cd->range = chain->range;
        if (0 < chain->ninfo) {
            cd->ninfo = chain->ninfo;
            PMIX_INFO_CREATE(cd->info, cd->ninfo);
            cd->nondefault = chain->nondefault;
            /* need to copy the info */
            for (n = 0; n < cd->ninfo; n++) {
                PMIX_INFO_XFER(&cd->info[n], &chain->info[n]);
            }
        }
        if (NULL != chain->targets) {
            cd->ntargets = chain->ntargets;
            PMIX_PROC_CREATE(cd->targets, cd->ntargets);
            memcpy(cd->targets, chain->targets, cd->ntargets * sizeof(pmix_proc_t));
        }
        if (NULL != chain->affected) {
            cd->naffected = chain->naffected;
            PMIX_PROC_CREATE(cd->affected, cd->naffected);
            if (NULL == cd->affected) {
                cd->naffected = 0;
                goto cleanup;
            }
            memcpy(cd->affected, chain->affected, cd->naffected * sizeof(pmix_proc_t));
        }
        /* cache it */
        rc = pmix_notify_event_cache(cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(cd);
            goto cleanup;
        }
        chain->cached = true;
    }

cleanup:
    PMIX_RELEASE(chain);
}

static void pmix_tool_notify_recv(struct pmix_peer_t *peer, pmix_ptl_hdr_t *hdr,
                                  pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t rc;
    int32_t cnt;
    pmix_cmd_t cmd;
    pmix_event_chain_t *chain;
    size_t ninfo;
    pmix_data_range_t range;
    PMIX_HIDE_UNUSED_PARAMS(peer, hdr, cbdata);

    pmix_output_verbose(2, pmix_client_globals.event_output,
                        "pmix:tool_notify_recv - processing event");

    /* a zero-byte buffer indicates that this recv is being
     * completed due to a lost connection */
    if (PMIX_BUFFER_IS_EMPTY(buf)) {
        return;
    }

    /* start the local notification chain */
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->final_cbfunc = _notify_complete;
    chain->final_cbdata = chain;

    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver, buf, &cmd, &cnt, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }
    /* unpack the status */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver, buf, &chain->status, &cnt, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }

    /* unpack the source of the event */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver, buf, &chain->source, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }

    /* unpack the info that might have been provided */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }

    /* we always leave space for event hdlr name and a callback object */
    chain->nallocated = ninfo + 2;
    PMIX_INFO_CREATE(chain->info, chain->nallocated);
    if (NULL == chain->info) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(chain);
        return;
    }

    if (0 < ninfo) {
        chain->ninfo = ninfo;
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver, buf, chain->info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(chain);
            goto error;
        }
    }
    /* unpack the range, if provided */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver, buf, &range, &cnt, PMIX_DATA_RANGE);
    if (PMIX_SUCCESS != rc && PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER == rc) {
        range = PMIX_RANGE_LOCAL;
    }
    if (PMIX_RANGE_LOCAL != range && pmix_globals.connected &&
        !(PMIX_CHECK_NSPACE(peer->nptr->nspace, pmix_client_globals.myserver->nptr->nspace) &&
          peer->info->pname.rank == pmix_client_globals.myserver->info->pname.rank)) {
        pmix_output_verbose(2, pmix_client_globals.event_output,
                            "[%s:%d] pmix:tool_notify_recv - relaying to server",
                            pmix_globals.myid.nspace, pmix_globals.myid.rank);
        rc = pmix_notify_server_of_event(chain->status, &chain->source, range,
                                         chain->info, chain->ninfo, NULL, NULL, false);
    }

    pmix_output_verbose(2, pmix_client_globals.event_output,
        "[%s:%d] pmix:tool_notify_recv - processing event %s from source %s:%d, calling errhandler",
        pmix_globals.myid.nspace, pmix_globals.myid.rank, PMIx_Error_string(chain->status),
        chain->source.nspace, chain->source.rank);

    rc = pmix_server_notify_client_of_event(chain->status, &chain->source, range,
                                            chain->info, chain->ninfo, _notify_complete, chain);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(chain);
        goto error;
    }
    return;

error:
    /* we always need to return */
    pmix_output_verbose(2, pmix_client_globals.event_output,
                        "pmix:tool_notify_recv - unpack error status =%d, calling def errhandler",
                        rc);
    chain = PMIX_NEW(pmix_event_chain_t);
    chain->status = rc;
    pmix_invoke_local_event_hdlr(chain);
}

static void tool_iof_handler(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
                             pmix_buffer_t *buf, void *cbdata)
{
    pmix_peer_t *peer = (pmix_peer_t *) pr;
    pmix_proc_t source;
    pmix_iof_channel_t channel;
    pmix_byte_object_t bo;
    int32_t cnt;
    pmix_status_t rc;
    size_t refid, ninfo = 0;
    pmix_iof_req_t *req;
    pmix_info_t *info = NULL;
    PMIX_HIDE_UNUSED_PARAMS(hdr, cbdata);

    pmix_output_verbose(2, pmix_client_globals.iof_output,
                        "recvd IOF with %d bytes",
                        (int) buf->bytes_used);

    /* if the buffer is empty, they are simply closing the socket */
    if (0 == buf->bytes_used) {
        return;
    }
    PMIX_BYTE_OBJECT_CONSTRUCT(&bo);

    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &source, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &channel, &cnt, PMIX_IOF_CHANNEL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &refid, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        PMIX_BFROPS_UNPACK(rc, peer, buf, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
    }
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &bo, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* lookup the handler for this IOF package */
    req = (pmix_iof_req_t *) pmix_pointer_array_get_item(&pmix_globals.iof_requests, refid);
    if (NULL != req && NULL != req->cbfunc) {
        req->cbfunc(refid, channel, &source, &bo, info, ninfo);
    } else {
        /* otherwise, simply write it out to the specified std IO channel */
        if (NULL != bo.bytes && 0 < bo.size) {
            pmix_iof_write_output(&source, channel, &bo);
        }
    }

cleanup:
    /* cleanup the memory */
    if (0 < ninfo) {
        PMIX_INFO_FREE(info, ninfo);
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&bo);
}

/* callback to receive job info */
static void job_data(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
                     pmix_buffer_t *buf, void *cbdata)
{
    pmix_status_t rc;
    char *nspace;
    int32_t cnt = 1;
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(pr, hdr);

    /* unpack the nspace - should be same as our own */
    PMIX_BFROPS_UNPACK(rc, pmix_client_globals.myserver, buf, &nspace, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        cb->status = PMIX_ERROR;
        PMIX_POST_OBJECT(cb);
        PMIX_WAKEUP_THREAD(&cb->lock);
        return;
    }

    /* decode it */
    PMIX_GDS_STORE_JOB_INFO(cb->status, pmix_client_globals.myserver, nspace, buf);
    cb->status = PMIX_SUCCESS;
    PMIX_POST_OBJECT(cb);
    PMIX_WAKEUP_THREAD(&cb->lock);
}

/* event handler registration callback */
static void evhandler_reg_callbk(pmix_status_t status, size_t evhandler_ref, void *cbdata)
{
    pmix_lock_t *lock = (pmix_lock_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(evhandler_ref);

    lock->status = status;
    PMIX_WAKEUP_THREAD(lock);
}

static void notification_fn(size_t evhdlr_registration_id, pmix_status_t status,
                            const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                            pmix_info_t results[], size_t nresults,
                            pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    pmix_lock_t *lock = NULL;
    char *name = NULL;
    size_t n;
    PMIX_HIDE_UNUSED_PARAMS(evhdlr_registration_id, status, source, results, nresults);

    pmix_output_verbose(2, pmix_client_globals.base_output,
                        "[%s:%d] DEBUGGER RELEASE RECVD",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank);
    if (NULL != info) {
        lock = NULL;
        for (n = 0; n < ninfo; n++) {
            if (0 == strncmp(info[n].key, PMIX_EVENT_RETURN_OBJECT, PMIX_MAX_KEYLEN)) {
                lock = (pmix_lock_t *) info[n].value.data.ptr;
            } else if (0 == strncmp(info[n].key, PMIX_EVENT_HDLR_NAME, PMIX_MAX_KEYLEN)) {
                name = info[n].value.data.string;
            }
        }
        /* if the object wasn't returned, then that is an error */
        if (NULL == lock) {
            pmix_output_verbose(2, pmix_client_globals.base_output,
                                "event handler %s failed to return object",
                                (NULL == name) ? "NULL" : name);
            /* let the event handler progress */
            if (NULL != cbfunc) {
                cbfunc(PMIX_SUCCESS, NULL, 0, NULL, NULL, cbdata);
            }
            return;
        }
    }
    if (NULL != lock) {
        PMIX_WAKEUP_THREAD(lock);
    }

    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

PMIX_EXPORT int PMIx_tool_init(pmix_proc_t *proc, pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    char *evar, *nspace = NULL;
    pmix_rank_t rank = PMIX_RANK_UNDEF;
    bool do_not_connect = false;
    bool nspace_given = false;
    bool nspace_in_enviro = false;
    bool rank_given = false;
    bool fwd_stdin = false;
    bool connect_optional = false;
    pmix_info_t ginfo, *iptr, evinfo[3];
    size_t n;
    pmix_ptl_posted_recv_t *rcv;
    pmix_proc_t wildcard, myserver;
    int fd;
    pmix_proc_type_t ptype = PMIX_PROC_TYPE_STATIC_INIT;
    pmix_cb_t cb;
    pmix_buffer_t *req;
    pmix_cmd_t cmd;
    pmix_iof_req_t *iofreq;
    pmix_lock_t reglock, releaselock;
    pmix_status_t code;
    pmix_value_t value;
    bool outputio = true;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    if (NULL == proc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_BAD_PARAM;
    }
    if (0 < pmix_globals.init_cntr) {
        /* since we have been called before, the nspace and
         * rank should be known. So return them here if
         * requested */
        if (NULL != proc) {
            PMIX_LOAD_PROCID(proc, pmix_globals.myid.nspace, pmix_globals.myid.rank);
        }
        ++pmix_globals.init_cntr;
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_SUCCESS;
    }
    /* init the parent procid to something innocuous */
    PMIX_LOAD_PROCID(&myparent, NULL, PMIX_RANK_UNDEF);

    /* backward compatibility fix - remove any directive to use
     * the old usock component so we avoid a warning message */
    if (NULL != (evar = getenv("PMIX_MCA_ptl"))) {
        if (0 == strcmp(evar, "usock")) {
            /* we cannot support a usock-only environment */
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            fprintf(stderr,
                    "-------------------------------------------------------------------\n");
            fprintf(stderr, "PMIx no longer supports the \"usock\" transport for client-server\n");
            fprintf(stderr,
                    "communication. A directive was detected that only allows that mode.\n");
            fprintf(stderr, "We cannot continue - please remove that constraint and try again.\n");
            fprintf(stderr,
                    "-------------------------------------------------------------------\n");
            return PMIX_ERR_INIT;
        }
        /* anything else should just be cleared */
        pmix_unsetenv("PMIX_MCA_ptl", &environ);
    }

    /* parse the input directives */
    PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_TOOL);
    if (NULL != info) {
        for (n = 0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_TOOL_DO_NOT_CONNECT)) {
                do_not_connect = PMIX_INFO_TRUE(&info[n]);
            } else if (0 == strncmp(info[n].key, PMIX_TOOL_NSPACE, PMIX_MAX_KEYLEN)) {
                if (NULL != nspace) {
                    /* cannot define it twice */
                    free(nspace);
                    PMIX_RELEASE_THREAD(&pmix_global_lock);
                    return PMIX_ERR_BAD_PARAM;
                }
                nspace = strdup(info[n].value.data.string);
                nspace_given = true;
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_TOOL_RANK)) {
                rank = info[n].value.data.rank;
                rank_given = true;
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_FWD_STDIN)) {
                /* they want us to forward our stdin to someone */
                fwd_stdin = PMIX_INFO_TRUE(&info[n]);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_LAUNCHER)) {
                if (PMIX_INFO_TRUE(&info[n])) {
                    PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_LAUNCHER);
                }
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_SCHEDULER)) {
                if (PMIX_INFO_TRUE(&info[n])) {
                    PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_SCHEDULER);
                }
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_TMPDIR)) {
                pmix_server_globals.tmpdir = strdup(info[n].value.data.string);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SYSTEM_TMPDIR)) {
                pmix_server_globals.system_tmpdir = strdup(info[n].value.data.string);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_TOOL_CONNECT_OPTIONAL)) {
                connect_optional = PMIX_INFO_TRUE(&info[n]);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_IOF_LOCAL_OUTPUT)) {
                outputio = PMIX_INFO_TRUE(&info[n]);
            }
        }
    }
    if (NULL == pmix_server_globals.tmpdir) {
        if (NULL == (evar = getenv("PMIX_SERVER_TMPDIR"))) {
            pmix_server_globals.tmpdir = strdup(pmix_tmp_directory());
        } else {
            pmix_server_globals.tmpdir = strdup(evar);
        }
    }
    if (NULL == pmix_server_globals.system_tmpdir) {
        if (NULL == (evar = getenv("PMIX_SYSTEM_TMPDIR"))) {
            pmix_server_globals.system_tmpdir = strdup(pmix_tmp_directory());
        } else {
            pmix_server_globals.system_tmpdir = strdup(evar);
        }
    }

    if ((nspace_given && !rank_given) || (!nspace_given && rank_given)) {
        /* can't have one and not the other */
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        if (NULL != nspace) {
            free(nspace);
        }
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_BAD_PARAM;
    }

    /* if we were not passed an nspace in the info keys,
     * check to see if we were given one in the env - this
     * will be the case when we are launched by a PMIx-enabled
     * daemon */
    if (!nspace_given) {
        if (NULL != (evar = getenv("PMIX_NAMESPACE"))) {
            nspace = strdup(evar);
            nspace_in_enviro = true;
        }
    }
    /* also look for the rank - it normally is zero, but if we
     * were launched, then it might have been as part of a
     * multi-process tool */
    if (!rank_given) {
        if (NULL != (evar = getenv("PMIX_RANK"))) {
            rank = strtol(evar, NULL, 10);
            if (!nspace_in_enviro) {
                /* this is an error - we can't have one and not
                 * the other */
                PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
                PMIX_RELEASE_THREAD(&pmix_global_lock);
                return PMIX_ERR_BAD_PARAM;
            }
            /* flag that this tool is also a client */
            if (PMIX_PROC_IS_LAUNCHER(&ptype)) {
                PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_CLIENT_LAUNCHER);
            } else {
                PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_CLIENT_TOOL);
            }
        } else if (nspace_in_enviro) {
            /* this is an error - we can't have one and not
             * the other */
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            if (NULL != nspace) {
                free(nspace);
            }
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            return PMIX_ERR_BAD_PARAM;
        }
    }

    /* setup the runtime - this init's the globals,
     * opens and initializes the required frameworks */
    if (PMIX_SUCCESS != (rc = pmix_rte_init(ptype.type, info, ninfo, pmix_tool_notify_recv))) {
        PMIX_ERROR_LOG(rc);
        if (NULL != nspace) {
            free(nspace);
        }
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* if we were given a keepalive pipe, register an
     * event to capture the event */
    if (NULL != (evar = getenv("PMIX_KEEPALIVE_PIPE"))) {
        rc = strtol(evar, NULL, 10);
        pmix_event_set(pmix_globals.evbase, &parentdied, rc, PMIX_EV_READ, pdiedfn, NULL);
        pmix_event_add(&parentdied, NULL);
        pmix_unsetenv("PMIX_KEEPALIVE_PIPE", &environ);
        pmix_fd_set_cloexec(rc); // don't let children inherit this
    }

    /* if we were given a name, then set it now */
    if (nspace_given || nspace_in_enviro) {
        PMIX_LOAD_PROCID(&pmix_globals.myid, nspace, rank);
        free(nspace);
        nspace = NULL;
    }

    /* setup the IO Forwarding recv */
    rcv = PMIX_NEW(pmix_ptl_posted_recv_t);
    rcv->tag = PMIX_PTL_TAG_IOF;
    rcv->cbfunc = tool_iof_handler;
    /* add it to the end of the list of recvs */
    pmix_list_append(&pmix_ptl_base.posted_recvs, &rcv->super);
    /* default tools to outputting their IOF */
    pmix_globals.iof_flags.local_output = outputio;

    /* setup the globals */
    PMIX_CONSTRUCT(&pmix_client_globals.groups, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_client_globals.pending_requests, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_client_globals.peers, pmix_pointer_array_t);
    pmix_pointer_array_init(&pmix_client_globals.peers, 1, INT_MAX, 1);
    pmix_client_globals.myserver = PMIX_NEW(pmix_peer_t);
    if (NULL == pmix_client_globals.myserver) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOMEM;
    }
    pmix_client_globals.myserver->nptr = PMIX_NEW(pmix_namespace_t);
    if (NULL == pmix_client_globals.myserver->nptr) {
        PMIX_RELEASE(pmix_client_globals.myserver);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOMEM;
    }
    pmix_client_globals.myserver->info = PMIX_NEW(pmix_rank_info_t);
    if (NULL == pmix_client_globals.myserver->info) {
        PMIX_RELEASE(pmix_client_globals.myserver);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOMEM;
    }

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix: init called");

    if (PMIX_PEER_IS_CLIENT(pmix_globals.mypeer)) {
        /* if we are a client, then we need to pickup the
         * rest of the envar-based server assignments */
        pmix_globals.pindex = -1;
        /* setup a rank_info object for us */
        pmix_globals.mypeer->info = PMIX_NEW(pmix_rank_info_t);
        if (NULL == pmix_globals.mypeer->info) {
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            return PMIX_ERR_NOMEM;
        }
        pmix_globals.mypeer->info->pname.nspace = strdup(pmix_globals.myid.nspace);
        pmix_globals.mypeer->info->pname.rank = pmix_globals.myid.rank;
    }

    /* select our bfrops compat module */
    pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(NULL);
    if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver->nptr->compat.bfrops = pmix_globals.mypeer->nptr->compat.bfrops;

    /* select our psec compat module - the selection may be based
     * on the corresponding envars that should have been passed
     * to us at launch */
    evar = getenv("PMIX_SECURITY_MODE");
    pmix_globals.mypeer->nptr->compat.psec = pmix_psec_base_assign_module(evar);
    if (NULL == pmix_globals.mypeer->nptr->compat.psec) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver->nptr->compat.psec = pmix_globals.mypeer->nptr->compat.psec;

    /* set the buffer type - the selection will be based
     * on the corresponding envars that should have been passed
     * to us at launch */
    evar = getenv("PMIX_BFROP_BUFFER_TYPE");
    if (NULL == evar) {
        /* just set to our default */
        pmix_globals.mypeer->nptr->compat.type = pmix_bfrops_globals.default_type;
    } else if (0 == strcmp(evar, "PMIX_BFROP_BUFFER_FULLY_DESC")) {
        pmix_globals.mypeer->nptr->compat.type = PMIX_BFROP_BUFFER_FULLY_DESC;
    } else {
        pmix_globals.mypeer->nptr->compat.type = PMIX_BFROP_BUFFER_NON_DESC;
    }
    /* the server will be using the same */
    pmix_client_globals.myserver->nptr->compat.type = pmix_globals.mypeer->nptr->compat.type;

    /* tools are restricted to the "hash" component for interacting
     * with a server's GDS framework */
    PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, "hash", PMIX_STRING);
    pmix_globals.mypeer->nptr->compat.gds = pmix_gds_base_assign_module(&ginfo, 1);
    PMIX_INFO_DESTRUCT(&ginfo);
    if (NULL == pmix_globals.mypeer->nptr->compat.gds) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    pmix_client_globals.myserver->nptr->compat.gds = pmix_globals.mypeer->nptr->compat.gds;

    /* tools can, in some scenarios, act as servers,
     * so initialize the server globals too */
    if (PMIX_SUCCESS != (rc = pmix_server_initialize())) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    /* setup the function pointers */
    memset(&pmix_host_server, 0, sizeof(pmix_server_module_t));

    if (do_not_connect) {
        /* ensure we mark that we are not connected */
        pmix_globals.connected = false;
        /* it is not an error if we were not given an nspace/rank */
        if (!nspace_given || !rank_given) {
            /* self-assign a namespace and rank for ourselves. Use our hostname:pid
             * for the nspace, and rank clearly is 0 */
            pmix_snprintf(pmix_globals.myid.nspace, PMIX_MAX_NSLEN - 1, "%s:%lu", pmix_globals.hostname,
                     (unsigned long) pmix_globals.pid);
            pmix_globals.myid.rank = 0;
            nspace_given = false;
            rank_given = false;
            /* also setup the client myserver to point to ourselves */
            pmix_client_globals.myserver->nptr->nspace = strdup(pmix_globals.myid.nspace);
            pmix_client_globals.myserver->info = PMIX_NEW(pmix_rank_info_t);
            pmix_client_globals.myserver->info->pname.nspace = strdup(pmix_globals.myid.nspace);
            pmix_client_globals.myserver->info->pname.rank = pmix_globals.myid.rank;
            pmix_client_globals.myserver->info->uid = pmix_globals.uid;
            pmix_client_globals.myserver->info->gid = pmix_globals.gid;
        }
    } else {
        /* connect to the server */
        rc = pmix_ptl.connect_to_peer((struct pmix_peer_t *) pmix_client_globals.myserver, info,
                                      ninfo);
        if (PMIX_SUCCESS != rc) {
            /* if connection wasn't optional, then error out */
            if (!connect_optional) {
                PMIX_RELEASE_THREAD(&pmix_global_lock);
                return rc;
            }
            /* if connection was optional, then we need to self-assign
             * a namespace and rank for ourselves. Use our hostname:pid
             * for the nspace, and rank clearly is 0 */
            pmix_snprintf(pmix_globals.myid.nspace, PMIX_MAX_NSLEN - 1, "%s:%lu", pmix_globals.hostname,
                     (unsigned long) pmix_globals.pid);
            pmix_globals.myid.rank = 0;
            nspace_given = false;
            rank_given = false;
            /* also setup the client myserver to point to ourselves */
            pmix_client_globals.myserver->nptr->nspace = strdup(pmix_globals.myid.nspace);
            pmix_client_globals.myserver->info = PMIX_NEW(pmix_rank_info_t);
            pmix_client_globals.myserver->info->pname.nspace = strdup(pmix_globals.myid.nspace);
            pmix_client_globals.myserver->info->pname.rank = pmix_globals.myid.rank;
            pmix_client_globals.myserver->info->uid = pmix_globals.uid;
            pmix_client_globals.myserver->info->gid = pmix_globals.gid;
            /* mark us as not connecting to avoid asking for our job info */
            do_not_connect = true;
        }
    }
    /* setup the wildcard ID */
    PMIX_LOAD_PROCID(&wildcard, pmix_globals.myid.nspace, PMIX_RANK_WILDCARD);
    /* pass back the ID */
    PMIX_LOAD_PROCID(proc, pmix_globals.myid.nspace, pmix_globals.myid.rank);
    /* cache the server in case we later want to call "set_server" on it */
    PMIX_RETAIN(pmix_client_globals.myserver);
    pmix_pointer_array_add(&pmix_server_globals.clients, pmix_client_globals.myserver);

    /* load into our own peer object */
    if (NULL == pmix_globals.mypeer->nptr->nspace) {
        pmix_globals.mypeer->nptr->nspace = strdup(pmix_globals.myid.nspace);
    }
    /* setup a rank_info object for us */
    pmix_globals.mypeer->info = PMIX_NEW(pmix_rank_info_t);
    if (NULL == pmix_globals.mypeer->info) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_NOMEM;
    }
    pmix_globals.mypeer->info->pname.nspace = strdup(pmix_globals.myid.nspace);
    pmix_globals.mypeer->info->pname.rank = pmix_globals.myid.rank;
    /* if we are acting as a server, then setup the global recv */
    if (PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer) ||
        PMIX_PEER_IS_SCHEDULER(pmix_globals.mypeer)) {
        /* setup the wildcard recv for inbound messages from clients */
        rcv = PMIX_NEW(pmix_ptl_posted_recv_t);
        rcv->tag = UINT32_MAX;
        rcv->cbfunc = pmix_server_message_handler;
        /* add it to the end of the list of recvs */
        pmix_list_append(&pmix_ptl_base.posted_recvs, &rcv->super);
    }

    /* open the pmdl framework and select the active modules for this environment
     * as we might need them if we are asking a server to launch something for us */
    rc = pmix_mca_base_framework_open(&pmix_pmdl_base_framework,
                                      PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    if (PMIX_SUCCESS != (rc = pmix_pmdl_base_select())) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* setup IOF */
    PMIX_IOF_SINK_DEFINE(&pmix_client_globals.iof_stdout, &pmix_globals.myid, 1,
                         PMIX_FWD_STDOUT_CHANNEL, pmix_iof_write_handler);
    PMIX_IOF_SINK_DEFINE(&pmix_client_globals.iof_stderr, &pmix_globals.myid, 2,
                         PMIX_FWD_STDERR_CHANNEL, pmix_iof_write_handler);
    /* create the default iof handler */
    iofreq = PMIX_NEW(pmix_iof_req_t);
    iofreq->channels = PMIX_FWD_STDOUT_CHANNEL | PMIX_FWD_STDERR_CHANNEL | PMIX_FWD_STDDIAG_CHANNEL;
    pmix_pointer_array_set_item(&pmix_globals.iof_requests, 0, iofreq);

    if (fwd_stdin) {
        /* setup the read - we don't want to set nonblocking on our
         * stdio stream.  If we do so, we set the file descriptor to
         * non-blocking for everyone that has that file descriptor, which
         * includes everyone else in our shell pipeline chain.  (See
         * http://lists.freebsd.org/pipermail/freebsd-hackers/2005-January/009742.html).
         * This causes things like "prun -np 1 big_app | cat" to lose
         * output, because cat's stdout is then ALSO non-blocking and cat
         * isn't built to deal with that case (same with almost all other
         * unix text utils).*/
        fd = fileno(stdin);
        if (isatty(fd)) {
            /* We should avoid trying to read from stdin if we
             * have a terminal, but are backgrounded.  Catch the
             * signals that are commonly used when we switch
             * between being backgrounded and not.  If the
             * filedescriptor is not a tty, don't worry about it
             * and always stay connected.
             */
            pmix_event_signal_set(pmix_globals.evauxbase, &stdinsig, SIGCONT,
                                  pmix_iof_stdin_cb, &stdinev);

            /* setup a read event to read stdin, but don't activate it yet. The
             * dst_name indicates who should receive the stdin. If that recipient
             * doesn't do a corresponding pull, however, then the stdin will
             * be dropped upon receipt at the local daemon
             */
            PMIX_CONSTRUCT(&stdinev, pmix_iof_read_event_t);
            stdinev.fd = fd;
            stdinev.always_readable = pmix_iof_fd_always_ready(fd);
            if (stdinev.always_readable) {
                pmix_event_evtimer_set(pmix_globals.evbase, &stdinev.ev,
                                       pmix_iof_read_local_handler, &stdinev);
            } else {
                pmix_event_set(pmix_globals.evbase, &stdinev.ev, fd, PMIX_EV_READ,
                               pmix_iof_read_local_handler, &stdinev);
            }
            /* check to see if we want the stdin read event to be
             * active - we will always at least define the event,
             * but may delay its activation
             */
            if (pmix_iof_stdin_check(fd)) {
                PMIX_IOF_READ_ACTIVATE(&stdinev);
            }
        } else {
            /* if we are not looking at a tty, just setup a read event
             * and activate it
             */
            PMIX_CONSTRUCT(&stdinev, pmix_iof_read_event_t);
            stdinev.fd = fd;
            stdinev.always_readable = pmix_iof_fd_always_ready(fd);
            if (stdinev.always_readable) {
                pmix_event_evtimer_set(pmix_globals.evbase, &stdinev.ev,
                                       pmix_iof_read_local_handler, &stdinev);
            } else {
                pmix_event_set(pmix_globals.evbase, &stdinev.ev, fd, PMIX_EV_READ,
                               pmix_iof_read_local_handler, &stdinev);
            }
            PMIX_IOF_READ_ACTIVATE(&stdinev);
        }
    }

    /* increment our init reference counter */
    pmix_globals.init_cntr++;

    /* fill in our local
     * datastore with typical job-related info. No point
     * in having the server generate these as we are
     * obviously a singleton, and so the values are well-known */
    rc = pmix_tool_init_info();
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* if we are connected, then send a request for our
     * job info - we do this as a non-blocking
     * transaction because some systems cannot handle very large
     * blocking operations and error out if we try them. */
    if (!do_not_connect && !PMIX_PEER_IS_SCHEDULER(pmix_client_globals.myserver)) {
        req = PMIX_NEW(pmix_buffer_t);
        cmd = PMIX_REQ_CMD;
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, req, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(req);
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            return rc;
        }
        /* send to the server */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, req, job_data, (void *) &cb);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            return rc;
        }
        /* wait for the data to return */
        PMIX_WAIT_THREAD(&cb.lock);
        rc = cb.status;
        PMIX_DESTRUCT(&cb);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            return rc;
        }
        /* quick check to see if we got something back. If this
         * is a launcher that is being executed multiple times
         * in a job-script, then the original registration data
         * may have been deleted after the first invocation. In
         * such a case, we simply regenerate it locally as it is
         * well-known */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &wildcard;
        cb.copy = true;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        if (PMIX_SUCCESS != rc) {
            pmix_output_verbose(5, pmix_client_globals.get_output,
                                "pmix:tool:client data not found in internal storage");
            rc = pmix_tool_init_info();
            if (PMIX_SUCCESS != rc) {
                PMIX_DESTRUCT(&cb);
                PMIX_RELEASE_THREAD(&pmix_global_lock);
                return rc;
            }
        }
        PMIX_DESTRUCT(&cb);
    }
    // enable show_help subsystem
    pmix_show_help_enabled = true;
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* if we are acting as a server, then start listening
     * and register the server receive */
    if (PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer) ||
        PMIX_PEER_IS_SCHEDULER(pmix_globals.mypeer)) {
        /* setup the fork/exec framework */
        rc = pmix_mca_base_framework_open(&pmix_pfexec_base_framework,
                                          PMIX_MCA_BASE_OPEN_DEFAULT);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_pfexec_base_select())) {
            return rc;
        }

        /* if we don't know our topology, we better get it now as we
         * increasingly rely on it - note that our host will hopefully
         * have passed it to us so we don't duplicate their storage! */
        if (PMIX_SUCCESS != (rc = pmix_hwloc_setup_topology(info, ninfo))) {
            return rc;
        }

        /* open the pnet framework and select the active modules for this environment */
        rc = pmix_mca_base_framework_open(&pmix_pnet_base_framework,
                                          PMIX_MCA_BASE_OPEN_DEFAULT);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        if (PMIX_SUCCESS != (rc = pmix_pnet_base_select())) {
            return rc;
        }

        /* start listening for connections */
        if (PMIX_SUCCESS != pmix_ptl_base_start_listening(info, ninfo)) {
            pmix_show_help("help-pmix-server.txt", "listener-thread-start", true);
            return PMIX_ERR_INIT;
        }
    }

    /* see if they gave us a rendezvous URI to which we are to call back */
    evar = getenv("PMIX_LAUNCHER_RNDZ_URI");
    if (NULL != evar) {
        /* attach to the specified server so it can
         * tell us what we are to do - save our
         * current server for now */
        PMIX_LOAD_PROCID(&myserver, pmix_client_globals.myserver->info->pname.nspace,
                         pmix_client_globals.myserver->info->pname.rank);
        PMIX_INFO_CREATE(iptr, 3);
        PMIX_INFO_LOAD(&iptr[0], PMIX_SERVER_URI, evar, PMIX_STRING);
        rc = 2; // give us two seconds to connect
        PMIX_INFO_LOAD(&iptr[1], PMIX_TIMEOUT, &rc, PMIX_INT);
        PMIX_INFO_LOAD(&iptr[2], PMIX_PRIMARY_SERVER, NULL, PMIX_BOOL);
        rc = PMIx_tool_attach_to_server(NULL, &myparent, iptr, 3);
        if (PMIX_SUCCESS != rc) {
            return PMIX_ERR_UNREACH;
        }

        /* save our parent ID */
        value.type = PMIX_PROC;
        value.data.proc = &myparent;
        rc = PMIx_Store_internal(&pmix_globals.myid, PMIX_PARENT_ID, &value);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        /* retrieve any job info it has for us */
        req = PMIX_NEW(pmix_buffer_t);
        cmd = PMIX_REQ_CMD;
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, req, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(req);
            return rc;
        }
        /* send to the server */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, req, job_data, (void *) &cb);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        /* wait for the data to return */
        PMIX_WAIT_THREAD(&cb.lock);
        rc = cb.status;
        PMIX_DESTRUCT(&cb);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }

        /* if the value was found, then we need to wait for debugger attach here */
        /* register for the debugger release notification */
        PMIX_CONSTRUCT_LOCK(&reglock);
        PMIX_CONSTRUCT_LOCK(&releaselock);
        PMIX_INFO_LOAD(&evinfo[0], PMIX_EVENT_RETURN_OBJECT, &releaselock, PMIX_POINTER);
        PMIX_INFO_LOAD(&evinfo[1], PMIX_EVENT_HDLR_NAME, "WAIT-FOR-RELEASE", PMIX_STRING);
        PMIX_INFO_LOAD(&evinfo[2], PMIX_EVENT_ONESHOT, NULL, PMIX_BOOL);
        pmix_output_verbose(2, pmix_client_globals.event_output,
                            "[%s:%d] WAITING IN INIT FOR RELEASE", pmix_globals.myid.nspace,
                            pmix_globals.myid.rank);
        code = PMIX_DEBUGGER_RELEASE;
        PMIx_Register_event_handler(&code, 1, evinfo, 3, notification_fn, evhandler_reg_callbk,
                                    (void *) &reglock);
        /* wait for registration to complete */
        PMIX_WAIT_THREAD(&reglock);
        PMIX_DESTRUCT_LOCK(&reglock);
        PMIX_INFO_DESTRUCT(&evinfo[0]);
        PMIX_INFO_DESTRUCT(&evinfo[1]);
        /* wait for release to arrive */
        PMIX_WAIT_THREAD(&releaselock);
        PMIX_DESTRUCT_LOCK(&releaselock);

        /* restore our original primary server */
        rc = PMIx_tool_set_server(&myserver, NULL, 0);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
    }

    /* register the tool supported attrs */
    rc = pmix_register_tool_attrs();

    return rc;
}

PMIX_EXPORT pmix_status_t pmix_tool_init_info(void)
{
    pmix_kval_t *kptr;
    pmix_status_t rc;
    pmix_proc_t wildcard;

    PMIX_LOAD_PROCID(&wildcard, pmix_globals.myid.nspace, PMIX_RANK_WILDCARD);

    /* the jobid is just our nspace */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_JOBID);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup(pmix_globals.myid.nspace);
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* our rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_INT;
    kptr->value->data.integer = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* nproc offset */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_NPROC_OFFSET);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* node size */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_NODE_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local peers */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_PEERS);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup("0");
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local leader */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCALLDR);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* universe size */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_UNIV_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* job size - we are our very own job, so we have no peers */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_JOB_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local size - only us in our job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_SIZE);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* max procs - since we are a self-started tool, there is no
     * allocation within which we can grow ourselves */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_MAX_PROCS);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 1;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app number */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APPNUM);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app leader */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APPLDR);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* app rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_APP_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* global rank */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_GLOBAL_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT32;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* local rank - we are alone in our job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_LOCAL_RANK);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_UINT16;
    kptr->value->data.uint32 = 0;
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* we cannot know the node rank as we don't know what
     * other processes are executing on this node - so
     * we'll add that info to the server-tool handshake
     * and load it from there */

    /* hostname */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_HOSTNAME);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup(pmix_globals.hostname);
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* we cannot know the RM's nodeid for this host, so
     * we'll add that info to the server-tool handshake
     * and load it from there */

    /* the nodemap is simply our hostname as there is no
     * regex to generate */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_NODE_MAP);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup(pmix_globals.hostname);
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* likewise, the proc map is just our rank as we are
     * the only proc in this job */
    kptr = PMIX_NEW(pmix_kval_t);
    kptr->key = strdup(PMIX_PROC_MAP);
    PMIX_VALUE_CREATE(kptr->value, 1);
    kptr->value->type = PMIX_STRING;
    kptr->value->data.string = strdup("0");
    PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &wildcard, PMIX_INTERNAL, kptr);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    PMIX_RELEASE(kptr); // maintain accounting

    /* store our server's ID */
    if (NULL != pmix_client_globals.myserver && NULL != pmix_client_globals.myserver->info
        && NULL != pmix_client_globals.myserver->info->pname.nspace) {
        kptr = PMIX_NEW(pmix_kval_t);
        kptr->key = strdup(PMIX_SERVER_NSPACE);
        PMIX_VALUE_CREATE(kptr->value, 1);
        kptr->value->type = PMIX_STRING;
        kptr->value->data.string = strdup(pmix_client_globals.myserver->info->pname.nspace);
        PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        PMIX_RELEASE(kptr); // maintain accounting
        kptr = PMIX_NEW(pmix_kval_t);
        kptr->key = strdup(PMIX_SERVER_RANK);
        PMIX_VALUE_CREATE(kptr->value, 1);
        kptr->value->type = PMIX_PROC_RANK;
        kptr->value->data.rank = pmix_client_globals.myserver->info->pname.rank;
        PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            return rc;
        }
        PMIX_RELEASE(kptr); // maintain accounting
    }

    return PMIX_SUCCESS;
}

pmix_status_t PMIx_tool_set_server_module(pmix_server_module_t *module)
{
    pmix_host_server = *module;
    /* mark that we are now a server */
    PMIX_SET_PEER_TYPE(pmix_globals.mypeer, PMIX_PROC_SERVER);
    return PMIX_SUCCESS;
}

typedef struct {
    pmix_lock_t lock;
    pmix_event_t ev;
    bool active;
} pmix_tool_timeout_t;

/* timer callback */
static void fin_timeout(int sd, short args, void *cbdata)
{
    pmix_tool_timeout_t *tev;
    tev = (pmix_tool_timeout_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:tool finwait timeout fired");
    if (tev->active) {
        tev->active = false;
        PMIX_WAKEUP_THREAD(&tev->lock);
    }
}
/* callback for finalize completion */
static void finwait_cbfunc(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
                           pmix_buffer_t *buf, void *cbdata)
{
    pmix_tool_timeout_t *tev;
    tev = (pmix_tool_timeout_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(pr, hdr, buf);

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:tool finwait_cbfunc received");
    if (tev->active) {
        tev->active = false;
        pmix_event_del(&tev->ev); // stop the timer
    }
    PMIX_WAKEUP_THREAD(&tev->lock);
}

static void checkev(int sd, short args, void *cbdata)
{
    pmix_lock_t *lock = (pmix_lock_t*)cbdata;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);
    PMIX_WAKEUP_THREAD(lock);
}

PMIX_EXPORT pmix_status_t PMIx_tool_finalize(void)
{
    pmix_buffer_t *msg;
    pmix_cmd_t cmd = PMIX_FINALIZE_CMD;
    pmix_status_t rc;
    pmix_tool_timeout_t tev;
    struct timeval tv = {5, 0};
    int n;
    pmix_peer_t *peer;
    pmix_pfexec_child_t *child;
    pmix_lock_t lock;
    pmix_event_t ev;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;
    pmix_globals.mypeer->finalized = true;
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:tool finalize called");

    /* if we are connected, then disconnect */
    if (pmix_globals.connected) {
        pmix_output_verbose(2, pmix_globals.debug_output,
                            "pmix:tool sending finalize sync to server");

        /* setup a cmd message to notify the PMIx
         * server that we are normally terminating */
        msg = PMIX_NEW(pmix_buffer_t);
        /* pack the cmd */
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, msg, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(msg);
            return rc;
        }
        /* setup a timer to protect ourselves should the server be unable
         * to answer for some reason */
        PMIX_CONSTRUCT_LOCK(&tev.lock);
        pmix_event_assign(&tev.ev, pmix_globals.evbase, -1, 0, fin_timeout, &tev);
        tev.active = true;
        PMIX_POST_OBJECT(&tev);
        pmix_event_add(&tev.ev, &tv);
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, msg, finwait_cbfunc, (void *) &tev);
        if (PMIX_SUCCESS != rc) {
            if (tev.active) {
                pmix_event_del(&tev.ev);
            }
            return rc;
        }

        /* wait for the ack to return */
        PMIX_WAIT_THREAD(&tev.lock);
        PMIX_DESTRUCT_LOCK(&tev.lock);

        if (tev.active) {
            pmix_event_del(&tev.ev);
        }
        pmix_output_verbose(2, pmix_globals.debug_output, "pmix:tool finalize sync received");
    }

    if (PMIX_PEER_IS_LAUNCHER(pmix_globals.mypeer)) {
        /* if we have launched children, then we need to cleanly
         * terminate them - do this before stopping our progress
         * thread as we need it for terminating procs */
        if (pmix_pfexec_globals.active) {
            pmix_event_del(pmix_pfexec_globals.handler);
            pmix_pfexec_globals.active = false;
        }
        PMIX_LIST_FOREACH (child, &pmix_pfexec_globals.children, pmix_pfexec_child_t) {
            pmix_pfexec.kill_proc(&child->proc);
        }
    }

    /* wait here until all active events have been processed */
    PMIX_CONSTRUCT_LOCK(&lock);
    pmix_event_assign(&ev, pmix_globals.evbase, -1, EV_WRITE, checkev, &lock);
    PMIX_POST_OBJECT(&lock);
    pmix_event_active(&ev, EV_WRITE, 1);
    PMIX_WAIT_THREAD(&lock);
    PMIX_DESTRUCT_LOCK(&lock);

    /* stop the progress thread, but leave the event base
     * still constructed. This will allow us to safely
     * tear down the infrastructure, including removal
     * of any events objects may be holding */
    (void) pmix_progress_thread_pause(NULL);

    /* flush anything that is still trying to be written out */
    pmix_iof_static_dump_output(&pmix_client_globals.iof_stdout);
    pmix_iof_static_dump_output(&pmix_client_globals.iof_stderr);

    PMIX_LIST_DESTRUCT(&pmix_client_globals.pending_requests);
    for (n = 0; n < pmix_client_globals.peers.size; n++) {
        peer = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_client_globals.peers, n);
        if (NULL != peer) {
            PMIX_RELEASE(peer);
        }
    }

    pmix_ptl_base_stop_listening();

    for (n = 0; n < pmix_server_globals.clients.size; n++) {
        peer = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, n);
        if (NULL != peer) {
            PMIX_RELEASE(peer);
        }
    }

    (void) pmix_mca_base_framework_close(&pmix_pnet_base_framework);
    PMIX_DESTRUCT(&pmix_server_globals.clients);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.collectives);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.remote_pnd);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.local_reqs);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.gdata);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.events);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.iof);

    (void) pmix_mca_base_framework_close(&pmix_pfexec_base_framework);
    (void) pmix_mca_base_framework_close(&pmix_pmdl_base_framework);
    (void) pmix_mca_base_framework_close(&pmix_pnet_base_framework);
    (void) pmix_mca_base_framework_close(&pmix_pstrg_base_framework);

    pmix_rte_finalize();
    if (NULL != pmix_globals.mypeer) {
        PMIX_RELEASE(pmix_globals.mypeer);
    }
    if (NULL != pmix_client_globals.myserver) {
        PMIX_RELEASE(pmix_client_globals.myserver);
    }

    /* finalize the class/object system */
    pmix_class_finalize();

    return PMIX_SUCCESS;
}

bool PMIx_tool_is_connected(void)
{
    return pmix_globals.connected;
}

pmix_status_t PMIx_tool_connect_to_server(pmix_proc_t *proc, pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;

    rc = PMIx_tool_attach_to_server(proc, NULL, info, ninfo);
    return rc;
}

static void retry_attach(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    pmix_kval_t *kptr;
    pmix_peer_t *peer;
    size_t n;
    pmix_status_t rc;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(cb);

    /* check for directives */
    cb->checked = false;
    for (n = 0; n < cb->ninfo; n++) {
        if (PMIX_CHECK_KEY(&cb->info[n], PMIX_PRIMARY_SERVER)) {
            cb->checked = PMIX_INFO_TRUE(&cb->info[n]);
            break;
        }
    }

    /* ask the ptl to establish connection to the new server */
    peer = PMIX_NEW(pmix_peer_t);
    /* setup the infrastructure - assume this new server will follow
     * same rules as our current one */
    peer->nptr = PMIX_NEW(pmix_namespace_t);
    peer->info = PMIX_NEW(pmix_rank_info_t);
    peer->nptr->compat.bfrops = pmix_globals.mypeer->nptr->compat.bfrops;
    peer->nptr->compat.psec = pmix_globals.mypeer->nptr->compat.psec;
    peer->nptr->compat.type = pmix_globals.mypeer->nptr->compat.type;
    peer->nptr->compat.gds = pmix_globals.mypeer->nptr->compat.gds;

    cb->status = pmix_ptl.connect_to_peer((struct pmix_peer_t *) peer, cb->info, cb->ninfo);

    if (PMIX_SUCCESS == cb->status) {
        /* return the name */
        cb->pname.nspace = strdup(peer->info->pname.nspace);
        cb->pname.rank = peer->info->pname.rank;
        /* add the peer to our known clients */
        pmix_pointer_array_add(&pmix_server_globals.clients, peer);
        if (cb->checked) {
            /* point our active server at this new one */
            pmix_client_globals.myserver = peer;
            /* mark that we are connected */
            pmix_globals.connected = true;
            /* update our active server's ID in the local key-value store */
            kptr = PMIX_NEW(pmix_kval_t);
            kptr->key = strdup(PMIX_SERVER_NSPACE);
            PMIX_VALUE_CREATE(kptr->value, 1);
            kptr->value->type = PMIX_STRING;
            kptr->value->data.string = strdup(peer->info->pname.nspace);
            PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            PMIX_RELEASE(kptr); // maintain accounting
            kptr = PMIX_NEW(pmix_kval_t);
            kptr->key = strdup(PMIX_SERVER_RANK);
            PMIX_VALUE_CREATE(kptr->value, 1);
            kptr->value->type = PMIX_PROC_RANK;
            kptr->value->data.rank = peer->info->pname.rank;
            PMIX_GDS_STORE_KV(rc, pmix_globals.mypeer, &pmix_globals.myid, PMIX_INTERNAL, kptr);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            PMIX_RELEASE(kptr); // maintain accounting
        }

    } else {
        PMIX_RELEASE(peer);
    }

    PMIX_WAKEUP_THREAD(&cb->lock);
    PMIX_POST_OBJECT(cb);
    return;
}

pmix_status_t PMIx_tool_attach_to_server(pmix_proc_t *myproc, pmix_proc_t *server,
                                         pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    pmix_cb_t *cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* check for bozo error */
    if (NULL == info || 0 == ninfo) {
        pmix_show_help("help-pmix-runtime.txt", "tool:no-server", true);
        return PMIX_ERR_BAD_PARAM;
    }

    cb = PMIX_NEW(pmix_cb_t);
    cb->info = info;
    cb->ninfo = ninfo;
    PMIX_THREADSHIFT(cb, retry_attach);

    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;

    /* if they gave us an address, we pass back our name */
    if (NULL != myproc) {
        memcpy(myproc, &pmix_globals.myid, sizeof(pmix_proc_t));
    }

    /* if the transition didn't succeed, then return at this point */
    if (PMIX_SUCCESS != rc) {
        return rc;
    }

    /* if they gave us an address, return the new server's ID */
    if (NULL != server) {
        PMIX_LOAD_PROCID(server, cb->pname.nspace, cb->pname.rank);
    }

    return PMIX_SUCCESS;
}

static void disc(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    pmix_peer_t *peer = NULL, *pr;
    int n;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(cb);

    if (NULL == cb->proc) {
        pmix_globals.connected = false;
        cb->status = PMIX_SUCCESS;
        PMIX_WAKEUP_THREAD(&cb->lock);
        PMIX_POST_OBJECT(cb);
        return;
    }

    /* see if we have this server */
    for (n = 0; n < pmix_server_globals.clients.size; n++) {
        pr = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients, n);
        if (NULL == pr) {
            continue;
        }
        if (PMIX_CHECK_NSPACE(cb->proc->nspace, pr->info->pname.nspace)
            && PMIX_CHECK_RANK(cb->proc->rank, pr->info->pname.rank)) {
            peer = pr;
            pmix_pointer_array_set_item(&pmix_server_globals.clients, n, NULL);
            break;
        }
    }
    if (NULL == peer) {
        cb->status = PMIX_ERR_NOT_FOUND;
        PMIX_WAKEUP_THREAD(&cb->lock);
        PMIX_POST_OBJECT(cb);
        return;
    }

    /* if we are disconnecting from the active server, then we enter a
     * "disconnected" state where we point the active server back at
     * ourselves - effectively the same as when we init without connecting */
    if (peer == pmix_client_globals.myserver) {
        PMIX_RETAIN(pmix_globals.mypeer);
        /* switch servers - we are in an event, so it is
         * safe to do so */
        pmix_client_globals.myserver = pmix_globals.mypeer;
        pmix_globals.connected = false;
    }

    /* now drop the connection */
    PMIX_RELEASE(peer);

    cb->status = PMIX_SUCCESS;
    PMIX_WAKEUP_THREAD(&cb->lock);

    PMIX_POST_OBJECT(cb);
    return;
}

pmix_status_t PMIx_tool_disconnect(const pmix_proc_t *server)
{
    pmix_status_t rc;
    pmix_cb_t *cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    cb = PMIX_NEW(pmix_cb_t);
    cb->proc = (pmix_proc_t *) server;
    PMIX_THREADSHIFT(cb, disc);

    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;
    cb->proc = NULL;
    PMIX_RELEASE(cb);

    return rc;
}

static void getsrvrs(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    int n;
    size_t ns;
    pmix_list_t srvrs;
    pmix_proclist_t *ps;
    pmix_peer_t *pr;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(cb);

    /* get servers */
    PMIX_CONSTRUCT(&srvrs, pmix_list_t);
    /* put our current active server at the front */
    if (pmix_globals.mypeer != pmix_client_globals.myserver) {
        ps = PMIX_NEW(pmix_proclist_t);
        PMIX_LOAD_PROCID(&ps->proc, pmix_client_globals.myserver->info->pname.nspace,
                         pmix_client_globals.myserver->info->pname.rank);
        pmix_list_append(&srvrs, &ps->super);
    }

    for (n = 0; n < pmix_server_globals.clients.size; n++) {
        pr = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients, n);
        if (NULL == pr) {
            continue;
        }
        /* if it is our current primary server, ignore it */
        if (pr == pmix_client_globals.myserver) {
            continue;
        }
        /* record it */
        ps = PMIX_NEW(pmix_proclist_t);
        PMIX_LOAD_PROCID(&ps->proc, pr->info->pname.nspace, pr->info->pname.rank);
        pmix_list_append(&srvrs, &ps->super);
    }

    ns = pmix_list_get_size(&srvrs);

    if (0 == ns) {
        /* we aren't connected to anyone */
        cb->status = PMIX_ERR_UNREACH;
        cb->nprocs = 0;
        cb->procs = NULL;
        PMIX_DESTRUCT(&srvrs);
        PMIX_WAKEUP_THREAD(&cb->lock);
        PMIX_POST_OBJECT(cb);
        return;
    }

    /* allocate the array */
    PMIX_PROC_CREATE(cb->procs, ns);
    cb->nprocs = ns;

    /* now load the array */
    n = 0;
    PMIX_LIST_FOREACH (ps, &srvrs, pmix_proclist_t) {
        memcpy(&cb->procs[n], &ps->proc, sizeof(pmix_proc_t));
        ++n;
    }
    cb->status = PMIX_SUCCESS;
    PMIX_LIST_DESTRUCT(&srvrs);

    PMIX_WAKEUP_THREAD(&cb->lock);
    PMIX_POST_OBJECT(cb);
    return;
}
pmix_status_t PMIx_tool_get_servers(pmix_proc_t *servers[], size_t *nservers)
{
    pmix_status_t rc;
    pmix_cb_t *cb;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    cb = PMIX_NEW(pmix_cb_t);

    PMIX_THREADSHIFT(cb, getsrvrs);
    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;
    *servers = cb->procs;
    *nservers = cb->nprocs;

    cb->procs = NULL; // protect the array
    cb->nprocs = 0;
    PMIX_RELEASE(cb);

    return rc;
}

static void retry_set(int sd, short args, void *cbdata)
{
    pmix_cb_t *cb = (pmix_cb_t *) cbdata;
    int n;
    pmix_peer_t *peer = NULL, *pr;
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(cb);

    /* if we are switching back to me, then there is no point in
     * searching the array of clients - I definitely won't be there! */
    if (PMIX_CHECK_NSPACE(cb->proc->nspace, pmix_globals.myid.nspace)
        && PMIX_CHECK_RANK(cb->proc->rank, pmix_globals.myid.rank)) {
        pmix_client_globals.myserver = pmix_globals.mypeer;
        pmix_globals.connected = true;
        goto done;
    }

    /* see if we have this server */
    for (n = 0; n < pmix_server_globals.clients.size; n++) {
        pr = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients, n);
        if (NULL == pr) {
            continue;
        }
        if (PMIX_CHECK_NSPACE(cb->proc->nspace, pr->info->pname.nspace)
            && PMIX_CHECK_RANK(cb->proc->rank, pr->info->pname.rank)) {
            peer = pr;
            break;
        }
    }
    if (NULL == peer) {
        /* do they want us to wait? */
        if (cb->checked) {
            /* have we timed out? */
            --cb->status;
            if (cb->status < 0) {
                cb->status = PMIX_ERR_NOT_FOUND;
                PMIX_WAKEUP_THREAD(&cb->lock);
                return;
            }
            PMIX_THREADSHIFT_DELAY(cb, retry_set, 0.25);
        } else {
            /* no - so just return failure */
            cb->status = PMIX_ERR_UNREACH;
            PMIX_WAKEUP_THREAD(&cb->lock);
        }
        PMIX_POST_OBJECT(cb);
        return;
    }

    /* if this is the current active server, then ignore the request */
    if (peer == pmix_client_globals.myserver) {
        pmix_globals.connected = true; // just ensure we mark ourselves as connected
        cb->status = PMIX_SUCCESS;
        PMIX_WAKEUP_THREAD(&cb->lock);
        PMIX_POST_OBJECT(cb);
        return;
    }

    /* switch the active server - we are in an event, so
     * it is safe to do so */
    PMIX_RETAIN(peer);
    pmix_client_globals.myserver = peer;
    pmix_globals.connected = true;

done:
    cb->status = PMIX_SUCCESS;
    PMIX_WAKEUP_THREAD(&cb->lock);
    PMIX_POST_OBJECT(cb);
    return;
}

pmix_status_t PMIx_tool_set_server(const pmix_proc_t *server, pmix_info_t info[], size_t ninfo)
{
    pmix_status_t rc;
    pmix_cb_t *cb;
    size_t n;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* threadshift this so we can access global structures */
    cb = PMIX_NEW(pmix_cb_t);
    cb->proc = (pmix_proc_t *) server;
    for (n = 0; n < ninfo; n++) {
        if (PMIX_CHECK_KEY(&info[n], PMIX_TIMEOUT)) {
            cb->status = 4 * info[n].value.data.integer;
        } else if (PMIX_CHECK_KEY(&info[n], PMIX_WAIT_FOR_CONNECTION)) {
            cb->checked = PMIX_INFO_TRUE(&info[n]);
        }
    }
    PMIX_THREADSHIFT(cb, retry_set);

    /* wait for completion */
    PMIX_WAIT_THREAD(&cb->lock);
    rc = cb->status;
    PMIX_RELEASE(cb);

    return rc;
}
