/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2014-2015 Artem Y. Polyakov <artpol84@gmail.com>.
 *                         All rights reserved.
 * Copyright (c) 2016-2020 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2023 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "src/include/pmix_config.h"

#include "src/include/pmix_socket_errno.h"
#include "src/include/pmix_stdint.h"

#include "pmix_common.h"
#include "include/pmix_server.h"

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
#include <ctype.h>
#include <sys/stat.h>

#include "src/common/pmix_attributes.h"
#include "src/hwloc/pmix_hwloc.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/mca/base/pmix_mca_base_vari.h"
#include "src/mca/bfrops/base/base.h"
#include "src/mca/gds/base/base.h"
#include "src/mca/pinstalldirs/base/base.h"
#include "src/mca/pgpu/base/base.h"
#include "src/mca/pmdl/base/base.h"
#include "src/mca/pnet/base/base.h"
#include "src/mca/preg/preg.h"
#include "src/mca/prm/base/base.h"
#include "src/mca/psensor/base/base.h"
#include "src/mca/pstrg/base/base.h"
#include "src/mca/ptl/base/base.h"
#include "src/runtime/pmix_progress_threads.h"
#include "src/runtime/pmix_rte.h"
#include "src/tool/pmix_tool_ops.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_show_help.h"

/* the server also needs access to client operations
 * as it can, and often does, behave as a client */
#include "pmix_server_ops.h"
#include "src/client/pmix_client_ops.h"

// global variables
pmix_server_globals_t pmix_server_globals = {
    .nspaces = PMIX_LIST_STATIC_INIT,
    .clients = PMIX_POINTER_ARRAY_STATIC_INIT,
    .collectives = PMIX_LIST_STATIC_INIT,
    .remote_pnd = PMIX_LIST_STATIC_INIT,
    .local_reqs = PMIX_LIST_STATIC_INIT,
    .gdata = PMIX_LIST_STATIC_INIT,
    .genvars = NULL,
    .events = PMIX_LIST_STATIC_INIT,
    .failedgrps = NULL,
    .iof = PMIX_LIST_STATIC_INIT,
    .iof_residuals = PMIX_LIST_STATIC_INIT,
    .psets = PMIX_LIST_STATIC_INIT,
    .max_iof_cache = 0,
    .tool_connections_allowed = false,
    .tmpdir = NULL,
    .system_tmpdir = NULL,
    .fence_localonly_opt = false,
    .get_output = -1,
    .get_verbose = 0,
    .connect_output = -1,
    .connect_verbose = 0,
    .fence_output = -1,
    .fence_verbose = 0,
    .pub_output = -1,
    .pub_verbose = 0,
    .spawn_output = -1,
    .spawn_verbose = 0,
    .event_output = -1,
    .event_verbose = 0,
    .iof_output = -1,
    .iof_verbose = 0,
    .base_output = -1,
    .base_verbose = 0,
    .group_output = -1,
    .group_verbose = 0
};

// local variables
static pmix_event_t parentdied;
static char *security_mode = NULL;
static char *bfrops_mode = NULL;
static char *gds_mode = NULL;
static pid_t mypid;
static pmix_proc_t myparent;

static void pdiedfn(int fd, short flags, void *arg)
{
    pmix_info_t info[2];
    pmix_proc_t keepalive;

    PMIX_HIDE_UNUSED_PARAMS(fd, flags, arg);

    PMIX_LOAD_PROCID(&keepalive, "PMIX_KEEPALIVE_PIPE", PMIX_RANK_UNDEF);

    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    PMIX_INFO_LOAD(&info[1], PMIX_EVENT_AFFECTED_PROC, &keepalive, PMIX_PROC);

    /* generate a job-terminated event */
    PMIx_Notify_event(PMIX_ERR_JOB_TERMINATED, &pmix_globals.myid,
                      PMIX_RANGE_PROC_LOCAL, info, 2,
                      NULL, NULL);
}

static void server_iof_handler(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
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

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "recvd IOF with %d bytes from %s",
                        (int) buf->bytes_used,
                        PMIX_PNAME_PRINT(&peer->info->pname));

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
static void job_data(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr, pmix_buffer_t *buf, void *cbdata)
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

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "[%s:%d] DEBUGGER RELEASE RECVD",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank);

    PMIX_HIDE_UNUSED_PARAMS(evhdlr_registration_id, status, source, results, nresults);

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
            pmix_output_verbose(2, pmix_server_globals.base_output,
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

static void debugger_aggregator(size_t evhdlr_registration_id, pmix_status_t status,
                                const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                                pmix_info_t results[], size_t nresults,
                                pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    pmix_proc_t proc;
    pmix_status_t rc;
    pmix_namespace_t *ns, *nptr;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "[%s:%d] DEBUGGER AGGREGATOR CALLED FOR NSPACE %s",
                        pmix_globals.myid.nspace, pmix_globals.myid.rank, source->nspace);

    PMIX_HIDE_UNUSED_PARAMS(evhdlr_registration_id, results, nresults);

    /* find the nspace tracker for this namespace */
    nptr = NULL;
    PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(ns->nspace, source->nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* only can happen if there is an error - nothing we can do*/
        goto done;
    }

    /* track the number of waiting-for-notify alerts we get */
    nptr->num_waiting--;

    if (nptr->num_waiting <= 0) {
        PMIX_LOAD_PROCID(&proc, source->nspace, PMIX_RANK_LOCAL_PEERS);
        /* pass an event to our host */
        rc = pmix_prm.notify(status, &proc, PMIX_RANGE_RM, info, ninfo, NULL, NULL);
        if (PMIX_SUCCESS != rc && PMIX_OPERATION_SUCCEEDED != rc &&
            PMIX_ERR_NOT_SUPPORTED != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

done:
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

static pmix_status_t register_singleton(char *name)
{
    char *tmp, *ptr;
    pmix_namespace_t *nptr;
    pmix_rank_t rank;
    pmix_rank_info_t *rinfo;

    tmp = strdup(name);
    ptr = strrchr(tmp, '.');
    *ptr = '\0';
    ++ptr;
    rank = strtoul(ptr, NULL, 10);

    nptr = PMIX_NEW(pmix_namespace_t);
    if (NULL == nptr) {
        free(tmp);
        return PMIX_ERR_NOMEM;
    }
    nptr->nspace = strdup(tmp);
    nptr->nlocalprocs = 1;
    nptr->nprocs = 1;
    pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    /* add this rank */
    rinfo = PMIX_NEW(pmix_rank_info_t);
    if (NULL == rinfo) {
        free(tmp);
        return PMIX_ERR_NOMEM;
    }
    rinfo->pname.nspace = strdup(tmp);
    rinfo->pname.rank = rank;
    rinfo->uid = geteuid();
    rinfo->gid = getegid();
    pmix_list_append(&nptr->ranks, &rinfo->super);
    nptr->all_registered = true;
    free(tmp);

    return PMIX_SUCCESS;
}

// local functions for connection support
pmix_status_t pmix_server_initialize(void)
{
    /* setup the server-specific globals */
    PMIX_CONSTRUCT(&pmix_server_globals.clients, pmix_pointer_array_t);
    pmix_pointer_array_init(&pmix_server_globals.clients, 1, INT_MAX, 1);
    PMIX_CONSTRUCT(&pmix_server_globals.nspaces, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.collectives, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.remote_pnd, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.local_reqs, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.gdata, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.events, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.iof, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.iof_residuals, pmix_list_t);
    PMIX_CONSTRUCT(&pmix_server_globals.psets, pmix_list_t);

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:server init called");

    /* setup the server verbosities */
    if (0 < pmix_server_globals.get_verbose) {
        /* set default output */
        pmix_server_globals.get_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.get_output, pmix_server_globals.get_verbose);
    }
    if (0 < pmix_server_globals.connect_verbose) {
        /* set default output */
        pmix_server_globals.connect_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.connect_output,
                                  pmix_server_globals.connect_verbose);
    }
    if (0 < pmix_server_globals.fence_verbose) {
        /* set default output */
        pmix_server_globals.fence_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.fence_output,
                                  pmix_server_globals.fence_verbose);
    }
    if (0 < pmix_server_globals.pub_verbose) {
        /* set default output */
        pmix_server_globals.pub_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.pub_output, \
                                  pmix_server_globals.pub_verbose);
    }
    if (0 < pmix_server_globals.spawn_verbose) {
        /* set default output */
        pmix_server_globals.spawn_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.spawn_output,
                                  pmix_server_globals.spawn_verbose);
    }
    if (0 < pmix_server_globals.event_verbose) {
        /* set default output */
        pmix_server_globals.event_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.event_output,
                                  pmix_server_globals.event_verbose);
    }
    if (0 < pmix_server_globals.iof_verbose) {
        /* set default output */
        pmix_server_globals.iof_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.iof_output,
                                  pmix_server_globals.iof_verbose);
    }
    /* setup the base verbosity */
    if (0 < pmix_server_globals.base_verbose) {
        /* set default output */
        pmix_server_globals.base_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.base_output,
                                  pmix_server_globals.base_verbose);
    }
    if (0 < pmix_server_globals.group_verbose) {
        /* set default output */
        pmix_server_globals.group_output = pmix_output_open(NULL);
        pmix_output_set_verbosity(pmix_server_globals.group_output,
                                  pmix_server_globals.group_verbose);
    }

    /* get our available security modules */
    security_mode = pmix_psec_base_get_available_modules();

    /* get our available bfrop modules */
    bfrops_mode = pmix_bfrops_base_get_available_modules();

    /* get available gds modules */
    gds_mode = pmix_gds_base_get_available_modules();

    return PMIX_SUCCESS;
}

PMIX_EXPORT pmix_status_t PMIx_server_init(pmix_server_module_t *module, pmix_info_t info[],
                                           size_t ninfo)
{
    pmix_ptl_posted_recv_t *req;
    pmix_status_t rc;
    size_t n;
    bool nspace_given = false, rank_given = false;
    bool share_topo = false;
    pmix_info_t ginfo, *iptr, evinfo[3];
    char *evar, *nspace = NULL;
    pmix_rank_t rank = PMIX_RANK_INVALID;
    pmix_rank_info_t *rinfo;
    pmix_proc_type_t ptype = PMIX_PROC_TYPE_STATIC_INIT;
    pmix_buffer_t *bfr;
    pmix_cmd_t cmd;
    pmix_cb_t cb;
    pmix_value_t value;
    pmix_lock_t reglock, releaselock;
    pmix_status_t code;
    pmix_ptl_posted_recv_t *rcv;
    bool outputio;
    char *singleton = NULL;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:server init called");

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

    /* init the parent procid to something innocuous */
    PMIX_LOAD_PROCID(&myparent, NULL, PMIX_RANK_UNDEF);

    if (NULL != getenv("PMIX_LAUNCHER_RNDZ_URI") ||
        NULL != getenv("PMIX_KEEPALIVE_PIPE")) {
        /* we have a parent tool, so default to
         * letting them output IOF */
        outputio = false;
    } else {
        outputio = true;
    }

    PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_SERVER);
    /* setup the function pointers */
    if (NULL != module) {
        pmix_host_server = *module;
    }

    if (NULL != info) {
        for (n = 0; n < ninfo; n++) {
            if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_GATEWAY)) {
                if (PMIX_INFO_TRUE(&info[n])) {
                    PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_GATEWAY);
                }
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_SCHEDULER)) {
                if (PMIX_INFO_TRUE(&info[n])) {
                    PMIX_SET_PROC_TYPE(&ptype, PMIX_PROC_SCHEDULER);
                }
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_TMPDIR)) {
                pmix_server_globals.tmpdir = strdup(info[n].value.data.string);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SYSTEM_TMPDIR)) {
                pmix_server_globals.system_tmpdir = strdup(info[n].value.data.string);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_NSPACE)) {
                nspace = info[n].value.data.string;
                nspace_given = true;
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_RANK)) {
                rank = info[n].value.data.rank;
                rank_given = true;
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SERVER_SHARE_TOPOLOGY)) {
                share_topo = true;
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_IOF_LOCAL_OUTPUT)) {
                outputio = PMIX_INFO_TRUE(&info[n]);
            } else if (PMIX_CHECK_KEY(&info[n], PMIX_SINGLETON)) {
                singleton = info[n].value.data.string;
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

    /* setup the runtime - this init's the globals,
     * opens and initializes the required frameworks */
    if (PMIX_SUCCESS != (rc = pmix_rte_init(ptype.type, info, ninfo, NULL))) {
        PMIX_ERROR_LOG(rc);
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

    /* assign our internal bfrops module */
    pmix_globals.mypeer->nptr->compat.bfrops = pmix_bfrops_base_assign_module(NULL);
    if (NULL == pmix_globals.mypeer->nptr->compat.bfrops) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    /* set the buffer type accordingly if we are given any directives */
    evar = getenv("PMIX_BFROP_BUFFER_TYPE");
    if (NULL == evar) {
        /* just set to our default */
        pmix_globals.mypeer->nptr->compat.type = pmix_bfrops_globals.default_type;
    } else if (0 == strcmp(evar, "PMIX_BFROP_BUFFER_FULLY_DESC")) {
        pmix_globals.mypeer->nptr->compat.type = PMIX_BFROP_BUFFER_FULLY_DESC;
    } else {
        pmix_globals.mypeer->nptr->compat.type = PMIX_BFROP_BUFFER_NON_DESC;
    }

    /* if we were passed directives, use them to guide our selection
     * of security modules */
    evar = getenv("PMIX_SECURITY_MODE");
    pmix_globals.mypeer->nptr->compat.psec = pmix_psec_base_assign_module(evar);
    if (NULL == pmix_globals.mypeer->nptr->compat.psec) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* assign our internal gds module */
    PMIX_INFO_LOAD(&ginfo, PMIX_GDS_MODULE, "hash", PMIX_STRING);
    pmix_globals.mypeer->nptr->compat.gds = pmix_gds_base_assign_module(&ginfo, 1);
    if (NULL == pmix_globals.mypeer->nptr->compat.gds) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    PMIX_INFO_DESTRUCT(&ginfo);

    /* we are our own server until something else happens */
    PMIX_RETAIN(pmix_globals.mypeer);
    pmix_client_globals.myserver = pmix_globals.mypeer;

    /* setup the server-specific globals */
    if (PMIX_SUCCESS != (rc = pmix_server_initialize())) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    /* setup the IO Forwarding recv */
    rcv = PMIX_NEW(pmix_ptl_posted_recv_t);
    rcv->tag = PMIX_PTL_TAG_IOF;
    rcv->cbfunc = server_iof_handler;
    /* add it to the end of the list of recvs */
    pmix_list_append(&pmix_ptl_base.posted_recvs, &rcv->super);
    /* set the default local output flag */
    pmix_globals.iof_flags.local_output = outputio;

    if (nspace_given) {
        PMIX_LOAD_NSPACE(pmix_globals.myid.nspace, nspace);
    } else {
        /* look for our namespace, if one was given */
        if (NULL == (evar = getenv("PMIX_SERVER_NAMESPACE"))) {
            /* use a fake namespace */
            PMIX_LOAD_NSPACE(pmix_globals.myid.nspace, "pmix-server");
        } else {
            PMIX_LOAD_NSPACE(pmix_globals.myid.nspace, evar);
        }
    }
    if (rank_given) {
        pmix_globals.myid.rank = rank;
    } else {
        /* look for our rank, if one was given */
        mypid = getpid();
        if (NULL == (evar = getenv("PMIX_SERVER_RANK"))) {
            /* use our pid */
            pmix_globals.myid.rank = mypid;
        } else {
            pmix_globals.myid.rank = strtol(evar, NULL, 10);
        }
    }

    /* copy it into mypeer entries */
    if (NULL == pmix_globals.mypeer->info) {
        rinfo = PMIX_NEW(pmix_rank_info_t);
        pmix_globals.mypeer->info = rinfo;
    } else {
        PMIX_RETAIN(pmix_globals.mypeer->info);
        rinfo = pmix_globals.mypeer->info;
    }
    if (NULL == pmix_globals.mypeer->nptr) {
        pmix_globals.mypeer->nptr = PMIX_NEW(pmix_namespace_t);
        /* ensure our own nspace is first on the list */
        PMIX_RETAIN(pmix_globals.mypeer->nptr);
        pmix_list_prepend(&pmix_globals.nspaces, &pmix_globals.mypeer->nptr->super);
    }
    pmix_globals.mypeer->nptr->nspace = strdup(pmix_globals.myid.nspace);
    rinfo->pname.nspace = strdup(pmix_globals.mypeer->nptr->nspace);
    rinfo->pname.rank = pmix_globals.myid.rank;
    rinfo->uid = pmix_globals.uid;
    rinfo->gid = pmix_globals.gid;
    pmix_client_globals.myserver->info = pmix_globals.mypeer->info;

    /* open the pmdl framework and select the active modules for this environment */
    rc = pmix_mca_base_framework_open(&pmix_pmdl_base_framework, PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    if (PMIX_SUCCESS != (rc = pmix_pmdl_base_select())) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    /* pass any params read from the default MCA
     * param files thru the pmdl components so they can
     * check for their vars and deal with them */
    pmix_pmdl.parse_file_envars(&pmix_mca_base_var_file_values);
    pmix_pmdl.parse_file_envars(&pmix_mca_base_var_override_values);

    /* open the psensor framework */
    rc = pmix_mca_base_framework_open(&pmix_psensor_base_framework,
                                      PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    if (PMIX_SUCCESS != (rc = pmix_psensor_base_select())) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* if we were started to support a singleton, register it now
     * so we won't reject it when it connects to us */
    if (NULL != singleton) {
        rc = register_singleton(singleton);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            return rc;
        }
    }

    /* setup the wildcard recv for inbound messages from clients */
    req = PMIX_NEW(pmix_ptl_posted_recv_t);
    req->tag = UINT32_MAX;
    req->cbfunc = pmix_server_message_handler;
    /* add it to the end of the list of recvs */
    pmix_list_append(&pmix_ptl_base.posted_recvs, &req->super);

    /* if we are a gateway, setup our IOF events */
    if (PMIX_PEER_IS_GATEWAY(pmix_globals.mypeer)) {
        /* setup IOF */
        PMIX_IOF_SINK_DEFINE(&pmix_client_globals.iof_stdout, &pmix_globals.myid, 1,
                             PMIX_FWD_STDOUT_CHANNEL, pmix_iof_write_handler);
        PMIX_IOF_SINK_DEFINE(&pmix_client_globals.iof_stderr, &pmix_globals.myid, 2,
                             PMIX_FWD_STDERR_CHANNEL, pmix_iof_write_handler);
    }

    /* register our attributes */
    if (PMIX_SUCCESS != (rc = pmix_register_server_attrs())) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* if we don't know our topology, we better get it now as we
     * increasingly rely on it - note that our host will hopefully
     * have passed it to us so we don't duplicate their storage! */
    if (PMIX_SUCCESS != (rc = pmix_hwloc_setup_topology(info, ninfo))) {
        /* if they told us to share our topology and we cannot do so,
         * then that is a reportable error */
        if (share_topo) {
            PMIX_RELEASE_THREAD(&pmix_global_lock);
            return rc;
        }
    }

    /* open the pnet and pgpu frameworks and select their active modules for this
     * environment Do this AFTER setting up the topology so the components can
     * check to see if they have any local assets */
    rc = pmix_mca_base_framework_open(&pmix_pnet_base_framework,
                                      PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    if (PMIX_SUCCESS != (rc = pmix_pnet_base_select())) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    rc = pmix_mca_base_framework_open(&pmix_pgpu_base_framework,
                                      PMIX_MCA_BASE_OPEN_DEFAULT);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }
    if (PMIX_SUCCESS != (rc = pmix_pgpu_base_select())) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return rc;
    }

    /* start listening for connections */
    if (PMIX_SUCCESS != pmix_ptl_base_start_listening(info, ninfo)) {
        pmix_show_help("help-pmix-server.txt", "listener-thread-start", true);
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        PMIx_server_finalize();
        return PMIX_ERR_INIT;
    }

    ++pmix_globals.init_cntr;
    // enable show_help subsystem
    pmix_show_help_enabled = true;
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* register a handler to catch/aggregate PMIX_EVENT_WAITING_FOR_NOTIFY
     * events prior to passing them to our host */
    PMIX_CONSTRUCT_LOCK(&reglock);
    PMIX_INFO_LOAD(&evinfo[0], PMIX_EVENT_HDLR_NAME, "DEBUGGER-AGGREGATOR", PMIX_STRING);
    code = PMIX_DEBUG_WAITING_FOR_NOTIFY;
    PMIx_Register_event_handler(&code, 1, evinfo, 1, debugger_aggregator, evhandler_reg_callbk,
                                (void *) &reglock);
    /* wait for registration to complete */
    PMIX_WAIT_THREAD(&reglock);
    PMIX_INFO_DESTRUCT(&evinfo[0]);
    PMIX_DESTRUCT_LOCK(&reglock);

    /* see if they gave us a rendezvous URI to which we are to call back */
    evar = getenv("PMIX_LAUNCHER_RNDZ_URI");
    if (NULL != evar) {
        /* attach to the specified tool so it can
         * tell us what we are to do */
        PMIX_INFO_CREATE(iptr, 3);
        PMIX_INFO_LOAD(&iptr[0], PMIX_SERVER_URI, evar, PMIX_STRING);
        rc = 2; // give us two seconds to connect
        PMIX_INFO_LOAD(&iptr[1], PMIX_TIMEOUT, &rc, PMIX_INT);
        /* since we are a server and they are a tool, we don't want
         * them to be our primary server to avoid circular logic */
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
        bfr = PMIX_NEW(pmix_buffer_t);
        cmd = PMIX_REQ_CMD;
        PMIX_BFROPS_PACK(rc, pmix_client_globals.myserver, bfr, &cmd, 1, PMIX_COMMAND);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(bfr);
            return rc;
        }
        /* send to the server */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        PMIX_PTL_SEND_RECV(rc, pmix_client_globals.myserver, bfr, job_data, (void *) &cb);
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
        /* restore our original primary server */
        rc = PMIx_tool_set_server(&pmix_globals.myid, NULL, 0);
        if (PMIX_SUCCESS != rc) {
            return rc;
        }
        /* wait for debugger attach here */
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
    }
    return PMIX_SUCCESS;
}

static void checkev(int fd, short args, void *cbdata)
{
    pmix_lock_t *lock = (pmix_lock_t*)cbdata;
    PMIX_HIDE_UNUSED_PARAMS(fd, args, cbdata);

    PMIX_WAKEUP_THREAD(lock);
}

PMIX_EXPORT pmix_status_t PMIx_server_finalize(void)
{
    int i;
    pmix_peer_t *peer;
    pmix_namespace_t *ns;
    pmix_lock_t lock;
    pmix_event_t ev;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }

    if (1 != pmix_globals.init_cntr) {
        --pmix_globals.init_cntr;
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_SUCCESS;
    }
    pmix_globals.init_cntr = 0;

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:server finalize called");

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

    /* flush any residual IOF into their respective channels */
    pmix_iof_flush_residuals();
    /* flush anything that is still trying to be written out */
    pmix_iof_static_dump_output(&pmix_client_globals.iof_stdout);
    pmix_iof_static_dump_output(&pmix_client_globals.iof_stderr);

    pmix_ptl_base_stop_listening();

    for (i = 0; i < pmix_server_globals.clients.size; i++) {
        peer = (pmix_peer_t*)pmix_pointer_array_get_item(&pmix_server_globals.clients, i);
        if (NULL != peer) {
            /* ensure that we do the specified cleanup - if this is an
             * abnormal termination, then the peer object may not be
             * at zero refcount */
            pmix_execute_epilog(&peer->epilog);
            PMIX_RELEASE(peer);
        }
    }
    PMIX_DESTRUCT(&pmix_server_globals.clients);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.collectives);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.remote_pnd);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.local_reqs);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.gdata);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.events);
    PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
        /* ensure that we do the specified cleanup - if this is an
         * abnormal termination, then the nspace object may not be
         * at zero refcount */
        pmix_execute_epilog(&ns->epilog);
    }
    if (NULL != pmix_server_globals.failedgrps) {
        PMIX_ARGV_FREE(pmix_server_globals.failedgrps);
    }
    PMIX_LIST_DESTRUCT(&pmix_server_globals.iof);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.iof_residuals);
    PMIX_LIST_DESTRUCT(&pmix_server_globals.psets);

    if (NULL != security_mode) {
        free(security_mode);
    }

    if (NULL != bfrops_mode) {
        free(bfrops_mode);
    }

    if (NULL != gds_mode) {
        free(gds_mode);
    }

    /* close the psensor framework */
    (void) pmix_mca_base_framework_close(&pmix_psensor_base_framework);
    /* close the pnet framework */
    (void) pmix_mca_base_framework_close(&pmix_pnet_base_framework);
    /* close the pstrg framework */
    (void) pmix_mca_base_framework_close(&pmix_pstrg_base_framework);

    PMIX_RELEASE_THREAD(&pmix_global_lock);
    PMIX_DESTRUCT_LOCK(&pmix_global_lock);

    pmix_rte_finalize();
    if (NULL != pmix_globals.mypeer) {
        PMIX_RELEASE(pmix_globals.mypeer);
    }

    if (NULL != pmix_server_globals.tmpdir) {
        free(pmix_server_globals.tmpdir);
    }

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server finalize complete");

    /* finalize the class/object system */
    pmix_class_finalize();

    return PMIX_SUCCESS;
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    pmix_lock_t *lock = (pmix_lock_t *) cbdata;
    lock->status = status;
    PMIX_WAKEUP_THREAD(lock);
}

static void _register_nspace(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_namespace_t *nptr, *tmp;
    pmix_status_t rc;
    size_t i, m, ninfo;
    pmix_info_t *iptr;
    bool all_def;
    pmix_server_trkr_t *trk;
    pmix_namespace_t *ns;
    pmix_trkr_caddy_t *tcd;
    pmix_gds_base_module_t *gds;
    pmix_kval_t *kv;
    pmix_proc_t proc;

    PMIX_ACQUIRE_OBJECT(caddy);

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:server _register_nspace %s",
                        cd->proc.nspace);

    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH (tmp, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        nptr = PMIX_NEW(pmix_namespace_t);
        if (NULL == nptr) {
            rc = PMIX_ERR_NOMEM;
            goto release;
        }
        nptr->nspace = strdup(cd->proc.nspace);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }
    if (0 > cd->nlocalprocs) {
        gds = nptr->compat.gds;
        if (NULL != gds) {
            /* this is just an update */
            for (i=0; i < cd->ninfo; i++) {
                if (PMIX_CHECK_KEY(&cd->info[i], PMIX_PROC_DATA)) {
                    iptr = (pmix_info_t*)cd->info[i].value.data.darray->array;
                    ninfo = cd->info[i].value.data.darray->size;
                    /* the first position is the rank */
                    PMIX_LOAD_PROCID(&proc, cd->proc.nspace, iptr[0].value.data.rank);
                    /* get the peer object for this rank */
                    for (m=1; m < ninfo; m++) {
                        PMIX_KVAL_NEW(kv, iptr[m].key);
                        PMIX_VALUE_XFER(rc, kv->value, &iptr[m].value);
                        gds->store(&proc, PMIX_REMOTE, kv);
                        PMIX_RELEASE(kv); // maintain refcount
                    }
                } else if (PMIX_CHECK_KEY(&cd->info[i], PMIX_GROUP_CONTEXT_ID)) {
                    PMIX_KVAL_NEW(kv, cd->info[i].key);
                    PMIX_VALUE_XFER(rc, kv->value, &cd->info[i].value);
                    gds->store(&proc, PMIX_GLOBAL, kv);
                    PMIX_RELEASE(kv); // maintain refcount
                }
            }
        }
        rc = PMIX_SUCCESS;
        goto release;
    }
    nptr->nlocalprocs = cd->nlocalprocs;

    /* see if we already have everyone */
    if (nptr->nlocalprocs == pmix_list_get_size(&nptr->ranks)) {
        nptr->all_registered = true;
    }

    /* check info directives to see if we want to store this info */
    for (i = 0; i < cd->ninfo; i++) {
        if (0 == strcmp(cd->info[i].key, PMIX_REGISTER_NODATA)) {
            /* nope - so we are done */
            rc = PMIX_SUCCESS;
            goto release;
        }
    }

    /* register nspace for each activate components */
    PMIX_GDS_ADD_NSPACE(rc, nptr->nspace, cd->nlocalprocs, cd->info, cd->ninfo);
    if (PMIX_SUCCESS != rc) {
        goto release;
    }

    /* store this data in our own GDS module - we will retrieve
     * it later so it can be passed down to the launched procs
     * once they connect to us and we know what GDS module they
     * are using */
    PMIX_GDS_CACHE_JOB_INFO(rc, pmix_globals.mypeer, nptr, cd->info, cd->ninfo);
    if (PMIX_SUCCESS != rc) {
        goto release;
    }

    /* give the programming models a chance to add anything they need */
    rc = pmix_pmdl.register_nspace(nptr);
    if (PMIX_SUCCESS != rc) {
        goto release;
    }

    /* check any pending trackers to see if they are
     * waiting for us. There is a slight race condition whereby
     * the host server could have spawned the local client and
     * it called back into the collective -before- our local event
     * would fire the register_client callback. Deal with that here. */
    all_def = true;
    PMIX_LIST_FOREACH (trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
        /* if this tracker is already complete, then we
         * don't need to update it */
        if (trk->def_complete) {
            continue;
        }
        /* the fact that the tracker is here means that the tracker was
         * created in response to at least one collective call being received
         * from a participant. However, not all local participants may have
         * already called the collective. While the collective created the
         * tracker, it would not have updated the number of local participants
         * from this nspace if they specified PMIX_RANK_WILDCARD in the list of
         * participants since the host hadn't yet called "register_nspace".
         * Take care of that here */
        for (i = 0; i < trk->npcs; i++) {
            /* since we have to do this search, let's see
             * if the nspaces are all completely registered */
            if (all_def) {
                /* so far, they have all been defined - check this one */
                PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
                    if (0 == strcmp(trk->pcs[i].nspace, ns->nspace)) {
                        if (SIZE_MAX == ns->nlocalprocs || !ns->all_registered) {
                            all_def = false;
                        }
                        break;
                    }
                }
            }
            /* now see if this nspace is the one we just registered */
            if (0 != strncmp(trk->pcs[i].nspace, nptr->nspace, PMIX_MAX_NSLEN)) {
                /* if not, then we really can't say anything more about it as
                 * we have no new information about this nspace */
                continue;
            }
            /* if this request was for all participants from this nspace, then
             * we handle this case here */
            if (PMIX_RANK_WILDCARD == trk->pcs[i].rank) {
                trk->nlocal = nptr->nlocalprocs;
                /* the total number of procs in this nspace was provided
                 * in the data blob delivered to register_nspace, so check
                 * to see if all the procs are local */
                if (nptr->nprocs != nptr->nlocalprocs) {
                    trk->local = false;
                }
                continue;
            }
        }
        /* update this tracker's status */
        trk->def_complete = all_def;
        /* is this now locally completed? */
        if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
            /* it did, so now we need to process it
             * we don't want to block someone
             * here, so kick any completed trackers into a
             * new event for processing */
            PMIX_EXECUTE_COLLECTIVE(tcd, trk, pmix_server_execute_collective);
        }
    }
    /* also check any pending local modex requests to see if
     * someone has been waiting for a request on a remote proc
     * in one of our nspaces, but we didn't know all the local procs
     * and so couldn't determine the proc was remote */
    pmix_pending_nspace_requests(nptr);
    rc = PMIX_SUCCESS;

release:
    cd->opcbfunc(rc, cd->cbdata);
    PMIX_RELEASE(cd);
}

/* setup the data for a job */
PMIX_EXPORT pmix_status_t PMIx_server_register_nspace(const pmix_nspace_t nspace, int nlocalprocs,
                                                      pmix_info_t info[], size_t ninfo,
                                                      pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_status_t rc;
    pmix_lock_t mylock;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    pmix_strncpy(cd->proc.nspace, nspace, PMIX_MAX_NSLEN);
    cd->nlocalprocs = nlocalprocs;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;
    /* copy across the info array, if given */
    if (0 < ninfo) {
        cd->ninfo = ninfo;
        cd->info = info;
    }

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _register_nspace);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        PMIX_DESTRUCT_LOCK(&mylock);
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        return rc;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _register_nspace);
    return PMIX_SUCCESS;
}

void pmix_server_purge_events(pmix_peer_t *peer, pmix_proc_t *proc)
{
    pmix_regevents_info_t *reginfo, *regnext;
    pmix_peer_events_info_t *prev, *pnext;
    pmix_iof_req_t *req;
    int i;
    pmix_notify_caddy_t *ncd;
    size_t n, m, p, ntgs;
    pmix_proc_t *tgs, *tgt;
    pmix_dmdx_local_t *dlcd, *dnxt;

    /* since the client is finalizing, remove them from any event
     * registrations they may still have on our list */
    PMIX_LIST_FOREACH_SAFE (reginfo, regnext, &pmix_server_globals.events, pmix_regevents_info_t) {
        PMIX_LIST_FOREACH_SAFE (prev, pnext, &reginfo->peers, pmix_peer_events_info_t) {
            if ((NULL != peer && prev->peer == peer)
                || (NULL != proc && NULL != prev->peer->info
                    && PMIX_CHECK_NAMES(proc, &prev->peer->info->pname))) {
                pmix_list_remove_item(&reginfo->peers, &prev->super);
                PMIX_RELEASE(prev);
                if (0 == pmix_list_get_size(&reginfo->peers)) {
                    pmix_list_remove_item(&pmix_server_globals.events, &reginfo->super);
                    PMIX_RELEASE(reginfo);
                    break;
                }
            }
        }
    }

    /* since the client is finalizing, remove them from any IOF
     * registrations they may still have on our list */
    for (i = 0; i < pmix_globals.iof_requests.size; i++) {
        if (NULL
            == (req = (pmix_iof_req_t *) pmix_pointer_array_get_item(&pmix_globals.iof_requests,
                                                                     i))) {
            continue;
        }
        /* protect against errors */
        if (NULL == req->requestor || NULL == req->requestor->info) {
            pmix_pointer_array_set_item(&pmix_globals.iof_requests, i, NULL);
            PMIX_RELEASE(req);
            continue;
        }
        if (NULL != peer && NULL == peer->info) {
            continue;
        }
        if ((NULL != peer && NULL != peer->info
             && PMIX_CHECK_NAMES(&req->requestor->info->pname, &peer->info->pname))
            || (NULL != proc && PMIX_CHECK_NAMES(&req->requestor->info->pname, proc))) {
            pmix_pointer_array_set_item(&pmix_globals.iof_requests, i, NULL);
            PMIX_RELEASE(req);
        }
    }

    /* see if this proc is involved in any direct modex requests */
    PMIX_LIST_FOREACH_SAFE (dlcd, dnxt, &pmix_server_globals.local_reqs, pmix_dmdx_local_t) {
        if ((NULL != peer && NULL != peer->info
             && PMIX_CHECK_NAMES(&peer->info->pname, &dlcd->proc))
            || (NULL != proc && PMIX_CHECK_PROCID(proc, &dlcd->proc))) {
            /* cleanup this request */
            pmix_list_remove_item(&pmix_server_globals.local_reqs, &dlcd->super);
            /* we can release the dlcd item here because we are not
             * releasing the tracker held by the host - we are only
             * releasing one item on that tracker */
            PMIX_RELEASE(dlcd);
        }
    }

    /* purge this client from any cached notifications */
    for (i = 0; i < pmix_globals.max_events; i++) {
        pmix_hotel_knock(&pmix_globals.notifications, i, (void **) &ncd);
        if (NULL != ncd && NULL != ncd->targets && 0 < ncd->ntargets) {
            tgt = NULL;
            for (n = 0; n < ncd->ntargets; n++) {
                if ((NULL != peer && NULL != peer->info
                     && PMIX_CHECK_NAMES(&peer->info->pname, &ncd->targets[n]))
                    || (NULL != proc && PMIX_CHECK_PROCID(proc, &ncd->targets[n]))) {
                    tgt = &ncd->targets[n];
                    break;
                }
            }
            if (NULL != tgt) {
                /* if this client was the only target, then just
                 * evict the notification */
                if (1 == ncd->ntargets) {
                    pmix_hotel_checkout(&pmix_globals.notifications, i);
                    PMIX_RELEASE(ncd);
                } else if (PMIX_RANK_WILDCARD == tgt->rank && NULL != proc
                           && PMIX_RANK_WILDCARD == proc->rank) {
                    /* we have to remove this target, but leave the rest */
                    ntgs = ncd->ntargets - 1;
                    PMIX_PROC_CREATE(tgs, ntgs);
                    p = 0;
                    for (m = 0; m < ncd->ntargets; m++) {
                        if (tgt != &ncd->targets[m]) {
                            memcpy(&tgs[p], &ncd->targets[n], sizeof(pmix_proc_t));
                            ++p;
                        }
                    }
                    PMIX_PROC_FREE(ncd->targets, ncd->ntargets);
                    ncd->targets = tgs;
                    ncd->ntargets = ntgs;
                }
            }
        }
    }

    if (NULL != peer) {
        /* ensure we honor any peer-level epilog requests */
        pmix_execute_epilog(&peer->epilog);
    }
}

static void remove_client(pmix_namespace_t *nptr, pmix_proc_t *p)
{
    pmix_rank_info_t *info, *inext;
    pmix_peer_t *peer;
    pmix_proc_t proc;

    PMIX_LIST_FOREACH_SAFE(info, inext, &nptr->ranks, pmix_rank_info_t) {
        if (NULL == p || info->pname.rank == p->rank) {
            if (NULL == p) {
                PMIX_LOAD_PROCID(&proc, info->pname.nspace, info->pname.rank);
            } else {
                memcpy(&proc, p, sizeof(pmix_proc_t));
            }
            /* if this client failed to call finalize, we still need
             * to restore any allocations that were given to it */
            peer = (pmix_peer_t *) pmix_pointer_array_get_item(&pmix_server_globals.clients, info->peerid);
            if (NULL == peer) {
                /* this peer never connected, and hence it won't finalize,
                 * so account for it here */
                nptr->nfinalized++;
                /* even if they never connected, resources were allocated
                 * to them, so we need to ensure they are properly released */
                pmix_pnet.child_finalized(&proc);
            } else {
                if (!peer->finalized) {
                    /* this peer connected to us, but is being deregistered
                     * without having finalized. This usually means an
                     * abnormal termination that was picked up by
                     * our host prior to our seeing the connection drop.
                     * It is also possible that we missed the dropped
                     * connection, so mark the peer as finalized so
                     * we don't duplicate account for it and take care
                     * of it here */
                    peer->finalized = true;
                    nptr->nfinalized++;
                }
                /* resources may have been allocated to them, so
                 * ensure they get cleaned up - this isn't true
                 * for tools, so don't clean them up */
                if (!PMIX_PEER_IS_TOOL(peer)) {
                    pmix_pnet.child_finalized(&proc);
                    pmix_psensor.stop(peer, NULL);
                }
                /* honor any registered epilogs */
                pmix_execute_epilog(&peer->epilog);
                /* ensure we close the socket to this peer so we don't
                 * generate "connection lost" events should it be
                 * subsequently "killed" by the host */
                CLOSE_THE_SOCKET(peer->sd);
                // remove it from our client array
                pmix_pointer_array_set_item(&pmix_server_globals.clients, info->peerid, NULL);
                PMIX_RELEASE(peer);
            }
            if (nptr->nlocalprocs == nptr->nfinalized) {
                pmix_pnet.local_app_finalized(nptr);
            }
            pmix_list_remove_item(&nptr->ranks, &info->super);
            PMIX_RELEASE(info);
            if (NULL != p) {
                break;
            }
        }
    }
    return;
}

static void _deregister_nspace(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_namespace_t *tmp, *nptr;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(cd);

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server _deregister_nspace %s",
                        cd->proc.nspace);

    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* flush anything that is still trying to be written out */
    pmix_iof_static_dump_output(&pmix_client_globals.iof_stdout);
    pmix_iof_static_dump_output(&pmix_client_globals.iof_stderr);

    /* release any job-level network resources */
    pmix_pnet.deregister_nspace(cd->proc.nspace);

    /* release any programming model info */
    pmix_pmdl.deregister_nspace(cd->proc.nspace);

    /* let our local storage clean up */
    PMIX_GDS_DEL_NSPACE(rc, cd->proc.nspace);

    /* remove any event registrations, IOF registrations, and
     * cached notifications targeting procs from this nspace */
    pmix_server_purge_events(NULL, &cd->proc);

    // find the nspace object
    nptr = NULL;
    PMIX_LIST_FOREACH (tmp, &pmix_globals.nspaces, pmix_namespace_t) {
        if (PMIX_CHECK_NSPACE(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        /* nothing to do */
        goto cleanup;
    }

    // ensure all local clients have been deregistered
    remove_client(nptr, NULL);

    /* perform any epilog */
    pmix_execute_epilog(&nptr->epilog);

    /* remove and release it */
    pmix_list_remove_item(&pmix_globals.nspaces, &nptr->super);
    PMIX_RELEASE(nptr);

cleanup:
    /* release the caller */
    cd->opcbfunc(rc, cd->cbdata);
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_server_deregister_nspace(const pmix_nspace_t nspace, pmix_op_cbfunc_t cbfunc,
                                               void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_lock_t mylock;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server deregister nspace %s",
                        nspace);

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        if (NULL != cbfunc) {
            cbfunc(PMIX_ERR_INIT, cbdata);
        }
        return;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    PMIX_LOAD_PROCID(&cd->proc, nspace, PMIX_RANK_WILDCARD);
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _deregister_nspace);
        PMIX_WAIT_THREAD(&mylock);
        PMIX_DESTRUCT_LOCK(&mylock);
        return;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_nspace);
}

static void _register_resources(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_kval_t *kv;
    size_t n;
    pmix_status_t rc = PMIX_SUCCESS;

    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* add any provided data to our global cache for all nspaces */
    for (n = 0; n < cd->ninfo; n++) {
        kv = PMIX_NEW(pmix_kval_t);
        kv->key = strdup(cd->info[n].key);
        kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
        PMIX_VALUE_XFER(rc, kv->value, &cd->info[n].value);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(kv);
            break;
        }
        pmix_list_append(&pmix_server_globals.gdata, &kv->super);
    }

    cd->opcbfunc(rc, cd->cbdata);
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_register_resources(pmix_info_t info[], size_t ninfo,
                                             pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_lock_t mylock;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server register resources");

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    cd->info = info;
    cd->ninfo = ninfo;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _register_resources);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        PMIX_DESTRUCT_LOCK(&mylock);
        return rc;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _register_resources);
    return PMIX_SUCCESS;
}

static void _deregister_resources(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_kval_t *kv;
    size_t n;

    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* find any matches in our global cache and remove them */
    for (n = 0; n < cd->ninfo; n++) {
        PMIX_LIST_FOREACH (kv, &pmix_server_globals.gdata, pmix_kval_t) {
            if (PMIX_CHECK_KEY(kv, cd->info[n].key)) {
                pmix_list_remove_item(&pmix_server_globals.gdata, &kv->super);
                PMIX_RELEASE(kv);
                break;
            }
        }
    }

    cd->opcbfunc(PMIX_SUCCESS, cd->cbdata);
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_deregister_resources(pmix_info_t info[], size_t ninfo,
                                               pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_lock_t mylock;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server deregister resources");

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    cd->info = info;
    cd->ninfo = ninfo;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _deregister_resources);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        PMIX_DESTRUCT_LOCK(&mylock);
        return rc;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_resources);
    return PMIX_SUCCESS;
}

static void myinfocbfunc(pmix_status_t status,
                         pmix_info_t *info, size_t ninfo,
                         void *cbdata,
                         pmix_release_cbfunc_t release_fn,
                         void *release_cbdata)
{
    pmix_lock_t *lock = (pmix_lock_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(info, ninfo, release_fn, release_cbdata);

    lock->status = status;
    PMIX_WAKEUP_THREAD(lock);
}

static void _session_control(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;

    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    cd->cbfunc.infocbfunc(PMIX_ERR_NOT_SUPPORTED, NULL, 0, cd->cbdata, NULL, NULL);
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_Session_control(uint32_t sessionID,
                                   const pmix_info_t directives[], size_t ndirs,
                                   pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_shift_caddy_t *cd;
    pmix_lock_t mylock;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server session control");

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->sessionid = sessionID;
    cd->directives = (pmix_info_t*)directives;
    cd->ndirs = ndirs;
    cd->cbfunc.infocbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->cbfunc.infocbfunc = myinfocbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _session_control);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        PMIX_DESTRUCT_LOCK(&mylock);
        return rc;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _session_control);
    return PMIX_SUCCESS;
}

void pmix_server_execute_collective(int sd, short args, void *cbdata)
{
    pmix_trkr_caddy_t *tcd = (pmix_trkr_caddy_t *) cbdata;
    pmix_server_trkr_t *trk = tcd->trk;
    pmix_server_caddy_t *cd;
    pmix_peer_t *peer;
    char *data = NULL;
    size_t sz = 0;
    pmix_byte_object_t bo;
    pmix_buffer_t bucket, pbkt;
    pmix_kval_t *kv;
    pmix_proc_t proc;
    bool first;
    pmix_status_t rc;
    pmix_list_t pnames;
    pmix_namelist_t *pn;
    bool found;
    pmix_cb_t cb;

    PMIX_ACQUIRE_OBJECT(tcd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* we don't need to check for non-NULL APIs here as
     * that was already done when the tracker was created */
    if (PMIX_FENCENB_CMD == trk->type) {
        /* if the user asked us to collect data, then we have
         * to provide any locally collected data to the host
         * server so they can circulate it - only take data
         * from the specified procs as not everyone is necessarily
         * participating! And only take data intended for remote
         * distribution as local data will be added when we send
         * the result to our local clients */
        if (trk->hybrid) {
            /* if this is a hybrid, then we pack everything using
             * the daemon-level bfrops module as each daemon is
             * going to have to unpack it, and then repack it for
             * each participant. */
            peer = pmix_globals.mypeer;
        } else {
            /* in some error situations, the list of local callbacks can
             * be empty - if that happens, we just need to call the fence
             * function to prevent others from hanging */
            if (0 == pmix_list_get_size(&trk->local_cbs)) {
                pmix_host_server.fence_nb(trk->pcs, trk->npcs, trk->info, trk->ninfo, data, sz,
                                          trk->modexcbfunc, trk);
                PMIX_RELEASE(tcd);
                return;
            }
            /* since all procs are the same, just use the first proc's module */
            cd = (pmix_server_caddy_t *) pmix_list_get_first(&trk->local_cbs);
            peer = cd->peer;
        }
        PMIX_CONSTRUCT(&bucket, pmix_buffer_t);

        unsigned char tmp = (unsigned char) trk->collect_type;
        PMIX_BFROPS_PACK(rc, peer, &bucket, &tmp, 1, PMIX_BYTE);

        if (PMIX_COLLECT_YES == trk->collect_type) {
            pmix_output_verbose(2, pmix_server_globals.base_output, "fence - assembling data");
            first = true;
            PMIX_CONSTRUCT(&pnames, pmix_list_t);
            PMIX_LIST_FOREACH (cd, &trk->local_cbs, pmix_server_caddy_t) {
                /* see if we have already gotten the contribution from
                 * this proc */
                found = false;
                PMIX_LIST_FOREACH (pn, &pnames, pmix_namelist_t) {
                    if (pn->pname == &cd->peer->info->pname) {
                        /* got it */
                        found = true;
                        break;
                    }
                }
                if (found) {
                    continue;
                } else {
                    pn = PMIX_NEW(pmix_namelist_t);
                    pn->pname = &cd->peer->info->pname;
                }
                if (trk->hybrid || first) {
                    /* setup the nspace */
                    pmix_strncpy(proc.nspace, cd->peer->info->pname.nspace, PMIX_MAX_NSLEN);
                    first = false;
                }
                proc.rank = cd->peer->info->pname.rank;
                /* get any remote contribution - note that there
                 * may not be a contribution */
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                cb.proc = &proc;
                cb.scope = PMIX_REMOTE;
                cb.copy = true;
                PMIX_GDS_FETCH_KV(rc, peer, &cb);
                if (PMIX_SUCCESS == rc) {
                    /* pack the returned kvals */
                    PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                    /* start with the proc id */
                    PMIX_BFROPS_PACK(rc, peer, &pbkt, &proc, 1, PMIX_PROC);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_DESTRUCT(&bucket);
                        return;
                    }
                    PMIX_LIST_FOREACH (kv, &cb.kvs, pmix_kval_t) {
                        PMIX_BFROPS_PACK(rc, peer, &pbkt, kv, 1, PMIX_KVAL);
                        if (PMIX_SUCCESS != rc) {
                            PMIX_ERROR_LOG(rc);
                            PMIX_DESTRUCT(&cb);
                            PMIX_DESTRUCT(&pbkt);
                            PMIX_DESTRUCT(&bucket);
                            return;
                        }
                    }
                    /* extract the resulting byte object */
                    PMIX_UNLOAD_BUFFER(&pbkt, bo.bytes, bo.size);
                    PMIX_DESTRUCT(&pbkt);
                    /* now pack that into the bucket for return */
                    PMIX_BFROPS_PACK(rc, peer, &bucket, &bo, 1, PMIX_BYTE_OBJECT);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_DESTRUCT(&cb);
                        PMIX_BYTE_OBJECT_DESTRUCT(&bo);
                        PMIX_DESTRUCT(&bucket);
                        PMIX_RELEASE(tcd);
                        return;
                    }
                }
                PMIX_DESTRUCT(&cb);
            }
            PMIX_LIST_DESTRUCT(&pnames);
        }
        PMIX_UNLOAD_BUFFER(&bucket, data, sz);
        PMIX_DESTRUCT(&bucket);
        pmix_host_server.fence_nb(trk->pcs, trk->npcs, trk->info, trk->ninfo, data, sz,
                                  trk->modexcbfunc, trk);
    } else if (PMIX_CONNECTNB_CMD == trk->type) {
        pmix_host_server.connect(trk->pcs, trk->npcs, trk->info, trk->ninfo, trk->op_cbfunc, trk);
    } else if (PMIX_DISCONNECTNB_CMD == trk->type) {
        pmix_host_server.disconnect(trk->pcs, trk->npcs, trk->info, trk->ninfo, trk->op_cbfunc,
                                    trk);
    } else {
        /* unknown type */
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        pmix_list_remove_item(&pmix_server_globals.collectives, &trk->super);
        PMIX_RELEASE(trk);
    }
    PMIX_RELEASE(tcd);
}

static void _register_client(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_rank_info_t *info;
    pmix_namespace_t *nptr, *ns;
    pmix_server_trkr_t *trk;
    pmix_trkr_caddy_t *tcd;
    bool all_def;
    size_t i;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server _register_client for nspace %s rank %d %s object",
                        cd->proc.nspace, cd->proc.rank,
                        (NULL == cd->server_object) ? "NULL" : "NON-NULL");

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(ns->nspace, cd->proc.nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* there is no requirement in the Standard that hosts register
         * an nspace prior to registering clients for that nspace. So
         * if we didn't find it, just add it to our collection now in
         * anticipation of eventually getting a "register_nspace" call */
        nptr = PMIX_NEW(pmix_namespace_t);
        if (NULL == nptr) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        nptr->nspace = strdup(cd->proc.nspace);
        pmix_list_append(&pmix_globals.nspaces, &nptr->super);
    }
    /* setup a peer object for this client - since the host server
     * only deals with the original processes and not any clones,
     * we know this function will be called only once per rank */
    info = PMIX_NEW(pmix_rank_info_t);
    if (NULL == info) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    info->pname.nspace = strdup(nptr->nspace);
    info->pname.rank = cd->proc.rank;
    info->uid = cd->uid;
    info->gid = cd->gid;
    info->server_object = cd->server_object;
    pmix_list_append(&nptr->ranks, &info->super);
    /* see if we have everyone - note that nlocalprocs is set to
     * a default value to ensure we don't execute this
     * test until the host calls "register_nspace" */
    if (SIZE_MAX != nptr->nlocalprocs && nptr->nlocalprocs == pmix_list_get_size(&nptr->ranks)) {
        nptr->all_registered = true;
        /* check any pending trackers to see if they are
         * waiting for us. There is a slight race condition whereby
         * the host server could have spawned the local client and
         * it called back into the collective -before- our local event
         * would fire the register_client callback. Deal with that here. */
        all_def = true;
        PMIX_LIST_FOREACH (trk, &pmix_server_globals.collectives, pmix_server_trkr_t) {
            /* if this tracker is already complete, then we
             * don't need to update it */
            if (trk->def_complete) {
                continue;
            }
            /* the fact that the tracker is here means that the tracker was
             * created in response to at least one collective call being received
             * from a participant. However, not all local participants may have
             * already called the collective. While the collective created the
             * tracker, it would not have updated the number of local participants
             * from this nspace UNLESS the collective involves all procs in the
             * nspace (i.e., they specified PMIX_RANK_WILDCARD in the list of
             * participants) AND the host already provided the number of local
             * procs for this nspace by calling "register_nspace". So avoid that
             * scenario here to avoid double-counting */
            for (i = 0; i < trk->npcs; i++) {
                /* since we have to do this search, let's see
                 * if the nspaces are all completely registered */
                if (all_def) {
                    /* so far, they have all been defined - check this one */
                    PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
                        if (0 == strcmp(trk->pcs[i].nspace, ns->nspace)) {
                            if (SIZE_MAX == ns->nlocalprocs || !ns->all_registered) {
                                all_def = false;
                            }
                            break;
                        }
                    }
                }
                /* now see if this nspace is the one to which the client we just
                 * registered belongs */
                if (0 != strncmp(trk->pcs[i].nspace, nptr->nspace, PMIX_MAX_NSLEN)) {
                    /* if not, then we really can't say anything more about it as
                     * we have no new information about this nspace */
                    continue;
                }
                /* if this request was for all participants from this nspace, then
                 * we handle this case elsewhere */
                if (PMIX_RANK_WILDCARD == trk->pcs[i].rank) {
                    continue;
                }
                /* see if the rank we just registered is a participant */
                if (cd->proc.rank == trk->pcs[i].rank) {
                    /* yes, we are included */
                    ++trk->nlocal;
                }
            }
            /* update this tracker's status */
            trk->def_complete = all_def;
            /* is this now locally completed? */
            if (trk->def_complete && pmix_list_get_size(&trk->local_cbs) == trk->nlocal) {
                /* it did, so now we need to process it
                 * we don't want to block someone
                 * here, so kick any completed trackers into a
                 * new event for processing */
                PMIX_EXECUTE_COLLECTIVE(tcd, trk, pmix_server_execute_collective);
            }
        }
        /* also check any pending local modex requests to see if
         * someone has been waiting for a request on a remote proc
         * in one of our nspaces, but we didn't know all the local procs
         * and so couldn't determine the proc was remote */
        pmix_pending_nspace_requests(nptr);
    }
    rc = PMIX_SUCCESS;

cleanup:
    /* let the caller know we are done */
    cd->opcbfunc(rc, cd->cbdata);
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_server_register_client(const pmix_proc_t *proc, uid_t uid, gid_t gid,
                                                      void *server_object, pmix_op_cbfunc_t cbfunc,
                                                      void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_status_t rc;
    pmix_lock_t mylock;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:server register client %s:%d",
                        proc->nspace, proc->rank);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    pmix_strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->uid = uid;
    cd->gid = gid;
    cd->server_object = server_object;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _register_client);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        PMIX_DESTRUCT_LOCK(&mylock);
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        return rc;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _register_client);
    return PMIX_SUCCESS;
}

static void _deregister_client(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_namespace_t *nptr, *tmp;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server _deregister_client for nspace %s rank %d", cd->proc.nspace,
                        cd->proc.rank);

    /* see if we already have this nspace */
    nptr = NULL;
    PMIX_LIST_FOREACH (tmp, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(tmp->nspace, cd->proc.nspace)) {
            nptr = tmp;
            break;
        }
    }
    if (NULL == nptr) {
        /* nothing to do */
        goto cleanup;
    }

    /* find and remove this client */
    remove_client(nptr, &cd->proc);

cleanup:
    cd->opcbfunc(PMIX_SUCCESS, cd->cbdata);
    PMIX_RELEASE(cd);
}

PMIX_EXPORT void PMIx_server_deregister_client(const pmix_proc_t *proc, pmix_op_cbfunc_t cbfunc,
                                               void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_lock_t mylock;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        if (NULL != cbfunc) {
            cbfunc(PMIX_ERR_INIT, cbdata);
        }
        return;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:server deregister client %s:%d",
                        proc->nspace, proc->rank);

    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        if (NULL != cbfunc) {
            cbfunc(PMIX_ERR_NOMEM, cbdata);
        }
        return;
    }
    pmix_strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _deregister_client);
        PMIX_WAIT_THREAD(&mylock);
        PMIX_DESTRUCT_LOCK(&mylock);
        return;
    }

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _deregister_client);
}

/* setup the envars for a child process */
PMIX_EXPORT pmix_status_t PMIx_server_setup_fork(const pmix_proc_t *proc, char ***env)
{
    char rankstr[128];
    pmix_listener_t *lt;
    pmix_status_t rc;
    char **varnames;
    int n;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:server setup_fork for nspace %s rank %u",
                        proc->nspace, proc->rank);

    /* pass the nspace */
    PMIx_Setenv("PMIX_NAMESPACE", proc->nspace, true, env);
    /* pass the rank */
    (void) pmix_snprintf(rankstr, 127, "%u", proc->rank);
    PMIx_Setenv("PMIX_RANK", rankstr, true, env);
    /* pass our rendezvous info */
    lt = &pmix_ptl_base.listener;
    if (NULL != lt->uri && NULL != lt->varname) {
        varnames = PMIx_Argv_split(lt->varname, ':');
        for (n = 0; NULL != varnames[n]; n++) {
            PMIx_Setenv(varnames[n], lt->uri, true, env);
        }
        PMIx_Argv_free(varnames);
    }

    /* pass our active security modules */
    PMIx_Setenv("PMIX_SECURITY_MODE", security_mode, true, env);
    /* pass the type of buffer we are using */
    if (PMIX_BFROP_BUFFER_FULLY_DESC == pmix_globals.mypeer->nptr->compat.type) {
        PMIx_Setenv("PMIX_BFROP_BUFFER_TYPE", "PMIX_BFROP_BUFFER_FULLY_DESC", true, env);
    } else {
        PMIx_Setenv("PMIX_BFROP_BUFFER_TYPE", "PMIX_BFROP_BUFFER_NON_DESC", true, env);
    }
    /* pass our available gds modules */
    PMIx_Setenv("PMIX_GDS_MODULE", gds_mode, true, env);

    /* get any PTL contribution such as tmpdir settings for session files */
    if (PMIX_SUCCESS != (rc = pmix_ptl_base_setup_fork(proc, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* get any network contribution */
    if (PMIX_SUCCESS != (rc = pmix_pnet.setup_fork(proc, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* get any GDS contributions */
    if (PMIX_SUCCESS != (rc = pmix_gds_base_setup_fork(proc, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* get any contribution for the specific programming
     * model/implementation, if known */
    if (PMIX_SUCCESS != (rc = pmix_pmdl.setup_fork(proc, env))) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    /* ensure we agree on our hostname */
    PMIx_Setenv("PMIX_HOSTNAME", pmix_globals.hostname, true, env);

    /* communicate our version */
    PMIx_Setenv("PMIX_VERSION", PMIX_VERSION, true, env);

    /* pass any global contributions */
    if (NULL != pmix_server_globals.genvars) {
        for (n = 0; NULL != pmix_server_globals.genvars[n]; n++) {
            PMIx_Argv_append_nosize(env, pmix_server_globals.genvars[n]);
        }
    }

    return PMIX_SUCCESS;
}

/***************************************************************************************************
 *  Support calls from the host server down to us requesting direct modex data provided by one     *
 *  of our local clients                                                                           *
 ***************************************************************************************************/

static void _dmodex_req(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_rank_info_t *info, *iptr;
    pmix_namespace_t *nptr, *ns;
    char *data = NULL;
    size_t sz = 0;
    pmix_dmdx_remote_t *dcd;
    pmix_status_t rc;
    pmix_buffer_t pbkt;
    pmix_kval_t *kv;
    pmix_cb_t cb;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_server_globals.base_output, "DMODX LOOKING FOR %s",
                        PMIX_NAME_PRINT(&cd->proc));

    /* this should be one of my clients, but a race condition
     * could cause this request to arrive prior to us having
     * been informed of it - so first check to see if we know
     * about this nspace yet */
    nptr = NULL;
    PMIX_LIST_FOREACH (ns, &pmix_globals.nspaces, pmix_namespace_t) {
        if (0 == strcmp(ns->nspace, cd->proc.nspace)) {
            nptr = ns;
            break;
        }
    }
    if (NULL == nptr) {
        /* we don't know this namespace yet, and so we obviously
         * haven't received the data from this proc yet - defer
         * the request until we do */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        if (NULL == dcd) {
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        return;
    }

    /* They are asking for job level data for this process */
    if (PMIX_RANK_WILDCARD == cd->proc.rank) {
        /* fetch the job-level info for this nspace */
        /* this is going to a remote peer, so inform the gds
         * that we need an actual copy of the data */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &cd->proc;
        cb.scope = PMIX_REMOTE;
        cb.copy = true;
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        if (PMIX_SUCCESS == rc) {
            /* assemble the provided data into a byte object */
            PMIX_LIST_FOREACH (kv, &cb.kvs, pmix_kval_t) {
                PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, kv, 1, PMIX_KVAL);
                if (PMIX_SUCCESS != rc) {
                    PMIX_DESTRUCT(&pbkt);
                    PMIX_DESTRUCT(&cb);
                    goto cleanup;
                }
            }
        }
        PMIX_DESTRUCT(&cb);
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
        goto cleanup;
    }

    /* see if we have this peer in our list */
    info = NULL;
    PMIX_LIST_FOREACH (iptr, &nptr->ranks, pmix_rank_info_t) {
        if (iptr->pname.rank == cd->proc.rank) {
            info = iptr;
            break;
        }
    }
    if (NULL == info) {
        /* rank isn't known yet - defer
         * the request until we do */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        return;
    }

    /* have we received the modex from this proc yet - if
     * not, then defer */
    if (!info->modex_recvd) {
        /* track the request so we can fulfill it once
         * data is recvd */
        dcd = PMIX_NEW(pmix_dmdx_remote_t);
        dcd->cd = cd;
        pmix_list_append(&pmix_server_globals.remote_pnd, &dcd->super);
        return;
    }

    /* collect the remote/global data from this proc */
    PMIX_CONSTRUCT(&cb, pmix_cb_t);
    cb.proc = &cd->proc;
    cb.scope = PMIX_REMOTE;
    cb.copy = true;
    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
    if (PMIX_SUCCESS == rc) {
        /* assemble the provided data into a byte object */
        PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
        PMIX_LIST_FOREACH (kv, &cb.kvs, pmix_kval_t) {
            PMIX_BFROPS_PACK(rc, pmix_globals.mypeer, &pbkt, kv, 1, PMIX_KVAL);
            if (PMIX_SUCCESS != rc) {
                PMIX_DESTRUCT(&pbkt);
                PMIX_DESTRUCT(&cb);
                goto cleanup;
            }
        }
        PMIX_UNLOAD_BUFFER(&pbkt, data, sz);
        PMIX_DESTRUCT(&pbkt);
    }
    PMIX_DESTRUCT(&cb);

cleanup:
    /* execute the callback */
    cd->cbfunc(rc, data, sz, cd->cbdata);
    if (NULL != data) {
        free(data);
    }
    PMIX_RELEASE(cd);
}

PMIX_EXPORT pmix_status_t PMIx_server_dmodex_request(const pmix_proc_t *proc,
                                                     pmix_dmodex_response_fn_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* protect against bozo */
    if (NULL == cbfunc || NULL == proc) {
        return PMIX_ERR_BAD_PARAM;
    }

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "%s pmix:server dmodex request for proc %s",
                        PMIX_NAME_PRINT(&pmix_globals.myid), PMIX_NAME_PRINT(proc));

    cd = PMIX_NEW(pmix_setup_caddy_t);
    pmix_strncpy(cd->proc.nspace, proc->nspace, PMIX_MAX_NSLEN);
    cd->proc.rank = proc->rank;
    cd->cbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* we have to push this into our event library to avoid
     * potential threading issues */
    PMIX_THREADSHIFT(cd, _dmodex_req);
    return PMIX_SUCCESS;
}

static void _store_internal(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;
    pmix_proc_t proc;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_strncpy(proc.nspace, cd->pname.nspace, PMIX_MAX_NSLEN);
    proc.rank = cd->pname.rank;
    PMIX_GDS_STORE_KV(cd->status, pmix_globals.mypeer, &proc, PMIX_INTERNAL, cd->kv);
    if (cd->lock.active) {
        PMIX_WAKEUP_THREAD(&cd->lock);
    }
}

PMIX_EXPORT pmix_status_t PMIx_Store_internal(const pmix_proc_t *proc, const char key[],
                                              pmix_value_t *val)
{
    pmix_shift_caddy_t *cd;
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    if (NULL == key || PMIX_MAX_KEYLEN < pmix_keylen(key)) {
        return PMIX_ERR_BAD_PARAM;
    }

    /* setup to thread shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->pname.nspace = strdup(proc->nspace);
    cd->pname.rank = proc->rank;

    cd->kv = PMIX_NEW(pmix_kval_t);
    if (NULL == cd->kv) {
        PMIX_RELEASE(cd);
        return PMIX_ERR_NOMEM;
    }
    cd->kv->key = strdup(key);
    cd->kv->value = (pmix_value_t *) malloc(sizeof(pmix_value_t));
    PMIX_BFROPS_VALUE_XFER(rc, pmix_globals.mypeer, cd->kv->value, val);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(cd);
        return rc;
    }

    PMIX_THREADSHIFT(cd, _store_internal);
    PMIX_WAIT_THREAD(&cd->lock);
    rc = cd->status;
    PMIX_RELEASE(cd);

    return rc;
}

PMIX_EXPORT pmix_status_t PMIx_generate_regex(const char *input, char **regexp)
{
    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    return pmix_preg.generate_node_regex(input, regexp);
}

PMIX_EXPORT pmix_status_t PMIx_generate_ppn(const char *input, char **regexp)
{
    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    return pmix_preg.generate_ppn(input, regexp);
}

static void _setup_op(pmix_status_t rc, void *cbdata)
{
    pmix_setup_caddy_t *fcd = (pmix_setup_caddy_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(rc);

    if (NULL != fcd->info) {
        PMIX_INFO_FREE(fcd->info, fcd->ninfo);
    }
    PMIX_RELEASE(fcd);
}

static void _setup_app(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_setup_caddy_t *fcd = NULL;
    pmix_status_t rc;
    pmix_list_t ilist;
    pmix_kval_t *kv;
    size_t n;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_CONSTRUCT(&ilist, pmix_list_t);

    /* pass to the network libraries */
    if (PMIX_SUCCESS != (rc = pmix_pnet.allocate(cd->nspace, cd->info, cd->ninfo, &ilist))) {
        goto depart;
    }

    /* pass to the programming model libraries */
    if (PMIX_SUCCESS != (rc = pmix_pmdl.harvest_envars(cd->nspace, cd->info, cd->ninfo, &ilist))) {
        goto depart;
    }

    /* setup the return callback */
    fcd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == fcd) {
        rc = PMIX_ERR_NOMEM;
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        goto depart;
    }

    /* if anything came back, construct an info array */
    if (0 < (fcd->ninfo = pmix_list_get_size(&ilist))) {
        PMIX_INFO_CREATE(fcd->info, fcd->ninfo);
        if (NULL == fcd->info) {
            rc = PMIX_ERR_NOMEM;
            PMIX_RELEASE(fcd);
            goto depart;
        }
        n = 0;
        PMIX_LIST_FOREACH (kv, &ilist, pmix_kval_t) {
            pmix_strncpy(fcd->info[n].key, kv->key, PMIX_MAX_KEYLEN);
            PMIx_Value_xfer(&fcd->info[n].value, kv->value);
            ++n;
        }
    }

depart:
    /* always execute the callback to avoid hanging */
    if (NULL != cd->setupcbfunc) {
        if (NULL == fcd) {
            cd->setupcbfunc(rc, NULL, 0, cd->cbdata, NULL, NULL);
        } else {
            cd->setupcbfunc(rc, fcd->info, fcd->ninfo, cd->cbdata, _setup_op, fcd);
        }
    }

    /* cleanup memory */
    PMIX_LIST_DESTRUCT(&ilist);
    if (NULL != cd->nspace) {
        free(cd->nspace);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_setup_application(const pmix_nspace_t nspace, pmix_info_t info[],
                                            size_t ninfo, pmix_setup_application_cbfunc_t cbfunc,
                                            void *cbdata)
{
    pmix_setup_caddy_t *cd;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != nspace) {
        cd->nspace = strdup(nspace);
    }
    cd->info = info;
    cd->ninfo = ninfo;
    cd->setupcbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, _setup_app);

    return PMIX_SUCCESS;
}

static void _setup_local_support(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* pass to the network libraries */
    rc = pmix_pnet.setup_local_network(cd->nspace, cd->info, cd->ninfo);

    /* pass the info back */
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }
    /* cleanup memory */
    if (NULL != cd->nspace) {
        free(cd->nspace);
    }
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_setup_local_support(const pmix_nspace_t nspace, pmix_info_t info[],
                                              size_t ninfo, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_status_t rc;
    pmix_lock_t mylock;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    if (NULL != nspace) {
        cd->nspace = strdup(nspace);
    }
    cd->info = info;
    cd->ninfo = ninfo;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _setup_local_support);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        PMIX_DESTRUCT_LOCK(&mylock);
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        return rc;
    }

    PMIX_THREADSHIFT(cd, _setup_local_support);

    return PMIX_SUCCESS;
}

static void _iofdeliver(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_iof_req_t *req;
    bool found = false;
    pmix_iof_cache_t *iof;
    int i;
    size_t n;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "PMIX:SERVER delivering IOF from %s on channel %s with %d bytes",
                        PMIX_NAME_PRINT(cd->procs),
                        PMIx_IOF_channel_string(cd->channels),
                        (int)cd->bo->size);

    /* output it locally if requested */
    rc = pmix_iof_write_output(cd->procs, cd->channels, cd->bo);
    if (0 > rc) {
        goto done;
    }

    /* cycle across our list of IOF requests and see who wants
     * this channel from this source */
    for (i = 0; i < pmix_globals.iof_requests.size; i++) {
        req = (pmix_iof_req_t *) pmix_pointer_array_get_item(&pmix_globals.iof_requests, i);
        if (NULL == req) {
            continue;
        }
        rc = pmix_iof_process_iof(cd->channels, cd->procs, cd->bo, cd->info, cd->ninfo, req);
        if (PMIX_OPERATION_SUCCEEDED == rc) {
            /* flag that we do have at least one registrant for this info,
             * so there is no need to cache it */
            found = true;
            rc = PMIX_SUCCESS;
        }
    }

    /* if nobody has registered for this yet, then cache it */
    if (!found) {
        pmix_output_verbose(2, pmix_server_globals.iof_output,
                            "PMIx:SERVER caching IOF %d",
                            (int)cd->bo->size);
        if (pmix_server_globals.max_iof_cache == pmix_list_get_size(&pmix_server_globals.iof)) {
            /* remove the oldest cached message */
            iof = (pmix_iof_cache_t *) pmix_list_remove_first(&pmix_server_globals.iof);
            if (NULL != iof) {
                PMIX_RELEASE(iof);
            }
        }
        /* add this output to our cache so it is cached until someone
         * registers to receive it */
        iof = PMIX_NEW(pmix_iof_cache_t);
        memcpy(&iof->source, cd->procs, sizeof(pmix_proc_t));
        iof->channel = cd->channels;
        /* copy the data */
        PMIX_BYTE_OBJECT_CREATE(iof->bo, 1);
        if (0 < cd->bo->size) {
            iof->bo->bytes = (char *) malloc(cd->bo->size);
            memcpy(iof->bo->bytes, cd->bo->bytes, cd->bo->size);
        }
        iof->bo->size = cd->bo->size;
        if (0 < cd->ninfo) {
            PMIX_INFO_CREATE(iof->info, cd->ninfo);
            iof->ninfo = cd->ninfo;
            for (n = 0; n < iof->ninfo; n++) {
                PMIX_INFO_XFER(&iof->info[n], &cd->info[n]);
            }
        }
        pmix_list_append(&pmix_server_globals.iof, &iof->super);
        rc = PMIX_SUCCESS;
    }

done:
    if (NULL != cd->opcbfunc) {
        cd->opcbfunc(rc, cd->cbdata);
    }

    /* release the caddy */
    cd->procs = NULL;
    cd->nprocs = 0;
    cd->info = NULL;
    cd->ninfo = 0;
    cd->bo = NULL;
    PMIX_RELEASE(cd);
}

pmix_status_t PMIx_server_IOF_deliver(const pmix_proc_t *source, pmix_iof_channel_t channel,
                                      const pmix_byte_object_t *bo, const pmix_info_t info[],
                                      size_t ninfo, pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_setup_caddy_t *cd;
    pmix_lock_t mylock;
    pmix_status_t rc;

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_setup_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->procs = (pmix_proc_t *) source;
    cd->nprocs = 1;
    cd->channels = channel;
    cd->bo = (pmix_byte_object_t *) bo;
    cd->info = (pmix_info_t *) info;
    cd->ninfo = ninfo;
    cd->opcbfunc = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->opcbfunc = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, _iofdeliver);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        PMIX_DESTRUCT_LOCK(&mylock);
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        return rc;
    }

    PMIX_THREADSHIFT(cd, _iofdeliver);
    return PMIX_SUCCESS;
}

static void cirelease(void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;

    PMIX_ACQUIRE_OBJECT(cd);

    if (NULL != cd->info) {
        PMIX_INFO_FREE(cd->info, cd->ninfo);
    }
    PMIX_RELEASE(cd);
}

static void clct(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t*)cbdata;
    pmix_list_t inventory;
    pmix_data_array_t darray;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_CONSTRUCT(&inventory, pmix_list_t);

    /* collect the pnet inventory */
    rc = pmix_pnet.collect_inventory(cd->directives, cd->ndirs, &inventory);
    if (PMIX_SUCCESS != rc) {
        goto report;
    }

    /* collect the pgpu inventory */
    rc = pmix_pgpu.collect_inventory(cd->directives, cd->ndirs, &inventory);
    if (PMIX_SUCCESS != rc) {
        goto report;
    }

    /* convert list to an array of info */
    rc = PMIx_Info_list_convert((void*)&inventory, &darray);
    if (PMIX_ERR_EMPTY == rc) {
        rc = PMIX_SUCCESS;
    } else if (PMIX_SUCCESS == rc) {
        cd->info = (pmix_info_t*)darray.array;
        cd->ninfo = darray.size;
    }

report:
    if (NULL != cd->cbfunc.infocbfunc) {
        cd->cbfunc.infocbfunc(rc, cd->info, cd->ninfo, cd->cbdata, cirelease, cd);
    }
    PMIX_LIST_DESTRUCT(&inventory);
    return;
}

pmix_status_t PMIx_server_collect_inventory(pmix_info_t directives[], size_t ndirs,
                                            pmix_info_cbfunc_t cbfunc, void *cbdata)
{
    pmix_shift_caddy_t *cd;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->directives = directives;
    cd->ndirs = ndirs;
    cd->cbfunc.infocbfunc = cbfunc;
    cd->cbdata = cbdata;
    PMIX_THREADSHIFT(cd, clct);

    return PMIX_SUCCESS;
}

static void dlinv(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    rc = pmix_pnet.deliver_inventory(cd->info, cd->ninfo, cd->directives, cd->ndirs);
    if (PMIX_SUCCESS != rc) {
        goto report;
    }

    rc = pmix_pgpu.deliver_inventory(cd->info, cd->ninfo, cd->directives, cd->ndirs);

report:
    if (NULL != cd->cbfunc.opcbfn) {
        cd->cbfunc.opcbfn(rc, cd->cbdata);
    }
    PMIX_RELEASE(cd);
    return;
}

pmix_status_t PMIx_server_deliver_inventory(pmix_info_t info[], size_t ninfo,
                                            pmix_info_t directives[], size_t ndirs,
                                            pmix_op_cbfunc_t cbfunc, void *cbdata)
{
    pmix_shift_caddy_t *cd;
    pmix_lock_t mylock;
    pmix_status_t rc;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* need to threadshift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == cd) {
        return PMIX_ERR_NOMEM;
    }
    cd->lock.active = false;
    cd->info = info;
    cd->ninfo = ninfo;
    cd->directives = directives;
    cd->ndirs = ndirs;
    cd->cbfunc.opcbfn = cbfunc;
    cd->cbdata = cbdata;

    /* if the provided callback is NULL, then substitute
     * our own internal cbfunc and block here */
    if (NULL == cbfunc) {
        PMIX_CONSTRUCT_LOCK(&mylock);
        cd->cbfunc.opcbfn = opcbfunc;
        cd->cbdata = &mylock;
        PMIX_THREADSHIFT(cd, dlinv);
        PMIX_WAIT_THREAD(&mylock);
        rc = mylock.status;
        PMIX_DESTRUCT_LOCK(&mylock);
        if (PMIX_SUCCESS == rc) {
            rc = PMIX_OPERATION_SUCCEEDED;
        }
        return rc;
    }

    PMIX_THREADSHIFT(cd, dlinv);

    return PMIX_SUCCESS;
}

pmix_status_t PMIx_server_generate_locality_string(const pmix_cpuset_t *cpuset, char **locality)
{
    pmix_status_t rc;

    /* just pass this down */
    rc = pmix_hwloc_generate_locality_string(cpuset, locality);
    return rc;
}

pmix_status_t PMIx_server_generate_cpuset_string(const pmix_cpuset_t *cpuset, char **cpuset_string)
{
    pmix_status_t rc;

    /* just pass this down */
    rc = pmix_hwloc_generate_cpuset_string(cpuset, cpuset_string);
    return rc;
}

typedef struct {
    pmix_info_t *info;
    size_t ninfo;
} mydata_t;

static void release_info(pmix_status_t status, void *cbdata)
{
    mydata_t *cd = (mydata_t *) cbdata;
    PMIX_HIDE_UNUSED_PARAMS(status);

    PMIX_ACQUIRE_OBJECT(cd);

    PMIX_INFO_FREE(cd->info, cd->ninfo);
    free(cd);
}

static void psetdef(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    /* the PMIx server needs to cache the process sets so it can
     * respond to queries for names and memberships */
    mydata_t *mydat;
    pmix_data_array_t *darray;
    pmix_proc_t *ptr;
    pmix_pset_t *ps;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    mydat = (mydata_t *) malloc(sizeof(mydata_t));
    mydat->ninfo = 3;
    PMIX_INFO_CREATE(mydat->info, mydat->ninfo);
    PMIX_INFO_LOAD(&mydat->info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    PMIX_INFO_LOAD(&mydat->info[1], PMIX_PSET_NAME, cd->nspace, PMIX_STRING);
    PMIX_DATA_ARRAY_CREATE(darray, cd->nprocs, PMIX_PROC);
    PMIX_LOAD_KEY(mydat->info[2].key, PMIX_PSET_MEMBERS);
    mydat->info[2].value.type = PMIX_DATA_ARRAY;
    mydat->info[2].value.data.darray = darray;
    ptr = (pmix_proc_t *) darray->array;
    memcpy(ptr, cd->procs, cd->nprocs * sizeof(pmix_proc_t));

    PMIx_Notify_event(PMIX_PROCESS_SET_DEFINE, &pmix_globals.myid, PMIX_RANGE_LOCAL,
                      mydat->info,  mydat->ninfo, release_info, (void *) mydat);

    /* now record the process set */
    ps = PMIX_NEW(pmix_pset_t);
    ps->name = strdup(cd->nspace);
    ps->members = (pmix_proc_t *) malloc(cd->nprocs * sizeof(pmix_proc_t));
    memcpy(ps->members, cd->procs, cd->nprocs * sizeof(pmix_proc_t));
    ps->nmembers = cd->nprocs;
    pmix_list_append(&pmix_server_globals.psets, &ps->super);

    PMIX_WAKEUP_THREAD(&cd->lock);
}

pmix_status_t PMIx_server_define_process_set(const pmix_proc_t *members, size_t nmembers,
                                             const char *pset_name)
{
    pmix_setup_caddy_t cd;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* need to threadshift this request */
    PMIX_CONSTRUCT(&cd, pmix_setup_caddy_t);
    cd.nspace = (char*)pset_name;
    cd.procs = (pmix_proc_t *) members;
    cd.nprocs = nmembers;
    cd.opcbfunc = opcbfunc;
    cd.cbdata = &cd.lock;
    PMIX_THREADSHIFT(&cd, psetdef);
    PMIX_WAIT_THREAD(&cd.lock);
    /* protect the input */
    cd.procs = NULL;
    cd.nprocs = 0;
    PMIX_DESTRUCT(&cd);
    return PMIX_SUCCESS;
}

static void psetdel(int sd, short args, void *cbdata)
{
    /* the PMIx server needs to delete the process set from its list */
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    mydata_t *mydat;
    pmix_pset_t *ps;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    mydat = (mydata_t *) malloc(sizeof(mydata_t));
    mydat->ninfo = 2;
    PMIX_INFO_CREATE(mydat->info, mydat->ninfo);
    PMIX_INFO_LOAD(&mydat->info[0], PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
    PMIX_INFO_LOAD(&mydat->info[1], PMIX_PSET_NAME, cd->nspace, PMIX_STRING);

    PMIx_Notify_event(PMIX_PROCESS_SET_DELETE, &pmix_globals.myid, PMIX_RANGE_LOCAL, mydat->info,
                      mydat->ninfo, release_info, (void *) mydat);

    /* now find this process set */
    PMIX_LIST_FOREACH (ps, &pmix_server_globals.psets, pmix_pset_t) {
        if (0 == strcmp(cd->nspace, ps->name)) {
            pmix_list_remove_item(&pmix_server_globals.psets, &ps->super);
            PMIX_RELEASE(ps);
            break;
        }
    }
    PMIX_WAKEUP_THREAD(&cd->lock);
}

pmix_status_t PMIx_server_delete_process_set(char *pset_name)
{
    pmix_setup_caddy_t cd;

    PMIX_ACQUIRE_THREAD(&pmix_global_lock);
    if (pmix_globals.init_cntr <= 0) {
        PMIX_RELEASE_THREAD(&pmix_global_lock);
        return PMIX_ERR_INIT;
    }
    PMIX_RELEASE_THREAD(&pmix_global_lock);

    /* need to threadshift this request */
    PMIX_CONSTRUCT(&cd, pmix_setup_caddy_t);
    cd.nspace = pset_name;
    cd.opcbfunc = opcbfunc;
    cd.cbdata = &cd.lock;
    PMIX_THREADSHIFT(&cd, psetdel);
    PMIX_WAIT_THREAD(&cd.lock);
    PMIX_DESTRUCT(&cd);
    return PMIX_SUCCESS;
}

/****    THE FOLLOWING CALLBACK FUNCTIONS ARE USED BY THE HOST SERVER    ****
 ****    THEY THEREFORE CAN OCCUR IN EITHER THE HOST SERVER'S THREAD     ****
 ****    CONTEXT, OR IN OUR OWN THREAD CONTEXT IF THE CALLBACK OCCURS    ****
 ****    IMMEDIATELY. THUS ANYTHING THAT ACCESSES A GLOBAL ENTITY        ****
 ****    MUST BE PUSHED INTO AN EVENT FOR PROTECTION                     ****/

static void op_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift here as no global data is
     * being accessed */

    /* setup the reply with the returned status */
    if (NULL == (reply = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        PMIX_RELEASE(cd);
        return;
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - send a copy to the originator */
    PMIX_PTL_SEND_ONEWAY(rc, cd->peer, reply, cd->hdr.tag);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
    }

    /* cleanup */
    PMIX_RELEASE(cd);
}

static void op_cbfunc2(pmix_status_t status, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift here as no global data is
     * being accessed */

    /* setup the reply with the returned status */
    if (NULL == (reply = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        PMIX_RELEASE(cd);
        return;
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - send a copy to the originator */
    PMIX_PTL_SEND_ONEWAY(rc, cd->peer, reply, cd->hdr.tag);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
    }

    /* ensure that we know the peer has finalized else we
     * will generate an event when the socket closes - yes,
     * it should have been done, but it is REALLY important
     * that it be set */
    cd->peer->finalized = true;
    /* cleanup the caddy */
    PMIX_RELEASE(cd);
}

static void _spcb(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *cd = (pmix_shift_caddy_t *) cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;
    pmix_proc_t proc;
    pmix_cb_t cb;
    pmix_kval_t *kv;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* setup the reply with the returned status */
    if (NULL == (reply = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, cd->cd->peer, reply, &cd->status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        goto cleanup;
    }
    /* pass back the name of the nspace */
    PMIX_BFROPS_PACK(rc, cd->cd->peer, reply, &cd->pname.nspace, 1, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        goto cleanup;
    }
    if (PMIX_SUCCESS == cd->status) {
        /* add the job-level info, if we have it */
        PMIX_LOAD_PROCID(&proc, cd->pname.nspace, PMIX_RANK_WILDCARD);
        /* this is going to a local client, so let the gds
         * have the option of returning a copy of the data,
         * or a pointer to local storage */
        PMIX_CONSTRUCT(&cb, pmix_cb_t);
        cb.proc = &proc;
        cb.scope = PMIX_SCOPE_UNDEF;
        cb.copy = false;
        PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
        if (PMIX_SUCCESS == rc) {
            PMIX_LIST_FOREACH (kv, &cb.kvs, pmix_kval_t) {
                PMIX_BFROPS_PACK(rc, cd->cd->peer, reply, kv, 1, PMIX_KVAL);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&cb);
                    goto cleanup;
                }
            }
            PMIX_DESTRUCT(&cb);
        }
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - tell the originator the result */
    PMIX_SERVER_QUEUE_REPLY(rc, cd->cd->peer, cd->cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

cleanup:
    /* cleanup */
    PMIX_RELEASE(cd->cd);
    PMIX_RELEASE(cd);
}

static void spawn_cbfunc(pmix_status_t status, char *nspace, void *cbdata)
{
    pmix_shift_caddy_t *cd;

    /* need to thread-shift this request */
    cd = PMIX_NEW(pmix_shift_caddy_t);
    cd->status = status;
    if (NULL != nspace) {
        cd->pname.nspace = strdup(nspace);
    }
    cd->cd = (pmix_server_caddy_t *) cbdata;

    PMIX_THREADSHIFT(cd, _spcb);
}

static void lookup_cbfunc(pmix_status_t status, pmix_pdata_t pdata[], size_t ndata, void *cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    /* no need to thread-shift as no global data is accessed */
    /* setup the reply with the returned status */
    if (NULL == (reply = PMIX_NEW(pmix_buffer_t))) {
        PMIX_ERROR_LOG(PMIX_ERR_OUT_OF_RESOURCE);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        return;
    }
    if (PMIX_SUCCESS == status) {
        /* pack the returned data objects */
        PMIX_BFROPS_PACK(rc, cd->peer, reply, &ndata, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            return;
        }
        PMIX_BFROPS_PACK(rc, cd->peer, reply, pdata, ndata, PMIX_PDATA);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            return;
        }
    }

    /* the function that created the server_caddy did a
     * retain on the peer, so we don't have to worry about
     * it still being present - tell the originator the result */
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }
    /* cleanup */
    PMIX_RELEASE(cd);
}

/* fence modex calls return here when the host RM has completed
 * the operation - any enclosed data is provided to us as a blob
 * which contains byte objects, one for each set of data. Our
 * peer servers will have packed the blobs using our common
 * GDS module, so use the mypeer one to unpack them */
static void _mdxcbfunc(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t *) cbdata;
    pmix_server_trkr_t *tracker = scd->tracker;
    pmix_buffer_t xfer, *reply;
    pmix_server_caddy_t *cd, *nxt;
    pmix_status_t rc = PMIX_SUCCESS, ret;
    pmix_nspace_caddy_t *nptr;
    pmix_list_t nslist;
    bool found;

    PMIX_ACQUIRE_OBJECT(scd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    if (NULL == tracker) {
        /* give them a release if they want it - this should
         * never happen, but protect against the possibility */
        if (NULL != scd->cbfunc.relfn) {
            scd->cbfunc.relfn(scd->cbdata);
        }
        PMIX_RELEASE(scd);
        return;
    }

    /* if we get here, then there are processes waiting
     * for a response */

    /* if the timer is active, clear it */
    if (tracker->event_active) {
        pmix_event_del(&tracker->ev);
    }

    /* pass the blobs being returned */
    PMIX_CONSTRUCT(&xfer, pmix_buffer_t);
    PMIX_CONSTRUCT(&nslist, pmix_list_t);

    if (PMIX_SUCCESS != scd->status) {
        rc = scd->status;
        goto finish_collective;
    }

    if (PMIX_COLLECT_INVALID == tracker->collect_type) {
        rc = PMIX_ERR_INVALID_ARG;
        goto finish_collective;
    }

    /* Collect the nptr list with uniq GDS components of all local
     * participants. It does not allow multiple storing to the
     * same GDS if participants have mutual GDS. */
    PMIX_LIST_FOREACH (cd, &tracker->local_cbs, pmix_server_caddy_t) {
        // see if we already have this nspace
        found = false;
        PMIX_LIST_FOREACH (nptr, &nslist, pmix_nspace_caddy_t) {
            if (0 == strcmp(nptr->ns->nspace, cd->peer->nptr->nspace)) {
                found = true;
                break;
            }
        }
        if (!found) {
            // add it
            nptr = PMIX_NEW(pmix_nspace_caddy_t);
            PMIX_RETAIN(cd->peer->nptr);
            nptr->ns = cd->peer->nptr;
            pmix_list_append(&nslist, &nptr->super);
        }
    }

    // Skip storing the data if we didn't collect it
    if (NULL == scd->data) {
        rc = PMIX_SUCCESS;
        goto finish_collective;
    }

    PMIX_LIST_FOREACH (nptr, &nslist, pmix_nspace_caddy_t) {
        /* pass the blobs being returned */
        PMIX_LOAD_BUFFER_NON_DESTRUCT(pmix_globals.mypeer, &xfer, scd->data, scd->ndata);
        PMIX_GDS_STORE_MODEX(rc, nptr->ns, &xfer, tracker);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            break;
        }
        /* do NOT destruct the xfer buffer as that would release the payload! */
    }

finish_collective:
    /* loop across all procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH_SAFE (cd, nxt, &tracker->local_cbs, pmix_server_caddy_t) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            break;
        }
        /* setup the reply, starting with the returned status */
        PMIX_BFROPS_PACK(ret, cd->peer, reply, &rc, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            goto cleanup;
        }
        /* let the gds have a chance to add any data it needs
         * for providing access to any collected data */
        PMIX_GDS_MARK_MODEX_COMPLETE(rc, cd->peer, &nslist, reply);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(reply);
            PMIX_ERROR_LOG(rc);
            goto cleanup;
        }
        pmix_output_verbose(2, pmix_server_globals.base_output,
                            "server:modex_cbfunc reply being sent to %s:%u",
                            cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
        PMIX_SERVER_QUEUE_REPLY(ret, cd->peer, cd->hdr.tag, reply);
        if (PMIX_SUCCESS != ret) {
            PMIX_RELEASE(reply);
        }
        /* remove this entry */
        pmix_list_remove_item(&tracker->local_cbs, &cd->super);
        PMIX_RELEASE(cd);
    }

cleanup:
    /* Protect data from being free'd because RM pass
     * the pointer that is set to the middle of some
     * buffer (the case with SLURM).
     * RM is responsible on the release of the buffer
     */
    xfer.base_ptr = NULL;
    xfer.bytes_used = 0;
    PMIX_DESTRUCT(&xfer);

    pmix_list_remove_item(&pmix_server_globals.collectives, &tracker->super);
    PMIX_RELEASE(tracker);
    PMIX_LIST_DESTRUCT(&nslist);

    /* we are done */
    if (NULL != scd->cbfunc.relfn) {
        scd->cbfunc.relfn(scd->cbdata);
    }
    PMIX_RELEASE(scd);
}

static void modex_cbfunc(pmix_status_t status, const char *data, size_t ndata, void *cbdata,
                         pmix_release_cbfunc_t relfn, void *relcbd)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t *) cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "server:modex_cbfunc called with %d bytes", (int) ndata);

    /* need to thread-shift this callback as it accesses global data */
    scd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == scd) {
        /* nothing we can do */
        if (NULL != relfn) {
            relfn(cbdata);
        }
        return;
    }
    scd->status = status;
    scd->data = data;
    scd->ndata = ndata;
    scd->tracker = tracker;
    scd->cbfunc.relfn = relfn;
    scd->cbdata = relcbd;
    PMIX_THREADSHIFT(scd, _mdxcbfunc);
}

static void get_cbfunc(pmix_status_t status, const char *data, size_t ndata, void *cbdata,
                       pmix_release_cbfunc_t relfn, void *relcbd)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) cbdata;
    pmix_buffer_t *reply, buf;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "server:get_cbfunc called with %d bytes", (int) ndata);

    /* no need to thread-shift here as no global data is accessed
     * and we are called from another internal function
     * (see pmix_server_get.c:pmix_pending_resolve) that
     * has already been thread-shifted */

    if (NULL == cd) {
        /* nothing to do - but be sure to give them
         * a release if they want it */
        if (NULL != relfn) {
            relfn(relcbd);
        }
        return;
    }

    /* setup the reply, starting with the returned status */
    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto cleanup;
    }
    /* if there are data, pack the blob being returned */
    if (NULL != data) {
        PMIX_CONSTRUCT(&buf, pmix_buffer_t);
        PMIX_LOAD_BUFFER(cd->peer, &buf, data, ndata);
        PMIX_BFROPS_COPY_PAYLOAD(rc, cd->peer, reply, &buf);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(reply);
            PMIX_DESTRUCT(&buf);
            goto cleanup;
        }
        buf.base_ptr = NULL;
        buf.bytes_used = 0;
        PMIX_DESTRUCT(&buf);
    }
    /* send the data to the requestor */
    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "server:get_cbfunc reply being sent to %s:%u", cd->peer->info->pname.nspace,
                        cd->peer->info->pname.rank);
    pmix_output_hexdump(10, pmix_server_globals.base_output, reply->base_ptr,
                        (reply->bytes_used < 256 ? reply->bytes_used : 256));

    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

cleanup:
    /* if someone wants a release, give it to them */
    if (NULL != relfn) {
        relfn(relcbd);
    }
    PMIX_RELEASE(cd);
}

static void _cnct(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t *) cbdata;
    pmix_server_trkr_t *tracker = scd->tracker;
    pmix_buffer_t *reply, pbkt;
    pmix_byte_object_t bo;
    pmix_status_t rc;
    int i;
    pmix_server_caddy_t *cd;
    char **nspaces = NULL;
    bool found;
    pmix_proc_t proc;
    pmix_cb_t cb;
    pmix_kval_t *kptr;

    PMIX_ACQUIRE_OBJECT(scd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    if (NULL == tracker) {
        /* nothing to do */
        return;
    }

    /* if we get here, then there are processes waiting
     * for a response */

    /* if the timer is active, clear it */
    if (tracker->event_active) {
        pmix_event_del(&tracker->ev);
    }

    /* find the unique nspaces that are participating */
    PMIX_LIST_FOREACH (cd, &tracker->local_cbs, pmix_server_caddy_t) {
        if (NULL == nspaces) {
            PMIx_Argv_append_nosize(&nspaces, cd->peer->info->pname.nspace);
        } else {
            found = false;
            for (i = 0; NULL != nspaces[i]; i++) {
                if (0 == strcmp(nspaces[i], cd->peer->info->pname.nspace)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                PMIx_Argv_append_nosize(&nspaces, cd->peer->info->pname.nspace);
            }
        }
    }

    /* loop across all local procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH (cd, &tracker->local_cbs, pmix_server_caddy_t) {
        /* setup the reply, starting with the returned status */
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        /* start with the status */
        PMIX_BFROPS_PACK(rc, cd->peer, reply, &scd->status, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            goto cleanup;
        }
        if (PMIX_SUCCESS == scd->status) {
            /* loop across all participating nspaces and include their
             * job-related info */
            for (i = 0; NULL != nspaces[i]; i++) {
                /* if this is the local proc's own nspace, then
                 * ignore it - it already has this info */
                if (0 == strncmp(nspaces[i], cd->peer->info->pname.nspace, PMIX_MAX_NSLEN)) {
                    continue;
                }

                /* this is a local request, so give the gds the option
                 * of returning a copy of the data, or a pointer to
                 * local storage */
                /* add the job-level info, if necessary */
                PMIX_LOAD_PROCID(&proc, nspaces[i], PMIX_RANK_WILDCARD);
                PMIX_CONSTRUCT(&cb, pmix_cb_t);
                /* this is for a local client, so give the gds the
                 * option of returning a complete copy of the data,
                 * or returning a pointer to local storage */
                cb.proc = &proc;
                cb.scope = PMIX_SCOPE_UNDEF;
                cb.copy = false;
                PMIX_GDS_FETCH_KV(rc, cd->peer, &cb);
                if (PMIX_SUCCESS != rc) {
                    /* try getting it from our storage */
                    PMIX_GDS_FETCH_KV(rc, pmix_globals.mypeer, &cb);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(reply);
                        PMIX_DESTRUCT(&cb);
                        goto error;
                    }
                }
                PMIX_CONSTRUCT(&pbkt, pmix_buffer_t);
                /* pack the nspace name */
                PMIX_BFROPS_PACK(rc, cd->peer, &pbkt, &nspaces[i], 1, PMIX_STRING);
                if (PMIX_SUCCESS != rc) {
                    PMIX_ERROR_LOG(rc);
                    PMIX_RELEASE(reply);
                    PMIX_DESTRUCT(&pbkt);
                    PMIX_DESTRUCT(&cb);
                    goto error;
                }
                PMIX_LIST_FOREACH (kptr, &cb.kvs, pmix_kval_t) {
                    PMIX_BFROPS_PACK(rc, cd->peer, &pbkt, kptr, 1, PMIX_KVAL);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(reply);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_DESTRUCT(&cb);
                        goto error;
                    }
                }
                PMIX_DESTRUCT(&cb);

                if (PMIX_PEER_IS_V1(cd->peer) || PMIX_PEER_IS_V20(cd->peer)) {
                    PMIX_BFROPS_PACK(rc, cd->peer, reply, &pbkt, 1, PMIX_BUFFER);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(reply);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_DESTRUCT(&cb);
                        goto error;
                    }
                } else {
                    PMIX_UNLOAD_BUFFER(&pbkt, bo.bytes, bo.size);
                    PMIX_BFROPS_PACK(rc, cd->peer, reply, &bo, 1, PMIX_BYTE_OBJECT);
                    if (PMIX_SUCCESS != rc) {
                        PMIX_ERROR_LOG(rc);
                        PMIX_RELEASE(reply);
                        PMIX_DESTRUCT(&pbkt);
                        PMIX_DESTRUCT(&cb);
                        goto error;
                    }
                }

                PMIX_DESTRUCT(&pbkt);
            }
        }
        pmix_output_verbose(2, pmix_server_globals.connect_output,
                            "server:cnct_cbfunc reply being sent to %s",
                            PMIX_PEER_PRINT(cd->peer));
        PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
        }
    }
    goto cleanup;

error:
    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* return an error status so they don't hang */
    scd->status = rc;
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &scd->status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        goto cleanup;
    }
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

cleanup:
    if (NULL != nspaces) {
        PMIx_Argv_free(nspaces);
    }
    pmix_list_remove_item(&pmix_server_globals.collectives, &tracker->super);
    PMIX_RELEASE(tracker);

    /* we are done */
    PMIX_RELEASE(scd);
}

static void cnct_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t *) cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_server_globals.connect_output,
                        "server:cnct_cbfunc called");

    /* need to thread-shift this callback as it accesses global data */
    scd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == scd) {
        /* nothing we can do */
        return;
    }
    scd->status = status;
    scd->tracker = tracker;
    PMIX_THREADSHIFT(scd, _cnct);
}

static void _discnct(int sd, short args, void *cbdata)
{
    pmix_shift_caddy_t *scd = (pmix_shift_caddy_t *) cbdata;
    pmix_server_trkr_t *tracker = scd->tracker;
    pmix_buffer_t *reply;
    pmix_status_t rc;
    pmix_server_caddy_t *cd;

    PMIX_ACQUIRE_OBJECT(scd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    if (NULL == tracker) {
        /* nothing to do */
        return;
    }

    /* if we get here, then there are processes waiting
     * for a response */

    /* if the timer is active, clear it */
    if (tracker->event_active) {
        pmix_event_del(&tracker->ev);
    }

    /* loop across all local procs in the tracker, sending them the reply */
    PMIX_LIST_FOREACH (cd, &tracker->local_cbs, pmix_server_caddy_t) {
        /* setup the reply */
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            rc = PMIX_ERR_NOMEM;
            goto cleanup;
        }
        /* return the status */
        PMIX_BFROPS_PACK(rc, cd->peer, reply, &scd->status, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            goto cleanup;
        }
        pmix_output_verbose(2, pmix_server_globals.connect_output,
                            "server:cnct_cbfunc reply being sent to %s:%u",
                            cd->peer->info->pname.nspace, cd->peer->info->pname.rank);
        PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(reply);
        }
    }

cleanup:
    /* cleanup the tracker -- the host RM is responsible for
     * telling us when to remove the nspace from our data */
    pmix_list_remove_item(&pmix_server_globals.collectives, &tracker->super);
    PMIX_RELEASE(tracker);

    /* we are done */
    PMIX_RELEASE(scd);
}

static void discnct_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_server_trkr_t *tracker = (pmix_server_trkr_t *) cbdata;
    pmix_shift_caddy_t *scd;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "server:discnct_cbfunc called on nspace %s",
                        (NULL == tracker) ? "NULL" : tracker->pname.nspace);

    /* need to thread-shift this callback as it accesses global data */
    scd = PMIX_NEW(pmix_shift_caddy_t);
    if (NULL == scd) {
        /* nothing we can do */
        return;
    }
    scd->status = status;
    scd->tracker = tracker;
    PMIX_THREADSHIFT(scd, _discnct);
}

static void regevents_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) cbdata;
    pmix_buffer_t *reply;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "server:regevents_cbfunc called status = %d", status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }
    PMIX_RELEASE(cd);
}

static void notifyerror_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_status_t rc;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) cbdata;
    pmix_buffer_t *reply;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "server:notifyerror_cbfunc called status = %d", status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
    }
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }
    PMIX_RELEASE(cd);
}

static void alloc_cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                         pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) qcd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:alloc callback with status %d",
                        status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, cd->peer, reply, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

    // cleanup
    if (NULL != qcd->queries) {
        PMIX_QUERY_FREE(qcd->queries, qcd->nqueries);
    }
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
}

static void query_cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                         pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_server_caddy_t *scd = (pmix_server_caddy_t*)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:query callback with status %s",
                        PMIx_Error_string(status));

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(scd);
        return;
    }
    PMIX_BFROPS_PACK(rc, scd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    PMIX_BFROPS_PACK(rc, scd->peer, reply, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, scd->peer, reply, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

    /* cache the data for any future requests */

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, scd->peer, scd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

    // cleanup
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
    PMIX_RELEASE(scd);
}

static void jctrl_cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                         pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) qcd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:jctrl callback with status %d",
                        status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, cd->peer, reply, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

    // cleanup
    if (NULL != qcd->queries) {
        PMIX_QUERY_FREE(qcd->queries, qcd->nqueries);
    }
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
}

static void monitor_cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                           pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) qcd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:monitor callback with status %d",
                        status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, cd->peer, reply, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

    // cleanup
    if (NULL != qcd->queries) {
        PMIX_QUERY_FREE(qcd->queries, qcd->nqueries);
    }
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
}

static void cred_cbfunc(pmix_status_t status, pmix_byte_object_t *credential, pmix_info_t info[],
                        size_t ninfo, void *cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) qcd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output, "pmix:get credential callback with status %d",
                        status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }

    /* pack the status */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }

    if (PMIX_SUCCESS == status) {
        /* pack the returned credential */
        PMIX_BFROPS_PACK(rc, cd->peer, reply, credential, 1, PMIX_BYTE_OBJECT);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto complete;
        }

        /* pack any returned data */
        PMIX_BFROPS_PACK(rc, cd->peer, reply, &ninfo, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto complete;
        }
        if (0 < ninfo) {
            PMIX_BFROPS_PACK(rc, cd->peer, reply, info, ninfo, PMIX_INFO);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
        }
    }

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

    // cleanup
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
}

static void validate_cbfunc(pmix_status_t status, pmix_info_t info[], size_t ninfo, void *cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) qcd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_globals.debug_output,
                        "pmix:validate credential callback with status %d", status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack any returned data */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, cd->peer, reply, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }
    // cleanup
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
}

static void _iofreg(int sd, short args, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_server_caddy_t *scd = (pmix_server_caddy_t *) cd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;
    pmix_iof_req_t *req;
    pmix_iof_cache_t *iof, *inxt;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(sd, args);

    /* setup the reply to the requestor */
    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* start with the status */
    PMIX_BFROPS_PACK(rc, scd->peer, reply, &cd->status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        goto cleanup;
    }

    /* was the request a success? */
    if (PMIX_SUCCESS != cd->status) {
        /* find and remove the tracker */
        req = (pmix_iof_req_t *) pmix_pointer_array_get_item(&pmix_globals.iof_requests,
                                                             cd->ncodes);
        if (NULL != req) {
            PMIX_RELEASE(req);
        }
        pmix_pointer_array_set_item(&pmix_globals.iof_requests, cd->ncodes, NULL);
    } else {
        /* return our reference ID for this handler */
        PMIX_BFROPS_PACK(rc, scd->peer, reply, &cd->ncodes, 1, PMIX_SIZE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            goto cleanup;
        }
    }

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "server:_iofreg reply being sent to %s:%u", scd->peer->info->pname.nspace,
                        scd->peer->info->pname.rank);
    PMIX_SERVER_QUEUE_REPLY(rc, scd->peer, scd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

    /* if the request succeeded, then process any cached IO - doing it here
     * guarantees that the IO will be received AFTER the client gets the
     * refid response */
    if (PMIX_SUCCESS == cd->status) {
        /* get the request */
        req = (pmix_iof_req_t *) pmix_pointer_array_get_item(&pmix_globals.iof_requests,
                                                             cd->ncodes);
        if (NULL != req) {
            PMIX_LIST_FOREACH_SAFE (iof, inxt, &pmix_server_globals.iof, pmix_iof_cache_t) {
                if (PMIX_OPERATION_SUCCEEDED
                    == pmix_iof_process_iof(iof->channel, &iof->source, iof->bo, iof->info,
                                            iof->ninfo, req)) {
                    pmix_list_remove_item(&pmix_server_globals.iof, &iof->super);
                    PMIX_RELEASE(iof);
                }
            }
        }
    }

cleanup:
    /* release the cached info */
    if (NULL != cd->procs) {
        PMIX_PROC_FREE(cd->procs, cd->nprocs);
    }
    PMIX_INFO_FREE(cd->info, cd->ninfo);
    /* we are done */
    PMIX_RELEASE(cd);
}

static void iofdereg(pmix_status_t status, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;
    pmix_server_caddy_t *scd = (pmix_server_caddy_t *) cd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    PMIX_ACQUIRE_OBJECT(cd);
    PMIX_HIDE_UNUSED_PARAMS(status);

    /* setup the reply to the requestor */
    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        rc = PMIX_ERR_NOMEM;
        goto cleanup;
    }
    /* its just the status */
    PMIX_BFROPS_PACK(rc, scd->peer, reply, &cd->status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_RELEASE(reply);
        goto cleanup;
    }

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "server:_iofreg reply being sent to %s:%u", scd->peer->info->pname.nspace,
                        scd->peer->info->pname.rank);
    PMIX_SERVER_QUEUE_REPLY(rc, scd->peer, scd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

cleanup:
    PMIX_RELEASE(cd);
}

static void iof_cbfunc(pmix_status_t status, void *cbdata)
{
    pmix_setup_caddy_t *cd = (pmix_setup_caddy_t *) cbdata;

    pmix_output_verbose(2, pmix_server_globals.iof_output,
                        "server:iof_cbfunc called with status %d", status);

    if (NULL == cd) {
        /* nothing to do */
        return;
    }
    cd->status = status;

    /* need to thread-shift this callback as it accesses global data */
    PMIX_THREADSHIFT(cd, _iofreg);
}

static void fabric_cbfunc(pmix_status_t status, pmix_info_t *info, size_t ninfo, void *cbdata,
                          pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_query_caddy_t *qcd = (pmix_query_caddy_t *) cbdata;
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *) qcd->cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output, "pmix:fabric callback with status %d",
                        status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ninfo) {
        PMIX_BFROPS_PACK(rc, cd->peer, reply, info, ninfo, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }

    // cleanup
    if (NULL != qcd->queries) {
        PMIX_QUERY_FREE(qcd->queries, qcd->nqueries);
    }
    if (NULL != qcd->info) {
        PMIX_INFO_FREE(qcd->info, qcd->ninfo);
    }
    PMIX_RELEASE(qcd);
    PMIX_RELEASE(cd);
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
}

static void dist_cbfunc(pmix_status_t status, pmix_device_distance_t *dist, size_t ndist, void *cbdata,
                        pmix_release_cbfunc_t release_fn, void *release_cbdata)
{
    pmix_server_caddy_t *cd = (pmix_server_caddy_t *)cbdata;
    pmix_buffer_t *reply;
    pmix_status_t rc;

    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "pmix:fabric callback with status %d",
                        status);

    reply = PMIX_NEW(pmix_buffer_t);
    if (NULL == reply) {
        PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
        PMIX_RELEASE(cd);
        return;
    }
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &status, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    /* pack the returned data */
    PMIX_BFROPS_PACK(rc, cd->peer, reply, &ndist, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto complete;
    }
    if (0 < ndist) {
        PMIX_BFROPS_PACK(rc, cd->peer, reply, dist, ndist, PMIX_DEVICE_DIST);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
    }

complete:
    // send reply
    PMIX_SERVER_QUEUE_REPLY(rc, cd->peer, cd->hdr.tag, reply);
    if (PMIX_SUCCESS != rc) {
        PMIX_RELEASE(reply);
    }
    PMIX_RELEASE(cd);
    if (NULL != release_fn) {
        release_fn(release_cbdata);
    }
}

/* the switchyard is the primary message handling function. It's purpose
 * is to take incoming commands (packed into a buffer), unpack them,
 * and then call the corresponding host server's function to execute
 * them. Some commands involve only a single proc (i.e., the one
 * sending the command) and can be executed while we wait. In these cases,
 * the switchyard will construct and pack a reply buffer to be returned
 * to the sender.
 *
 * Other cases (either multi-process collective or cmds that require
 * an async reply) cannot generate an immediate reply. In these cases,
 * the reply buffer will be NULL. An appropriate callback function will
 * be called that will be responsible for eventually replying to the
 * calling processes.
 *
 * Should an error be encountered at any time within the switchyard, an
 * error reply buffer will be returned so that the caller can be notified,
 * thereby preventing the process from hanging. */
static pmix_status_t server_switchyard(pmix_peer_t *peer, uint32_t tag, pmix_buffer_t *buf)
{
    pmix_status_t rc = PMIX_ERR_NOT_SUPPORTED;
    int32_t cnt;
    pmix_cmd_t cmd;
    pmix_server_caddy_t *cd;
    pmix_proc_t proc;
    pmix_buffer_t *reply;

    /* protect against zero-byte buffers - these can come if the
     * connection is dropped due to a process failure */
    if (PMIX_BUFFER_IS_EMPTY(buf)) {
        return PMIX_ERR_LOST_CONNECTION;
    }

    /* retrieve the cmd */
    cnt = 1;
    PMIX_BFROPS_UNPACK(rc, peer, buf, &cmd, &cnt, PMIX_COMMAND);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    pmix_output_verbose(2, pmix_server_globals.base_output,
                        "%s recvd pmix cmd %s from %s bytes %u",
                        PMIX_NAME_PRINT(&pmix_globals.myid),
                        pmix_command_string(cmd),
                        PMIX_PEER_PRINT(peer),
                        (unsigned int) buf->bytes_used);

    /* if I am a tool, all I can do is relay this to my primary server
     * if I am connected - if not connected, then I must return an error */
    if (PMIX_PEER_IS_TOOL(pmix_globals.mypeer)) {
        rc = pmix_tool_relay_op(cmd, peer, buf, tag);
        if (PMIX_ERR_NOT_SUPPORTED != rc) {
            return rc;
        }
        /* if the tool relay doesn't support it, let it
         * be processed by the logic tree */
    }

    /* if I am a server, then redirect the cmd to the appropriate
     * function for processing */

    if (PMIX_REQ_CMD == cmd) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return PMIX_ERR_NOMEM;
        }
        PMIX_GDS_REGISTER_JOB_INFO(rc, peer, reply);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_RELEASE(reply);
            return rc;
        }
        PMIX_SERVER_QUEUE_REPLY(rc, peer, tag, reply);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(reply);
        }
        peer->nptr->ndelivered++;
        return PMIX_SUCCESS;
    }

    if (PMIX_ABORT_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_abort(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_COMMIT_CMD == cmd) {
        rc = pmix_server_commit(peer, buf);
        if (!PMIX_PEER_IS_V1(peer)) {
            reply = PMIX_NEW(pmix_buffer_t);
            if (NULL == reply) {
                PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
                return PMIX_ERR_NOMEM;
            }
            PMIX_BFROPS_PACK(rc, peer, reply, &rc, 1, PMIX_STATUS);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
            }
            PMIX_SERVER_QUEUE_REPLY(rc, peer, tag, reply);
            if (PMIX_SUCCESS != rc) {
                PMIX_RELEASE(reply);
            }
        }
        return PMIX_SUCCESS; // don't reply twice
    }

    if (PMIX_FENCENB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_fence(cd, buf, modex_cbfunc, op_cbfunc))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_GETNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_get(buf, get_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_FINALIZE_CMD == cmd) {
        pmix_output_verbose(2, pmix_server_globals.base_output, "recvd FINALIZE");
        peer->nptr->nfinalized++;
        /* purge events */
        pmix_server_purge_events(peer, NULL);
        PMIX_GDS_CADDY(cd, peer, tag);
        /* call the local server, if supported */
        if (NULL != pmix_host_server.client_finalized &&
            PMIX_PEER_IS_CLIENT(peer)) {
            pmix_strncpy(proc.nspace, peer->info->pname.nspace, PMIX_MAX_NSLEN);
            proc.rank = peer->info->pname.rank;
            /* now tell the host server */
            rc = pmix_host_server.client_finalized(&proc, peer->info->server_object, op_cbfunc2, cd);
            if (PMIX_SUCCESS == rc) {
                /* don't reply to them ourselves - we will do so when the host
                 * server calls us back */
                return rc;
            } else if (PMIX_OPERATION_SUCCEEDED == rc) {
                /* they did it atomically */
                rc = PMIX_SUCCESS;
            }
            /* if the call doesn't succeed (e.g., they provided the stub
             * but return NOT_SUPPORTED), then the callback function
             * won't be called, but we still need to cleanup
             * any lingering references to this peer and answer
             * the client. Thus, we call the callback function ourselves
             * in this case */
            op_cbfunc2(rc, cd);
            /* return SUCCESS as the cbfunc generated the return msg
             * and released the cd object */
            return PMIX_SUCCESS;
        }
        /* if the host doesn't provide a client_finalized function,
         * we still need to ensure that we cleanup any lingering
         * references to this peer. We use the callback function
         * here as well to ensure the client gets its required
         * response and that we delay before cleaning up the
         * connection*/
        op_cbfunc2(PMIX_SUCCESS, cd);
        /* return SUCCESS as the cbfunc generated the return msg
         * and released the cd object */
        return PMIX_SUCCESS;
    }

    if (PMIX_PUBLISHNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_publish(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_LOOKUPNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_lookup(peer, buf, lookup_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_UNPUBLISHNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_unpublish(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_SPAWNNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_spawn(peer, buf, spawn_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_CONNECTNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_connect(cd, buf, cnct_cbfunc);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_DISCONNECTNB_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_disconnect(cd, buf, discnct_cbfunc);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_REGEVENTS_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_register_events(peer, buf, regevents_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_DEREGEVENTS_CMD == cmd) {
        pmix_server_deregister_events(peer, buf);
        return PMIX_SUCCESS;
    }

    if (PMIX_NOTIFY_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_event_recvd_from_client(peer, buf, notifyerror_cbfunc, cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_QUERY_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        rc = pmix_server_query(peer, buf, query_cbfunc, cd);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_LOG_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_log(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_ALLOC_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_alloc(peer, buf, alloc_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_JOB_CONTROL_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_job_ctrl(peer, buf, jctrl_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_MONITOR_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_monitor(peer, buf, monitor_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_GET_CREDENTIAL_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_get_credential(peer, buf, cred_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_VALIDATE_CRED_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS
            != (rc = pmix_server_validate_credential(peer, buf, validate_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_IOF_PULL_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_iofreg(peer, buf, iof_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_IOF_PUSH_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_iofstdin(peer, buf, op_cbfunc, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_IOF_DEREG_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_iofdereg(peer, buf, iofdereg, cd))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_GROUP_CONSTRUCT_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_grpconstruct(cd, buf))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_GROUP_DESTRUCT_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_grpdestruct(cd, buf))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_FABRIC_REGISTER_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_fabric_register(cd, buf, fabric_cbfunc))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_FABRIC_UPDATE_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_fabric_update(cd, buf, fabric_cbfunc))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_COMPUTE_DEVICE_DISTANCES_CMD == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_device_dists(cd, buf, dist_cbfunc))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    if (PMIX_REFRESH_CACHE == cmd) {
        PMIX_GDS_CADDY(cd, peer, tag);
        if (PMIX_SUCCESS != (rc = pmix_server_refresh_cache(cd, buf, op_cbfunc))) {
            PMIX_RELEASE(cd);
        }
        return rc;
    }

    return PMIX_ERR_NOT_SUPPORTED;
}

void pmix_server_message_handler(struct pmix_peer_t *pr, pmix_ptl_hdr_t *hdr,
                                 pmix_buffer_t *buf, void *cbdata)
{
    pmix_peer_t *peer = (pmix_peer_t *) pr;
    pmix_buffer_t *reply;
    pmix_status_t rc, ret;

    pmix_output_verbose(2, pmix_server_globals.base_output, "SWITCHYARD for %s:%u:%d",
                        peer->info->pname.nspace, peer->info->pname.rank, peer->sd);
    PMIX_HIDE_UNUSED_PARAMS(cbdata);

    ret = server_switchyard(peer, hdr->tag, buf);
    /* send the return, if there was an error returned */
    if (PMIX_SUCCESS != ret) {
        reply = PMIX_NEW(pmix_buffer_t);
        if (NULL == reply) {
            PMIX_ERROR_LOG(PMIX_ERR_NOMEM);
            return;
        }
        if (PMIX_OPERATION_SUCCEEDED == ret) {
            ret = PMIX_SUCCESS;
        }
        PMIX_BFROPS_PACK(rc, pr, reply, &ret, 1, PMIX_STATUS);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
        }
        PMIX_SERVER_QUEUE_REPLY(rc, peer, hdr->tag, reply);
        if (PMIX_SUCCESS != rc) {
            PMIX_RELEASE(reply);
        }
    }
}
