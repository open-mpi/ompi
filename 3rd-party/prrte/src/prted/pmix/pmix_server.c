/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2011      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2017 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * Copyright (c) 2023      Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "types.h"

#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif
#include <fcntl.h>
#ifdef HAVE_NETINET_IN_H
#    include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#    include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#    include <netdb.h>
#endif
#include <ctype.h>

#include "prte_stdint.h"
#include "src/class/pmix_hotel.h"
#include "src/class/pmix_list.h"
#include "src/mca/base/pmix_mca_base_var.h"
#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_argv.h"
#include "src/util/error.h"
#include "src/util/pmix_os_dirpath.h"
#include "src/util/pmix_os_path.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"
#include "src/util/pmix_environ.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/rml/rml_contact.h"
#include "src/rml/rml.h"
#include "src/runtime/prte_data_server.h"
#include "src/runtime/prte_globals.h"
#include "src/threads/pmix_threads.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/prted/pmix/pmix_server.h"
#include "src/prted/pmix/pmix_server_internal.h"

/*
 * Local utility functions
 */
static void pmix_server_dmdx_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                  prte_rml_tag_t tg, void *cbdata);
static void pmix_server_dmdx_resp(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                  prte_rml_tag_t tg, void *cbdata);
static void pmix_server_log(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                            prte_rml_tag_t tg, void *cbdata);
static void pmix_server_sched(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                              prte_rml_tag_t tg, void *cbdata);

#define PRTE_PMIX_SERVER_MIN_ROOMS 4096

pmix_server_globals_t prte_pmix_server_globals = {0};
static pmix_topology_t mytopology = {0};

static pmix_server_module_t pmix_server = {
    .client_connected = pmix_server_client_connected_fn,
    .client_finalized = pmix_server_client_finalized_fn,
    .abort = pmix_server_abort_fn,
    .fence_nb = pmix_server_fencenb_fn,
    .direct_modex = pmix_server_dmodex_req_fn,
    .publish = pmix_server_publish_fn,
    .lookup = pmix_server_lookup_fn,
    .unpublish = pmix_server_unpublish_fn,
    .spawn = pmix_server_spawn_fn,
    .connect = pmix_server_connect_fn,
    .disconnect = pmix_server_disconnect_fn,
    .register_events = pmix_server_register_events_fn,
    .deregister_events = pmix_server_deregister_events_fn,
    .notify_event = pmix_server_notify_event,
    .query = pmix_server_query_fn,
    .tool_connected = pmix_tool_connected_fn,
    .log = pmix_server_log_fn,
    .job_control = pmix_server_job_ctrl_fn,
    .iof_pull = pmix_server_iof_pull_fn,
    .push_stdin = pmix_server_stdin_fn,
    .group = pmix_server_group_fn,
    .allocate = pmix_server_alloc_fn,
#if PMIX_NUMERIC_VERSION >= 0x00050000
    .session_control = pmix_server_session_ctrl_fn
#endif
};

typedef struct {
    char *function;
    char **attrs;
} prte_regattr_input_t;

static prte_regattr_input_t prte_attributes[] = {
    {.function = "PMIx_Init", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Finalize", .attrs = (char *[]){"PMIX_EMBED_BARRIER", NULL}},
    {.function = "PMIx_Abort", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Fence", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Fence_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Get",
     .attrs = (char *[]){"PMIX_GET_REFRESH_CACHE", "PMIX_REQUIRED_KEY", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Get_nb",
     .attrs = (char *[]){"PMIX_GET_REFRESH_CACHE", "PMIX_REQUIRED_KEY", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Publish", .attrs = (char *[]){"PMIX_RANGE", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Publish_nb", .attrs = (char *[]){"PMIX_RANGE", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Lookup", .attrs = (char *[]){"PMIX_RANGE", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Lookup_nb", .attrs = (char *[]){"PMIX_RANGE", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Unpublish", .attrs = (char *[]){"PMIX_RANGE", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Unpublish_nb", .attrs = (char *[]){"PMIX_RANGE", "PMIX_TIMEOUT", NULL}},
    {.function = "PMIx_Spawn",
     .attrs = (char *[]){"PMIX_HOST",
                         "PMIX_HOSTFILE",
                         "PMIX_ADD_HOSTFILE",
                         "PMIX_ADD_HOST",
                         "PMIX_PREFIX",
                         "PMIX_WDIR",
                         "PMIX_PRELOAD_BIN",
                         "PMIX_PRELOAD_FILES",
                         "PMIX_SET_ENVAR",
                         "PMIX_ADD_ENVAR",
                         "PMIX_UNSET_ENVAR",
                         "PMIX_PREPEND_ENVAR",
                         "PMIX_APPEND_ENVAR",
                         "PMIX_ENVARS_HARVESTED",
                         "PMIX_PSET_NAME",
                         "PMIX_PERSONALITY",
                         "PMIX_MAPPER",
                         "PMIX_DISPLAY_MAP",
                         "PMIX_PPR",
                         "PMIX_MAPBY",
                         "PMIX_RANKBY",
                         "PMIX_BINDTO",
                         "PMIX_CPUS_PER_PROC",
                         "PMIX_NO_PROCS_ON_HEAD",
                         "PMIX_NO_OVERSUBSCRIBE",
                         "PMIX_REPORT_BINDINGS",
                         "PMIX_CPU_LIST",
                         "PMIX_JOB_RECOVERABLE",
                         "PMIX_MAX_RESTARTS",
                         "PMIX_JOB_CONTINUOUS",
                         "PMIX_NON_PMI",
                         "PMIX_PARENT_ID",
                         "PMIX_REQUESTOR_IS_TOOL",
                         "PMIX_NOTIFY_COMPLETION",
                         "PMIX_DEBUG_STOP_ON_EXEC",
                         "PMIX_DEBUG_STOP_IN_INIT",
                         "PMIX_DEBUG_STOP_IN_APP",
                         "PMIX_TAG_OUTPUT",
                         "PMIX_IOF_TAG_OUTPUT",
                         "PMIX_TIMESTAMP_OUTPUT",
                         "PMIX_IOF_TIMESTAMP_OUTPUT",
                         "PMIX_IOF_XML_OUTPUT",
                         "PMIX_OUTPUT_TO_FILE",
                         "PMIX_IOF_OUTPUT_TO_FILE",
                         "PMIX_OUTPUT_TO_DIRECTORY",
                         "PMIX_IOF_OUTPUT_TO_DIRECTORY",
                         "PMIX_OUTPUT_NOCOPY",
                         "PMIX_IOF_FILE_ONLY",
                         "PMIX_MERGE_STDERR_STDOUT",
                         "PMIX_IOF_MERGE_STDERR_STDOUT",
                         "PMIX_STDIN_TGT",
                         "PMIX_INDEX_ARGV",
                         "PMIX_DEBUGGER_DAEMONS",
                         "PMIX_SPAWN_TOOL",
                         "PMIX_DEBUG_TARGET",
                         "PMIX_DEBUG_DAEMONS_PER_NODE",
                         "PMIX_DEBUG_DAEMONS_PER_PROC",
                         "PMIX_TIMEOUT",
                         "PMIX_TIMEOUT_STACKTRACES",
                         "PMIX_TIMEOUT_REPORT_STATE",
                         NULL}},
    {.function = "PMIx_Spawn_nb",
     .attrs = (char *[]){"PMIX_HOST",
                         "PMIX_HOSTFILE",
                         "PMIX_ADD_HOSTFILE",
                         "PMIX_ADD_HOST",
                         "PMIX_PREFIX",
                         "PMIX_WDIR",
                         "PMIX_PRELOAD_BIN",
                         "PMIX_PRELOAD_FILES",
                         "PMIX_SET_ENVAR",
                         "PMIX_ADD_ENVAR",
                         "PMIX_UNSET_ENVAR",
                         "PMIX_PREPEND_ENVAR",
                         "PMIX_APPEND_ENVAR",
                         "PMIX_ENVARS_HARVESTED",
                         "PMIX_PSET_NAME",
                         "PMIX_PERSONALITY",
                         "PMIX_MAPPER",
                         "PMIX_DISPLAY_MAP",
                         "PMIX_PPR",
                         "PMIX_MAPBY",
                         "PMIX_RANKBY",
                         "PMIX_BINDTO",
                         "PMIX_CPUS_PER_PROC",
                         "PMIX_NO_PROCS_ON_HEAD",
                         "PMIX_NO_OVERSUBSCRIBE",
                         "PMIX_REPORT_BINDINGS",
                         "PMIX_CPU_LIST",
                         "PMIX_JOB_RECOVERABLE",
                         "PMIX_MAX_RESTARTS",
                         "PMIX_JOB_CONTINUOUS",
                         "PMIX_NON_PMI",
                         "PMIX_PARENT_ID",
                         "PMIX_REQUESTOR_IS_TOOL",
                         "PMIX_NOTIFY_COMPLETION",
                         "PMIX_DEBUG_STOP_ON_EXEC",
                         "PMIX_DEBUG_STOP_IN_INIT",
                         "PMIX_DEBUG_STOP_IN_APP",
                         "PMIX_TAG_OUTPUT",
                         "PMIX_IOF_TAG_OUTPUT",
                         "PMIX_TIMESTAMP_OUTPUT",
                         "PMIX_IOF_TIMESTAMP_OUTPUT",
                         "PMIX_IOF_XML_OUTPUT",
                         "PMIX_OUTPUT_TO_FILE",
                         "PMIX_IOF_OUTPUT_TO_FILE",
                         "PMIX_OUTPUT_TO_DIRECTORY",
                         "PMIX_IOF_OUTPUT_TO_DIRECTORY",
                         "PMIX_OUTPUT_NOCOPY",
                         "PMIX_IOF_FILE_ONLY",
                         "PMIX_MERGE_STDERR_STDOUT",
                         "PMIX_IOF_MERGE_STDERR_STDOUT",
                         "PMIX_STDIN_TGT",
                         "PMIX_INDEX_ARGV",
                         "PMIX_DEBUGGER_DAEMONS",
                         "PMIX_SPAWN_TOOL",
                         "PMIX_DEBUG_TARGET",
                         "PMIX_DEBUG_DAEMONS_PER_NODE",
                         "PMIX_DEBUG_DAEMONS_PER_PROC",
                         "PMIX_TIMEOUT",
                         "PMIX_TIMEOUT_STACKTRACES",
                         "PMIX_TIMEOUT_REPORT_STATE",
                         NULL}},
    {.function = "PMIx_Connect", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Connect_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Disconnect", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Disconnect_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Query_info",
     .attrs = (char *[]){"PMIX_QUERY_NAMESPACES",
                         "PMIX_QUERY_NAMESPACE_INFO",
                         "PMIX_QUERY_SPAWN_SUPPORT",
                         "PMIX_QUERY_DEBUG_SUPPORT",
                         "PMIX_HWLOC_XML_V1",
                         "PMIX_HWLOC_XML_V2",
                         "PMIX_PROC_URI",
                         "PMIX_QUERY_PROC_TABLE",
                         "PMIX_QUERY_LOCAL_PROC_TABLE",
                         "PMIX_QUERY_NUM_PSETS",
                         "PMIX_QUERY_PSET_NAMES",
                         "PMIX_JOB_SIZE",
                         "PMIX_QUERY_NUM_GROUPS",
                         "PMIX_QUERY_GROUP_NAMES",
                         "PMIX_QUERY_GROUP_MEMBERSHIP",
                         NULL}},
    {.function = "PMIx_Query_info_nb",
     .attrs = (char *[]){"PMIX_QUERY_NAMESPACES",
                         "PMIX_QUERY_NAMESPACE_INFO",
                         "PMIX_QUERY_SPAWN_SUPPORT",
                         "PMIX_QUERY_DEBUG_SUPPORT",
                         "PMIX_HWLOC_XML_V1",
                         "PMIX_HWLOC_XML_V2",
                         "PMIX_PROC_URI",
                         "PMIX_QUERY_PROC_TABLE",
                         "PMIX_QUERY_LOCAL_PROC_TABLE",
                         "PMIX_QUERY_NUM_PSETS",
                         "PMIX_QUERY_PSET_NAMES",
                         "PMIX_JOB_SIZE",
                         "PMIX_QUERY_NUM_GROUPS",
                         "PMIX_QUERY_GROUP_NAMES",
                         "PMIX_QUERY_GROUP_MEMBERSHIP",
                         "PMIX_QUERY_ALLOCATION",
                         "PMIX_QUERY_ALLOC_STATUS",
                         NULL}},
    {.function = "PMIx_Log", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Log_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Job_control",
     .attrs = (char *[]){"PMIX_JOB_CTRL_KILL",
                         "PMIX_JOB_CTRL_TERMINATE",
                         "PMIX_JOB_CTRL_SIGNAL",
                         NULL}},
    {.function = "PMIx_Job_control_nb",
     .attrs = (char *[]){"PMIX_JOB_CTRL_KILL",
                         "PMIX_JOB_CTRL_TERMINATE",
                         "PMIX_JOB_CTRL_SIGNAL",
                         NULL}},
    {.function = "PMIx_Group_construct",
     .attrs = (char *[]){"PMIX_GROUP_ASSIGN_CONTEXT_ID",
                         "PMIX_EMBED_BARRIER",
                         "PMIX_GROUP_ENDPT_DATA",
                         NULL}},
    {.function = "PMIx_Group_construct_nb",
     .attrs = (char *[]){"PMIX_GROUP_ASSIGN_CONTEXT_ID",
                         "PMIX_EMBED_BARRIER",
                         "PMIX_GROUP_ENDPT_DATA",
                         NULL}},
    {.function = "PMIx_Group_invite", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_invite_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_join", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_join_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_leave", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_leave_nb", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Group_destruct", .attrs = (char *[]){"PMIX_EMBED_BARRIER", NULL}},
    {.function = "PMIx_Group_destruct_nb", .attrs = (char *[]){"PMIX_EMBED_BARRIER", NULL}},
    {.function = "PMIx_Register_event_handler", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Deregister_event_handler", .attrs = (char *[]){"N/A", NULL}},
    {.function = "PMIx_Notify_event", .attrs = (char *[]){"NONE", NULL}},
    {.function = "PMIx_Allocate_resources",
     .attrs = (char *[]){"PMIX_ALLOC_REQ_ID",
                         "PMIX_ALLOC_NUM_NODES",
                         "PMIX_ALLOC_NODE_LIST",
                         "PMIX_ALLOC_NUM_CPUS",
                         "PMIX_ALLOC_NUM_CPU_LIST",
                         "PMIX_ALLOC_CPU_LIST",
                         "PMIX_ALLOC_MEM_SIZE",
                         "PMIX_ALLOC_TIME",
                         "PMIX_ALLOC_QUEUE",
                         "PMIX_ALLOC_PREEMPTIBLE",
                         NULL}},
#if PMIX_NUMERIC_VERSION >= 0x00050000
    {.function = "PMIx_Session_control",
     .attrs = (char *[]){"PMIX_SESSION_CTRL_ID",
                         "PMIX_SESSION_APP",
                         "PMIX_SESSION_PAUSE",
                         "PMIX_SESSION_RESUME",
                         "PMIX_SESSION_TERMINATE",
                         "PMIX_SESSION_PREEMPT",
                         "PMIX_SESSION_RESTORE",
                         "PMIX_SESSION_SIGNAL",
                         "PMIX_SESSION_COMPLETE",
                         NULL}},
#endif
    {.function = ""},
};

static void send_error(int status, pmix_proc_t *idreq, pmix_proc_t *remote, int remote_room);
static void _mdxresp(int sd, short args, void *cbdata);
static void modex_resp(pmix_status_t status, char *data, size_t sz, void *cbdata);

static char *generate_dist = "fabric,gpu,network";
void pmix_server_register_params(void)
{
    int i;

    /* register a verbosity */
    prte_pmix_server_globals.verbosity = -1;
    (void) pmix_mca_base_var_register("prte", "pmix", NULL, "server_verbose",
                                      "Debug verbosity for PMIx server",
                                      PMIX_MCA_BASE_VAR_TYPE_INT,
                                      &prte_pmix_server_globals.verbosity);
    if (0 <= prte_pmix_server_globals.verbosity) {
        prte_pmix_server_globals.output = pmix_output_open(NULL);
        pmix_output_set_verbosity(prte_pmix_server_globals.output,
                                  prte_pmix_server_globals.verbosity);
    }

    /* whether or not to wait for the universal server */
    prte_pmix_server_globals.wait_for_server = false;
    (void)
        pmix_mca_base_var_register("prte", "pmix", NULL, "wait_for_server",
                                   "Whether or not to wait for the session-level server to start",
                                   PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                   &prte_pmix_server_globals.wait_for_server);

    /* whether or not to drop a session-level tool rendezvous point */
    prte_pmix_server_globals.session_server = false;
    (void) pmix_mca_base_var_register("prte", "pmix", NULL, "session_server",
                                      "Whether or not to drop a session-level tool rendezvous point",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &prte_pmix_server_globals.session_server);

    /* whether or not to drop a system-level tool rendezvous point */
    prte_pmix_server_globals.system_server = false;
    (void) pmix_mca_base_var_register("prte", "pmix", NULL, "system_server",
                                      "Whether or not to drop a system-level tool rendezvous point",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &prte_pmix_server_globals.system_server);

    /* whether or not to drop a system-level tool rendezvous point */
    (void) pmix_mca_base_var_register("prte", "pmix", NULL, "generate_distances",
                                      "Device types whose distances are to be provided (default=none, options=fabric,gpu,network",
                                      PMIX_MCA_BASE_VAR_TYPE_STRING,
                                      &generate_dist);
    prte_pmix_server_globals.generate_dist = 0;
    if (NULL != generate_dist) {
        char **tmp = PMIX_ARGV_SPLIT_COMPAT(generate_dist, ',');
        for (i=0; NULL != tmp[i]; i++) {
            if (0 == strcasecmp(tmp[i], "fabric")) {
                prte_pmix_server_globals.generate_dist |= PMIX_DEVTYPE_OPENFABRICS;
            } else if (0 == strcasecmp(tmp[i], "gpu")) {
                prte_pmix_server_globals.generate_dist |= PMIX_DEVTYPE_GPU;
            } else if (0 == strcasecmp(tmp[i], "network")) {
                prte_pmix_server_globals.generate_dist |= PMIX_DEVTYPE_NETWORK;
            }
        }
        PMIX_ARGV_FREE_COMPAT(tmp);
    }

    prte_pmix_server_globals.system_controller = false;
    (void) pmix_mca_base_var_register("prte", "pmix", NULL, "system_controller",
                                      "Whether or not to act as the system-wide controller",
                                      PMIX_MCA_BASE_VAR_TYPE_BOOL,
                                      &prte_pmix_server_globals.system_server);

}

static void timeout_cbfunc(int sd, short args, void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t*)cbdata;
    PRTE_HIDE_UNUSED_PARAMS(sd, args);

    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "REQUEST TIMED OUT - LOCAL REFID %d REMOTE REFID %d",
                        req->local_index, req->remote_index);

    /* mark that we timed out */
    req->timed_out = true;

    /* don't let the caller hang */
    if (0 <= req->remote_index) {
        /* this was a remote request */
        send_error(PMIX_ERR_TIMEOUT, &req->tproc, &req->proxy, req->remote_index);
        /* note: we cannot release the req object because whomever
         * we gave it to (host or PMIx server library) is still
         * using it. We'll take care of it once the current holder
         * returns.
         */
        return;
    }

    /* this was a local request */
    if (NULL != req->opcbfunc) {
        req->opcbfunc(PMIX_ERR_TIMEOUT, req->cbdata);
    } else if (NULL != req->mdxcbfunc) {
        req->mdxcbfunc(PMIX_ERR_TIMEOUT, NULL, 0, req->cbdata, NULL, NULL);
    } else if (NULL != req->spcbfunc) {
        req->spcbfunc(PMIX_ERR_TIMEOUT, NULL, req->cbdata);
    } else if (NULL != req->lkcbfunc) {
        req->lkcbfunc(PMIX_ERR_TIMEOUT, NULL, 0, req->cbdata);
    }
    /* note: we cannot release the req object because whomever
     * we gave it to (host or PMIx server library) is still
     * using it. We'll take care of it once the current holder
     * returns.
     */
}

/* NOTE: this function must be called from within an event! */
void prte_pmix_server_clear(pmix_proc_t *pname)
{
    int n;
    pmix_server_req_t *req;

    for (n = 0; n < prte_pmix_server_globals.remote_reqs.size; n++) {
        req = (pmix_server_req_t*)pmix_pointer_array_get_item(&prte_pmix_server_globals.remote_reqs, n);
        if (NULL != req) {
            if (!PMIX_CHECK_NSPACE(req->tproc.nspace, pname->nspace) ||
                !PMIX_CHECK_RANK(req->tproc.rank, pname->rank)) {
                continue;
            }
            /* delete the timeout event, if active */
            if (req->event_active) {
                prte_event_del(&req->ev);
            }
            /* delete the cycle event, if active */
            if (req->cycle_active) {
                prte_event_del(&req->cycle);
            }
            pmix_pointer_array_set_item(&prte_pmix_server_globals.remote_reqs, n, NULL);
            if (!req->inprogress) {
                /* if the request is in progress, then someone (host or PMIx server
                 * library) has the req object - so we cannot release it yet.
                 * We'll take care of it once the current holder returns.
                 * If that isn't the case, then release it */
                PMIX_RELEASE(req);
            }
        }
    }
}

/* provide a callback function for lost connections to allow us
 * to cleanup after any tools once they depart */
static void lost_connection_hdlr(size_t evhdlr_registration_id, pmix_status_t status,
                                 const pmix_proc_t *source, pmix_info_t info[], size_t ninfo,
                                 pmix_info_t *results, size_t nresults,
                                 pmix_event_notification_cbfunc_fn_t cbfunc, void *cbdata)
{
    prte_pmix_tool_t *tl;
    PRTE_HIDE_UNUSED_PARAMS(evhdlr_registration_id, status,
                            info, ninfo, results, nresults);

    /* scan the list of attached tools to see if this one is there */
    PMIX_LIST_FOREACH(tl, &prte_pmix_server_globals.tools, prte_pmix_tool_t)
    {
        if (PMIX_CHECK_PROCID(&tl->name, source)) {
            /* take this tool off the list */
            pmix_list_remove_item(&prte_pmix_server_globals.tools, &tl->super);
            /* release it */
            PMIX_RELEASE(tl);
            break;
        }
    }

    /* we _always_ have to execute the evhandler callback or
     * else the event progress engine will hang */
    if (NULL != cbfunc) {
        cbfunc(PMIX_EVENT_ACTION_COMPLETE, NULL, 0, NULL, NULL, cbdata);
    }
}

static void regcbfunc(pmix_status_t status, size_t ref, void *cbdata)
{
    prte_pmix_lock_t *lock = (prte_pmix_lock_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(status, ref);

    PMIX_ACQUIRE_OBJECT(lock);
    lock->status = status;
    PRTE_PMIX_WAKEUP_THREAD(lock);
}

/*
 * Initialize global variables used w/in the server.
 */
int pmix_server_init(void)
{
    int rc;
    void *ilist;
    pmix_data_array_t darray;
    pmix_info_t *info, myinf;
    size_t n, ninfo;
    pmix_value_t *val;
    char *tmp;
    pmix_status_t prc;
    prte_pmix_lock_t lock;
    bool flag;

    if (prte_pmix_server_globals.initialized) {
        return PRTE_SUCCESS;
    }
    prte_pmix_server_globals.initialized = true;

    /* setup the server's state variables */
    PMIX_CONSTRUCT(&prte_pmix_server_globals.psets, pmix_list_t);
    PMIX_CONSTRUCT(&prte_pmix_server_globals.groups, pmix_list_t);
    PMIX_CONSTRUCT(&prte_pmix_server_globals.tools, pmix_list_t);
    PMIX_CONSTRUCT(&prte_pmix_server_globals.local_reqs, pmix_pointer_array_t);
    pmix_pointer_array_init(&prte_pmix_server_globals.local_reqs, 128, INT_MAX, 2);
    PMIX_CONSTRUCT(&prte_pmix_server_globals.remote_reqs, pmix_pointer_array_t);
    pmix_pointer_array_init(&prte_pmix_server_globals.remote_reqs, 128, INT_MAX, 2);
    PMIX_CONSTRUCT(&prte_pmix_server_globals.notifications, pmix_list_t);
    prte_pmix_server_globals.server = *PRTE_NAME_INVALID;
    prte_pmix_server_globals.scheduler_connected = false;
    prte_pmix_server_globals.scheduler_set_as_server = false;

    PMIX_INFO_LIST_START(ilist);

    /* tell the server our hostname so we agree on it */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_HOSTNAME, prte_process_info.nodename, PMIX_STRING);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

#ifdef PMIX_EXTERNAL_AUX_EVENT_BASE
    /* give the server our event base to use for signal trapping */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_EXTERNAL_AUX_EVENT_BASE, prte_event_base, PMIX_POINTER);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }
#endif

    /* if PMIx is version 4 or higher, then we can pass our
     * topology object down to the server library for its use
     * and for passing to any local clients */
    mytopology.source = "hwloc";
    mytopology.topology = prte_hwloc_topology;
    PMIX_INFO_CONSTRUCT(&myinf);
    PMIX_LOAD_KEY(myinf.key, PMIX_TOPOLOGY2);
    myinf.value.type = PMIX_TOPO;
    myinf.value.data.topo = &mytopology;
    PMIX_INFO_LIST_INSERT(prc, ilist, &myinf);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }
    // tell the server to share this topology for us
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_SHARE_TOPOLOGY, NULL, PMIX_BOOL);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

    /* tell the server our temp directory */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_TMPDIR,
                       prte_process_info.top_session_dir,
                       PMIX_STRING);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

    /* tell the server to use its own internal monitoring */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_ENABLE_MONITORING,
                       NULL, PMIX_BOOL);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }
    /* if requested, tell the server to drop a session-level
     * PMIx connection point */
    if (prte_pmix_server_globals.session_server) {
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_TOOL_SUPPORT,
                           NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != prc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
    }

    if (PRTE_PROC_IS_MASTER) {
        /* if requested, tell the server to drop a system-level
         * PMIx connection point - only do this for the HNP as, in
         * at least one case, a daemon can be colocated with the
         * HNP and would overwrite the server rendezvous file */
        if (prte_pmix_server_globals.system_server) {
            PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_SYSTEM_SUPPORT,
                               NULL, PMIX_BOOL);
            if (PMIX_SUCCESS != prc) {
                PMIX_INFO_LIST_RELEASE(ilist);
                rc = prte_pmix_convert_status(prc);
                return rc;
            }
        }
#ifdef PMIX_SERVER_SYS_CONTROLLER
        /* if requested, tell the server that we are the system
         * controller */
        if (prte_pmix_server_globals.system_controller) {
            PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_SYS_CONTROLLER,
                               NULL, PMIX_BOOL);
            if (PMIX_SUCCESS != prc) {
                PMIX_INFO_LIST_RELEASE(ilist);
                rc = prte_pmix_convert_status(prc);
                return rc;
            }
        }
#endif
    }

    /* if requested, tell the server library to output our PMIx URI */
    if (NULL != prte_pmix_server_globals.report_uri) {
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_TCP_REPORT_URI,
                           prte_pmix_server_globals.report_uri, PMIX_STRING);
        if (PMIX_SUCCESS != prc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
    }

    /* if we were started to support a singleton, then let the server library know */
    if (NULL != prte_pmix_server_globals.singleton) {
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SINGLETON,
                           prte_pmix_server_globals.singleton, PMIX_STRING);
        if (PMIX_SUCCESS != prc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
    }

    if (NULL != prte_progress_thread_cpus) {
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_BIND_PROGRESS_THREAD,
                           prte_progress_thread_cpus, PMIX_STRING);
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_BIND_REQUIRED,
                           &prte_bind_progress_thread_reqd, PMIX_BOOL);
    }

    /* if we are the MASTER, then we are the gateway */
    if (PRTE_PROC_IS_MASTER) {
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_GATEWAY, NULL, PMIX_BOOL);
        if (PMIX_SUCCESS != prc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
        /* if we are also persistent, then we do not output IOF ourselves */
        if (prte_persistent) {
            flag = false;
#ifdef PMIX_SERVER_CONTROLLER
            // declare ourselves the system controller
            PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_SYS_CONTROLLER, NULL, PMIX_BOOL);
            if (PMIX_SUCCESS != prc) {
                PMIX_INFO_LIST_RELEASE(ilist);
                rc = prte_pmix_convert_status(prc);
                return rc;
            }
#endif
        } else {
            /* if we have a parent or we are in persistent mode, then we
             * don't write out ourselves */
            if (NULL != getenv("PMIX_LAUNCHER_RNDZ_URI") || prte_persistent) {
                flag = false;
            } else {
                flag = true;
            }
        }
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_IOF_LOCAL_OUTPUT, &flag, PMIX_BOOL);
        if (PMIX_SUCCESS != prc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
    } else {
        /* prted's never locally output */
        flag = false;
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_IOF_LOCAL_OUTPUT, &flag, PMIX_BOOL);
        if (PMIX_SUCCESS != prc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
    }

    /* PRTE always allows remote tool connections */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_REMOTE_CONNECTIONS, &flag, PMIX_BOOL);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

    /* if we were launched by a debugger, then we need to have
     * notification of our termination sent */
    if (PRTE_PROC_IS_MASTER && NULL != getenv("PMIX_LAUNCHER_PAUSE_FOR_TOOL")) {
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_EVENT_SILENT_TERMINATION, &flag, PMIX_BOOL);
        if (PMIX_SUCCESS != prc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
     }

    /* tell the server what we are doing with FQDN */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_HOSTNAME_KEEP_FQDN, &prte_keep_fqdn_hostnames, PMIX_BOOL);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

    /* tell the server our name so we agree on our identifier */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_NSPACE, prte_process_info.myproc.nspace, PMIX_STRING);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_SERVER_RANK, &prte_process_info.myproc.rank, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

    /* convert to an info array */
    PMIX_INFO_LIST_CONVERT(prc, ilist, &darray);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }
    PMIX_INFO_LIST_RELEASE(ilist);
    info = (pmix_info_t*)darray.array;
    ninfo = darray.size;

    /* setup the local server */
    if (PMIX_SUCCESS != (prc = PMIx_server_init(&pmix_server, info, ninfo))) {
        /* pmix will provide a nice show_help output here */
        PMIX_INFO_FREE(info, ninfo);
        return prte_pmix_convert_status(prc);
    }
    PMIX_INFO_FREE(info, ninfo);
    rc = PRTE_SUCCESS;

#ifdef PMIX_VERSION_NUMERIC
    /* find out what version of PMIx is being used - note that
     * it is NOT an error to not be able to retrieve this
     * value as it just means the PMIx library pre-dates
     * introduction of the ability to retrieve the version */
    prc = PMIx_Get(NULL, PMIX_VERSION_NUMERIC, NULL, 0, &val);
    if (PMIX_SUCCESS == prc) {
        // check the version
        if (val->data.uint32 < PRTE_PMIX_MINIMUM_VERSION) {
            pmix_show_help("help-prted.txt", "min-pmix-violation", true,
                           PRTE_PMIX_MINIMUM_VERSION, val->data.uint32);
            PMIX_VALUE_RELEASE(val);
            return PRTE_ERR_SILENT;
        }
        PMIX_VALUE_RELEASE(val);
    }
#endif

    /* register our support */
    for (n = 0; 0 != strlen(prte_attributes[n].function); n++) {
        prc = PMIx_Register_attributes(prte_attributes[n].function, prte_attributes[n].attrs);
        if (PMIX_SUCCESS != prc) {
            return prte_pmix_convert_status(prc);
        }
    }
    /* register our local resources */
    PMIX_INFO_LIST_START(ilist);

    /* register our hostname so everyone agrees on it */
    PMIX_INFO_LIST_ADD(prc, ilist, PMIX_HOSTNAME, prte_process_info.nodename, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

    // check for aliases
    if (NULL != prte_process_info.aliases) {
        tmp = PMIX_ARGV_JOIN_COMPAT(prte_process_info.aliases, ',');
        PMIX_INFO_LIST_ADD(prc, ilist, PMIX_HOSTNAME_ALIASES, tmp, PMIX_STRING);
        free(tmp);
        if (PMIX_SUCCESS != rc) {
            PMIX_INFO_LIST_RELEASE(ilist);
            rc = prte_pmix_convert_status(prc);
            return rc;
        }
    }
    /* convert to an info array */
    PMIX_INFO_LIST_CONVERT(prc, ilist, &darray);
    if (PMIX_SUCCESS != prc) {
        PMIX_INFO_LIST_RELEASE(ilist);
        rc = prte_pmix_convert_status(prc);
        return rc;
    }

    PMIX_INFO_LIST_RELEASE(ilist);
    info = (pmix_info_t*)darray.array;
    ninfo = darray.size;
    prc = PMIx_server_register_resources(info, ninfo, NULL, NULL);
    PMIX_INFO_FREE(info, ninfo);
    rc = prte_pmix_convert_status(prc);

    /* register the "lost-connection" event handler */
    PRTE_PMIX_CONSTRUCT_LOCK(&lock);
    prc = PMIX_ERR_LOST_CONNECTION;
    PMIx_Register_event_handler(&prc, 1, NULL, 0, lost_connection_hdlr, regcbfunc, &lock);
    PRTE_PMIX_WAIT_THREAD(&lock);
    prc = lock.status;
    PRTE_PMIX_DESTRUCT_LOCK(&lock);
    rc = prte_pmix_convert_status(prc);

    return rc;
}

void pmix_server_start(void)
{
    /* setup our local data server */
    prte_data_server_init();

    /* setup recv for direct modex requests */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DIRECT_MODEX,
                  PRTE_RML_PERSISTENT, pmix_server_dmdx_recv, NULL);

    /* setup recv for replies to direct modex requests */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DIRECT_MODEX_RESP,
                  PRTE_RML_PERSISTENT, pmix_server_dmdx_resp, NULL);

    /* setup recv for replies to proxy launch requests */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_LAUNCH_RESP,
                  PRTE_RML_PERSISTENT, pmix_server_launch_resp, NULL);

    /* setup recv for replies from data server */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DATA_CLIENT,
                  PRTE_RML_PERSISTENT, pmix_server_keyval_client, NULL);

    /* setup recv for notifications */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_NOTIFICATION,
                  PRTE_RML_PERSISTENT, pmix_server_notify, NULL);

    /* setup recv for jobid return */
    PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_JOBID_RESP,
                  PRTE_RML_PERSISTENT, pmix_server_jobid_return, NULL);

    if (PRTE_PROC_IS_MASTER) {
        /* setup recv for logging requests */
        PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_LOGGING,
                      PRTE_RML_PERSISTENT, pmix_server_log, NULL);
        /* setup recv for scheduler requests */
        PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_SCHED,
                      PRTE_RML_PERSISTENT, pmix_server_sched, NULL);
    }
}

void pmix_server_finalize(void)
{
    if (!prte_pmix_server_globals.initialized) {
        return;
    }

    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "%s Finalizing PMIX server",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));

    /* stop receives */
    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DIRECT_MODEX);
    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DIRECT_MODEX_RESP);
    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_LAUNCH_RESP);
    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_DATA_CLIENT);
    PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_NOTIFICATION);
    if (PRTE_PROC_IS_MASTER) {
        PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_LOGGING);
        PRTE_RML_CANCEL(PRTE_NAME_WILDCARD, PRTE_RML_TAG_SCHED);
    }

    /* finalize our local data server */
    prte_data_server_finalize();

    /* cleanup collectives */
    pmix_server_req_t *cd;
    for (int i = 0; i < prte_pmix_server_globals.local_reqs.size; i++) {
      cd = (pmix_server_req_t*)pmix_pointer_array_get_item(&prte_pmix_server_globals.local_reqs, i);
      if (NULL != cd) {
          PMIX_RELEASE(cd);
      }
    }
    for (int i = 0; i < prte_pmix_server_globals.remote_reqs.size; i++) {
      cd = (pmix_server_req_t*)pmix_pointer_array_get_item(&prte_pmix_server_globals.remote_reqs, i);
      if (NULL != cd) {
          PMIX_RELEASE(cd);
      }
    }

    PMIX_DESTRUCT(&prte_pmix_server_globals.remote_reqs);
    PMIX_DESTRUCT(&prte_pmix_server_globals.local_reqs);
    PMIX_LIST_DESTRUCT(&prte_pmix_server_globals.notifications);
    PMIX_LIST_DESTRUCT(&prte_pmix_server_globals.psets);
    PMIX_LIST_DESTRUCT(&prte_pmix_server_globals.groups);

    /* shutdown the local server */
    prte_pmix_server_globals.initialized = false;
}

static void send_error(int status, pmix_proc_t *idreq, pmix_proc_t *remote, int remote_index)
{
    pmix_data_buffer_t *reply;
    pmix_status_t prc, pstatus;

    /* pack the status */
    pstatus = prte_pmix_convert_rc(status);
    PMIX_DATA_BUFFER_CREATE(reply);
    if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, reply, &pstatus, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
        return;
    }
    /* pack the id of the requested proc */
    if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, reply, idreq, 1, PMIX_PROC))) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
        return;
    }

    /* pack the remote daemon's request index */
    if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, reply, &remote_index, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
        return;
    }

    /* send the response */
    PRTE_RML_SEND(prc, remote->rank, reply, PRTE_RML_TAG_DIRECT_MODEX_RESP);
    if (PRTE_SUCCESS != prc) {
        PRTE_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
    }
}

static void _mdxresp(int sd, short args, void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t *) cbdata;
    pmix_data_buffer_t *reply;
    pmix_status_t prc;
    PRTE_HIDE_UNUSED_PARAMS(sd, args);

    PMIX_ACQUIRE_OBJECT(req);

    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "%s XMITTING DATA FOR PROC %s:%u",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        req->tproc.nspace, req->tproc.rank);

    /* remove us from the pending array */
    pmix_pointer_array_set_item(&prte_pmix_server_globals.remote_reqs, req->local_index, NULL);

    /* pack the status */
    PMIX_DATA_BUFFER_CREATE(reply);
    if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, reply, &req->pstatus, 1, PMIX_STATUS))) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
        goto error;
    }
    /* pack the id of the requested proc */
    if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, reply, &req->tproc, 1, PMIX_PROC))) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
        goto error;
    }

    /* pack the remote daemon's request index */
    if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, reply, &req->remote_index, 1, PMIX_INT))) {
        PMIX_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
        goto error;
    }
    if (PMIX_SUCCESS == req->pstatus) {
        /* return any provided data */
        if (PMIX_SUCCESS != (prc = PMIx_Data_pack(NULL, reply, &req->sz, 1, PMIX_SIZE))) {
            PMIX_ERROR_LOG(prc);
            PMIX_DATA_BUFFER_RELEASE(reply);
            goto error;
        }
        if (0 < req->sz) {
            if (PMIX_SUCCESS
                != (prc = PMIx_Data_pack(NULL, reply, req->data, req->sz, PMIX_BYTE))) {
                PMIX_ERROR_LOG(prc);
                PMIX_DATA_BUFFER_RELEASE(reply);
                goto error;
            }
            free(req->data);
        }
    }

    /* send the response */
    PRTE_RML_SEND(prc, req->proxy.rank, reply, PRTE_RML_TAG_DIRECT_MODEX_RESP);
    if (PRTE_SUCCESS != prc) {
        PRTE_ERROR_LOG(prc);
        PMIX_DATA_BUFFER_RELEASE(reply);
    }

error:
    PMIX_RELEASE(req);
    return;
}

/* the modex_resp function takes place in the local PMIx server's
 * progress thread - we must therefore thread-shift it so we can
 * access our global data */
static void modex_resp(pmix_status_t status, char *data, size_t sz, void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t *) cbdata;

    PMIX_ACQUIRE_OBJECT(req);

    /* clear any timeout event */
    if (req->event_active) {
        prte_event_del(&req->ev);
        req->event_active = false;
    }
    if (req->cycle_active) {
        prte_event_del(&req->cycle);
        req->cycle_active = false;
    }

    req->inprogress = false; // we are done processing this request
    req->pstatus = status;
    if (PMIX_SUCCESS == status && NULL != data) {
        /* we need to preserve the data as the caller
         * will free it upon our return */
        req->data = (char *) malloc(sz);
        if (NULL == req->data) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        }
        memcpy(req->data, data, sz);
        req->sz = sz;
    }
    prte_event_set(prte_event_base, &(req->ev), -1, PRTE_EV_WRITE, _mdxresp, req);
    PMIX_POST_OBJECT(req);
    prte_event_active(&(req->ev), PRTE_EV_WRITE, 1);
}

static void dmdx_check(int sd, short args, void *cbdata)
{
    pmix_server_req_t *req = (pmix_server_req_t*)cbdata;
    prte_job_t *jdata;
    prte_proc_t *proc;
    struct timeval tv = {2, 0};
    pmix_value_t *pval = NULL;
    pmix_status_t rc;
    PRTE_HIDE_UNUSED_PARAMS(sd, args);

    /* do we know about this job? */
    jdata = prte_get_job_data_object(req->tproc.nspace);
    if (NULL == jdata) {
        /* wait some more */
        pmix_output_verbose(2, prte_pmix_server_globals.output,
                            "%s dmdx:recv dmdx_check cannot find job object - delaying",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        PMIX_POST_OBJECT(req);
        prte_event_evtimer_add(&req->cycle, &tv);
        return;
    }

    /* we know about this job - look for the proc */
    proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, req->tproc.rank);
    if (NULL == proc) {
        /* this is truly an error, so notify the sender */
        send_error(PRTE_ERR_NOT_FOUND, &req->tproc, &req->proxy, req->remote_index);
        if (req->event_active) {
            /* delete the timeout event */
            prte_event_del(&req->ev);
        }
        pmix_pointer_array_set_item(&prte_pmix_server_globals.remote_reqs, req->local_index, NULL);
        PMIX_RELEASE(req);
        return;
    }
    if (!PRTE_FLAG_TEST(proc, PRTE_PROC_FLAG_LOCAL)) {
        /* send back an error - they obviously have made a mistake */
        send_error(PRTE_ERR_NOT_FOUND, &req->tproc, &req->proxy, req->remote_index);
        if (req->event_active) {
            /* delete the timeout event */
            prte_event_del(&req->ev);
        }
        pmix_pointer_array_set_item(&prte_pmix_server_globals.remote_reqs, req->local_index, NULL);
        PMIX_RELEASE(req);
        return;
    }

    if (NULL != req->key) {
        pmix_output_verbose(2, prte_pmix_server_globals.output,
                            "%s dmdx:check for key %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), req->key);
        /* see if we have it */
        if (PMIX_SUCCESS != PMIx_Get(&req->tproc, req->key, req->info, req->ninfo, &pval)) {
            pmix_output_verbose(2, prte_pmix_server_globals.output,
                                "%s dmdx:recv key %s not found - resetting wait",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), req->key);
            PMIX_POST_OBJECT(req);
            prte_event_evtimer_add(&req->cycle, &tv);
            return;
        }
        PMIX_VALUE_RELEASE(pval);
        /* we do have it, so fetch payload */
    }

    /* ask our local PMIx server for the data */
    req->inprogress = true;
    rc = PMIx_server_dmodex_request(&req->tproc, modex_resp, req);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        req->inprogress = false;
        send_error(rc, &req->tproc, &req->proxy, req->remote_index);
        if (req->event_active) {
            /* delete the timeout event */
            prte_event_del(&req->ev);
        }
        pmix_pointer_array_set_item(&prte_pmix_server_globals.remote_reqs, req->local_index, NULL);
        PMIX_RELEASE(req);
        return;
    }
    return;
}

static void pmix_server_dmdx_recv(int status, pmix_proc_t *sender,
                                  pmix_data_buffer_t *buffer,
                                  prte_rml_tag_t tg, void *cbdata)
{
    int rc, index;
    int32_t cnt, timeout = 0;
    struct timeval tv = {0, 0};
    prte_job_t *jdata;
    prte_proc_t *proc;
    pmix_server_req_t *req;
    pmix_proc_t pproc;
    pmix_status_t prc;
    pmix_info_t *info = NULL, *iptr;
    size_t ninfo;
    char *key = NULL;
    size_t sz, n, refreshidx;
    bool refresh_cache = false;
    pmix_value_t *pval = NULL;
    PRTE_HIDE_UNUSED_PARAMS(status, tg, cbdata);

    cnt = 1;
    if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, &pproc, &cnt, PMIX_PROC))) {
        PMIX_ERROR_LOG(prc);
        return;
    }
    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "%s dmdx:recv processing request from proc %s for proc %s:%u",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender), pproc.nspace,
                        pproc.rank);
    /* and the remote daemon's tracking index */
    cnt = 1;
    if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, &index, &cnt, PMIX_INT))) {
        PMIX_ERROR_LOG(prc);
        return;
    }
    cnt = 1;
    if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, &ninfo, &cnt, PMIX_SIZE))) {
        PMIX_ERROR_LOG(prc);
        return;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, info, &cnt, PMIX_INFO))) {
            PMIX_ERROR_LOG(prc);
            return;
        }
    }

    /* see if they want us to await a particular key before sending
     * the response */
    if (NULL != info) {
        for (sz = 0; sz < ninfo; sz++) {
            if (PMIX_CHECK_KEY(&info[sz], PMIX_REQUIRED_KEY)) {
                key = strdup(info[sz].value.data.string);
                continue;
            }
            if (PMIX_CHECK_KEY(&info[sz], PMIX_TIMEOUT)) {
                PMIX_VALUE_GET_NUMBER(prc, &info[sz].value, timeout, int32_t);
                if (PMIX_SUCCESS != prc) {
                    PMIX_ERROR_LOG(prc);
                    if (NULL != info) {
                        PMIX_INFO_FREE(info, ninfo);
                    }
                    return;
                }
                continue;
            }
            if (PMIX_CHECK_KEY(&info[sz], PMIX_GET_REFRESH_CACHE)) {
                refresh_cache = PMIX_INFO_TRUE(&info[sz]);
                refreshidx = sz;
                continue;
            }
        }
    }

    if (refresh_cache) {
        if (1 < ninfo) {
            // need to remove the refresh cache key to avoid loops
            PMIX_INFO_CREATE(iptr, ninfo - 1);
            sz = 0;
            for (n = 0; n < ninfo; n++) {
                if (n == refreshidx) {
                    continue;
                }
                PMIX_INFO_XFER(&iptr[sz], &info[n]);
                ++sz;
            }
            PMIX_INFO_FREE(info, ninfo);
            info = iptr;
            ninfo = sz;
        } else {
            // refresh was the only key
            PMIX_INFO_FREE(info, ninfo);
            info = NULL;
            ninfo = 0;
        }
    }

    /* do we know about this job? */
    jdata = prte_get_job_data_object(pproc.nspace);
    if (NULL == jdata) {
        /* not having the jdata means that we haven't unpacked the
         * the launch message for this job yet - this is a race
         * condition, so just log the request and we will fill
         * it later */
        pmix_output_verbose(2, prte_pmix_server_globals.output,
                            "%s dmdx:recv request cannot find job object - delaying",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        req = PMIX_NEW(pmix_server_req_t);
        pmix_asprintf(&req->operation, "DMDX: %s:%d", __FILE__, __LINE__);
        req->proxy = *sender;
        memcpy(&req->tproc, &pproc, sizeof(pmix_proc_t));
        req->info = info;
        req->ninfo = ninfo;
        if (NULL != key) {
            req->key = key;
            key = NULL;
        }
        /* store THEIR index to the request */
        req->remote_index = index;
        /* store it in my remote reqs, assigning the index in that array
         * to the req->local_index as this is MY index to the request */
        req->local_index = pmix_pointer_array_add(&prte_pmix_server_globals.remote_reqs, req);

        /* setup the cycle timer so we periodically wake up and try again */
        prte_event_evtimer_set(prte_event_base, &req->cycle, dmdx_check, req);
        req->cycle_active = true;
        PMIX_POST_OBJECT(req);
        tv.tv_sec = 2;
        prte_event_evtimer_add(&req->cycle, &tv);

        /* if they asked for a timeout, then set that too */
        if (0 < timeout) {
            prte_event_evtimer_set(prte_event_base, &req->ev, timeout_cbfunc, req);
            req->event_active = true;
            PMIX_POST_OBJECT(req);
            tv.tv_sec = timeout;
            prte_event_evtimer_add(&req->cycle, &tv);
        }
        return;
    }

    /* we know about this job - look for the proc */
    proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, pproc.rank);
    if (NULL == proc) {
        /* this is truly an error, so notify the sender */
        send_error(PRTE_ERR_NOT_FOUND, &pproc, sender, index);
        return;
    }
    if (!PRTE_FLAG_TEST(proc, PRTE_PROC_FLAG_LOCAL)) {
        /* send back an error - they obviously have made a mistake */
        send_error(PRTE_ERR_NOT_FOUND, &pproc, sender, index);
        return;
    }

    if (NULL != key) {
        pmix_output_verbose(2, prte_pmix_server_globals.output,
                            "%s dmdx:recv checking for key %s",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), key);
        /* see if we have it */
        if (PMIX_SUCCESS != PMIx_Get(&pproc, key, info, ninfo, &pval)) {
            pmix_output_verbose(2, prte_pmix_server_globals.output,
                                "%s dmdx:recv key %s not found - delaying",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), key);
            /* we don't - wait for awhile */
            req = PMIX_NEW(pmix_server_req_t);
            pmix_asprintf(&req->operation, "DMDX: %s:%d", __FILE__, __LINE__);
            req->proxy = *sender;
            memcpy(&req->tproc, &pproc, sizeof(pmix_proc_t));
            req->info = info;
            req->ninfo = ninfo;
            req->key = key;
            key = NULL;
            req->remote_index = index;
            /* store it in my remote reqs, assigning the index in that array
             * to the req->local_index as this is MY index to the request */
            req->local_index = pmix_pointer_array_add(&prte_pmix_server_globals.remote_reqs, req);

            /* setup the cycle timer so we periodically wake up and try again */
            prte_event_evtimer_set(prte_event_base, &req->cycle, dmdx_check, req);
            req->cycle_active = true;
            PMIX_POST_OBJECT(req);
            tv.tv_sec = 2;
            prte_event_evtimer_add(&req->cycle, &tv);

            /* if they asked for a timeout, then set that too */
            if (0 < timeout) {
                prte_event_evtimer_set(prte_event_base, &req->ev, timeout_cbfunc, req);
                req->event_active = true;
                PMIX_POST_OBJECT(req);
                tv.tv_sec = timeout;
                prte_event_evtimer_add(&req->ev, &tv);
            }
            return;
        }
        /* we do already have it, so go get the payload */
        PMIX_VALUE_RELEASE(pval);
        pmix_output_verbose(2, prte_pmix_server_globals.output,
                            "%s dmdx:recv key %s found - retrieving payload",
                            PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), key);
    }

    if (NULL != key) {
        free(key);
        key = NULL;
    }

    /* track the request since the call down to the PMIx server
     * is asynchronous */
    req = PMIX_NEW(pmix_server_req_t);
    pmix_asprintf(&req->operation, "DMDX: %s:%d", __FILE__, __LINE__);
    req->proxy = *sender;
    memcpy(&req->tproc, &pproc, sizeof(pmix_proc_t));
    req->info = info;
    req->ninfo = ninfo;
    req->remote_index = index;
    /* store it in my remote reqs, assigning the index in that array
     * to the req->local_index as this is MY index to the request */
    req->local_index = pmix_pointer_array_add(&prte_pmix_server_globals.remote_reqs, req);

    /* if they asked for a timeout, then set that too */
    if (0 < timeout) {
        prte_event_evtimer_set(prte_event_base, &req->ev, timeout_cbfunc, req);
        req->event_active = true;
        PMIX_POST_OBJECT(req);
        tv.tv_sec = timeout;
        prte_event_evtimer_add(&req->ev, &tv);
    }

    /* ask our local PMIx server for the data */
    req->inprogress = true;
    prc = PMIx_server_dmodex_request(&pproc, modex_resp, req);
    if (PMIX_SUCCESS != prc) {
        PMIX_ERROR_LOG(prc);
        if (req->event_active) {
            /* delete the timeout event */
            prte_event_del(&req->ev);
        }
        if (req->cycle_active) {
            prte_event_del(&req->cycle);
        }
        req->inprogress = false;
        pmix_pointer_array_set_item(&prte_pmix_server_globals.remote_reqs, req->local_index, NULL);
        send_error(rc, &pproc, sender, index);
        return;
    }
    return;
}

typedef struct {
    pmix_object_t super;
    char *data;
    int32_t ndata;
} datacaddy_t;
static void dccon(datacaddy_t *p)
{
    p->data = NULL;
    p->ndata = 0;
}
static void dcdes(datacaddy_t *p)
{
    if (NULL != p->data) {
        free(p->data);
    }
}
static PMIX_CLASS_INSTANCE(datacaddy_t,
                           pmix_object_t,
                           dccon, dcdes);

static void relcbfunc(void *relcbdata)
{
    datacaddy_t *d = (datacaddy_t *) relcbdata;

    PMIX_RELEASE(d);
}

static void pmix_server_dmdx_resp(int status, pmix_proc_t *sender,
                                  pmix_data_buffer_t *buffer,
                                  prte_rml_tag_t tg, void *cbdata)
{
    int index, n;
    int32_t cnt;
    pmix_server_req_t *req;
    datacaddy_t *d;
    pmix_proc_t pproc;
    size_t psz;
    pmix_status_t prc, pret;
    PRTE_HIDE_UNUSED_PARAMS(status, tg, cbdata);

    pmix_output_verbose(2, prte_pmix_server_globals.output,
                        "%s dmdx:recv response recvd from proc %s with %d bytes",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender),
                        (int) buffer->bytes_used);

    d = PMIX_NEW(datacaddy_t);

    /* unpack the status */
    cnt = 1;
    if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, &pret, &cnt, PMIX_STATUS))) {
        PMIX_ERROR_LOG(prc);
        PMIX_RELEASE(d);
        return;
    }

    /* unpack the id of the target whose info we just received */
    cnt = 1;
    if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, &pproc, &cnt, PMIX_PROC))) {
        PMIX_ERROR_LOG(prc);
        PMIX_RELEASE(d);
        return;
    }

    /* unpack our tracking index */
    cnt = 1;
    if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, &index, &cnt, PMIX_INT))) {
        PMIX_ERROR_LOG(prc);
        PMIX_RELEASE(d);
        return;
    }

    /* unload the remainder of the buffer */
    if (PMIX_SUCCESS == pret) {
        cnt = 1;
        if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, &psz, &cnt, PMIX_SIZE))) {
            PMIX_ERROR_LOG(prc);
            PMIX_RELEASE(d);
            return;
        }
        if (0 < psz) {
            d->ndata = psz;
            d->data = (char *) malloc(psz);
            if (NULL == d->data) {
                PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            }
            cnt = psz;
            if (PMIX_SUCCESS != (prc = PMIx_Data_unpack(NULL, buffer, d->data, &cnt, PMIX_BYTE))) {
                PMIX_ERROR_LOG(prc);
                PMIX_RELEASE(d);
                return;
            }
        }
    }

    /* get the request out of the tracking array */
    req = (pmix_server_req_t*)pmix_pointer_array_get_item(&prte_pmix_server_globals.local_reqs, index);
    /* return the returned data to the requestor */
    if (NULL != req) {
        if (NULL != req->mdxcbfunc) {
            PMIX_RETAIN(d);
            req->mdxcbfunc(pret, d->data, d->ndata, req->cbdata, relcbfunc, d);
        }
        pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, index, NULL);
        PMIX_RELEASE(req);
    } else {
        pmix_output_verbose(2, prte_pmix_server_globals.output,
                            "REQ WAS NULL IN ARRAY INDEX %d",
                            index);
    }

    /* now see if anyone else was waiting for data from this target */
    for (n = 0; n < prte_pmix_server_globals.local_reqs.size; n++) {
        req = (pmix_server_req_t*)pmix_pointer_array_get_item(&prte_pmix_server_globals.local_reqs, n);
        if (NULL == req) {
            continue;
        }
        if (PMIX_CHECK_PROCID(&req->tproc, &pproc)) {
            if (NULL != req->mdxcbfunc) {
                PMIX_RETAIN(d);
                req->mdxcbfunc(pret, d->data, d->ndata, req->cbdata, relcbfunc, d);
            }
            pmix_pointer_array_set_item(&prte_pmix_server_globals.local_reqs, n, NULL);
            PMIX_RELEASE(req);
        }
    }
    PMIX_RELEASE(d); // maintain accounting
}


static void log_cbfunc(pmix_status_t status, void *cbdata)
{
    prte_pmix_server_op_caddy_t *scd = (prte_pmix_server_op_caddy_t *) cbdata;

    if (PMIX_SUCCESS != status && PMIX_OPERATION_SUCCEEDED != status) {
        pmix_output(prte_pmix_server_globals.output, "LOG FAILED");
    }
    if (NULL != scd->info) {
        PMIX_INFO_FREE(scd->info, scd->ninfo);
    }
    if (NULL != scd->directives) {
        PMIX_INFO_FREE(scd->directives, scd->ndirs);
    }
    PMIX_RELEASE(scd);
}


static void pmix_server_log(int status, pmix_proc_t *sender,
                            pmix_data_buffer_t *buffer,
                            prte_rml_tag_t tg, void *cbdata)
{
    int rc;
    int32_t cnt;
    size_t n, ninfo, ndirs;
    pmix_info_t *info;
    pmix_status_t ret;
    pmix_byte_object_t boptr;
    pmix_data_buffer_t pbkt;
    prte_pmix_server_op_caddy_t *scd;
    pmix_proc_t source;
    prte_job_t *jdata;
    bool noagg;
    bool flag;
    PRTE_HIDE_UNUSED_PARAMS(status, sender, tg, cbdata);

    /* unpack the source of the request */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &source, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    /* look up the job for this source */
    jdata = prte_get_job_data_object(source.nspace);
    if (NULL == jdata) {
        /* should never happen */
        PMIX_ERROR_LOG(PMIX_ERR_NOT_FOUND);
        return;
    }
    noagg = prte_get_attribute(&jdata->attributes, PRTE_JOB_NOAGG_HELP, NULL, PMIX_BOOL);

    /* unpack the number of info */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

   /* unpack the number of directives */
   cnt = 1;
   rc = PMIx_Data_unpack(NULL, buffer, &ndirs, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    PMIX_BYTE_OBJECT_CONSTRUCT(&boptr);
    /* unpack the info blob */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &boptr, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    PMIX_INFO_CREATE(info, ninfo);
    PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);
    rc = PMIx_Data_load(&pbkt, &boptr);
    for (n = 0; n < ninfo; n++) {
        cnt = 1;
        ret = PMIx_Data_unpack(NULL, &pbkt, (void *) &info[n], &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(info, ninfo);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_BYTE_OBJECT_DESTRUCT(&boptr);
            return;
        }
    }
    PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
    PMIX_BYTE_OBJECT_DESTRUCT(&boptr);

    PMIX_BYTE_OBJECT_CONSTRUCT(&boptr);
    /* unpack the directives blob */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &boptr, &cnt, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_BYTE_OBJECT_CONSTRUCT(&boptr);
        PMIX_ERROR_LOG(rc);
        return;
    }

    scd = PMIX_NEW(prte_pmix_server_op_caddy_t);
    /* if we are not going to aggregate, then indicate so */
    if (noagg) {
        scd->ndirs = ndirs + 3;
    } else {
        scd->ndirs = ndirs + 2;  // need to locally add two directives
    }
    PMIX_INFO_CREATE(scd->directives, scd->ndirs);
    PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);
    rc = PMIx_Data_load(&pbkt, &boptr);
    for (n = 0; n < ndirs; n++) {
        cnt = 1;
        ret = PMIx_Data_unpack(NULL, &pbkt, (void *) &scd->directives[n], &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_INFO_FREE(scd->directives, scd->ndirs);
            PMIX_RELEASE(scd);
            PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
            PMIX_BYTE_OBJECT_CONSTRUCT(&boptr);
            return;
        }
    }
    PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
    PMIX_BYTE_OBJECT_CONSTRUCT(&boptr);

    /* indicate that only ONE PMIx log component should handle this request */
    PMIX_INFO_LOAD(&scd->directives[ndirs], PMIX_LOG_ONCE, NULL, PMIX_BOOL);
    /* protect against infinite loop should the PMIx server push
     * this back up to us */
    PMIX_INFO_LOAD(&scd->directives[ndirs+1], "prte.log.noloop", NULL, PMIX_BOOL);
    if (noagg) {
        flag = false;
        PMIX_INFO_LOAD(&scd->directives[ndirs+2], PMIX_LOG_AGG, &flag, PMIX_BOOL);
    }
    scd->info = info;
    scd->ninfo = ninfo;
    /* pass the array down to be logged */
    rc = PMIx_Log_nb(scd->info, scd->ninfo, scd->directives, scd->ndirs, log_cbfunc, scd);
    if (PMIX_SUCCESS != rc) {
        if (NULL != scd->info) {
            PMIX_INFO_FREE(scd->info, scd->ninfo);
        }
        if (NULL != scd->directives) {
            PMIX_INFO_FREE(scd->directives, scd->ndirs);
        }
        PMIX_RELEASE(scd);
    }
}

static void pmix_server_sched(int status, pmix_proc_t *sender,
                              pmix_data_buffer_t *buffer,
                              prte_rml_tag_t tg, void *cbdata)
{
    pmix_status_t rc;
    uint8_t cmd;
    int32_t cnt;
    size_t ninfo;
    pmix_alloc_directive_t allocdir;
    uint32_t sessionID;
    pmix_info_t *info = NULL;
    pmix_proc_t source;
    pmix_server_req_t *req;
    int refid;
    PRTE_HIDE_UNUSED_PARAMS(status, sender, tg, cbdata);

    /* unpack the command */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &cmd, &cnt, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }

    /* unpack the reference ID for this request */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &refid, &cnt, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return;
    }
    /* now that we have the remote refID, we can at least send
     * a reply that the remote end can understand */

    /* unpack the source of the request */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &source, &cnt, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        goto reply;
    }

    if (0 == cmd) {
        /* allocation request - unpack the directive */
        cnt = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &allocdir, &cnt, PMIX_ALLOC_DIRECTIVE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto reply;
        }
    } else {
        /* session control request */
        cnt = 1;
        rc = PMIx_Data_unpack(NULL, buffer, &sessionID, &cnt, PMIX_UINT32);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            goto reply;
        }
    }

   /* unpack the number of info */
   cnt = 1;
   rc = PMIx_Data_unpack(NULL, buffer, &ninfo, &cnt, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
            goto reply;
    }
    if (0 < ninfo) {
        PMIX_INFO_CREATE(info, ninfo);
        cnt = ninfo;
        rc = PMIx_Data_unpack(NULL, buffer, info, &cnt, PMIX_INFO);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_INFO_FREE(info, ninfo);
            goto reply;
        }
    }

    if (!prte_pmix_server_globals.scheduler_connected) {
        /* the scheduler has not attached to us - there is
         * nothing we can do */
        rc = PMIX_ERR_NOT_SUPPORTED;
        if (NULL != info) {
            PMIX_INFO_FREE(info, ninfo);
        }
        goto reply;
    }

    /* if we have not yet set the scheduler as our server, do so */
    if (!prte_pmix_server_globals.scheduler_set_as_server) {
        rc = PMIx_tool_set_server(&prte_pmix_server_globals.scheduler, NULL, 0);
        if (PMIX_SUCCESS != rc) {
            if (NULL != info) {
                PMIX_INFO_FREE(info, ninfo);
            }
            goto reply;
        }
        prte_pmix_server_globals.scheduler_set_as_server = true;
    }

    /* track the request */
    req = PMIX_NEW(pmix_server_req_t);

    if (0 == cmd) {
        rc = PMIx_Allocation_request_nb(allocdir, info, ninfo,
                                        req->infocbfunc, req);
    } else {
#if PMIX_NUMERIC_VERSION < 0x00050000
        rc = PMIX_ERR_NOT_SUPPORTED;
#else
        rc = PMIx_Session_control(sessionID, info, ninfo,
                                  req->infocbfunc, req);
#endif
    }
    if (PMIX_SUCCESS != rc) {
        if (NULL != info) {
            PMIX_INFO_FREE(info, ninfo);
        }
        goto reply;
    }
    return;

reply:
    /* send an error response */

    return;

}

int pmix_server_cache_job_info(prte_job_t *jdata, pmix_info_t *info)
{
    prte_info_item_t *kv;
    pmix_list_t *cache;

    /* cache for inclusion with job info at registration */
    kv = PMIX_NEW(prte_info_item_t);
    PMIX_INFO_XFER(&kv->info, info);
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_INFO_CACHE,
                           (void **) &cache, PMIX_POINTER)) {
        pmix_list_append(cache, &kv->super);
    } else {
        cache = PMIX_NEW(pmix_list_t);
        pmix_list_append(cache, &kv->super);
        prte_set_attribute(&jdata->attributes, PRTE_JOB_INFO_CACHE, PRTE_ATTR_GLOBAL,
                           (void *) cache, PMIX_POINTER);

    }
    return 0;
}

/****    INSTANTIATE LOCAL OBJECTS    ****/
static void opcon(prte_pmix_server_op_caddy_t *p)
{
    memset(&p->proct, 0, sizeof(pmix_proc_t));
    p->procs = NULL;
    p->nprocs = 0;
    p->eprocs = NULL;
    p->neprocs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->directives = NULL;
    p->ndirs = 0;
    p->apps = NULL;
    p->napps = 0;
    p->cbfunc = NULL;
    p->allocdir = 0;
    p->sessionID = UINT32_MAX;
    p->infocbfunc = NULL;
    p->toolcbfunc = NULL;
    p->spcbfunc = NULL;
    p->cbdata = NULL;
    p->server_object = NULL;
}
PMIX_CLASS_INSTANCE(prte_pmix_server_op_caddy_t,
                    pmix_object_t,
                    opcon, NULL);

static void rqcon(pmix_server_req_t *p)
{
    p->event_active = false;
    p->cycle_active = false;
    p->inprogress = false;
    p->timed_out = false;
    p->operation = NULL;
    p->cmdline = NULL;
    p->key = NULL;
    p->flag = true;
    p->launcher = false;
    p->scheduler = false;
    p->local_index = -1;
    p->remote_index = -1;
    p->uid = 0;
    p->gid = 0;
    p->pid = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->data = NULL;
    p->sz = 0;
    p->range = PMIX_RANGE_SESSION;
    p->proxy = *PRTE_NAME_INVALID;
    p->target = *PRTE_NAME_INVALID;
    p->jdata = NULL;
    PMIX_DATA_BUFFER_CONSTRUCT(&p->msg);
    p->timeout = prte_pmix_server_globals.timeout;
    p->opcbfunc = NULL;
    p->mdxcbfunc = NULL;
    p->spcbfunc = NULL;
    p->lkcbfunc = NULL;
    p->rlcbfunc = NULL;
    p->toolcbfunc = NULL;
    p->infocbfunc = NULL;
    p->cbdata = NULL;
}
static void rqdes(pmix_server_req_t *p)
{
    if (NULL != p->operation) {
        free(p->operation);
    }
    if (NULL != p->cmdline) {
        free(p->cmdline);
    }
    if (NULL != p->key) {
        free(p->key);
    }
    if (NULL != p->jdata) {
        PMIX_RELEASE(p->jdata);
    }
    PMIX_DATA_BUFFER_DESTRUCT(&p->msg);
}
PMIX_CLASS_INSTANCE(pmix_server_req_t,
                    pmix_object_t,
                    rqcon, rqdes);

static void mdcon(prte_pmix_mdx_caddy_t *p)
{
    p->sig = NULL;
    p->buf = NULL;
    PMIX_BYTE_OBJECT_CONSTRUCT(&p->ctrls);
    p->procs = NULL;
    p->nprocs = 0;
    p->info = NULL;
    p->ninfo = 0;
    p->cbdata = NULL;
    p->grpcbfunc = NULL;
    p->mdxcbfunc = NULL;
    p->infocbfunc = NULL;
    p->opcbfunc = NULL;
}
static void mddes(prte_pmix_mdx_caddy_t *p)
{
    if (NULL != p->sig) {
        PMIX_RELEASE(p->sig);
    }
    if (NULL != p->buf) {
        PMIX_DATA_BUFFER_RELEASE(p->buf);
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&p->ctrls);
}
PMIX_CLASS_INSTANCE(prte_pmix_mdx_caddy_t,
                    pmix_object_t,
                    mdcon, mddes);

static void pscon(pmix_server_pset_t *p)
{
    p->name = NULL;
    p->members = NULL;
    p->num_members = 0;
}
static void psdes(pmix_server_pset_t *p)
{
    if (NULL != p->name) {
        free(p->name);
    }
    if (NULL != p->members) {
        free(p->members);
    }
}
PMIX_CLASS_INSTANCE(pmix_server_pset_t,
                    pmix_list_item_t,
                    pscon, psdes);

PMIX_CLASS_INSTANCE(prte_pmix_tool_t,
                    pmix_list_item_t,
                    NULL, NULL);
