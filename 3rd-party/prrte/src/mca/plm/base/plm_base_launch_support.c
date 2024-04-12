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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2009      Institut National de Recherche en Informatique
 *                         et Automatique. All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC.
 * Copyright (c) 2013-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2016-2020 IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2023      Advanced Micro Devices, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "prte_config.h"
#include "constants.h"

#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */
#include <ctype.h>

#include "src/class/pmix_pointer_array.h"
#include "src/hwloc/hwloc-internal.h"
#include "src/include/hash_string.h"
#include "src/pmix/pmix-internal.h"
#include "src/prted/pmix/pmix_server.h"

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/ess/ess.h"
#include "src/mca/filem/base/base.h"
#include "src/mca/filem/filem.h"
#include "src/mca/grpcomm/base/base.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/odls/odls_types.h"
#include "src/mca/ras/base/base.h"
#include "src/mca/rmaps/base/base.h"
#include "src/mca/rmaps/rmaps.h"
#include "src/rml/rml_contact.h"
#include "src/rml/rml.h"
#include "src/mca/rtc/rtc.h"
#include "src/mca/state/base/base.h"
#include "src/mca/state/state.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_locks.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/runtime.h"
#include "src/threads/pmix_threads.h"
#include "src/util/dash_host/dash_host.h"
#include "src/util/hostfile/hostfile.h"
#include "src/util/pmix_argv.h"
#include "src/util/name_fns.h"
#include "src/util/pmix_net.h"
#include "src/util/nidmap.h"
#include "src/util/pmix_printf.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_environ.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/plm/base/base.h"
#include "src/mca/plm/base/plm_private.h"

void prte_plm_base_set_slots(prte_node_t *node)
{
    if (0 == strncmp(prte_set_slots, "cores", strlen(prte_set_slots))) {
        if (NULL != node->topology && NULL != node->topology->topo) {
            node->slots = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                             HWLOC_OBJ_CORE, 0);
        }
    } else if (0 == strncmp(prte_set_slots, "sockets", strlen(prte_set_slots))) {
        if (NULL != node->topology && NULL != node->topology->topo) {
            node->slots = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                             HWLOC_OBJ_SOCKET, 0);
            if (0 == node->slots) {
                /* some systems don't report sockets - in this case,
                 * use numanodes */
                node->slots = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                                 HWLOC_OBJ_NUMANODE, 0);
            }
        }
    } else if (0 == strncmp(prte_set_slots, "numas", strlen(prte_set_slots))) {
        if (NULL != node->topology && NULL != node->topology->topo) {
            node->slots = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                             HWLOC_OBJ_NUMANODE, 0);
        }
    } else if (0 == strncmp(prte_set_slots, "hwthreads", strlen(prte_set_slots))) {
        if (NULL != node->topology && NULL != node->topology->topo) {
            node->slots = prte_hwloc_base_get_nbobjs_by_type(node->topology->topo,
                                                             HWLOC_OBJ_PU, 0);
        }
    } else {
        /* must be a number */
        node->slots = strtol(prte_set_slots, NULL, 10);
    }
    /* mark the node as having its slots "given" */
    PRTE_FLAG_SET(node, PRTE_NODE_FLAG_SLOTS_GIVEN);
}

void prte_plm_base_daemons_reported(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_topology_t *t;
    prte_node_t *node;
    int i;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* if we are not launching, then we just assume that all
     * daemons share our topology */
    if (prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL) &&
        PMIX_CHECK_NSPACE(caddy->jdata->nspace, PRTE_PROC_MY_NAME->nspace)) {
        node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);
        t = node->topology;
        for (i = 1; i < prte_node_pool->size; i++) {
            if (NULL == (node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i))) {
                continue;
            }
            if (NULL == node->topology) {
                node->topology = t;
                node->available = prte_hwloc_base_filter_cpus(node->topology->topo);
            }
            node->state = PRTE_NODE_STATE_UP;
        }
    }

    /* if this is an unmanaged allocation, then set the default
     * slots on each node as directed or using default
     */
    if (!prte_managed_allocation || prte_set_slots_override) {
        caddy->jdata->total_slots_alloc = 0;
        for (i = 0; i < prte_node_pool->size; i++) {
            node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i);
            if (NULL == node) {
                continue;
            }
            if (!PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_SLOTS_GIVEN)) {
                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                     "%s plm:base:setting slots for node %s by %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name,
                                     prte_set_slots));
                prte_plm_base_set_slots(node);
            }
            caddy->jdata->total_slots_alloc += node->slots;
        }
    } else {
        /* for managed allocations, the total slots allocated is fixed at time of allocation */
        caddy->jdata->total_slots_alloc = prte_ras_base.total_slots_alloc;
    }

    if (prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_DISPLAY_ALLOC, NULL, PMIX_BOOL)) {
        prte_ras_base_display_alloc(caddy->jdata);
    }

    /* progress the job */
    caddy->jdata->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
    PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_VM_READY);

    /* cleanup */
    PMIX_RELEASE(caddy);
}

void prte_plm_base_allocation_complete(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_node_t *node;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* if we don't want to launch, then we at least want
     * to map so we can see where the procs would have
     * gone - so skip to the mapping state */
    if (prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
        node = (prte_node_t*)pmix_pointer_array_get_item(prte_node_pool, 0);
        prte_rmaps_base.require_hwtcpus = !prte_hwloc_base_core_cpus(node->topology->topo);
    } else {
        /* move the state machine along */
        caddy->jdata->state = PRTE_JOB_STATE_ALLOCATION_COMPLETE;
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_LAUNCH_DAEMONS);
    }

    /* cleanup */
    PMIX_RELEASE(caddy);
}

void prte_plm_base_daemons_launched(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* do NOT increment the state - we wait for the
     * daemons to report that they have actually
     * started before moving to the right state
     */
    /* cleanup */
    PMIX_RELEASE(caddy);
}

static void files_ready(int status, void *cbdata)
{
    prte_job_t *jdata = (prte_job_t *) cbdata;

    if (PRTE_SUCCESS != status) {
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FILES_POSN_FAILED);
    } else {
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_MAP);
    }
}

void prte_plm_base_vm_ready(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_node_t *node;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* progress the job */
    caddy->jdata->state = PRTE_JOB_STATE_VM_READY;

    /* check the first daemon's node for topology
     * limitations - or the HNP's node if we didn't
     * launch any daemons */
    node = (prte_node_t*)pmix_pointer_array_get_item(prte_node_pool, 1);
    if (NULL == node) {
        node = (prte_node_t*)pmix_pointer_array_get_item(prte_node_pool, 0);
    }
    if (NULL != node && NULL != node->topology &&
        NULL != node->topology->topo) {
        prte_rmaps_base.require_hwtcpus = !prte_hwloc_base_core_cpus(node->topology->topo);
    }

    /* position any required files */
    if (PRTE_SUCCESS != prte_filem.preposition_files(caddy->jdata, files_ready, caddy->jdata)) {
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_FILES_POSN_FAILED);
    }

    /* cleanup */
    PMIX_RELEASE(caddy);
}

void prte_plm_base_mapping_complete(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* move the state machine along */
    caddy->jdata->state = PRTE_JOB_STATE_MAP_COMPLETE;
    PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_SYSTEM_PREP);

    /* cleanup */
    PMIX_RELEASE(caddy);
}

/* catch spawn timeout */
static void spawn_timeout_cb(int fd, short event, void *cbdata)
{
    prte_job_t *jdata = (prte_job_t *) cbdata;
    prte_timer_t *timer = NULL;
    pmix_proc_t proc;
    int timeout, *tp;
    char *st;
    pmix_byte_object_t bo;
    PRTE_HIDE_UNUSED_PARAMS(fd, event);

    PMIX_ACQUIRE_OBJECT(jdata);

    /* Display a useful message to the user */
    tp = &timeout;
    if (!prte_get_attribute(&jdata->attributes, PRTE_SPAWN_TIMEOUT, (void **) &tp, PMIX_INT)) {
        /* This shouldn't happen, but at least don't segv / display
         *something* if it does */
        timeout = -1;
    }
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT_EVENT, (void **) &timer, PMIX_POINTER)) {
        prte_event_evtimer_del(timer->ev);
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:launch deleting timeout for job %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace)));
        PMIX_RELEASE(timer);
        prte_remove_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT_EVENT);
    }
    pmix_asprintf(&st, "--------------------------------------------------------------------------\n"
                       "The user-provided time limit for job launch has been reached:\n\n"
                       "  Timeout: %d seconds\n\n"
                       "The job will now be aborted.  Please check your environment to\n"
                       "identify the source of the delay and try again.\n"
                       "--------------------------------------------------------------------------\n",
                  timeout);
    bo.bytes = st;
    bo.size = strlen(st);
    PMIX_LOAD_PROCID(&proc, jdata->nspace, PMIX_RANK_WILDCARD);
    PMIx_server_IOF_deliver(&proc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
    free(st);

    /* abort the job */
    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FAILED_TO_START);
    jdata->exit_code = PRTE_ERR_TIMEOUT;

    if (!prte_persistent) {
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_TIMEOUT);
    }
}

static void stack_trace_recv(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                             prte_rml_tag_t tag, void *cbdata)
{
    pmix_byte_object_t pbo;
    pmix_data_buffer_t blob;
    char *st, *st2;
    int32_t cnt;
    pmix_proc_t name;
    char *hostname, *nspace;
    pid_t pid;
    prte_job_t *jdata = NULL;
    prte_timer_t *timer;
    prte_proc_t proc;
    pmix_pointer_array_t parray;
    int rc;
    pmix_byte_object_t bo;

    PMIX_DATA_BUFFER_CONSTRUCT(&blob);
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    pmix_output_verbose(5, prte_plm_base_framework.framework_output,
                        "%s: stacktrace recvd from %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PRTE_NAME_PRINT(sender));

    /* unpack the stack_trace blob */
    cnt = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &nspace, &cnt, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_DESTRUCT(&blob);
        return;
    }
    jdata = prte_get_job_data_object(nspace);
    if (NULL == jdata) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        free(nspace);
        return;
    }
    free(nspace);

    while (PMIX_SUCCESS == (rc = PMIx_Data_unpack(NULL, buffer, &pbo, &cnt, PMIX_BYTE_OBJECT))) {
        PMIx_Data_load(&blob, &pbo);
        /* first piece is the name of the process */
        cnt = 1;
        rc = PMIx_Data_unpack(NULL, &blob, &name, &cnt, PMIX_PROC);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&blob);
            goto DONE;
        }
        rc = PMIx_Data_unpack(NULL, &blob, &hostname, &cnt, PMIX_STRING);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&blob);
            goto DONE;
        }
        rc = PMIx_Data_unpack(NULL, &blob, &pid, &cnt, PMIX_PID);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&blob);
            goto DONE;
        }
        pmix_asprintf(&st, "STACK TRACE FOR PROC %s (%s, PID %lu)\n",
                      PRTE_NAME_PRINT(&name), hostname,
                      (unsigned long) pid);
        PMIX_ARGV_APPEND_NOSIZE_COMPAT(&jdata->traces, st);
        free(hostname);
        free(st);
        /* unpack the stack_trace until complete */
        cnt = 1;
        while (PRTE_SUCCESS == (rc = PMIx_Data_unpack(NULL, &blob, &st, &cnt, PMIX_STRING))) {
            pmix_asprintf(&st2, "\t%s", st); // has its own newline
            PMIX_ARGV_APPEND_NOSIZE_COMPAT(&jdata->traces, st2);
            free(st);
            free(st2);
            cnt = 1;
        }
        if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            PMIX_ERROR_LOG(rc);
        }
        PMIX_DATA_BUFFER_DESTRUCT(&blob);
        cnt = 1;
    }
    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        PMIX_ERROR_LOG(rc);
    }

DONE:
    jdata->ntraces++;
    if (prte_process_info.num_daemons == jdata->ntraces) {
        timer = NULL;
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_TRACE_TIMEOUT_EVENT,
                               (void **) &timer, PMIX_POINTER) &&
            NULL != timer) {
            prte_event_evtimer_del(timer->ev);
            /* timer is an prte_timer_t object */
            PMIX_RELEASE(timer);
            prte_remove_attribute(&jdata->attributes, PRTE_JOB_TRACE_TIMEOUT_EVENT);
        }
        /* output the results - note that the output might need to go to a
         * tool instead of just to stderr, so we use the PMIx IOF deliver
         * function to ensure it gets where it needs to go */
        PMIX_LOAD_PROCID(&name, jdata->nspace, PMIX_RANK_WILDCARD);
        for (cnt=0; NULL != jdata->traces[cnt]; cnt++) {
            bo.bytes = jdata->traces[cnt];
            bo.size = strlen(jdata->traces[cnt]);
            PMIx_server_IOF_deliver(&name, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
        }
        /* abort the job */
        PMIX_CONSTRUCT(&parray, pmix_pointer_array_t);
        /* create an object */
        PMIX_LOAD_PROCID(&proc.name, jdata->nspace, PMIX_RANK_WILDCARD);
        cnt = pmix_pointer_array_add(&parray, &proc);
        if (PRTE_SUCCESS != (rc = prte_plm.terminate_procs(&parray))) {
            PRTE_ERROR_LOG(rc);
        }
        PMIX_DESTRUCT(&parray);
    }
}

static void stack_trace_timeout(int sd, short args, void *cbdata)
{
    prte_timer_t *timer;
    prte_job_t *jdata = (prte_job_t *) cbdata;
    prte_proc_t proc;
    pmix_pointer_array_t parray;
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(sd, args);

    /* clear the timer */
    timer = NULL;
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT_EVENT, (void **) &timer, PMIX_POINTER)
        && NULL != timer) {
        prte_event_evtimer_del(timer->ev);
        /* timer is an prte_timer_t object */
        PMIX_RELEASE(timer);
        prte_remove_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT_EVENT);
    }

    /* abort the job */
    PMIX_CONSTRUCT(&parray, pmix_pointer_array_t);
    /* create an object */
    PMIX_LOAD_PROCID(&proc.name, jdata->nspace, PMIX_RANK_WILDCARD);
    pmix_pointer_array_add(&parray, &proc);
    if (PRTE_SUCCESS != (rc = prte_plm.terminate_procs(&parray))) {
        PRTE_ERROR_LOG(rc);
    }
    PMIX_DESTRUCT(&parray);
}

static void job_timeout_cb(int fd, short event, void *cbdata)
{
    prte_job_t *jdata = (prte_job_t *) cbdata;
    prte_timer_t *timer = NULL;
    prte_proc_t *proc, prc;
    pmix_proc_t pc;
    int i, rc, timeout, *tp;
    pmix_pointer_array_t parray;
    pmix_byte_object_t bo;
    char *st;
    PRTE_HIDE_UNUSED_PARAMS(fd, event);

    PMIX_ACQUIRE_OBJECT(jdata);

    /* Display a useful message to the user */
    tp = &timeout;
    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_TIMEOUT, (void **) &tp, PMIX_INT)) {
        /* This shouldn't happen, but at least don't segv / display
         *something* if it does */
        timeout = -1;
    }
    pmix_asprintf(&st, "--------------------------------------------------------------------------\n"
                       "The user-provided time limit for job execution has been reached:\n\n"
                       "  Timeout: %d seconds\n\n"
                       "The job will now be aborted.  Please check your code and/or\n"
                       "adjust/remove the job execution time limit (as specified by --timeout\n"
                       "command line option or MPIEXEC_TIMEOUT environment variable).\n"
                       "--------------------------------------------------------------------------\n",
                  timeout);
    bo.bytes = st;
    bo.size = strlen(st);
    PMIX_LOAD_PROCID(&pc, jdata->nspace, PMIX_RANK_WILDCARD);
    PMIx_server_IOF_deliver(&pc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
    free(st);
    PRTE_UPDATE_EXIT_STATUS(PRTE_ERR_TIMEOUT);

    /* see if they want proc states reported */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_REPORT_STATE, NULL, PMIX_BOOL)) {
        /* output the results - note that the output might need to go to a
         * tool instead of just to stderr, so we use the PMIx IOF deliver
         * function to ensure it gets where it needs to go */
        pmix_asprintf(&st, "DATA FOR JOB: %s\n", PRTE_JOBID_PRINT(jdata->nspace));
        bo.bytes = st;
        bo.size = strlen(st);
        PMIx_server_IOF_deliver(&pc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
        free(st);
        pmix_asprintf(&st, "\tNum apps: %d\tNum procs: %d\tJobState: %s\tAbort: %s\n",
                      (int) jdata->num_apps, (int) jdata->num_procs, prte_job_state_to_str(jdata->state),
                      (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) ? "True" : "False");
        bo.bytes = st;
        bo.size = strlen(st);
        PMIx_server_IOF_deliver(&pc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
        free(st);
        pmix_asprintf(&st, "\tNum launched: %ld\tNum reported: %ld\tNum terminated: %ld\n\n\tProcs:\n",
                      (long) jdata->num_launched, (long) jdata->num_reported,
                      (long) jdata->num_terminated);
        bo.bytes = st;
        bo.size = strlen(st);
        PMIx_server_IOF_deliver(&pc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
        free(st);
        for (i = 0; i < jdata->procs->size; i++) {
            if (NULL != (proc = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, i))) {
                pmix_asprintf(&st, "\t\tRank: %s\tNode: %s\tPID: %u\tState: %s\tExitCode %d\n",
                              PRTE_VPID_PRINT(proc->name.rank),
                              (NULL == proc->node) ? "UNKNOWN" : proc->node->name,
                              (unsigned int) proc->pid, prte_proc_state_to_str(proc->state),
                              proc->exit_code);
                bo.bytes = st;
                bo.size = strlen(st);
                PMIx_server_IOF_deliver(&pc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
                free(st);
            }
        }
        st = "\n";
        bo.bytes = st;
        bo.size = strlen(st);
        PMIx_server_IOF_deliver(&pc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);
    }

    /* see if they want stacktraces */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_STACKTRACES, NULL, PMIX_BOOL)) {
        /* if they asked for stack_traces, attempt to get them, but timeout
         * if we cannot do so */
        prte_daemon_cmd_flag_t command = PRTE_DAEMON_GET_STACK_TRACES;
        pmix_data_buffer_t buffer;
        prte_grpcomm_signature_t *sig;

        bo.bytes = "Waiting for stack traces (this may take a few moments)...\n";
        bo.size = strlen(bo.bytes);
        PMIx_server_IOF_deliver(&pc, PMIX_FWD_STDERR_CHANNEL, &bo, NULL, 0, NULL, NULL);

        /* set the recv */
        PRTE_RML_RECV(PRTE_NAME_WILDCARD, PRTE_RML_TAG_STACK_TRACE,
                      PRTE_RML_PERSISTENT, stack_trace_recv, NULL);

        /* setup the buffer */
        PMIX_DATA_BUFFER_CONSTRUCT(&buffer);
        /* pack the command */
        rc = PMIx_Data_pack(NULL, &buffer, &command, 1, PMIX_UINT8);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&buffer);
            goto giveup;
        }
        /* pack the jobid */
        rc = PMIx_Data_pack(NULL, &buffer, &jdata->nspace, 1, PMIX_PROC_NSPACE);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&buffer);
            goto giveup;
        }
        /* goes to all daemons */
        sig = PMIX_NEW(prte_grpcomm_signature_t);
        sig->signature = (pmix_proc_t *) malloc(sizeof(pmix_proc_t));
        PMIX_LOAD_PROCID(&sig->signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
        sig->sz = 1;
        if (PRTE_SUCCESS != (rc = prte_grpcomm.xcast(sig, PRTE_RML_TAG_DAEMON, &buffer))) {
            PRTE_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_DESTRUCT(&buffer);
            goto giveup;
        }
        PMIX_DATA_BUFFER_DESTRUCT(&buffer);
        /* maintain accounting */
        PMIX_RELEASE(sig);
        /* we will terminate after we get the stack_traces, but set a timeout
         * just in case we never hear back from everyone */
        if (prte_stack_trace_wait_timeout > 0) {
            timer = PMIX_NEW(prte_timer_t);
            prte_event_evtimer_set(prte_event_base, timer->ev, stack_trace_timeout, jdata);
            timer->tv.tv_sec = prte_stack_trace_wait_timeout;
            timer->tv.tv_usec = 0;
            prte_set_attribute(&jdata->attributes, PRTE_JOB_TRACE_TIMEOUT_EVENT,
                               PRTE_ATTR_LOCAL, timer, PMIX_POINTER);
            PMIX_POST_OBJECT(timer);
            prte_event_evtimer_add(timer->ev, &timer->tv);
        }
        return;
    }

giveup:
    /* abort the job */
    PMIX_CONSTRUCT(&parray, pmix_pointer_array_t);
    PMIX_LOAD_PROCID(&prc.name, jdata->nspace, PMIX_RANK_WILDCARD);
    pmix_pointer_array_add(&parray, &prc);
    if (PRTE_SUCCESS != (rc = prte_plm.terminate_procs(&parray))) {
        PRTE_ERROR_LOG(rc);
    }
    PMIX_DESTRUCT(&parray);
}


void prte_plm_base_setup_job(int fd, short args, void *cbdata)
{
    int rc;
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_timer_t *timer = NULL;
    int time, *tp;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:setup_job",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    if (PRTE_JOB_STATE_INIT != caddy->job_state) {
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
        PMIX_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* start by getting a jobid */
    if (PMIX_NSPACE_INVALID(caddy->jdata->nspace)) {
        if (PRTE_SUCCESS != (rc = prte_plm_base_create_jobid(caddy->jdata))) {
            PRTE_ERROR_LOG(rc);
            PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
            PMIX_RELEASE(caddy);
            return;
        }
    }

    /* if the spawn operation has a timeout assigned to it, setup the timer for it */
    tp = &time;
    if (prte_get_attribute(&caddy->jdata->attributes, PRTE_SPAWN_TIMEOUT, (void **) &tp, PMIX_INT)) {
        /* setup a timer to monitor execution time */
        timer = PMIX_NEW(prte_timer_t);
        timer->payload = caddy->jdata;
        prte_event_evtimer_set(prte_event_base, timer->ev, spawn_timeout_cb, caddy->jdata);
        timer->tv.tv_sec = time;
        timer->tv.tv_usec = 0;
        prte_set_attribute(&caddy->jdata->attributes, PRTE_SPAWN_TIMEOUT_EVENT, PRTE_ATTR_LOCAL, timer, PMIX_POINTER);
        PMIX_POST_OBJECT(timer);
        prte_event_evtimer_add(timer->ev, &timer->tv);
    }

    /* if the overall job has a timeout assigned to it, setup the timer for it */
    tp = &time;
    if (prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_TIMEOUT, (void **) &tp, PMIX_INT)) {
        /* setup a timer to monitor execution time */
        timer = PMIX_NEW(prte_timer_t);
        timer->payload = caddy->jdata;
        prte_event_evtimer_set(prte_event_base, timer->ev, job_timeout_cb, caddy->jdata);
        timer->tv.tv_sec = time;
        timer->tv.tv_usec = 0;
        prte_set_attribute(&caddy->jdata->attributes, PRTE_JOB_TIMEOUT_EVENT, PRTE_ATTR_LOCAL, timer, PMIX_POINTER);
        PMIX_POST_OBJECT(timer);
        prte_event_evtimer_add(timer->ev, &timer->tv);
    }

    // if we are not going to launch this job, then ensure we output something - otherwise,
    // we will simply silently exit
    if (prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL) &&
        !prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_DISPLAY_MAP, NULL, PMIX_BOOL) &&
        !prte_get_attribute(&caddy->jdata->attributes, PRTE_JOB_DISPLAY_DEVEL_MAP, NULL, PMIX_BOOL)) {
        pmix_output(0, "SETTING");
        // default to the devel map
        prte_set_attribute(&caddy->jdata->attributes, PRTE_JOB_DISPLAY_DEVEL_MAP, PRTE_ATTR_GLOBAL,
                           NULL, PMIX_BOOL);
    }

    /* set the job state to the next position */
    PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_INIT_COMPLETE);

    /* cleanup */
    PMIX_RELEASE(caddy);
}

void prte_plm_base_setup_job_complete(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);
    /* nothing to do here but move along */
    PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_ALLOCATE);
    PMIX_RELEASE(caddy);
}

void prte_plm_base_complete_setup(int fd, short args, void *cbdata)
{
    prte_job_t *jdata;
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    pmix_output_verbose(5, prte_plm_base_framework.framework_output, "%s complete_setup on job %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(caddy->jdata->nspace));

    /* bozo check */
    if (PRTE_JOB_STATE_SYSTEM_PREP != caddy->job_state) {
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
        PMIX_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* convenience */
    jdata = caddy->jdata;

    /* set the job state to the next position */
    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_LAUNCH_APPS);

    /* cleanup */
    PMIX_RELEASE(caddy);
}


void prte_plm_base_launch_apps(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata;
    prte_daemon_cmd_flag_t command;
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* convenience */
    jdata = caddy->jdata;

    if (PRTE_JOB_STATE_LAUNCH_APPS != caddy->job_state) {
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
        PMIX_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:launch_apps for job %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_JOBID_PRINT(jdata->nspace)));

    /* pack the appropriate add_local_procs command */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_FIXED_DVM, NULL, PMIX_BOOL)) {
        command = PRTE_DAEMON_DVM_ADD_PROCS;
    } else {
        command = PRTE_DAEMON_ADD_LOCAL_PROCS;
    }
    rc = PMIx_Data_pack(NULL, &jdata->launch_msg, &command, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
        PMIX_RELEASE(caddy);
        return;
    }

    /* get the local launcher's required data */
    if (PRTE_SUCCESS != (rc = prte_odls.get_add_procs_data(&jdata->launch_msg, jdata->nspace))) {
        PRTE_ERROR_LOG(rc);
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
    }

    PMIX_RELEASE(caddy);
    return;
}

void prte_plm_base_send_launch_msg(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_grpcomm_signature_t *sig;
    prte_job_t *jdata;
    int rc;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    /* convenience */
    jdata = caddy->jdata;

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:send launch msg for job %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace)));

    /* if we don't want to launch the apps, now is the time to leave */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DO_NOT_LAUNCH, NULL, PMIX_BOOL)) {
        /* go ahead and register the job */
        rc = prte_pmix_server_register_nspace(jdata);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
        }
        /* if we are persistent, then we remain alive - otherwise, declare
         * all jobs complete and terminate */
        if (prte_persistent) {
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_TERMINATED);
        } else {
            prte_never_launched = true;
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_ALL_JOBS_COMPLETE);
        }
        PMIX_RELEASE(caddy);
        return;
    }

    /* goes to all daemons */
    sig = PMIX_NEW(prte_grpcomm_signature_t);
    sig->signature = (pmix_proc_t *) malloc(sizeof(pmix_proc_t));
    PMIX_LOAD_PROCID(&sig->signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
    sig->sz = 1;
    if (PRTE_SUCCESS != (rc = prte_grpcomm.xcast(sig, PRTE_RML_TAG_DAEMON, &jdata->launch_msg))) {
        PRTE_ERROR_LOG(rc);
        PMIX_RELEASE(sig);
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_NEVER_LAUNCHED);
        PMIX_RELEASE(caddy);
        return;
    }
    PMIX_DATA_BUFFER_DESTRUCT(&jdata->launch_msg);
    PMIX_DATA_BUFFER_CONSTRUCT(&jdata->launch_msg);
    /* maintain accounting */
    PMIX_RELEASE(sig);

    /* track that we automatically are considered to have reported - used
     * only to report launch progress
     */
    caddy->jdata->num_daemons_reported++;

    /* cleanup */
    PMIX_RELEASE(caddy);
}

int prte_plm_base_spawn_response(int32_t status, prte_job_t *jdata)
{
    int rc;
    pmix_data_buffer_t *answer;
    int room, *rmptr;
    pmix_info_t *iptr;
    size_t ninfo;
    time_t timestamp;
    pmix_proc_t *nptr;
    void *tinfo;
    int n;
    char *name;
    pmix_data_array_t darray;
    prte_app_context_t *app;

    /* if the requestor simply told us to terminate, they won't
     * be waiting for a response */
    if (PMIX_NSPACE_INVALID(jdata->originator.nspace)) {
        return PRTE_SUCCESS;
    }

    /* if the response has already been sent, don't do it again */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_SPAWN_NOTIFIED, NULL, PMIX_BOOL)) {
        return PRTE_SUCCESS;
    }

    /* if the requestor was a tool, use PMIx to notify them of
     * launch complete as they won't be listening on PRRTE oob */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_DVM_JOB, NULL, PMIX_BOOL)) {

        /* dvm job => launch was requested by a TOOL, so we notify the launch proxy
         * and NOT the originator (as that would be us) */
        nptr = NULL;
        if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_LAUNCH_PROXY, (void **) &nptr, PMIX_PROC) ||
            NULL == nptr) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            return PRTE_ERR_NOT_FOUND;
        }

        /* direct an event back to our controller */
        timestamp = time(NULL);
        PMIX_INFO_LIST_START(tinfo);
        /* target this notification solely to that one tool */
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_EVENT_CUSTOM_RANGE, nptr, PMIX_PROC);
        PMIX_PROC_RELEASE(nptr);
        /* pass the nspace of the spawned job */
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_NSPACE, jdata->nspace, PMIX_STRING);
        for (n=0; n < jdata->apps->size; n++) {
            app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, n);
            if (NULL == app) {
                continue;
            }
            /* if pset name was assigned, pass it */
            if (prte_get_attribute(&app->attributes, PRTE_APP_PSET_NAME, (void**) &name, PMIX_STRING)) {
                PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_PSET_NAME, name, PMIX_STRING);
                free(name);
            }
            /* pass the argv from each app */
            name = PMIX_ARGV_JOIN_COMPAT(app->argv, ' ');
            PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_APP_ARGV, name, PMIX_STRING);
            free(name);
        }

        /* not to be delivered to a default event handler */
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_EVENT_NON_DEFAULT, NULL, PMIX_BOOL);
        /* provide the timestamp */
        PMIX_INFO_LIST_ADD(rc, tinfo, PMIX_EVENT_TIMESTAMP, &timestamp, PMIX_TIME);
        /* protect against loops */
        PMIX_INFO_LIST_ADD(rc, tinfo, "prte.notify.donotloop", NULL, PMIX_BOOL);
        PMIX_INFO_LIST_CONVERT(rc, tinfo, &darray);
        if (PMIX_ERR_EMPTY == rc) {
            iptr = NULL;
            ninfo = 0;
        } else if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PRTE_UPDATE_EXIT_STATUS(rc);
            PMIX_INFO_LIST_RELEASE(tinfo);
            PMIX_PROC_RELEASE(nptr);
            return rc;
        } else {
            iptr = (pmix_info_t *) darray.array;
            ninfo = darray.size;
        }
        PMIX_INFO_LIST_RELEASE(tinfo);
        PMIx_Notify_event(PMIX_LAUNCH_COMPLETE, &prte_process_info.myproc, PMIX_RANGE_CUSTOM,
                          iptr, ninfo, NULL, NULL);
        PMIX_INFO_FREE(iptr, ninfo);
    }

    rmptr = &room;
    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_ROOM_NUM, (void **) &rmptr, PMIX_INT)) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }

    /* if the originator is me, then just do the notification */
    if (PMIX_CHECK_PROCID(&jdata->originator, PRTE_PROC_MY_NAME)) {
        pmix_server_notify_spawn(jdata->nspace, room, status);
        return PRTE_SUCCESS;
    }

    /* prep the response to the spawn requestor */
    PMIX_DATA_BUFFER_CREATE(answer);

    /* pack the status */
    rc = PMIx_Data_pack(NULL, answer, &status, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return prte_pmix_convert_status(rc);
    }
    /* pack the jobid */
    rc = PMIx_Data_pack(NULL, answer, &jdata->nspace, 1, PMIX_PROC_NSPACE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return prte_pmix_convert_status(rc);
    }
    /* pack the room number */
    rc = PMIx_Data_pack(NULL, answer, &room, 1, PMIX_INT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return prte_pmix_convert_status(rc);
    }

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:launch sending dyn release of job %s to %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace),
                         PRTE_NAME_PRINT(&jdata->originator)));
    PRTE_RML_SEND(rc, jdata->originator.rank, answer, PRTE_RML_TAG_LAUNCH_RESP);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(answer);
        return rc;
    }

    return PRTE_SUCCESS;
}

void prte_plm_base_post_launch(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    int32_t rc, n;
    prte_job_t *jdata;
    prte_proc_t *proc;
    prte_app_context_t *app;
    prte_timer_t *timer;
    char *file = NULL;
    FILE *fp;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* convenience */
    jdata = caddy->jdata;

    /* if a timer was defined, cancel it */
    if (prte_get_attribute(&jdata->attributes, PRTE_SPAWN_TIMEOUT_EVENT, (void **) &timer, PMIX_POINTER)) {
        prte_event_evtimer_del(timer->ev);
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:launch deleting spawn timeout for job %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace)));
        PMIX_RELEASE(timer);
        prte_remove_attribute(&jdata->attributes, PRTE_SPAWN_TIMEOUT_EVENT);
    }

    if (PRTE_JOB_STATE_RUNNING != caddy->job_state) {
        /* error mgr handles this */
        PMIX_RELEASE(caddy);
        return;
    }
    /* update job state */
    caddy->jdata->state = caddy->job_state;

    /* complete wiring up the iof */
    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:launch wiring up iof for job %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace)));

    /* if requested, output the proctable */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_OUTPUT_PROCTABLE, (void**)&file, PMIX_STRING)) {
        /* if file="-", then output to stdout */
        if (0 == strcmp(file, "-")) {
            fp = stdout;
        } else if (0 == strcmp(file, "+")) {
            fp = stderr;
        } else {
            /* attempt to open the specified file */
            fp = fopen(file, "w");
            if (NULL == fp) {
                pmix_output(0, "Unable to open file %s for output of proctable", file);
                goto next;
            }
        }
        for (n=0; n < jdata->procs->size; n++) {
            proc = (prte_proc_t*)pmix_pointer_array_get_item(jdata->procs, n);
            if (NULL == proc) {
                continue;
            }
            app = (prte_app_context_t*)pmix_pointer_array_get_item(jdata->apps, proc->app_idx);
            fprintf(fp, "(rank, host, exe, pid) = (%u, %s, %s, %d)\n",
                    proc->name.rank, proc->node->name, app->app, proc->pid);
        }
        if (stdout != fp && stderr != fp) {
            fclose(fp);
        }
    }

next:
    /* notify the spawn requestor */
    rc = prte_plm_base_spawn_response(PRTE_SUCCESS, jdata);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }

    /* cleanup */
    PMIX_RELEASE(caddy);
}

void prte_plm_base_registered(int fd, short args, void *cbdata)
{
    prte_job_t *jdata;
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /* convenience */
    jdata = caddy->jdata;

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:launch %s registered", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_JOBID_PRINT(jdata->nspace)));

    if (PRTE_JOB_STATE_REGISTERED != caddy->job_state) {
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:launch job %s not registered - state %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace),
                             prte_job_state_to_str(caddy->job_state)));
        PRTE_ACTIVATE_JOB_STATE(caddy->jdata, PRTE_JOB_STATE_FORCED_EXIT);
        PMIX_RELEASE(caddy);
        return;
    }
    /* update job state */
    jdata->state = caddy->job_state;

    PMIX_RELEASE(caddy);
}

/* daemons callback when they start - need to listen for them */
static bool prted_failed_launch;
static prte_job_t *jdatorted = NULL;

/* callback for topology reports */
void prte_plm_base_daemon_topology(int status, pmix_proc_t *sender,
                                   pmix_data_buffer_t *buffer,
                                   prte_rml_tag_t tag, void *cbdata)
{
    hwloc_topology_t topo;
    int rc, idx;
    char *sig;
    prte_proc_t *daemon = NULL, *dptr, *dnxt;
    prte_topology_t *t, *t2;
    int i;
    prte_job_t *jdata;
    uint8_t flag;
    pmix_data_buffer_t datbuf, *data;
    pmix_byte_object_t bo, pbo;
    pmix_topology_t ptopo;
    PRTE_HIDE_UNUSED_PARAMS(status, tag, cbdata);

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:daemon_topology recvd for daemon %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(sender)));

    /* get the daemon job, if necessary */
    if (NULL == jdatorted) {
        jdatorted = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    }
    daemon = (prte_proc_t *) pmix_pointer_array_get_item(jdatorted->procs, sender->rank);
    if (NULL == daemon) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        prted_failed_launch = true;
        goto CLEANUP;
    }
    PMIX_DATA_BUFFER_CONSTRUCT(&datbuf);
    /* unpack the flag to see if this payload is compressed */
    idx = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &flag, &idx, PMIX_BOOL);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        prted_failed_launch = true;
        goto CLEANUP;
    }
    /* unpack the data */
    idx = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &pbo, &idx, PMIX_BYTE_OBJECT);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        prted_failed_launch = true;
        goto CLEANUP;
    }
    /* if compressed, decompress it */
    if (flag) {
        /* decompress the data */
        if (PMIx_Data_decompress((uint8_t *) pbo.bytes, pbo.size,
                                 (uint8_t **) &bo.bytes, &bo.size)) {
            /* the data has been uncompressed */
            rc = PMIx_Data_load(&datbuf, &bo);
            PMIX_BYTE_OBJECT_DESTRUCT(&bo);
        } else {
            pmix_show_help("help-prte-runtime.txt", "failed-to-uncompress",
                           true, prte_process_info.nodename);
            prted_failed_launch = true;
            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
            goto CLEANUP;
        }
    } else {
        rc = PMIx_Data_load(&datbuf, &pbo);
    }
    PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
    data = &datbuf;

    /* unpack the topology signature for this node */
    idx = 1;
    rc = PMIx_Data_unpack(NULL, data, &sig, &idx, PMIX_STRING);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        prted_failed_launch = true;
        PMIX_DATA_BUFFER_DESTRUCT(data);
        goto CLEANUP;
    }

    /* find it in the array */
    t = NULL;
    for (i = 0; i < prte_node_topologies->size; i++) {
        t2 = (prte_topology_t *) pmix_pointer_array_get_item(prte_node_topologies, i);
        if (NULL == t2) {
            continue;
        }
        /* just check the signature */
        if (0 == strcmp(sig, t2->sig)) {
            t = t2;
            break;
        }
    }
    free(sig);
    if (NULL == t) {
        /* should never happen */
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        prted_failed_launch = true;
        PMIX_DATA_BUFFER_DESTRUCT(data);
        goto CLEANUP;
    }

    /* unpack the topology */
    idx = 1;
    rc = PMIx_Data_unpack(NULL, data, &ptopo, &idx, PMIX_TOPO);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        prted_failed_launch = true;
        PMIX_DATA_BUFFER_DESTRUCT(data);
        goto CLEANUP;
    }
    topo = ptopo.topology;
    ptopo.topology = NULL;
    PMIX_TOPOLOGY_DESTRUCT(&ptopo);
    PMIX_DATA_BUFFER_DESTRUCT(data);
    /* record the final topology */
    t->topo = topo;
    /* update the node's available processors */
    if (NULL != daemon->node->available) {
        hwloc_bitmap_free(daemon->node->available);
    }
    /* Apply any CPU filters (not preserved by the XML) */
    daemon->node->available = prte_hwloc_base_filter_cpus(topo);

    /* process any cached daemons that match this signature */
    PMIX_LIST_FOREACH_SAFE(dptr, dnxt, &prte_plm_globals.daemon_cache, prte_proc_t) {
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:report_topo processing cached daemon %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             PRTE_NAME_PRINT(&dptr->name)));
        if (0 == strcmp(dptr->node->topology->sig, t->sig)) {
            dptr->node->topology = t;
            dptr->node->available = prte_hwloc_base_filter_cpus(topo);
            jdatorted->num_reported++;
            pmix_list_remove_item(&prte_plm_globals.daemon_cache, &dptr->super);
        }
    }

CLEANUP:
    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:orted:report_topo launch %s for daemon %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         prted_failed_launch ? "failed" : "completed", PRTE_NAME_PRINT(sender)));

    if (prted_failed_launch) {
        PRTE_ACTIVATE_JOB_STATE(jdatorted, PRTE_JOB_STATE_FAILED_TO_START);
        return;
    } else {
        jdatorted->num_reported++;
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:orted_report_launch recvd %d of %d reported daemons",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), jdatorted->num_reported,
                             jdatorted->num_procs));
        if (jdatorted->num_procs == jdatorted->num_reported) {
            bool dvm = true;
            jdatorted->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
            /* activate the daemons_reported state for all jobs
             * whose daemons were launched
             */
            for (i = 1; i < prte_job_data->size; i++) {
                jdata = (prte_job_t *) pmix_pointer_array_get_item(prte_job_data, i);
                if (NULL == jdata) {
                    continue;
                }
                dvm = false;
                if (PRTE_JOB_STATE_DAEMONS_LAUNCHED == jdata->state) {
                    PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
                }
            }
            if (dvm) {
                /* must be launching a DVM - activate the state */
                PRTE_ACTIVATE_JOB_STATE(jdatorted, PRTE_JOB_STATE_DAEMONS_REPORTED);
            }
        }
    }
}

static void opcbfunc(pmix_status_t status, void *cbdata)
{
    prte_pmix_lock_t *lock = (prte_pmix_lock_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(status);
    PRTE_PMIX_WAKEUP_THREAD(lock);
}

void prte_plm_base_daemon_callback(int status, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                   prte_rml_tag_t tag, void *cbdata)
{
    char *ptr;
    int idx;
    pmix_status_t ret;
    prte_proc_t *daemon = NULL, *dptr;
    prte_job_t *jdata;
    pmix_proc_t dname;
    pmix_data_buffer_t *relay;
    char *sig;
    prte_topology_t *t, *mytopo;
    hwloc_topology_t topo;
    int i;
    bool found;
    prte_daemon_cmd_flag_t cmd;
    char *alias;
    uint8_t naliases, ni;
    char *nodename = NULL;
    pmix_info_t *info;
    size_t ninfo;
    pmix_byte_object_t pbo, bo;
    pmix_data_buffer_t pbuf;
    int32_t flag;
    bool compressed;
    pmix_data_buffer_t datbuf, *data;
    pmix_topology_t ptopo;
    pmix_value_t cnctinfo;
    pmix_list_t cachelist;

    PRTE_HIDE_UNUSED_PARAMS(status, sender, tag, cbdata);

    /* get the daemon job, if necessary */
    if (NULL == jdatorted) {
        jdatorted = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    }

    /* get my topology */
    mytopo = (prte_topology_t *) pmix_pointer_array_get_item(prte_node_topologies, 0);
    if (NULL == mytopo) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        prted_failed_launch = true;
        goto CLEANUP;
    }

    /* multiple daemons could be in this buffer, so unpack until we exhaust the data */
    idx = 1;
    while (PMIX_SUCCESS == (ret = PMIx_Data_unpack(NULL, buffer, &dname, &idx, PMIX_PROC))) {

        pmix_output_verbose(5, prte_plm_base_framework.framework_output,
                             "%s plm:base:orted_report_launch from daemon %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&dname));

        /* update state and record for this daemon contact info */
        daemon = (prte_proc_t *) pmix_pointer_array_get_item(jdatorted->procs, dname.rank);
        if (NULL == daemon) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            prted_failed_launch = true;
            goto CLEANUP;
        }
        daemon->state = PRTE_PROC_STATE_RUNNING;
        /* record that this daemon is alive */
        PRTE_FLAG_SET(daemon, PRTE_PROC_FLAG_ALIVE);
        /* unload its contact info */
        PMIX_VALUE_CONSTRUCT(&cnctinfo);
        cnctinfo.type = PMIX_STRING;
        idx = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &cnctinfo.data.string, &idx, PMIX_STRING);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            prted_failed_launch = true;
            goto CLEANUP;
        }
        /* store this for later distribution */
        ret = PMIx_Store_internal(&dname, PMIX_PROC_URI, &cnctinfo);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            PMIX_VALUE_DESTRUCT(&cnctinfo);
            prted_failed_launch = true;
            goto CLEANUP;
        }
        daemon->rml_uri = strdup(cnctinfo.data.string);
        PMIX_VALUE_DESTRUCT(&cnctinfo);

        /* unpack the node name */
        idx = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &nodename, &idx, PMIX_STRING);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            prted_failed_launch = true;
            goto CLEANUP;
        }

        if (!pmix_net_isaddr(nodename) &&
            NULL != (ptr = strchr(nodename, '.'))) {
            /* retain the non-fqdn name as an alias */
            *ptr = '\0';
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&daemon->node->aliases, nodename);
            *ptr = '.';
        }

        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:orted_report_launch from daemon %s on node %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&daemon->name),
                             nodename));

        /* mark the daemon as launched */
        PRTE_FLAG_SET(daemon->node, PRTE_NODE_FLAG_DAEMON_LAUNCHED);
        daemon->node->state = PRTE_NODE_STATE_UP;

        /* first, store the nodename itself. in case the nodename isn't
         * the same as what we were given by the allocation, we replace
         * the node's name with the returned value and store the allocation
         * value as an alias. For example, a hostfile
         * might contain an IP address instead of the value returned
         * by gethostname, yet the daemon will have returned the latter
         * and apps may refer to the host by that name
         */
        if (0 != strcmp(nodename, daemon->node->name)) {
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&daemon->node->aliases, daemon->node->name);
            free(daemon->node->name);
            daemon->node->name = strdup(nodename);
        }

        /* unpack and store the provided aliases */
        idx = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &naliases, &idx, PMIX_UINT8);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            prted_failed_launch = true;
            goto CLEANUP;
        }
        for (ni = 0; ni < naliases; ni++) {
            idx = 1;
            ret = PMIx_Data_unpack(NULL, buffer, &alias, &idx, PMIX_STRING);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                prted_failed_launch = true;
                goto CLEANUP;
            }
            PMIX_ARGV_APPEND_UNIQUE_COMPAT(&daemon->node->aliases, alias);
            free(alias);
        }

        if (0 < pmix_output_get_verbosity(prte_plm_base_framework.framework_output)) {
            pmix_output(0, "ALIASES FOR NODE %s (%s)", daemon->node->name, nodename);
            if (NULL != daemon->node->aliases) {
                for (ni=0; NULL != daemon->node->aliases[ni]; ni++) {
                    pmix_output(0, "\tALIAS: %s", daemon->node->aliases[ni]);
                }
            }
        }

        /* unpack the topology signature for that node */
        idx = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &sig, &idx, PMIX_STRING);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            prted_failed_launch = true;
            goto CLEANUP;
        }
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s RECEIVED TOPOLOGY SIG %s FROM NODE %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), sig, nodename));

        if (NULL == prte_base_compute_node_sig) {
            prte_base_compute_node_sig = strdup(sig);
            if (prte_hnp_is_allocated && 0 != strcmp(sig, mytopo->sig)) {
                prte_hetero_nodes = true;
            }
        } else if (!prte_hetero_nodes) {
            if (0 != strcmp(sig, prte_base_compute_node_sig) ||
                (prte_hnp_is_allocated && 0 != strcmp(sig, mytopo->sig))) {
                prte_hetero_nodes = true;
            }
        }

        /* rank=1 always sends its topology back */
        topo = NULL;
        if (1 == dname.rank) {
            prte_plm_globals.daemon1_has_reported = true;
            PMIX_DATA_BUFFER_CONSTRUCT(&datbuf);
            /* unpack the flag to see if this payload is compressed */
            idx = 1;
            ret = PMIx_Data_unpack(NULL, buffer, &compressed, &idx, PMIX_BOOL);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                prted_failed_launch = true;
                goto CLEANUP;
            }
            /* unpack the data */
            idx = 1;
            ret = PMIx_Data_unpack(NULL, buffer, &pbo, &idx, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                prted_failed_launch = true;
                goto CLEANUP;
            }
            /* only need to process it if our signatures differ */
            if (0 == strcmp(sig, mytopo->sig)) {
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                topo = mytopo->topo;
            } else {
                if (compressed) {
                    /* decompress the data */
                    if (PMIx_Data_decompress((uint8_t *) pbo.bytes, pbo.size,
                                             (uint8_t **) &bo.bytes, &bo.size)) {
                        /* the data has been uncompressed */
                        ret = PMIx_Data_load(&datbuf, &bo);
                        PMIX_BYTE_OBJECT_DESTRUCT(&bo);
                        if (PMIX_SUCCESS != ret) {
                            PMIX_ERROR_LOG(ret);
                            prted_failed_launch = true;
                            PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                            goto CLEANUP;
                        }
                    } else {
                        pmix_show_help("help-prte-runtime.txt", "failed-to-uncompress",
                                       true, prte_process_info.nodename);
                        prted_failed_launch = true;
                        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                        PMIX_BYTE_OBJECT_DESTRUCT(&bo);
                        goto CLEANUP;
                    }
                } else {
                    ret = PMIx_Data_load(&datbuf, &pbo);
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        prted_failed_launch = true;
                        PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                        goto CLEANUP;
                    }
                }
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                data = &datbuf;

                /* unpack the available topology information */
                idx = 1;
                ret = PMIx_Data_unpack(NULL, data, &ptopo, &idx, PMIX_TOPO);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    prted_failed_launch = true;
                    goto CLEANUP;
                }
                topo = ptopo.topology;
                ptopo.topology = NULL;
                PMIX_TOPOLOGY_DESTRUCT(&ptopo);
                /* cleanup */
                PMIX_DATA_BUFFER_DESTRUCT(data);
            }
        }

        /* see if they provided their inventory */
        idx = 1;
        ret = PMIx_Data_unpack(NULL, buffer, &flag, &idx, PMIX_INT8);
        if (PMIX_SUCCESS != ret) {
            PMIX_ERROR_LOG(ret);
            prted_failed_launch = true;
            goto CLEANUP;
        }
        if (1 == flag) {
            ret = PMIx_Data_unpack(NULL, buffer, &pbo, &idx, PMIX_BYTE_OBJECT);
            if (PMIX_SUCCESS != ret) {
                PMIX_ERROR_LOG(ret);
                prted_failed_launch = true;
                goto CLEANUP;
            }
            /* if nothing is present, then ignore it */
            if (0 < pbo.size) {
                prte_pmix_lock_t lock;
                /* load the bytes into a PMIx data buffer for unpacking */
                PMIX_DATA_BUFFER_CONSTRUCT(&pbuf);
                ret = PMIx_Data_load(&pbuf, &pbo);
                PMIX_BYTE_OBJECT_DESTRUCT(&pbo);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    prted_failed_launch = true;
                    goto CLEANUP;
                }
                idx = 1;
                ret = PMIx_Data_unpack(NULL, &pbuf, &ninfo, &idx, PMIX_SIZE);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
                    prted_failed_launch = true;
                    goto CLEANUP;
                }
                PMIX_INFO_CREATE(info, ninfo);
                idx = ninfo;
                ret = PMIx_Data_unpack(NULL, &pbuf, info, &idx, PMIX_INFO);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_INFO_FREE(info, ninfo);
                    PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
                    prted_failed_launch = true;
                    goto CLEANUP;
                }
                PMIX_DATA_BUFFER_DESTRUCT(&pbuf);
                PRTE_PMIX_CONSTRUCT_LOCK(&lock);
                ret = PMIx_server_deliver_inventory(info, ninfo, NULL, 0, opcbfunc, &lock);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_INFO_FREE(info, ninfo);
                    prted_failed_launch = true;
                    goto CLEANUP;
                }
                PRTE_PMIX_WAIT_THREAD(&lock);
                PRTE_PMIX_DESTRUCT_LOCK(&lock);
            }
        }

        /* do we already have this topology from some other node? */
        found = false;
        for (i = 0; i < prte_node_topologies->size; i++) {
            t = (prte_topology_t *) pmix_pointer_array_get_item(prte_node_topologies, i);
            if (NULL == t) {
                continue;
            }
            /* just check the signature */
            if (0 == strcmp(sig, t->sig)) {
                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                     "%s TOPOLOGY SIGNATURE ALREADY RECORDED IN POSN %d",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), i));
                daemon->node->topology = t;
                found = true;
                /* the topology in this struct can be NULL in the case
                 * where an earlier daemon other than daemon1 reported the
                 * signature but did not include its topology */
                if (NULL == t->topo) {
                    if (1 == dname.rank) {
                        /* we will have received its topology */
                        t->topo = topo;
                    } else {
                        break;
                    }
                }
                /* update the node's available processors */
                if (NULL != daemon->node->available) {
                    hwloc_bitmap_free(daemon->node->available);
                }
                daemon->node->available = prte_hwloc_base_filter_cpus(t->topo);
                free(sig);
                break;
            }
        }

        if (1 == dname.rank) {
            /* process any cached daemons */
            PMIX_CONSTRUCT(&cachelist, pmix_list_t);
            while (NULL != (dptr = (prte_proc_t*)pmix_list_remove_first(&prte_plm_globals.daemon_cache))) {
                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                     "%s plm:base:prted_daemon_cback processing cached daemon %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&dptr->name)));
                if (0 == strcmp(dptr->node->topology->sig, sig)) {
                    dptr->node->topology = t;
                    dptr->node->available = prte_hwloc_base_filter_cpus(topo);
                    jdatorted->num_reported++;
                } else {
                    /* see if this topology has already been requested */
                    compressed = false;
                    if (NULL != prte_plm_globals.cache) {
                        for (i=0; NULL != prte_plm_globals.cache[i]; i++) {
                            if (0 == strcmp(prte_plm_globals.cache[i], dptr->node->topology->sig)) {
                                /* already requested - cache it */
                                pmix_list_append(&cachelist, &dptr->super);
                                compressed = true;
                                break;
                            }
                        }
                    }
                    if (compressed) {
                        continue;
                    }
                    /* we need to request this topology */
                    PMIX_DATA_BUFFER_CREATE(relay);
                    cmd = PRTE_DAEMON_REPORT_TOPOLOGY_CMD;
                    ret = PMIx_Data_pack(NULL, relay, &cmd, 1, PMIX_UINT8);
                    if (PMIX_SUCCESS != ret) {
                        PMIX_ERROR_LOG(ret);
                        PMIX_DATA_BUFFER_RELEASE(relay);
                        prted_failed_launch = true;
                        goto CLEANUP;
                    }
                    /* send it */
                    PRTE_RML_SEND(ret, dptr->name.rank, relay, PRTE_RML_TAG_DAEMON);
                    if (PRTE_SUCCESS != ret) {
                        PRTE_ERROR_LOG(ret);
                        PMIX_DATA_BUFFER_RELEASE(relay);
                        prted_failed_launch = true;
                        goto CLEANUP;
                    }
                    /* track that we requested it */
                    PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_plm_globals.cache, dptr->node->topology->sig);
                }
            }
            /* transfer back any cached items */
            while (NULL != (dptr = (prte_proc_t*)pmix_list_remove_first(&cachelist))) {
                pmix_list_append(&prte_plm_globals.daemon_cache, &dptr->super);
            }
            PMIX_DESTRUCT(&cachelist);
        }

        if (!found) {
            /* signature not found - record it */
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s NEW TOPOLOGY - ADDING SIGNATURE",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            t = PMIX_NEW(prte_topology_t);
            t->sig = sig;
            t->index = pmix_pointer_array_add(prte_node_topologies, t);
            daemon->node->topology = t;
            if (NULL != topo) {
                t->topo = topo;
                /* update the node's available processors */
                if (NULL != daemon->node->available) {
                    hwloc_bitmap_free(daemon->node->available);
                }
                daemon->node->available = prte_hwloc_base_filter_cpus(t->topo);
            }
        }
        if (!prte_plm_globals.daemon1_has_reported) {
            if (NULL == daemon->node->topology->topo) {
                /* if daemon1 has not reported and the topology is
                 * different than the one for DVM controller, then cache this daemon
                 * for later processing */
                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                     "%s CACHING DAEMON %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                    PRTE_NAME_PRINT(&dname)));
                pmix_list_append(&prte_plm_globals.daemon_cache, &daemon->super);
                /* we will count this node as completed
                 * when we get the full topology back */
                if (NULL != nodename) {
                    free(nodename);
                    nodename = NULL;
                }
                idx = 1;
                continue;
            }
        } else if (1 != dname.rank && NULL == daemon->node->topology->topo) {
            /* see if we already have requested a topology for this signature */
            compressed = false;
            if (NULL != prte_plm_globals.cache) {
                for (i=0; NULL != prte_plm_globals.cache[i]; i++) {
                    if (0 == strcmp(prte_plm_globals.cache[i], daemon->node->topology->sig)) {
                        /* already requested - cache it */
                        compressed = true;
                        break;
                    }
                }
            }
            if (!compressed) {
                /* request the complete topology from that node */
                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                     "%s REQUESTING TOPOLOGY FROM %s FOR SIG %s",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     PRTE_NAME_PRINT(&dname),
                                     daemon->node->topology->sig));
                /* construct the request */
                PMIX_DATA_BUFFER_CREATE(relay);
                cmd = PRTE_DAEMON_REPORT_TOPOLOGY_CMD;
                ret = PMIx_Data_pack(NULL, relay, &cmd, 1, PMIX_UINT8);
                if (PMIX_SUCCESS != ret) {
                    PMIX_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_RELEASE(relay);
                    prted_failed_launch = true;
                    goto CLEANUP;
                }
                /* send it */
                PRTE_RML_SEND(ret, dname.rank, relay, PRTE_RML_TAG_DAEMON);
                if (PRTE_SUCCESS != ret) {
                    PRTE_ERROR_LOG(ret);
                    PMIX_DATA_BUFFER_RELEASE(relay);
                    prted_failed_launch = true;
                    goto CLEANUP;
                }
                /* record that it was sent */
                PMIX_ARGV_APPEND_NOSIZE_COMPAT(&prte_plm_globals.cache, daemon->node->topology->sig);
            }
            /* we will count this node as completed
             * when we get the full topology back */
            if (NULL != nodename) {
                free(nodename);
                nodename = NULL;
            }
            idx = 1;
            continue;
        }

    CLEANUP:
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:orted_report_launch %s for daemon %s at contact %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             prted_failed_launch ? "failed" : "completed", PRTE_NAME_PRINT(&dname),
                             (NULL == daemon) ? "UNKNOWN" : daemon->rml_uri));

        if (NULL != nodename) {
            free(nodename);
            nodename = NULL;
        }

        if (prted_failed_launch) {
            PRTE_ACTIVATE_JOB_STATE(jdatorted, PRTE_JOB_STATE_FAILED_TO_START);
            return;
        } else {
            jdatorted->num_reported++;
            jdatorted->num_daemons_reported++;
            PMIX_OUTPUT_VERBOSE(
                (5, prte_plm_base_framework.framework_output,
                 "%s plm:base:orted_report_launch job %s recvd %d of %d reported daemons",
                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdatorted->nspace),
                 jdatorted->num_reported, jdatorted->num_procs));
            found = prte_get_attribute(&jdatorted->attributes, PRTE_JOB_SHOW_PROGRESS, NULL, PMIX_BOOL);
            if (found &&
                (0 == jdatorted->num_reported % 100 ||
                 jdatorted->num_reported == prte_process_info.num_daemons)) {
                PRTE_ACTIVATE_JOB_STATE(jdatorted, PRTE_JOB_STATE_REPORT_PROGRESS);
            }
            if (jdatorted->num_procs == jdatorted->num_reported) {
                bool dvm = true;
                jdatorted->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
                /* activate the daemons_reported state for all jobs
                 * whose daemons were launched
                 */
                for (i = 1; i < prte_job_data->size; i++) {
                    jdata = (prte_job_t *) pmix_pointer_array_get_item(prte_job_data, i);
                    if (NULL == jdata) {
                        continue;
                    }
                    dvm = false;
                    if (PRTE_JOB_STATE_DAEMONS_LAUNCHED == jdata->state) {
                        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_REPORTED);
                    }
                }
                if (dvm) {
                    /* must be launching a DVM - activate the state */
                    PRTE_ACTIVATE_JOB_STATE(jdatorted, PRTE_JOB_STATE_DAEMONS_REPORTED);
                }
            }
        }
        idx = 1;
    }

    if (PMIX_ERR_UNPACK_READ_PAST_END_OF_BUFFER != ret) {
        PMIX_ERROR_LOG(ret);
        PRTE_ACTIVATE_JOB_STATE(jdatorted, PRTE_JOB_STATE_FAILED_TO_START);
    }
}

void prte_plm_base_daemon_failed(int st, pmix_proc_t *sender, pmix_data_buffer_t *buffer,
                                 prte_rml_tag_t tag, void *cbdata)
{
    int status, rc;
    int32_t n;
    pmix_rank_t vpid;
    prte_proc_t *daemon = NULL;
    PRTE_HIDE_UNUSED_PARAMS(st, sender, tag, cbdata);

    /* get the daemon job, if necessary */
    if (NULL == jdatorted) {
        jdatorted = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
    }

    /* unpack the daemon that failed */
    n = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &vpid, &n, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
        goto finish;
    }

    /* unpack the exit status */
    n = 1;
    rc = PMIx_Data_unpack(NULL, buffer, &status, &n, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        status = PRTE_ERROR_DEFAULT_EXIT_CODE;
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
    } else {
        PRTE_UPDATE_EXIT_STATUS(WEXITSTATUS(status));
    }

    /* find the daemon and update its state/status */
    if (NULL == (daemon = (prte_proc_t *) pmix_pointer_array_get_item(jdatorted->procs, vpid))) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        goto finish;
    }
    daemon->state = PRTE_PROC_STATE_FAILED_TO_START;
    daemon->exit_code = status;

finish:
    if (NULL == daemon) {
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_PROC_STATE_FAILED_TO_START);
        return;
    } else {
        PRTE_ACTIVATE_PROC_STATE(&daemon->name, PRTE_PROC_STATE_FAILED_TO_START);
    }
}

int prte_plm_base_setup_prted_cmd(int *argc, char ***argv)
{
    int i, loc;
    char **tmpv;

    /* set default location to be 0, indicating that
     * only a single word is in the cmd
     */
    loc = 0;
    /* split the command apart in case it is multi-word */
    tmpv = PMIX_ARGV_SPLIT_COMPAT(prte_launch_agent, ' ');
    for (i = 0; NULL != tmpv && NULL != tmpv[i]; ++i) {
        if (0 == strcmp(tmpv[i], "prted")) {
            loc = i;
        }
        pmix_argv_append(argc, argv, tmpv[i]);
    }
    PMIX_ARGV_FREE_COMPAT(tmpv);

    return loc;
}

/* pass all options as MCA params so anything we pickup
 * from the environment can be checked for duplicates
 */
int prte_plm_base_prted_append_basic_args(int *argc, char ***argv, char *ess, int *proc_vpid_index)
{
    char *param = NULL, **tmpv;
    int i, j, cnt, offset;
    prte_job_t *jdata;
    unsigned long num_procs;
    bool ignore;

    /* check for debug flags */
    if (prte_debug_flag) {
        pmix_argv_append(argc, argv, "--debug");
    }
    if (prte_debug_daemons_flag) {
        pmix_argv_append(argc, argv, "--debug-daemons");
    }
    if (prte_debug_daemons_file_flag) {
        pmix_argv_append(argc, argv, "--debug-daemons-file");
    }
    if (prte_leave_session_attached) {
        pmix_argv_append(argc, argv, "--leave-session-attached");
    }
    if (prte_allow_run_as_root) {
        pmix_argv_append(argc, argv, "--allow-run-as-root");
    }

    /* the following is not an mca param */
    if (NULL != getenv("PRTE_TEST_PRTED_SUICIDE")) {
        pmix_argv_append(argc, argv, "--test-suicide");
    }

    /* tell the orted what ESS component to use */
    if (NULL != ess) {
        pmix_argv_append(argc, argv, "--prtemca");
        pmix_argv_append(argc, argv, "ess");
        pmix_argv_append(argc, argv, ess);
    }

    /* pass the daemon nspace */
    pmix_argv_append(argc, argv, "--prtemca");
    pmix_argv_append(argc, argv, "ess_base_nspace");
    pmix_argv_append(argc, argv, prte_process_info.myproc.nspace);
    free(param);

    /* setup to pass the vpid */
    if (NULL != proc_vpid_index) {
        pmix_argv_append(argc, argv, "--prtemca");
        pmix_argv_append(argc, argv, "ess_base_vpid");
        *proc_vpid_index = *argc;
        pmix_argv_append(argc, argv, "<template>");
    }

    /* pass the total number of daemons that will be in the system */
    if (PRTE_PROC_IS_MASTER) {
        jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
        num_procs = jdata->num_procs;
    } else {
        num_procs = prte_process_info.num_daemons;
    }
    pmix_argv_append(argc, argv, "--prtemca");
    pmix_argv_append(argc, argv, "ess_base_num_procs");
    pmix_asprintf(&param, "%lu", num_procs);
    pmix_argv_append(argc, argv, param);
    free(param);

    /* pass the HNP uri */
    pmix_argv_append(argc, argv, "--prtemca");
    pmix_argv_append(argc, argv, "prte_hnp_uri");
    pmix_argv_append(argc, argv, prte_process_info.my_hnp_uri);

    /* if --xterm was specified, pass that along */
    if (NULL != prte_xterm) {
        pmix_argv_append(argc, argv, "--prtemca");
        pmix_argv_append(argc, argv, "prte_xterm");
        pmix_argv_append(argc, argv, prte_xterm);
    }

    /* look for any envars that relate to us and pass
     * them along on the cmd line */
    offset = strlen("PRTE_MCA_");
    for (i=0; NULL != environ[i]; i++) {
        if (0 == strncmp(environ[i], "PMIX_MCA_", offset) ||
            0 == strncmp(environ[i], "PRTE_MCA_", offset)) {
            tmpv = PMIX_ARGV_SPLIT_COMPAT(environ[i], '=');
            /* check for duplicate */
            ignore = false;
            for (j = 0; j < *argc; j++) {
                if (0 == strcmp((*argv)[j], &tmpv[0][offset])) {
                    ignore = true;
                    break;
                }
            }
            if (!ignore) {
                /* pass it along */
                if (0 == strncmp(tmpv[0], "PRTE_MCA_", offset)) {
                    pmix_argv_append(argc, argv, "--prtemca");
                } else {
                    pmix_argv_append(argc, argv, "--pmixmca");
                }
                pmix_argv_append(argc, argv, &tmpv[0][offset]);
                pmix_argv_append(argc, argv, tmpv[1]);
            }
            PMIX_ARGV_FREE_COMPAT(tmpv);
        }
    }

    /* pass along any cmd line MCA params provided to mpirun,
     * being sure to "purge" any that would cause problems
     * on backend nodes and ignoring all duplicates
     */
    cnt = PMIX_ARGV_COUNT_COMPAT(prted_cmd_line);
    for (i = 0; i < cnt; i += 3) {
        /* if the specified option is more than one word, we don't
         * have a generic way of passing it as some environments ignore
         * any quotes we add, while others don't - so we ignore any
         * such options. In most cases, this won't be a problem as
         * they typically only apply to things of interest to the HNP.
         * Individual environments can add these back into the cmd line
         * as they know if it can be supported
         */
        if (NULL != strchr(prted_cmd_line[i + 2], ' ')) {
            continue;
        }
        /* The daemon will attempt to open the PLM on the remote
         * end. Only a few environments allow this, so the daemon
         * only opens the PLM -if- it is specifically told to do
         * so by giving it a specific PLM module. To ensure we avoid
         * confusion, do not include any directives here
         */
        if (0 == strcmp(prted_cmd_line[i + 1], "plm")) {
            continue;
        }
        /* check for duplicate */
        ignore = false;
        for (j = 0; j < *argc; j++) {
            if (0 == strcmp((*argv)[j], prted_cmd_line[i + 1])) {
                ignore = true;
                break;
            }
        }
        if (!ignore) {
            /* pass it along */
            pmix_argv_append(argc, argv, prted_cmd_line[i]);
            pmix_argv_append(argc, argv, prted_cmd_line[i + 1]);
            pmix_argv_append(argc, argv, prted_cmd_line[i + 2]);
        }
    }

    return PRTE_SUCCESS;
}

void prte_plm_base_wrap_args(char **args)
{
    int i;
    char *tstr;

    for (i = 0; NULL != args && NULL != args[i]; i++) {
        /* if the arg ends in "mca", then we wrap its arguments */
        if (strlen(args[i]) > 3 && 0 == strcmp(args[i] + strlen(args[i]) - 3, "mca")) {
            /* it was at the end */
            if (NULL == args[i + 1] || NULL == args[i + 2]) {
                /* this should be impossible as the error would
                 * have been detected well before here, but just
                 * be safe */
                return;
            }
            i += 2;
            /* if the argument already has quotes, then leave it alone */
            if ('\"' == args[i][0]) {
                continue;
            }
            pmix_asprintf(&tstr, "\"%s\"", args[i]);
            free(args[i]);
            args[i] = tstr;
        }
    }
}

int prte_plm_base_setup_virtual_machine(prte_job_t *jdata)
{
    prte_node_t *node, *nptr;
    prte_proc_t *proc, *pptr;
    prte_job_map_t *map = NULL;
    int rc, i;
    prte_job_t *daemons;
    pmix_list_t nodes, tnodes;
    pmix_list_item_t *item, *next;
    prte_app_context_t *app;
    bool one_filter = false;
    int num_nodes;
    bool default_hostfile_used;
    char *hosts = NULL;
    bool singleton = false;
    bool multi_sim = false;

    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                         "%s plm:base:setup_vm",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));

    if (NULL == (daemons = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace))) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        return PRTE_ERR_NOT_FOUND;
    }
    if (NULL == daemons->map) {
        daemons->map = PMIX_NEW(prte_job_map_t);
    }
    map = daemons->map;

    /* if this job is being launched against a fixed DVM, then there is
     * nothing for us to do - the DVM will stand as is */
    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_FIXED_DVM, NULL, PMIX_BOOL)) {
        /* mark that the daemons have reported so we can proceed */
        daemons->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
        map->num_new_daemons = 0;
        return PRTE_SUCCESS;
    }

    PMIX_CONSTRUCT(&nodes, pmix_list_t);

    if (prte_get_attribute(&jdata->attributes, PRTE_JOB_EXTEND_DVM, NULL, PMIX_BOOL)) {
        // nodes have been added, so extend the DVM
        prte_remove_attribute(&jdata->attributes, PRTE_JOB_EXTEND_DVM);
        goto construct;
    }

    /* if this is a dynamic spawn, then we don't make any changes to
     * the virtual machine unless specifically requested to do so
     */
    if (!PMIX_NSPACE_INVALID(jdata->originator.nspace)) {
        if (0 == map->num_nodes) {
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s plm:base:setup_vm creating map",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            /* this is the first time thru, so the vm is just getting
             * defined - create a map for it and put us in as we
             * are obviously already here! The ess will already
             * have assigned our node to us.
             */
            node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);
            if (NULL == node) {
                PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                return PRTE_ERR_NOT_FOUND;
            }
            pmix_pointer_array_add(map->nodes, (void *) node);
            ++(map->num_nodes);
            /* maintain accounting */
            PMIX_RETAIN(node);
            /* mark that this is from a singleton */
            singleton = true;
        }
        for (i = 1; i < prte_node_pool->size; i++) {
            if (NULL == (node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i))) {
                continue;
            }
            /* only add in nodes marked as "added" */
            if (!singleton && PRTE_NODE_STATE_ADDED != node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                     "%s plm_base:setup_vm NODE %s WAS NOT ADDED",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name));
                continue;
            }
            PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                 "%s plm_base:setup_vm ADDING NODE %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), node->name));
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            PMIX_RETAIN(node);
            pmix_list_append(&nodes, &node->super);
            /* reset the state so it can be used for mapping */
            node->state = PRTE_NODE_STATE_UP;
        }
        map->num_new_daemons = 0;
        /* if we didn't get anything, then there is nothing else to
         * do as no other daemons are to be launched
         */
        if (0 == pmix_list_get_size(&nodes)) {
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s plm:base:setup_vm no new daemons required",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            PMIX_DESTRUCT(&nodes);
            /* mark that the daemons have reported so we can proceed */
            daemons->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
            PRTE_FLAG_UNSET(daemons, PRTE_JOB_FLAG_UPDATED);
            return PRTE_SUCCESS;
        }
        /* if we got some new nodes to launch, we need to handle it */
        goto process;
    }

    /* if we are not working with a virtual machine, then we
     * look across all jobs and ensure that the "VM" contains
     * all nodes with application procs on them
     */
    multi_sim = prte_get_attribute(&jdata->attributes, PRTE_JOB_MULTI_DAEMON_SIM, NULL, PMIX_BOOL);
    if (prte_get_attribute(&daemons->attributes, PRTE_JOB_NO_VM, NULL, PMIX_BOOL) || multi_sim) {
        /* loop across all nodes and include those that have
         * num_procs > 0 && no daemon already on them
         */
        for (i = 1; i < prte_node_pool->size; i++) {
            if (NULL == (node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i))) {
                continue;
            }
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (PRTE_NODE_STATE_DO_NOT_USE == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_USE", node->name));
                /* reset the state so it can be used another time */
                node->state = PRTE_NODE_STATE_UP;
                continue;
            }
            if (PRTE_NODE_STATE_DOWN == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED DOWN", node->name));
                continue;
            }
            if (PRTE_NODE_STATE_NOT_INCLUDED == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_INCLUDE", node->name));
                /* not to be used */
                continue;
            }
            if (0 < node->num_procs || multi_sim) {
                /* retain a copy for our use in case the item gets
                 * destructed along the way
                 */
                PMIX_RETAIN(node);
                pmix_list_append(&nodes, &node->super);
            }
        }
        if (multi_sim) {
            goto process;
        }
        /* see if anybody had procs */
        if (0 == pmix_list_get_size(&nodes)) {
            /* if the HNP has some procs, then we are still good */
            node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);
            if (NULL == node) {
                PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                return PRTE_ERR_NOT_FOUND;
            }
            if (0 < node->num_procs) {
                PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                     "%s plm:base:setup_vm only HNP in use",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
                PMIX_DESTRUCT(&nodes);
                map->num_nodes = 1;
                /* mark that the daemons have reported so we can proceed */
                daemons->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
                return PRTE_SUCCESS;
            }
            /* well, if the HNP doesn't have any procs, and neither did
             * anyone else...then we have a big problem
             */
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
            return PRTE_ERR_FATAL;
        }
        goto process;
    }

    if (0 == map->num_nodes) {
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm creating map",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        /* this is the first time thru, so the vm is just getting
         * defined - put us in as we
         * are obviously already here! The ess will already
         * have assigned our node to us.
         */
        node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);
        if (NULL == node) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            return PRTE_ERR_NOT_FOUND;
        }
        pmix_pointer_array_add(map->nodes, (void *) node);
        ++(map->num_nodes);
        /* maintain accounting */
        PMIX_RETAIN(node);
    }

    /* zero-out the number of new daemons as we will compute this
     * each time we are called
     */
    map->num_new_daemons = 0;

    /* if this is an unmanaged allocation, then we use
     * the nodes that were specified for the union of
     * all apps - there is no need to collect all
     * available nodes and "filter" them
     */
    if (!prte_managed_allocation) {
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s setup:vm: working unmanaged allocation",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        default_hostfile_used = false;
        PMIX_CONSTRUCT(&tnodes, pmix_list_t);
        hosts = NULL;
        if (prte_get_attribute(&jdata->attributes, PRTE_JOB_FILE, (void **) &hosts, PMIX_STRING)) {
            /* use the file, if provided */
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s using rank/seqfile %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                 hosts));
            if (PRTE_SUCCESS != (rc = prte_util_add_hostfile_nodes(&tnodes, hosts))) {
                PRTE_ERROR_LOG(rc);
                free(hosts);
                return rc;
            }
            free(hosts);
        } else {
            for (i = 0; i < jdata->apps->size; i++) {
                if (NULL
                    == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i))) {
                    continue;
                }
                /* if the app provided a dash-host, then use those nodes */
                hosts = NULL;
                if (prte_get_attribute(&app->attributes, PRTE_APP_DASH_HOST, (void **) &hosts,
                                       PMIX_STRING)) {
                    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                         "%s using dash_host", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
                    if (PRTE_SUCCESS
                        != (rc = prte_util_add_dash_host_nodes(&tnodes, hosts, false))) {
                        PRTE_ERROR_LOG(rc);
                        free(hosts);
                        return rc;
                    }
                    free(hosts);
                } else if (prte_get_attribute(&app->attributes, PRTE_APP_HOSTFILE, (void **) &hosts,
                                              PMIX_STRING)) {
                    /* otherwise, if the app provided a hostfile, then use that */
                    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                         "%s using hostfile %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                         hosts));
                    if (PRTE_SUCCESS != (rc = prte_util_add_hostfile_nodes(&tnodes, hosts))) {
                        PRTE_ERROR_LOG(rc);
                        free(hosts);
                        return rc;
                    }
                    free(hosts);
                } else if (NULL != prte_default_hostfile) {
                    if (!default_hostfile_used) {
                        /* fall back to the default hostfile, if provided */
                        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                             "%s using default hostfile %s",
                                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                             prte_default_hostfile));
                        if (PRTE_SUCCESS
                            != (rc = prte_util_add_hostfile_nodes(&tnodes,
                                                                  prte_default_hostfile))) {
                            PRTE_ERROR_LOG(rc);
                            return rc;
                        }
                        /* only include it once */
                        default_hostfile_used = true;
                    }
                }
            }
        }

        /* cycle thru the resulting list, finding the nodes on
         * the node pool array while removing ourselves
         * and all nodes that are down or otherwise unusable
         */
        while (NULL != (item = pmix_list_remove_first(&tnodes))) {
            nptr = (prte_node_t *) item;
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output, "%s checking node %s",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), nptr->name));
            for (i = 0; i < prte_node_pool->size; i++) {
                node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i);
                if (NULL == node) {
                    continue;
                }
                if (!prte_nptr_match(node, nptr)) {
                    continue;
                }
                /* have a match - now see if we want this node */
                /* ignore nodes that are marked as do-not-use for this mapping */
                if (PRTE_NODE_STATE_DO_NOT_USE == node->state) {
                    PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                         "NODE %s IS MARKED NO_USE", node->name));
                    /* reset the state so it can be used another time */
                    node->state = PRTE_NODE_STATE_UP;
                    break;
                }
                if (PRTE_NODE_STATE_DOWN == node->state) {
                    PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                         "NODE %s IS MARKED DOWN", node->name));
                    break;
                }
                if (PRTE_NODE_STATE_NOT_INCLUDED == node->state) {
                    PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                         "NODE %s IS MARKED NO_INCLUDE", node->name));
                    break;
                }
                /* if this node is us, ignore it */
                if (0 == node->index) {
                    PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                         "%s ignoring myself", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
                    break;
                }
                /* we want it - add it to list */
                PMIX_RETAIN(node);
                pmix_list_append(&nodes, &node->super);
            }
            PMIX_RELEASE(nptr);
        }
        PMIX_LIST_DESTRUCT(&tnodes);
        /* if we didn't get anything, then we are the only node in the
         * allocation - so there is nothing else to do as no other
         * daemons are to be launched
         */
        if (0 == pmix_list_get_size(&nodes)) {
            PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                                 "%s plm:base:setup_vm only HNP in allocation",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            PMIX_DESTRUCT(&nodes);
            /* mark that the daemons have reported so we can proceed */
            daemons->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
            PRTE_FLAG_UNSET(daemons, PRTE_JOB_FLAG_UPDATED);
            return PRTE_SUCCESS;
        }
        /* continue processing */
        goto process;
    }

construct:
    /* construct a list of available nodes */
    for (i = 1; i < prte_node_pool->size; i++) {
        if (NULL != (node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, i))) {
            /* ignore nodes that are marked as do-not-use for this mapping */
            if (PRTE_NODE_STATE_DO_NOT_USE == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_USE", node->name));
                /* reset the state so it can be used another time */
                node->state = PRTE_NODE_STATE_UP;
                continue;
            }
            if (PRTE_NODE_STATE_DOWN == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED DOWN", node->name));
                continue;
            }
            if (PRTE_NODE_STATE_NOT_INCLUDED == node->state) {
                PMIX_OUTPUT_VERBOSE((10, prte_plm_base_framework.framework_output,
                                     "NODE %s IS MARKED NO_INCLUDE", node->name));
                /* not to be used */
                continue;
            }
            /* retain a copy for our use in case the item gets
             * destructed along the way
             */
            PMIX_RETAIN(node);
            pmix_list_append(&nodes, &node->super);
            /* by default, mark these as not to be included
             * so the filtering logic works correctly
             */
            PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_MAPPED);
        }
    }

    /* if we didn't get anything, then we are the only node in the
     * system - so there is nothing else to do as no other
     * daemons are to be launched
     */
    if (0 == pmix_list_get_size(&nodes)) {
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm only HNP in allocation",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        /* cleanup */
        PMIX_DESTRUCT(&nodes);
        /* mark that the daemons have reported so we can proceed */
        daemons->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
        PRTE_FLAG_UNSET(daemons, PRTE_JOB_FLAG_UPDATED);
        return PRTE_SUCCESS;
    }

    /* filter across the union of all app_context specs - if the HNP
     * was allocated, then we have to include
     * ourselves in case someone has specified a -host or hostfile
     * that includes the head node. We will remove ourselves later
     * as we clearly already exist
     */
    if (prte_hnp_is_allocated) {
        node = (prte_node_t *) pmix_pointer_array_get_item(prte_node_pool, 0);
        if (NULL == node) {
            PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
            return PRTE_ERR_NOT_FOUND;
        }
        PMIX_RETAIN(node);
        pmix_list_prepend(&nodes, &node->super);
    }
    for (i = 0; i < jdata->apps->size; i++) {
        if (NULL == (app = (prte_app_context_t *) pmix_pointer_array_get_item(jdata->apps, i))) {
            continue;
        }
        if (PRTE_SUCCESS != (rc = prte_rmaps_base_filter_nodes(app, &nodes, false))
            && rc != PRTE_ERR_TAKE_NEXT_OPTION) {
            PRTE_ERROR_LOG(rc);
            return rc;
        }
        if (PRTE_SUCCESS == rc) {
            /* we filtered something */
            one_filter = true;
        }
    }

    if (one_filter) {
        /* at least one filtering option was executed, so
         * remove all nodes that were not mapped
         */
        item = pmix_list_get_first(&nodes);
        while (item != pmix_list_get_end(&nodes)) {
            next = pmix_list_get_next(item);
            node = (prte_node_t *) item;
            if (!PRTE_FLAG_TEST(node, PRTE_NODE_FLAG_MAPPED)) {
                pmix_list_remove_item(&nodes, item);
                PMIX_RELEASE(item);
            } else {
                /* The filtering logic sets this flag only for nodes which
                 * are kept after filtering. This flag will be subsequently
                 * used in rmaps components and must be reset here */
                PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_MAPPED);
            }
            item = next;
        }
    }

    /* ensure we are not on the list */
    if (0 < pmix_list_get_size(&nodes)) {
        item = pmix_list_get_first(&nodes);
        node = (prte_node_t *) item;
        if (0 == node->index) {
            pmix_list_remove_item(&nodes, item);
            PMIX_RELEASE(item);
        }
    }

    /* if we didn't get anything, then we are the only node in the
     * allocation - so there is nothing else to do as no other
     * daemons are to be launched
     */
    if (0 == pmix_list_get_size(&nodes)) {
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm only HNP left",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        PMIX_DESTRUCT(&nodes);
        /* mark that the daemons have reported so we can proceed */
        daemons->state = PRTE_JOB_STATE_DAEMONS_REPORTED;
        PRTE_FLAG_UNSET(daemons, PRTE_JOB_FLAG_UPDATED);
        return PRTE_SUCCESS;
    }

process:
    /* cycle thru all available nodes and find those that do not already
     * have a daemon on them - no need to include our own as we are
     * obviously already here! If a max vm size was given, then limit
     * the overall number of active nodes to the given number. Only
     * count the HNP's node if it was included in the allocation
     */
    if (prte_hnp_is_allocated) {
        num_nodes = 1;
    } else {
        num_nodes = 0;
    }
    while (NULL != (item = pmix_list_remove_first(&nodes))) {
        /* if a max size was given and we are there, then exit the loop */
        if (0 < prte_max_vm_size && num_nodes == prte_max_vm_size) {
            /* maintain accounting */
            PMIX_RELEASE(item);
            break;
        }
        node = (prte_node_t *) item;
        /* if this node is already in the map, skip it */
        if (NULL != node->daemon) {
            num_nodes++;
            /* maintain accounting */
            PMIX_RELEASE(item);
            continue;
        }
        /* add the node to the map - we retained it
         * when adding it to the list, so we don't need
         * to retain it again
         */
        pmix_pointer_array_add(map->nodes, (void *) node);
        ++(map->num_nodes);
        num_nodes++;
        /* create a new daemon object for this node */
        proc = PMIX_NEW(prte_proc_t);
        if (NULL == proc) {
            PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        PMIX_LOAD_NSPACE(proc->name.nspace, PRTE_PROC_MY_NAME->nspace);
        if (PMIX_RANK_VALID - 1 <= daemons->num_procs) {
            /* no more daemons available */
            pmix_show_help("help-prte-rmaps-base.txt", "out-of-vpids", true);
            PMIX_RELEASE(proc);
            return PRTE_ERR_OUT_OF_RESOURCE;
        }
        proc->name.rank = daemons->num_procs; /* take the next available vpid */
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm add new daemon %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name)));
        /* add the daemon to the daemon job object */
        if (0
            > (rc = pmix_pointer_array_set_item(daemons->procs, proc->name.rank, (void *) proc))) {
            PRTE_ERROR_LOG(rc);
            return rc;
        }
        ++daemons->num_procs;
        PMIX_OUTPUT_VERBOSE((5, prte_plm_base_framework.framework_output,
                             "%s plm:base:setup_vm assigning new daemon %s to node %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&proc->name),
                             node->name));
        /* point the node to the daemon */
        node->daemon = proc;
        PMIX_RETAIN(proc); /* maintain accounting */
        /* point the proc to the node and maintain accounting */
        proc->node = node;
        PMIX_RETAIN(node);
        if (prte_plm_globals.daemon_nodes_assigned_at_launch) {
            PRTE_FLAG_SET(node, PRTE_NODE_FLAG_LOC_VERIFIED);
        } else {
            PRTE_FLAG_UNSET(node, PRTE_NODE_FLAG_LOC_VERIFIED);
        }
        /* track number of daemons to be launched */
        ++map->num_new_daemons;
        /* and their starting vpid */
        if (PMIX_RANK_INVALID == map->daemon_vpid_start) {
            map->daemon_vpid_start = proc->name.rank;
        }
        /* loop across all app procs on this node and update their parent */
        for (i = 0; i < node->procs->size; i++) {
            if (NULL != (pptr = (prte_proc_t *) pmix_pointer_array_get_item(node->procs, i))) {
                pptr->parent = proc->name.rank;
            }
        }
    }

    if (prte_process_info.num_daemons != daemons->num_procs) {
        /* more daemons are being launched - update the routing tree to
         * ensure that the HNP knows how to route messages via
         * the daemon routing tree - this needs to be done
         * here to avoid potential race conditions where the HNP
         * hasn't unpacked its launch message prior to being
         * asked to communicate.
         */
        prte_process_info.num_daemons = daemons->num_procs;

        /* ensure all routing plans are up-to-date - we need this
         * so we know how to tree-spawn and/or xcast info */
        prte_rml_compute_routing_tree();
    }

    /* mark that the daemon job changed */
    PRTE_FLAG_SET(daemons, PRTE_JOB_FLAG_UPDATED);

    /* if new daemons are being launched, mark that this job
     * caused it to happen */
    if (0 < map->num_new_daemons) {
        rc = prte_set_attribute(&jdata->attributes, PRTE_JOB_LAUNCHED_DAEMONS, true,
                                NULL, PMIX_BOOL);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            return rc;
        }
    }

    return PRTE_SUCCESS;
}
