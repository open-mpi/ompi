/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2008 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007-2015 Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2019      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2021      Amazon.com, Inc. or its affiliates.  All Rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include <stdio.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#    include <sys/param.h>
#endif
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#ifdef HAVE_SYS_TYPES_H
#    include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_TIME_H
#    include <sys/time.h>
#endif /* HAVE_SYS_TIME_H */

#include "src/mca/errmgr/errmgr.h"
#include "src/mca/plm/plm.h"
#include "src/mca/state/state.h"

#include "src/threads/pmix_threads.h"
#include "src/util/pmix_output.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/runtime/prte_data_server.h"
#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_locks.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/runtime.h"

/*
 * Globals
 */
static int num_aborted = 0;
static int num_killed = 0;
static int num_failed_start = 0;

void prte_quit(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);
    
    PMIX_ACQUIRE_OBJECT(caddy);

    /* cleanup */
    if (NULL != caddy) {
        PMIX_RELEASE(caddy);
    }

    /* check one-time lock to protect against "bounce" */
    if (pmix_mutex_trylock(&prte_quit_lock)) { /* returns 1 if already locked */
        return;
    }

    /* flag that the event lib should no longer be looped
     * so we will exit
     */
    prte_event_base_active = false;
    PMIX_POST_OBJECT(prte_event_base_active);
    /* break the event loop - this will cause the loop to exit upon
       completion of any current event */
    prte_event_base_loopexit(prte_event_base);
}

static char *print_aborted_job(prte_job_t *job,
                               prte_app_context_t *approc,
                               prte_proc_t *proc,
                               prte_node_t *node)
{
    char *output = NULL;

    if (PRTE_PROC_STATE_FAILED_TO_START == proc->state ||
        PRTE_PROC_STATE_FAILED_TO_LAUNCH == proc->state) {
        switch (proc->exit_code) {
        case PMIX_ERR_SILENT:
        case PRTE_ERR_SILENT:
            /* say nothing - it was already reported */
            break;
        case PMIX_ERR_SYS_LIMITS_PIPES:
            output = pmix_show_help_string("help-prun.txt", "prun:sys-limit-pipe", true,
                                           prte_tool_basename, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PMIX_ERR_PIPE_SETUP_FAILURE:
            output = pmix_show_help_string("help-prun.txt", "prun:pipe-setup-failure", true,
                                           prte_tool_basename, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PMIX_ERR_SYS_LIMITS_CHILDREN:
            output = pmix_show_help_string("help-prun.txt", "prun:sys-limit-children", true,
                                           prte_tool_basename, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PMIX_ERR_SYS_LIMITS_FILES:
            output = pmix_show_help_string("help-prun.txt", "prun:sys-limit-files", true,
                                           prte_tool_basename, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PRTE_ERR_FAILED_GET_TERM_ATTRS:
            output = pmix_show_help_string("help-prun.txt", "prun:failed-term-attrs", true,
                                           prte_tool_basename, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PMIX_ERR_JOB_WDIR_NOT_FOUND:
            output = pmix_show_help_string("help-prun.txt", "prun:wdir-not-found", true,
                                           prte_tool_basename, approc->cwd, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PMIX_ERR_JOB_WDIR_NOT_ACCESSIBLE:
             output = pmix_show_help_string("help-prun.txt", "prun:wdir-not-accessible", true,
                                            prte_tool_basename, approc->cwd, node->name,
                                            (unsigned long) proc->name.rank);
             break;
        case PMIX_ERR_JOB_EXE_NOT_FOUND:
            output = pmix_show_help_string("help-prun.txt", "prun:exe-not-found", true,
                                           prte_tool_basename, (unsigned long) proc->name.rank,
                                           prte_tool_basename, prte_tool_basename, node->name,
                                           approc->app);
            break;
        case PMIX_ERR_EXE_NOT_ACCESSIBLE:
            output = pmix_show_help_string("help-prun.txt", "prun:exe-not-accessible", true,
                                           prte_tool_basename, approc->app, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PRTE_ERR_MULTIPLE_AFFINITIES:
            output = pmix_show_help_string("help-prun.txt", "prun:multiple-paffinity-schemes", true,
                                           NULL);
            break;
        case PRTE_ERR_TOPO_SLOT_LIST_NOT_SUPPORTED:
            output = pmix_show_help_string("help-prun.txt", "prun:topo-not-supported", true,
                                           prte_process_info.nodename,
                                           "rankfile containing a slot_list of ", NULL,
                                           approc->app);
            break;
        case PRTE_ERR_INVALID_NODE_RANK:
            output = pmix_show_help_string("help-prun.txt", "prun:invalid-node-rank", true);
            break;
        case PRTE_ERR_INVALID_LOCAL_RANK:
            output = pmix_show_help_string("help-prun.txt", "prun:invalid-local-rank", true);
            break;
        case PRTE_ERR_NOT_ENOUGH_CORES:
            output = pmix_show_help_string("help-prun.txt", "prun:not-enough-resources", true,
                                           "sockets", node->name, "bind-to-core", approc->app);
            break;
        case PRTE_ERR_TOPO_CORE_NOT_SUPPORTED:
            output = pmix_show_help_string("help-prun.txt", "prun:topo-not-supported", true,
                                           node->name, "bind-to-core", "", approc->app);
            break;
        case PRTE_ERR_INVALID_PHYS_CPU:
            output = pmix_show_help_string("help-prun.txt", "prun:invalid-phys-cpu", true);
            break;
        case PRTE_ERR_NOT_ENOUGH_SOCKETS:
            output = pmix_show_help_string("help-prun.txt", "prun:not-enough-resources", true,
                                           "sockets", node->name, "bind-to-socket", approc->app);
            break;
        case PRTE_ERR_TOPO_SOCKET_NOT_SUPPORTED:
            output = pmix_show_help_string("help-prun.txt", "prun:topo-not-supported", true,
                                           node->name, "bind-to-socket", "", approc->app);
            break;
        case PRTE_ERR_MODULE_NOT_FOUND:
            output = pmix_show_help_string("help-prun.txt", "prun:paffinity-missing-module", true,
                                           node->name);
            break;
        case PRTE_ERR_SLOT_LIST_RANGE:
            output = pmix_show_help_string("help-prun.txt", "prun:invalid-slot-list-range", true,
                                           node->name, NULL);
            break;
        case PRTE_ERR_PIPE_READ_FAILURE:
            output = pmix_show_help_string("help-prun.txt", "prun:pipe-read-failure", true,
                                           prte_tool_basename, node->name,
                                           (unsigned long) proc->name.rank);
            break;
        case PRTE_ERR_SOCKET_NOT_AVAILABLE:
            output = pmix_show_help_string("help-prun.txt", "prun:proc-socket-not-avail", true,
                                           prte_tool_basename, PRTE_ERROR_NAME(proc->exit_code),
                                           node->name, (unsigned long) proc->name.rank);
            break;

        default:
            if (0 != proc->exit_code) {
                output = pmix_show_help_string("help-prun.txt", "prun:proc-failed-to-start", true,
                                               prte_tool_basename, proc->exit_code,
                                               PRTE_ERROR_NAME(proc->exit_code), node->name,
                                               (unsigned long) proc->name.rank);
            } else {
                output = pmix_show_help_string("help-prun.txt",
                                               "prun:proc-failed-to-start-no-status", true,
                                               prte_tool_basename, node->name);
            }
        }
        return output;
    } else if (PRTE_PROC_STATE_ABORTED == proc->state ||
               PRTE_PROC_STATE_CALLED_ABORT == proc->state) {
        output = pmix_show_help_string("help-prun.txt", "prun:proc-ordered-abort", true,
                                       prte_tool_basename, (unsigned long) proc->name.rank,
                                       (unsigned long) proc->pid, node->name, prte_tool_basename);
        return output;
    } else if (PRTE_PROC_STATE_ABORTED_BY_SIG == job->state) { /* aborted by signal */
#ifdef HAVE_STRSIGNAL
        if (NULL != strsignal(WTERMSIG(proc->exit_code))) {
            output = pmix_show_help_string("help-prun.txt", "prun:proc-aborted-strsignal", true,
                                           prte_tool_basename, (unsigned long) proc->name.rank,
                                           (unsigned long) proc->pid, node->name,
                                           WTERMSIG(proc->exit_code),
                                           strsignal(WTERMSIG(proc->exit_code)));
        } else {
#endif
            output = pmix_show_help_string("help-prun.txt", "prun:proc-aborted", true,
                                           prte_tool_basename, (unsigned long) proc->name.rank,
                                           (unsigned long) proc->pid, node->name,
                                           WTERMSIG(proc->exit_code));
#ifdef HAVE_STRSIGNAL
        }
#endif
        return output;
    } else if (PRTE_PROC_STATE_TERM_WO_SYNC == proc->state) { /* proc exited w/o finalize */
        output = pmix_show_help_string("help-prun.txt", "prun:proc-exit-no-sync", true,
                                       prte_tool_basename, (unsigned long) proc->name.rank,
                                       (unsigned long) proc->pid, node->name, prte_tool_basename,
                                       prte_tool_basename);
        return output;
    } else if (PRTE_PROC_STATE_COMM_FAILED == proc->state) {
        output = pmix_show_help_string("help-prun.txt", "prun:proc-comm-failed", true,
                                       PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                       PRTE_NAME_PRINT(&proc->name), node->name);
        return output;
    } else if (PRTE_PROC_STATE_SENSOR_BOUND_EXCEEDED == proc->state) {
        switch (proc->exit_code) {
        case PRTE_ERR_MEM_LIMIT_EXCEEDED:
            output = pmix_show_help_string("help-prun.txt", "prun:proc-mem-exceeded", true,
                                           PRTE_NAME_PRINT(&proc->name), node->name);
            break;
        case PRTE_ERR_PROC_STALLED:
            output = pmix_show_help_string("help-prun.txt", "prun:proc-stalled", true);
            break;

        default:
            output = pmix_show_help_string("help-prun.txt", "prun:proc-sensor-exceeded", true);
        }
        return output;
    } else if (PRTE_PROC_STATE_HEARTBEAT_FAILED == proc->state) {
        output = pmix_show_help_string("help-prun.txt", "prun:proc-heartbeat-failed", true,
                                       prte_tool_basename, PRTE_NAME_PRINT(&proc->name),
                                       node->name);
        return output;
    } else if (PRTE_PROC_STATE_TERM_NON_ZERO == proc->state) {
        if (prte_get_attribute(&job->attributes, PRTE_JOB_ERROR_NONZERO_EXIT, NULL, PMIX_BOOL)) {
            output = pmix_show_help_string("help-prun.txt", "prun:non-zero-exit", true,
                                           prte_tool_basename, PRTE_NAME_PRINT(&proc->name),
                                           proc->exit_code);
            return output;
        }
    }

    /* nothing here */
    return NULL;
}

/*
 * On abnormal termination - dump the
 * exit status of the aborted procs.
 */

static char *dump_job(prte_job_t *job)

{
    int32_t i;
    prte_proc_t *proc, *pptr;
    prte_app_context_t *approc;
    prte_node_t *node;

    /* cycle through and count the number that were killed or aborted */
    for (i = 0; i < job->procs->size; i++) {
        if (NULL == (pptr = (prte_proc_t *) pmix_pointer_array_get_item(job->procs, i))) {
            /* array is left-justified - we are done */
            break;
        }
        if (PRTE_PROC_STATE_FAILED_TO_START == pptr->state ||
            PRTE_PROC_STATE_FAILED_TO_LAUNCH == pptr->state) {
            ++num_failed_start;
        } else if (PRTE_PROC_STATE_ABORTED == pptr->state) {
            ++num_aborted;
        } else if (PRTE_PROC_STATE_ABORTED_BY_SIG == pptr->state) {
            ++num_killed;
        } else if (PRTE_PROC_STATE_SENSOR_BOUND_EXCEEDED == pptr->state) {
            ++num_killed;
        }
    }
    /* see if there is a guilty party */
    proc = NULL;
    if (!prte_get_attribute(&job->attributes, PRTE_JOB_ABORTED_PROC, (void **) &proc, PMIX_POINTER)
        || NULL == proc) {
        return NULL;
    }

    approc = (prte_app_context_t *) pmix_pointer_array_get_item(job->apps, proc->app_idx);
    node = proc->node;
    return print_aborted_job(job, approc, proc, node);
}

char *prte_dump_aborted_procs(prte_job_t *jdata)
{
    prte_job_t *job, *launcher;
    char *output = NULL;

    /* if we already reported it, then don't do it again */
    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ERR_REPORTED)) {
        return NULL;
    }
    PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_ERR_REPORTED);

    /* if this job is not a launcher itself, then get the launcher for this job */
    if (PMIX_NSPACE_INVALID(jdata->launcher)) {
        launcher = jdata;
    } else {
        launcher = prte_get_job_data_object(jdata->launcher);
        if (NULL == launcher) {
            output = strdup("LAUNCHER JOB OBJECT NOT FOUND");
            return output;
        }
    }

    /* cycle thru all the children of this launcher to find the
     * one that caused the error */
    /* if this is a non-persistent job, it won't have any child
     * jobs, so look at it directly */
    if (0 == pmix_list_get_size(&launcher->children)) {
        output = dump_job(jdata);
    } else {
        PMIX_LIST_FOREACH(job, &launcher->children, prte_job_t)
        {
            output = dump_job(job);
            if (NULL != output) {
                break;
            }
        }
    }

    return output;
}
