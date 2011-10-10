/* -*- C -*-
 *
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
 * Copyright (c) 2006-2010 Cisco Systems, Inc. All rights reserved.
 * Copyright (c) 2007-2009 Sun Microsystems, Inc. All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <stdio.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <errno.h>
#include <signal.h>
#include <ctype.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif  /* HAVE_SYS_TYPES_H */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif  /* HAVE_SYS_WAIT_H */
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif  /* HAVE_SYS_TIME_H */

#include "orte/mca/plm/plm.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/debugger/debugger.h"
#include "orte/mca/routed/routed.h"

#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_quit.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_data_server.h"

/*
 * Globals
 */

static int num_aborted = 0;
static int num_killed = 0;
static int num_failed_start = 0;

static void dump_aborted_procs(void);

void orte_jobs_complete(void)
{
#if !ORTE_DISABLE_FULL_SUPPORT
    /* check one-time lock to protect against multiple calls */
    if (!opal_atomic_trylock(&orte_jobs_complete_lock)) { /* returns 1 if already locked */
        return;
    }

    /* if we never launched, just skip this part to avoid
     * meaningless error messages
     */
    if (orte_never_launched) {
        ORTE_UPDATE_EXIT_STATUS(orte_exit_status);
        orte_quit();
    }
    
    if (0 != orte_exit_status && !orte_execute_quiet) {
        /* abnormal termination of some kind */
        dump_aborted_procs();
        /* If we showed more abort messages than were allowed,
           show a followup message here */
        if (num_failed_start > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s failed to start",
                    num_failed_start, ((num_failed_start > 1) ? "es" : ""));
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
        if (num_aborted > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s aborted",
                    num_aborted, ((num_aborted > 1) ? "es" : ""));
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
        if (num_killed > 1) {
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "<stderr>");
            }
            fprintf(orte_xml_fp, "%d total process%s killed (some possibly by %s during cleanup)",
                    num_killed, ((num_killed > 1) ? "es" : ""), orte_basename);
            if (orte_xml_output) {
                fprintf(orte_xml_fp, "&#010;</stderr>");
            }
            fprintf(orte_xml_fp, "\n");
        }
    }
    
    /* if the debuggers were run, clean up */
    orte_debugger.finalize();

    if (0 < orte_routed.num_routes()) {
        orte_plm.terminate_orteds();
    }
#endif
}

void orte_quit(void)
{
    /* check one-time lock to protect against "bounce" */
    if (!opal_atomic_trylock(&orte_quit_lock)) { /* returns 1 if already locked */
        return;
    }

    /* flag that we are finalizing */
    orte_finalizing = true;

    /* whack any lingering session directory files from our jobs */
    orte_session_dir_cleanup(ORTE_JOBID_WILDCARD);

#if !ORTE_DISABLE_FULL_SUPPORT    
    /* cleanup our data server */
    orte_data_server_finalize();
#endif    

    /* cleanup and leave */
    orte_finalize();

#if !ORTE_DISABLE_FULL_SUPPORT
        if (NULL != orte_basename) {
        free(orte_basename);
    }
        
    if (orte_debug_flag) {
        fprintf(stderr, "orterun: exiting with status %d\n", orte_exit_status);
    }
    exit(orte_exit_status);
#else
    exit(0);
#endif
}



#if !ORTE_DISABLE_FULL_SUPPORT
/*
 * On abnormal termination - dump the
 * exit status of the aborted procs.
 */

static void dump_aborted_procs(void)
{
    orte_std_cntr_t i, n;
    orte_proc_t *proc, *pptr;
    orte_app_context_t *app, *approc;
    orte_job_t *job;
    orte_node_t *node;
    
    /* find the job that caused the problem - be sure to start the loop
     * at 1 as the daemons are in 0 and will clearly be "running", so no
     * point in checking them
     */
    for (n=1; n < orte_job_data->size; n++) {
        if (NULL == (job = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, n))) {
            /* the array is no longer left-justified, so we have to continue */
            continue;
        }
        if (ORTE_JOB_STATE_UNDEF != job->state &&
            ORTE_JOB_STATE_INIT != job->state &&
            ORTE_JOB_STATE_LAUNCHED != job->state &&
            ORTE_JOB_STATE_RUNNING != job->state &&
            ORTE_JOB_STATE_TERMINATED != job->state &&
            ORTE_JOB_STATE_ABORT_ORDERED != job->state) {
            /* this is a guilty party */
            proc = job->aborted_proc;
            /* always must be at least one app */
            app = (orte_app_context_t*)opal_pointer_array_get_item(job->apps, 0);
            /* cycle through and count the number that were killed or aborted */
            for (i=0; i < job->procs->size; i++) {
                if (NULL == (pptr = (orte_proc_t*)opal_pointer_array_get_item(job->procs, i))) {
                    /* array is left-justfied - we are done */
                    continue;
                }
                if (ORTE_PROC_STATE_FAILED_TO_START == pptr->state) {
                    ++num_failed_start;
                } else if (ORTE_PROC_STATE_ABORTED == pptr->state) {
                    ++num_aborted;
                } else if (ORTE_PROC_STATE_ABORTED_BY_SIG == pptr->state) {
                    ++num_killed;
                } else if (ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED == pptr->state) {
                    ++num_killed;
                }
            }

            if (NULL == proc) {
                continue;
            }

            approc = (orte_app_context_t*)opal_pointer_array_get_item(job->apps, proc->app_idx);
            node = proc->node;
            if (ORTE_JOB_STATE_FAILED_TO_START == job->state) {
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start-no-status-no-node", true,
                                   orte_basename);
                    return;
                }
                switch (OPAL_SOS_GET_ERROR_CODE(proc->exit_code)) {
                    case ORTE_ERR_SYS_LIMITS_PIPES:
                        orte_show_help("help-orterun.txt", "orterun:sys-limit-pipe", true,
                                       orte_basename, proc->node->name,
                                       (unsigned long)proc->name.vpid);
                        break;
                        case ORTE_ERR_PIPE_SETUP_FAILURE:
                        orte_show_help("help-orterun.txt", "orterun:pipe-setup-failure", true,
                                       orte_basename, proc->node->name,
                                       (unsigned long)proc->name.vpid);
                        break;
                    case ORTE_ERR_SYS_LIMITS_CHILDREN:
                        orte_show_help("help-orterun.txt", "orterun:sys-limit-children", true,
                                       orte_basename, proc->node->name,
                                       (unsigned long)proc->name.vpid);
                        break;
                    case ORTE_ERR_FAILED_GET_TERM_ATTRS:
                        orte_show_help("help-orterun.txt", "orterun:failed-term-attrs", true,
                                       orte_basename, proc->node->name,
                                       (unsigned long)proc->name.vpid);
                        break;
                    case ORTE_ERR_WDIR_NOT_FOUND:
                        orte_show_help("help-orterun.txt", "orterun:wdir-not-found", true,
                                       orte_basename, approc->cwd,
                                       proc->node->name, (unsigned long)proc->name.vpid);
                        break;
                    case ORTE_ERR_EXE_NOT_FOUND:
                        orte_show_help("help-orterun.txt", "orterun:exe-not-found", true,
                                       orte_basename, 
                                       (unsigned long)proc->name.vpid,
                                       orte_basename, 
                                       orte_basename, 
                                       proc->node->name, 
                                       approc->app);
                        break;
                    case ORTE_ERR_EXE_NOT_ACCESSIBLE:
                        orte_show_help("help-orterun.txt", "orterun:exe-not-accessible", true,
                                       orte_basename, approc->app, proc->node->name,
                                       (unsigned long)proc->name.vpid);
                        break;
                    case ORTE_ERR_MULTIPLE_AFFINITIES:
                        orte_show_help("help-orterun.txt",
                                       "orterun:multiple-paffinity-schemes", true, proc->slot_list);
                        break;
                    case ORTE_ERR_TOPO_SLOT_LIST_NOT_SUPPORTED:
                        orte_show_help("help-orterun.txt",
                                       "orterun:topo-not-supported", 
                                       true, orte_process_info.nodename, "rankfile containing a slot_list of ", 
                                       proc->slot_list, approc->app);
                        break;
                    case ORTE_ERR_INVALID_NODE_RANK:
                        orte_show_help("help-orterun.txt",
                                       "orterun:invalid-node-rank", true);
                        break;
                    case ORTE_ERR_INVALID_LOCAL_RANK:
                        orte_show_help("help-orterun.txt",
                                       "orterun:invalid-local-rank", true);
                        break;
                    case ORTE_ERR_NOT_ENOUGH_CORES:
                        orte_show_help("help-orterun.txt",
                                       "orterun:not-enough-resources", true,
                                       "sockets", node->name,
                                       "bind-to-core", approc->app);
                        break;
                    case ORTE_ERR_TOPO_CORE_NOT_SUPPORTED:
                        orte_show_help("help-orterun.txt",
                                       "orterun:topo-not-supported", 
                                       true, node->name, "bind-to-core", "",
                                       approc->app);
                        break;
                    case ORTE_ERR_INVALID_PHYS_CPU:
                        orte_show_help("help-orterun.txt",
                                       "orterun:invalid-phys-cpu", true);
                        break;
                    case ORTE_ERR_NOT_ENOUGH_SOCKETS:
                        orte_show_help("help-orterun.txt",
                                       "orterun:not-enough-resources", true,
                                       "sockets", node->name,
                                       "bind-to-socket", approc->app);
                        break;
                    case ORTE_ERR_TOPO_SOCKET_NOT_SUPPORTED:
                        orte_show_help("help-orterun.txt",
                                       "orterun:topo-not-supported", 
                                       true, node->name, "bind-to-socket", "",
                                       approc->app);
                        break;
                    case ORTE_ERR_MODULE_NOT_FOUND:
                        orte_show_help("help-orterun.txt",
                                       "orterun:paffinity-missing-module", 
                                       true, node->name);
                        break;
                    case ORTE_ERR_SLOT_LIST_RANGE:
                        orte_show_help("help-orterun.txt",
                                       "orterun:invalid-slot-list-range", 
                                       true, node->name, proc->slot_list);
                        break;
                    case ORTE_ERR_PIPE_READ_FAILURE:
                        orte_show_help("help-orterun.txt", "orterun:pipe-read-failure", true,
                                       orte_basename, node->name, (unsigned long)proc->name.vpid);
                        break;
                    case ORTE_ERR_SOCKET_NOT_AVAILABLE:
                        orte_show_help("help-orterun.txt", "orterun:proc-socket-not-avail", true,
                                       orte_basename, ORTE_ERROR_NAME(proc->exit_code), node->name,
                                       (unsigned long)proc->name.vpid);
                        break;

                    default:
                        if (0 != proc->exit_code) {
                            orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start", true,
                                           orte_basename, ORTE_ERROR_NAME(proc->exit_code), node->name,
                                           (unsigned long)proc->name.vpid);
                        } else {
                            orte_show_help("help-orterun.txt", "orterun:proc-failed-to-start-no-status", true,
                                           orte_basename, node->name);
                        }
                        break;
                }
            } else if (ORTE_JOB_STATE_ABORTED == job->state) {
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-aborted-unknown", true,
                                   orte_basename);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-ordered-abort", true,
                                   orte_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                   node->name, orte_basename);
                }
            } else if (ORTE_JOB_STATE_ABORTED_BY_SIG == job->state) {  /* aborted by signal */
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-aborted-signal-unknown", true,
                                   orte_basename);
                } else {
#ifdef HAVE_STRSIGNAL
                    if (NULL != strsignal(WTERMSIG(proc->exit_code))) {
                        orte_show_help("help-orterun.txt", "orterun:proc-aborted-strsignal", true,
                                       orte_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                       node->name, WTERMSIG(proc->exit_code), 
                                       strsignal(WTERMSIG(proc->exit_code)));
                    } else {
#endif
                        orte_show_help("help-orterun.txt", "orterun:proc-aborted", true,
                                       orte_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                       node->name, WTERMSIG(proc->exit_code));
#ifdef HAVE_STRSIGNAL
                    }
#endif
                }
            } else if (ORTE_JOB_STATE_ABORTED_WO_SYNC == job->state) { /* proc exited w/o finalize */
                if (NULL == proc) {
                    orte_show_help("help-orterun.txt", "orterun:proc-exit-no-sync-unknown", true,
                                   orte_basename, orte_basename);
                } else {
                    orte_show_help("help-orterun.txt", "orterun:proc-exit-no-sync", true,
                                   orte_basename, (unsigned long)proc->name.vpid, (unsigned long)proc->pid,
                                   node->name, orte_basename, orte_basename);
                }
            } else if (ORTE_JOB_STATE_COMM_FAILED == job->state) {
                orte_show_help("help-orterun.txt", "orterun:proc-comm-failed", true,
                               ORTE_NAME_PRINT(&proc->name), node->name);
            } else if (ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED == job->state) {
                switch (proc->exit_code) {
                    case ORTE_ERR_MEM_LIMIT_EXCEEDED:
                        orte_show_help("help-orterun.txt", "orterun:proc-mem-exceeded", true,
                                       ORTE_NAME_PRINT(&proc->name), node->name);
                        break;
                    case ORTE_ERR_PROC_STALLED:
                        orte_show_help("help-orterun.txt", "orterun:proc-stalled", true);
                        break;

                    default:
                        orte_show_help("help-orterun.txt", "orterun:proc-sensor-exceeded", true);
                        break;
                }
            } else if (ORTE_JOB_STATE_CALLED_ABORT == job->state) {
                orte_show_help("help-orterun.txt", "orterun:proc-called-abort", true,
                               orte_basename,
                               (0 == strncmp("orte", orte_basename, 4)) ? "orte" : "MPI");
            } else if (ORTE_JOB_STATE_HEARTBEAT_FAILED == job->state) {
                orte_show_help("help-orterun.txt", "orterun:proc-heartbeat-failed", true,
                               orte_basename, ORTE_NAME_PRINT(&proc->name), node->name);
            } else if (orte_abort_non_zero_exit &&
                       ORTE_JOB_STATE_NON_ZERO_TERM == job->state) {
                orte_show_help("help-orterun.txt", "orterun:non-zero-exit", true,
                               orte_basename, ORTE_NAME_PRINT(&proc->name), proc->exit_code);
            }
            return;
        }
    }
}
#endif
