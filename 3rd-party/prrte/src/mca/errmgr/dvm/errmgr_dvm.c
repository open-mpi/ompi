/*
 * Copyright (c) 2009-2011 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010-2017 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011      Oracle and/or all its affiliates.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#    include <unistd.h>
#endif /* HAVE_UNISTD_H */
#include <string.h>
#ifdef HAVE_SYS_WAIT_H
#    include <sys/wait.h>
#endif
#include "src/pmix/pmix-internal.h"
#include <pmix.h>
#include <pmix_server.h>

#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"

#include "src/mca/ess/ess.h"
#include "src/mca/grpcomm/grpcomm.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/plm/base/base.h"
#include "src/mca/rmaps/rmaps_types.h"
#include "src/rml/rml.h"
#include "src/mca/state/state.h"

#include "src/threads/pmix_threads.h"
#include "src/util/error_strings.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/pmix_show_help.h"

#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_locks.h"
#include "src/runtime/prte_quit.h"

#include "src/mca/errmgr/base/base.h"
#include "src/mca/errmgr/base/errmgr_private.h"
#include "src/mca/errmgr/errmgr.h"

#include "errmgr_dvm.h"

static int init(void);
static int finalize(void);

/******************
 * dvm module
 ******************/
prte_errmgr_base_module_t prte_errmgr_dvm_module = {
    .init = init,
    .finalize = finalize,
    .logfn = prte_errmgr_base_log
};

/*
 * Local functions
 */
static void job_errors(int fd, short args, void *cbdata);
static void proc_errors(int fd, short args, void *cbdata);
static void check_send_notification(prte_job_t *jdata,
                                    prte_proc_t *proc,
                                    pmix_status_t event);

static int init(void)
{
    /* setup state machine to trap job errors */
    prte_state.add_job_state(PRTE_JOB_STATE_ERROR, job_errors);

    /* set the lost connection state to run at MSG priority so
     * we can process any last messages from the proc
     */
    prte_state.add_proc_state(PRTE_PROC_STATE_COMM_FAILED, proc_errors);

    prte_state.add_proc_state(PRTE_PROC_STATE_ERROR, proc_errors);

    return PRTE_SUCCESS;
}

static int finalize(void)
{
    return PRTE_SUCCESS;
}

static void _terminate_job(pmix_nspace_t jobid)
{
    pmix_pointer_array_t procs;
    prte_proc_t pobj;

    PMIX_CONSTRUCT(&procs, pmix_pointer_array_t);
    pmix_pointer_array_init(&procs, 1, 1, 1);
    PMIX_CONSTRUCT(&pobj, prte_proc_t);
    PMIX_LOAD_PROCID(&pobj.name, jobid, PMIX_RANK_WILDCARD);
    pmix_pointer_array_add(&procs, &pobj);
    prte_plm.terminate_procs(&procs);
    PMIX_DESTRUCT(&procs);
    PMIX_DESTRUCT(&pobj);
}

static void job_errors(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata;
    prte_job_state_t jobstate;
    int32_t rc;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    /*
     * if prte is trying to shutdown, just let it
     */
    if (prte_finalizing) {
        return;
    }

    /* if the jdata is NULL, then it is referencing the daemon job */
    if (NULL == caddy->jdata) {
        caddy->jdata = prte_get_job_data_object(PRTE_PROC_MY_NAME->nspace);
        PMIX_RETAIN(caddy->jdata);
    }

    /* update the state */
    jdata = caddy->jdata;
    jobstate = caddy->job_state;
    jdata->state = jobstate;

    PMIX_OUTPUT_VERBOSE((1, prte_errmgr_base_framework.framework_output,
                         "%s errmgr:dvm: job %s reported state %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace),
                         prte_job_state_to_str(jobstate)));

    if (PMIX_CHECK_NSPACE(jdata->nspace, PRTE_PROC_MY_NAME->nspace)) {
        if (PRTE_JOB_STATE_FAILED_TO_START == jdata->state
            || PRTE_JOB_STATE_NEVER_LAUNCHED == jdata->state
            || PRTE_JOB_STATE_FAILED_TO_LAUNCH == jdata->state
            || PRTE_JOB_STATE_CANNOT_LAUNCH == jdata->state) {
            prte_routing_is_enabled = false;
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            PMIX_RELEASE(caddy);
            return;
        }
        /* if the daemon job aborted and we haven't heard from everyone yet,
         * then this could well have been caused by a daemon not finding
         * a way back to us. In this case, output a message indicating a daemon
         * died without reporting. Otherwise, say nothing as we
         * likely already output an error message */
        if (PRTE_JOB_STATE_ABORTED == jobstate && jdata->num_procs != jdata->num_reported) {
            prte_routing_is_enabled = false;
            pmix_show_help("help-errmgr-base.txt", "failed-daemon", true);
        }
        /* there really isn't much else we can do since the problem
         * is in the DVM itself, so best just to terminate */
        jdata->num_terminated = jdata->num_procs;
        /* activate the terminated state so we can exit */
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_TERMINATED);
        PMIX_RELEASE(caddy);
        return;
    }

    /* all other cases involve jobs submitted to the DVM - therefore,
     * we only inform the submitter of the problem, but do NOT terminate
     * the DVM itself */

    PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                         "%s errmgr:dvm sending notification of job %s failure to %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace),
                         PRTE_NAME_PRINT(&jdata->originator)));

    /* all jobs were spawned by a requestor, so ensure that requestor
     * has been notified that the spawn completed - otherwise, a quick-failing
     * job might not generate a spawn response */
    rc = prte_pmix_convert_job_state_to_error(jobstate);
    rc = prte_plm_base_spawn_response(rc, jdata);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
    }

    /* ensure we terminate any processes left running in the DVM */
    _terminate_job(jdata->nspace);

    /* if the job never launched, then we need to let the
     * state machine know this job failed - it has no
     * other means of being alerted since no proc states
     * will be triggered */
    if (PRTE_JOB_STATE_FAILED_TO_START == jdata->state
        || PRTE_JOB_STATE_NEVER_LAUNCHED == jdata->state
        || PRTE_JOB_STATE_FAILED_TO_LAUNCH == jdata->state
        || PRTE_JOB_STATE_ALLOC_FAILED == jdata->state
        || PRTE_JOB_STATE_MAP_FAILED == jdata->state
        || PRTE_JOB_STATE_CANNOT_LAUNCH == jdata->state) {
        PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_TERMINATED);
    }

    /* cleanup */
    PMIX_RELEASE(caddy);
}

static void proc_errors(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata;
    prte_proc_t *pptr, *proct;
    pmix_proc_t *proc = &caddy->name;
    prte_proc_state_t state = caddy->proc_state;
    int i;
    int32_t i32, *i32ptr;
    bool flag;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    PMIX_OUTPUT_VERBOSE((1, prte_errmgr_base_framework.framework_output,
                         "%s errmgr:dvm: for proc %s state %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                         PRTE_NAME_PRINT(proc), prte_proc_state_to_str(state)));

    /* get the job object */
    if (prte_finalizing || NULL == (jdata = prte_get_job_data_object(proc->nspace))) {
        /* could be a race condition */
        PMIX_RELEASE(caddy);
        return;
    }
    pptr = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, proc->rank);
    if (NULL == pptr) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        goto cleanup;
    }

    if (PMIX_CHECK_NSPACE(jdata->nspace, PRTE_PROC_MY_NAME->nspace)) {
        /* NOTE: this is a daemon process that had the error */
        /* we MUST handle a communication failure with special care to
         * avoid normal termination issues */
        if (PRTE_PROC_STATE_COMM_FAILED == state ||
            PRTE_PROC_STATE_HEARTBEAT_FAILED == state ||
            PRTE_PROC_STATE_UNABLE_TO_SEND_MSG == state ||
            PRTE_PROC_STATE_FAILED_TO_START == state) {
            /* if this is my own connection, ignore it */
            if (PRTE_PROC_MY_NAME->rank == proc->rank) {
                PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                                     "%s Comm failure on my own connection - ignoring it",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
                goto cleanup;
            }
            /* mark the daemon as gone */
            PRTE_FLAG_UNSET(pptr, PRTE_PROC_FLAG_ALIVE);
            /* update the state */
            pptr->state = state;
            /* adjust our num_procs */
            --prte_process_info.num_daemons;
            /* if we have ordered prteds to terminate or abort
             * is in progress, record it */
            if (prte_prteds_term_ordered || prte_abnormal_term_ordered) {
                PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                                     "%s Comm failure: daemons terminating - recording daemon %s as gone",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc)));
                /* remove from dependent routes, if it is one */
                prte_rml_route_lost(proc->rank);
                /* if all my routes and local children are gone, then terminate ourselves */
                if (0 == pmix_list_get_size(&prte_rml_base.children)) {
                    for (i = 0; i < prte_local_children->size; i++) {
                        proct = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
                        if (NULL != proct &&
                            PRTE_FLAG_TEST(pptr, PRTE_PROC_FLAG_ALIVE) &&
                            proct->state < PRTE_PROC_STATE_UNTERMINATED) {
                            /* at least one is still alive */
                            PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                                                 "%s Comm failure: at least one proc (%s) still alive",
                                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                                 PRTE_NAME_PRINT(&proct->name)));
                            goto cleanup;
                        }
                    }
                    /* call our appropriate exit procedure */
                    PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                                         "%s errmgr_dvm: all routes and children gone - ordering exit",
                                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
                    PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
                } else {
                    PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                                         "%s Comm failure: %d routes remain alive",
                                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                         (int) pmix_list_get_size(&prte_rml_base.children)));
                }
                goto cleanup;
            }
            PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                                 "%s Comm failure: daemon %s - aborting",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc)));
            /* record the first one to fail */
            if (!PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) {
                /* output an error message so the user knows what happened */
                pmix_show_help("help-errmgr-base.txt", "node-died", true,
                               PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), prte_process_info.nodename,
                               PRTE_NAME_PRINT(proc), pptr->node->name);
                /* mark the daemon job as failed */
                jdata->state = PRTE_JOB_STATE_COMM_FAILED;
                /* point to the lowest rank to cause the problem */
                prte_set_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC, PRTE_ATTR_LOCAL, pptr,
                                   PMIX_POINTER);
                /* retain the object so it doesn't get free'd */
                PMIX_RETAIN(pptr);
                PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_ABORTED);
                /* update our exit code */
                jdata->exit_code = pptr->exit_code;
                /* just in case the exit code hadn't been set, do it here - this
                 * won't override any reported exit code */
                if (0 == jdata->exit_code) {
                    jdata->exit_code = PRTE_ERR_COMM_FAILURE;
                }
            }
        } else {
            pmix_output(0, "UNSUPPORTED DAEMON ERROR STATE: %s", prte_proc_state_to_str(state));
        }
        /* since communications have failed, we have to simply force ourselves
         * to terminate as we cannot rely on the routing tree to get messages
         * out to other daemons */
        prte_abnormal_term_ordered = true;
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
        goto cleanup;
    }

    /* update the proc state - can get multiple reports on a proc
     * depending on circumstances, so ensure we only do this once
     */
    if (pptr->state < PRTE_PROC_STATE_TERMINATED) {
        pptr->state = state;
    }

    /* if we were ordered to terminate, mark this proc as dead and see if
     * any of our routes or local children remain alive - if not, then
     * terminate ourselves. */
    if (prte_prteds_term_ordered) {
        for (i = 0; i < prte_local_children->size; i++) {
            proct = (prte_proc_t*)pmix_pointer_array_get_item(prte_local_children, i);
            if (NULL != proct) {
                if (PRTE_FLAG_TEST(proct, PRTE_PROC_FLAG_ALIVE)) {
                    goto keep_going;
                }
            }
        }
        /* if all my routes and children are gone, then terminate
           ourselves nicely (i.e., this is a normal termination) */
        if (0 == pmix_list_get_size(&prte_rml_base.children)) {
            PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                                 "%s errmgr:default:dvm all routes gone - exiting",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
        }
    }

keep_going:
    /* always mark the waitpid as having fired */
    PRTE_ACTIVATE_PROC_STATE(&pptr->name, PRTE_PROC_STATE_WAITPID_FIRED);
    /* if this is a remote proc, we won't hear anything more about it
     * as the default behavior would be to terminate the job. So be sure to
     * mark the IOF as having completed too so we correctly mark this proc
     * as dead and notify everyone as required */
    if (!PRTE_FLAG_TEST(pptr, PRTE_PROC_FLAG_LOCAL)) {
        PRTE_ACTIVATE_PROC_STATE(&pptr->name, PRTE_PROC_STATE_IOF_COMPLETE);
    }

    /* simplify later checks */
    flag = (prte_get_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE, NULL, PMIX_BOOL) ||
            prte_get_attribute(&jdata->attributes, PRTE_JOB_CONTINUOUS, NULL, PMIX_BOOL));

    /* ensure we record the failed proc properly so we can report
     * the error once we terminate
     */
    switch (state) {
    case PRTE_PROC_STATE_KILLED_BY_CMD:
        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s killed by cmd",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc)));
        /* we ordered this proc to die, so it isn't an abnormal termination
         * and we don't flag it as such
         */
        if (jdata->num_terminated >= jdata->num_procs) {
            /* this job has terminated */
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_TERMINATED);
        } else if (flag) {
            /* at least one proc survives - send out a notification if one is requested */
            check_send_notification(jdata, pptr, PMIX_ERR_PROC_KILLED_BY_CMD);
        }
        break;

    case PRTE_PROC_STATE_ABORTED_BY_SIG:
        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s aborted by signal",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc)));
        if (flag) {
            /* send out a notification if one is requested */
            check_send_notification(jdata, pptr, PMIX_ERR_PROC_ABORTED_BY_SIG);
        } else {
            if (!PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) {
                jdata->state = PRTE_JOB_STATE_ABORTED_BY_SIG;
                /* point to the first rank to cause the problem */
                prte_set_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC, PRTE_ATTR_LOCAL, pptr,
                                   PMIX_POINTER);
                /* retain the object so it doesn't get free'd */
                PMIX_RETAIN(pptr);
                PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_ABORTED);
                jdata->exit_code = pptr->exit_code;
                _terminate_job(jdata->nspace);
            }
        }
        break;

    case PRTE_PROC_STATE_TERM_WO_SYNC:
        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s terminated without sync",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc)));
        if (flag) {
            /* send out a notification if one is requested */
            check_send_notification(jdata, pptr, PMIX_ERR_PROC_TERM_WO_SYNC);
        } else {
            if (!PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) {
                jdata->state = PRTE_JOB_STATE_ABORTED_WO_SYNC;
                /* point to the first rank to cause the problem */
                prte_set_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC, PRTE_ATTR_LOCAL, pptr,
                                   PMIX_POINTER);
                /* retain the object so it doesn't get free'd */
                PMIX_RETAIN(pptr);
                PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_ABORTED);
                jdata->exit_code = pptr->exit_code;
                /* send out a notification is one is requested */
                check_send_notification(jdata, pptr, PMIX_ERR_PROC_TERM_WO_SYNC);
                /* now treat a special case - if the proc exit'd without a required
                 * sync, it may have done so with a zero exit code. We want to ensure
                 * that the user realizes there was an error, so in this -one- case,
                 * we overwrite the process' exit code with the default error code
                 */
                if (0 == jdata->exit_code) {
                    jdata->exit_code = PRTE_ERROR_DEFAULT_EXIT_CODE;
                }
                /* kill the job */
                _terminate_job(jdata->nspace);
            }
        }
        break;

    case PRTE_PROC_STATE_FAILED_TO_START:
    case PRTE_PROC_STATE_FAILED_TO_LAUNCH:
        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s %s", PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                             PRTE_NAME_PRINT(proc), prte_proc_state_to_str(state)));
        if (!PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) {
            if (PRTE_PROC_STATE_FAILED_TO_START) {
                jdata->state = PRTE_JOB_STATE_FAILED_TO_START;
            } else {
                jdata->state = PRTE_JOB_STATE_FAILED_TO_LAUNCH;
            }
            /* point to the first rank to cause the problem */
            prte_set_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC, PRTE_ATTR_LOCAL, pptr,
                               PMIX_POINTER);
            /* update our exit code */
            jdata->exit_code = pptr->exit_code;
            /* just in case the exit code hadn't been set, do it here - this
             * won't override any reported exit code */
            if (0 == jdata->exit_code) {
                jdata->exit_code = PRTE_ERR_FAILED_TO_START;
            }
            /* retain the object so it doesn't get free'd */
            PMIX_RETAIN(pptr);
            PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_ABORTED);
            /* kill the job */
            _terminate_job(jdata->nspace);
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FAILED_TO_START);
        }
        /* if this was a daemon, report it */
        if (PMIX_CHECK_NSPACE(jdata->nspace, PRTE_PROC_MY_NAME->nspace)) {
            /* output a message indicating we failed to launch a daemon */
            pmix_show_help("help-errmgr-base.txt", "failed-daemon-launch",
                           true, prte_tool_basename);
        }
        break;

    case PRTE_PROC_STATE_CALLED_ABORT:
        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s called abort with exit code %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc),
                             pptr->exit_code));
        /* this proc ordered the job to abort */
        if (flag) {
            /* send out a notification if one is requested */
            check_send_notification(jdata, pptr, PMIX_ERR_PROC_REQUESTED_ABORT);
        } else {
            if (!PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) {
                jdata->state = PRTE_JOB_STATE_CALLED_ABORT;
                /* point to the first proc to cause the problem */
                prte_set_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC, PRTE_ATTR_LOCAL, pptr,
                                   PMIX_POINTER);
                /* retain the object so it doesn't get free'd */
                PMIX_RETAIN(pptr);
                PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_ABORTED);
                jdata->exit_code = pptr->exit_code;
                /* kill the job */
                _terminate_job(jdata->nspace);
            }
        }
        break;

    case PRTE_PROC_STATE_TERM_NON_ZERO:
        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s exited with non-zero status %d",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc),
                             pptr->exit_code));
        jdata->exit_code = pptr->exit_code;
        PRTE_FLAG_UNSET(pptr, PRTE_PROC_FLAG_ALIVE);
        jdata->num_terminated++;
        /* track the number of non-zero exits */
        i32 = 0;
        i32ptr = &i32;
        prte_get_attribute(&jdata->attributes, PRTE_JOB_NUM_NONZERO_EXIT, (void **) &i32ptr,
                           PMIX_INT32);
        ++i32;
        prte_set_attribute(&jdata->attributes, PRTE_JOB_NUM_NONZERO_EXIT, PRTE_ATTR_LOCAL, i32ptr,
                           PMIX_INT32);
        if (jdata->num_terminated >= jdata->num_procs) {
            /* this job has terminated */
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_TERMINATED);
        } else if (flag && prte_get_attribute(&jdata->attributes, PRTE_JOB_ERROR_NONZERO_EXIT, NULL, PMIX_BOOL)) {
            check_send_notification(jdata, pptr, PMIX_ERR_EXIT_NONZERO_TERM);
        } else {
            if (!PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) {
                jdata->state = PRTE_JOB_STATE_NON_ZERO_TERM;
                /* point to the first rank to cause the problem */
                prte_set_attribute(&jdata->attributes, PRTE_JOB_ABORTED_PROC,
                                   PRTE_ATTR_LOCAL, pptr, PMIX_POINTER);
                /* retain the object so it doesn't get free'd */
                PMIX_RETAIN(pptr);
                PRTE_FLAG_SET(jdata, PRTE_JOB_FLAG_ABORTED);
                /* kill the job */
                _terminate_job(jdata->nspace);
            }
        }
        break;

    default:
        /* shouldn't get this, but terminate job if required */
        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:dvm: proc %s default error %s",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc),
                             prte_proc_state_to_str(state)));
        if (jdata->num_terminated == jdata->num_procs) {
            PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_TERMINATED);
        }
        break;
    }

cleanup:
    PMIX_RELEASE(caddy);
}

static void check_send_notification(prte_job_t *jdata,
                                    prte_proc_t *proc,
                                    pmix_status_t event)
{
    prte_grpcomm_signature_t sig;
    int rc;
    pmix_info_t *info;
    size_t ninfo;
    pmix_proc_t target;
    pmix_data_buffer_t pbkt;
    pmix_data_range_t range = PMIX_RANGE_CUSTOM;

    pmix_output_verbose(5, prte_state_base_framework.framework_output,
                        "%s errmgr:dvm:sending notification %s affected proc %s",
                        PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                        PMIx_Error_string(event),
                        PRTE_NAME_PRINT(&proc->name));

    if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_NOTIFY_ERRORS, NULL, PMIX_BOOL) ||
        prte_dvm_abort_ordered) {
        return;
    }
    /* we checked for termination due to the specific error we encountered, but
     * it is possible that we received another type of error that wouldn't have
     * resulted in terminating the job. Thus, we still check to see if the job
     * is being aborted before sending anything out */
    if (PRTE_FLAG_TEST(jdata, PRTE_JOB_FLAG_ABORTED)) {
        /* this job has already been aborted, so we don't need to notify
         * about the fate of any proc within it */
        return;
    }
    /* notify the other procs of the termination */
    PMIX_LOAD_PROCID(&target, jdata->nspace, PMIX_RANK_WILDCARD);

    /* pack the info for sending */
    PMIX_DATA_BUFFER_CONSTRUCT(&pbkt);

    /* we need to add a flag indicating this came from an invalid proc so that we will
     * inject it into our own PMIx server library */
    rc = PMIx_Data_pack(NULL, &pbkt, &PRTE_NAME_INVALID->rank, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
        return;
    }
    /* pack the status code */
    rc = PMIx_Data_pack(NULL, &pbkt, &event, 1, PMIX_STATUS);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
        return;
    }
    /* pack the source - it cannot be me as that will cause
     * the pmix server to upcall the event back to me */
    rc = PMIx_Data_pack(NULL, &pbkt, &proc->name, 1, PMIX_PROC);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
        return;
    }
    /* pack the range */
    rc = PMIx_Data_pack(NULL, &pbkt, &range, 1, PMIX_DATA_RANGE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
        return;
    }

    /* setup the info */
    if (-1 != proc->exit_code) {
        ninfo = 3;
    } else {
        ninfo = 2;
    }
    PMIX_INFO_CREATE(info, ninfo);
    PMIX_INFO_LOAD(&info[0], PMIX_EVENT_AFFECTED_PROC, &proc->name, PMIX_PROC);
    PMIX_INFO_LOAD(&info[1], PMIX_EVENT_CUSTOM_RANGE, &target, PMIX_PROC);
    if (-1 != proc->exit_code) {
        PMIX_INFO_LOAD(&info[2], PMIX_EXIT_CODE, &proc->exit_code, PMIX_INT);
    }

    /* pack the number of infos */
    rc = PMIx_Data_pack(NULL, &pbkt, &ninfo, 1, PMIX_SIZE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_INFO_FREE(info, ninfo);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
        return;
    }
    /* pack the infos themselves */
    rc = PMIx_Data_pack(NULL, &pbkt, info, ninfo, PMIX_INFO);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_INFO_FREE(info, ninfo);
        PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
        return;
    }
    PMIX_INFO_FREE(info, ninfo);

    /* xcast it to everyone */
    PMIX_CONSTRUCT(&sig, prte_grpcomm_signature_t);
    sig.signature = (pmix_proc_t *) malloc(sizeof(pmix_proc_t));
    PMIX_LOAD_PROCID(&sig.signature[0], PRTE_PROC_MY_NAME->nspace, PMIX_RANK_WILDCARD);
    sig.sz = 1;

    if (PRTE_SUCCESS != (rc = prte_grpcomm.xcast(&sig, PRTE_RML_TAG_NOTIFICATION, &pbkt))) {
        PRTE_ERROR_LOG(rc);
    }
    PMIX_DESTRUCT(&sig);
    PMIX_DATA_BUFFER_DESTRUCT(&pbkt);
}
