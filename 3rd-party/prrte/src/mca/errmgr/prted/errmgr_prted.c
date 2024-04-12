/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2004-2023 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2014-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2017      IBM Corporation. All rights reserved.
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

#include "src/pmix/pmix-internal.h"
#include "src/util/pmix_output.h"
#include "src/util/pmix_printf.h"

#include "src/threads/pmix_threads.h"
#include "src/util/error_strings.h"
#include "src/util/name_fns.h"
#include "src/util/proc_info.h"
#include "src/util/session_dir.h"
#include "src/util/pmix_show_help.h"

#include "src/mca/ess/ess.h"
#include "src/mca/iof/base/base.h"
#include "src/mca/odls/base/base.h"
#include "src/mca/plm/plm_types.h"
#include "src/rml/rml.h"
#include "src/mca/state/state.h"

#include "src/runtime/prte_globals.h"
#include "src/runtime/prte_quit.h"
#include "src/runtime/prte_wait.h"

#include "src/mca/errmgr/base/base.h"
#include "src/mca/errmgr/base/errmgr_private.h"
#include "src/mca/errmgr/errmgr.h"

#include "errmgr_prted.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);
static void prted_abort(int error_code, char *fmt, ...);

/******************
 * prted module
 ******************/
prte_errmgr_base_module_t prte_errmgr_prted_module = {
    .init = init,
    .finalize = finalize,
    .logfn = prte_errmgr_base_log
};

/* Local functions */
static bool any_live_children(pmix_nspace_t job);
static int pack_state_update(pmix_data_buffer_t *alert, prte_job_t *jobdat);
static int pack_state_for_proc(pmix_data_buffer_t *alert, prte_proc_t *child);
static void failed_start(prte_job_t *jobdat);
static void killprocs(pmix_nspace_t job, pmix_rank_t vpid);

static void job_errors(int fd, short args, void *cbdata);
static void proc_errors(int fd, short args, void *cbdata);

/************************
 * API Definitions
 ************************/
static int init(void)
{
    /* setup state machine to trap job errors */
    prte_state.add_job_state(PRTE_JOB_STATE_ERROR, job_errors);

    /* set the lost connection state to run at MSG priority so
     * we can process any last messages from the proc
     */
    prte_state.add_proc_state(PRTE_PROC_STATE_COMM_FAILED, proc_errors);

    /* setup state machine to trap proc errors */
    prte_state.add_proc_state(PRTE_PROC_STATE_ERROR, proc_errors);

    return PRTE_SUCCESS;
}

static int finalize(void)
{
    return PRTE_SUCCESS;
}

static void wakeup(int sd, short args, void *cbdata)
{
    PRTE_HIDE_UNUSED_PARAMS(sd, args, cbdata);
    /* nothing more we can do */
    PMIX_ACQUIRE_OBJECT(cbdata);
    prte_quit(0, 0, NULL);
}

/* this function only gets called when FORCED_TERMINATE
 * has been invoked, which means that there is some
 * internal failure (e.g., to pack/unpack a correct value).
 * We could just exit, but that doesn't result in any
 * meaningful error message to the user. Likewise, just
 * printing something to stdout/stderr won't necessarily
 * get back to the user. Instead, we will send an error
 * report to mpirun and give it a chance to order our
 * termination. In order to ensure we _do_ terminate,
 * we set a timer - if it fires before we receive the
 * termination command, then we will exit on our own. This
 * protects us in the case that the failure is in the
 * messaging system itself */
static void prted_abort(int error_code, char *fmt, ...)
{
    va_list arglist;
    char *outmsg = NULL;
    prte_plm_cmd_flag_t cmd;
    pmix_data_buffer_t *alert;
    pmix_rank_t null = PMIX_RANK_INVALID;
    prte_proc_state_t state = PRTE_PROC_STATE_CALLED_ABORT;
    prte_timer_t *timer;
    int rc;

    /* only do this once */
    if (prte_abnormal_term_ordered) {
        return;
    }

    /* set the aborting flag */
    prte_abnormal_term_ordered = true;

    /* If there was a message, construct it */
    va_start(arglist, fmt);
    if (NULL != fmt) {
        pmix_vasprintf(&outmsg, fmt, arglist);
    }
    va_end(arglist);

    /* use the show-help system to get the message out */
    pmix_show_help("help-errmgr-base.txt", "simple-message", true, outmsg);

    /* tell the HNP we are in distress */
    PMIX_DATA_BUFFER_CREATE(alert);

    /* pack update state command */
    cmd = PRTE_PLM_UPDATE_PROC_STATE;
    rc = PMIx_Data_pack(NULL, alert, &cmd, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* pack the jobid */
    rc = PMIx_Data_pack(NULL, alert, &PRTE_PROC_MY_NAME->nspace, 1, PMIX_PROC_NSPACE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* pack our vpid */
    rc = PMIx_Data_pack(NULL, alert, &PRTE_PROC_MY_NAME->rank, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* pack our pid */
    rc = PMIx_Data_pack(NULL, alert, &prte_process_info.pid, 1, PMIX_PID);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* pack our state */
    rc = PMIx_Data_pack(NULL, alert, &state, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* pack our exit code */
    rc = PMIx_Data_pack(NULL, alert, &error_code, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* flag that this job is complete so the receiver can know */
    rc = PMIx_Data_pack(NULL, alert, &null, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }

    /* send it */
    PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, alert, PRTE_RML_TAG_PLM);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_RELEASE(alert);
        /* we can't communicate, so give up */
        prte_quit(0, 0, NULL);
        return;
    }

cleanup:
    /* set a timer for exiting - this also gives the message a chance
     * to get out! */
    if (NULL == (timer = PMIX_NEW(prte_timer_t))) {
        PRTE_ERROR_LOG(PRTE_ERR_OUT_OF_RESOURCE);
        return;
    }
    timer->tv.tv_sec = 5;
    timer->tv.tv_usec = 0;
    prte_event_evtimer_set(prte_event_base, timer->ev, wakeup, NULL);
    PMIX_POST_OBJECT(timer);
    prte_event_evtimer_add(timer->ev, &timer->tv);
}

static void job_errors(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata;
    prte_job_state_t jobstate;
    int rc;
    prte_plm_cmd_flag_t cmd;
    pmix_data_buffer_t *alert;
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
                         "%s errmgr:prted: job %s repprted error state %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace),
                         prte_job_state_to_str(jobstate)));

    switch (jobstate) {
    case PRTE_JOB_STATE_FAILED_TO_START:
        failed_start(jdata);
        break;
    case PRTE_JOB_STATE_COMM_FAILED:
        /* kill all local procs */
        killprocs(NULL, PMIX_RANK_WILDCARD);
        /* order termination */
        prted_abort(PRTE_ERROR_DEFAULT_EXIT_CODE, "Daemon %s: comm failure",
                    PRTE_NAME_PRINT(PRTE_PROC_MY_NAME));
        goto cleanup;
    case PRTE_JOB_STATE_HEARTBEAT_FAILED:
        /* let the HNP handle this */
        goto cleanup;

    default:
        break;
    }
    PMIX_DATA_BUFFER_CREATE(alert);
    /* pack update state command */
    cmd = PRTE_PLM_UPDATE_PROC_STATE;
    rc = PMIx_Data_pack(NULL, alert, &cmd, 1, PMIX_UINT8);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* pack the job info */
    if (PMIX_SUCCESS != (rc = pack_state_update(alert, jdata))) {
        PMIX_ERROR_LOG(rc);
        PMIX_DATA_BUFFER_RELEASE(alert);
        goto cleanup;
    }
    /* send it */
    PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, alert, PRTE_RML_TAG_PLM);
    if (PRTE_SUCCESS != rc) {
        PRTE_ERROR_LOG(rc);
        PMIX_RELEASE(alert);
    }

cleanup:
    PMIX_RELEASE(caddy);
}

static void proc_errors(int fd, short args, void *cbdata)
{
    prte_state_caddy_t *caddy = (prte_state_caddy_t *) cbdata;
    prte_job_t *jdata;
    pmix_proc_t *proc = &caddy->name;
    prte_proc_state_t state = caddy->proc_state;
    prte_proc_t *child, *ptr;
    pmix_data_buffer_t *alert;
    prte_plm_cmd_flag_t cmd;
    int rc = PRTE_SUCCESS;
    int i;
    prte_wait_tracker_t *t2;
    PRTE_HIDE_UNUSED_PARAMS(fd, args);

    PMIX_ACQUIRE_OBJECT(caddy);

    PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                         "%s errmgr:prted:proc_errors process %s error state %s",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc),
                         prte_proc_state_to_str(state)));

    /*
     * if prte is trying to shutdown, just let it
     */
    if (prte_finalizing) {
        PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:prted:proc_errors finalizing - ignoring error",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        goto cleanup;
    }

    /* if this is a heartbeat failure, let the HNP handle it */
    if (PRTE_PROC_STATE_HEARTBEAT_FAILED == state) {
        PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:prted:proc_errors heartbeat failed - ignoring error",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        goto cleanup;
    }

    /* if this was a failed comm, then see if it was to our
     * lifeline
     */
    if (PRTE_PROC_STATE_LIFELINE_LOST == state || PRTE_PROC_STATE_UNABLE_TO_SEND_MSG == state
        || PRTE_PROC_STATE_NO_PATH_TO_TARGET == state || PRTE_PROC_STATE_PEER_UNKNOWN == state
        || PRTE_PROC_STATE_FAILED_TO_CONNECT == state) {
        PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:prted lifeline lost or unable to communicate - exiting",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        /* set our exit status */
        PRTE_UPDATE_EXIT_STATUS(PRTE_ERROR_DEFAULT_EXIT_CODE);
        /* kill our children */
        killprocs(NULL, PMIX_RANK_WILDCARD);
        /* terminate - our routed children will see
         * us leave and automatically die
         */
        prte_quit(0, 0, NULL);
        goto cleanup;
    }

    /* get the job object */
    if (NULL == (jdata = prte_get_job_data_object(proc->nspace))) {
        /* must already be complete */
        PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:prted:proc_errors NULL jdata - ignoring error",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        goto cleanup;
    }

    if (PRTE_PROC_STATE_COMM_FAILED == state) {
        /* if it is our own connection, ignore it */
        if (PRTE_EQUAL == prte_util_compare_name_fields(PRTE_NS_CMP_ALL, PRTE_PROC_MY_NAME, proc)) {
            PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                                 "%s errmgr:prted:proc_errors comm_failed to self - ignoring error",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            goto cleanup;
        }
        /* was it a daemon? */
        if (!PMIX_CHECK_NSPACE(proc->nspace, PRTE_PROC_MY_NAME->nspace)) {
            /* nope - we can't seem to trust that we will catch the waitpid
             * in this situation, so push this over to be handled as if
             * it were a waitpid trigger so we don't create a bunch of
             * duplicate code */
            PMIX_OUTPUT_VERBOSE(
                (2, prte_errmgr_base_framework.framework_output,
                 "%s errmgr:prted:proc_errors comm_failed to non-daemon - handling as waitpid",
                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
            /* get the proc_t */
            if (NULL
                == (child = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs,
                                                                        proc->rank))) {
                PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
                goto cleanup;
            }
            /* leave the exit code alone - process this as a waitpid */
            t2 = PMIX_NEW(prte_wait_tracker_t);
            PMIX_RETAIN(child); // protect against race conditions
            t2->child = child;
            prte_event_set(prte_event_base, &t2->ev, -1, PRTE_EV_WRITE,
                           prte_odls_base_default_wait_local_proc, t2);
            prte_event_active(&t2->ev, PRTE_EV_WRITE, 1);
            goto cleanup;
        }
        PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:default:prted daemon %s exited",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(proc)));

        if (prte_prteds_term_ordered) {
            /* are any of my children still alive */
            for (i = 0; i < prte_local_children->size; i++) {
                if (NULL
                    != (child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children,
                                                                            i))) {
                    if (PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {
                        PMIX_OUTPUT_VERBOSE((5, prte_state_base_framework.framework_output,
                                             "%s errmgr:default:prted[%s(%d)] proc %s is alive",
                                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), __FILE__, __LINE__,
                                             PRTE_NAME_PRINT(&child->name)));
                        goto cleanup;
                    }
                }
            }
            /* if all my routes and children are gone, then terminate
               ourselves nicely (i.e., this is a normal termination) */
            if (0 == pmix_list_get_size(&prte_rml_base.children)) {
                PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                                     "%s errmgr:default:prted all routes gone - exiting",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            } else {
                PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                                     "%s errmgr:default:prted not exiting, num_routes() == %d",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME),
                                     (int) pmix_list_get_size(&prte_rml_base.children)));
            }
        }
        /* if not, then we can continue */
        goto cleanup;
    }

    if (NULL == (child = (prte_proc_t *) pmix_pointer_array_get_item(jdata->procs, proc->rank))) {
        PRTE_ERROR_LOG(PRTE_ERR_NOT_FOUND);
        PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_FORCED_EXIT);
        goto cleanup;
    }
    /* if this is not a local proc for this job, we can
     * ignore this call
     */
    if (!PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_LOCAL)) {
        PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:prted:proc_errors proc is not local - ignoring error",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
        goto cleanup;
    }

    PMIX_OUTPUT_VERBOSE(
        (2, prte_errmgr_base_framework.framework_output, "%s errmgr:prted got state %s for proc %s",
         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), prte_proc_state_to_str(state), PRTE_NAME_PRINT(proc)));

    if (PRTE_PROC_STATE_TERM_NON_ZERO == state) {
        /* update the state */
        child->state = state;
        /* report this as abnormal termination to the HNP, unless we already have
         * done so for this job */
        if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_FAIL_NOTIFIED, NULL, PMIX_BOOL)) {
            PMIX_DATA_BUFFER_CREATE(alert);
            /* pack update state command */
            cmd = PRTE_PLM_UPDATE_PROC_STATE;
            rc = PMIx_Data_pack(NULL, alert, &cmd, 1, PMIX_UINT8);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(alert);
                return;
            }
            /* pack only the data for this proc - have to start with the jobid
             * so the receiver can unpack it correctly
             */
            rc = PMIx_Data_pack(NULL, alert, &proc->nspace, 1, PMIX_PROC_NSPACE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(alert);
                return;
            }

            /* now pack the child's info */
            if (PMIX_SUCCESS != (rc = pack_state_for_proc(alert, child))) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(alert);
                return;
            }
            /* send it */
            PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                                 "%s errmgr:prted reporting proc %s abnormally terminated with "
                                 "non-zero status (local procs = %d)",
                                 PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&child->name),
                                 jdata->num_local_procs));
            PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, alert, PRTE_RML_TAG_PLM);
            if (PRTE_SUCCESS != rc) {
                PRTE_ERROR_LOG(rc);
                PMIX_RELEASE(alert);
            }
            /* mark that we notified the HNP for this job so we don't do it again;
             * recoverable jobs need to receive every notifications, though. */
            if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE, NULL, PMIX_BOOL)) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_FAIL_NOTIFIED, PRTE_ATTR_LOCAL, NULL,
                                   PMIX_BOOL);
            }
        }
        /* if the proc has terminated, notify the state machine */
        if (PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_IOF_COMPLETE)
            && PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_WAITPID)
            && !PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_RECORDED)) {
            PRTE_ACTIVATE_PROC_STATE(proc, PRTE_PROC_STATE_TERMINATED);
        }
        goto cleanup;
    }

    if (PRTE_PROC_STATE_FAILED_TO_START == state || PRTE_PROC_STATE_FAILED_TO_LAUNCH == state) {
        /* update the proc state */
        child->state = state;
        /* count the proc as having "terminated" */
        jdata->num_terminated++;
        /* leave the error report in this case to the
         * state machine, which will receive notice
         * when all local procs have attempted to start
         * so that we send a consolidated error report
         * back to the HNP
         */
        if (jdata->num_local_procs == jdata->num_terminated) {
            /* let the state machine know */
            if (PRTE_PROC_STATE_FAILED_TO_START == state) {
                PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FAILED_TO_START);
            } else {
                PRTE_ACTIVATE_JOB_STATE(jdata, PRTE_JOB_STATE_FAILED_TO_LAUNCH);
            }
        }
        goto cleanup;
    }

    if (PRTE_PROC_STATE_TERMINATED < state) {
        /* if we were ordered to terminate, see if
         * any of our routes or local children remain alive - if not, then
         * terminate ourselves. */
        if (prte_prteds_term_ordered) {
            /* mark the child as no longer alive and update the counters, if necessary.
             * we have to do this here as we aren't going to send this to the state
             * machine, and we want to keep the bookkeeping accurate just in case */
            if (PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {
                PRTE_FLAG_UNSET(child, PRTE_PROC_FLAG_ALIVE);
            }
            if (!PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_RECORDED)) {
                PRTE_FLAG_SET(child, PRTE_PROC_FLAG_RECORDED);
                jdata->num_terminated++;
            }
            for (i = 0; i < prte_local_children->size; i++) {
                if (NULL
                    != (child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children,
                                                                            i))) {
                    if (PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {
                        goto keep_going;
                    }
                }
            }
            /* if all my routes and children are gone, then terminate
               ourselves nicely (i.e., this is a normal termination) */
            if (0 == pmix_list_get_size(&prte_rml_base.children)) {
                PMIX_OUTPUT_VERBOSE((2, prte_errmgr_base_framework.framework_output,
                                     "%s errmgr:default:prted all routes gone - exiting",
                                     PRTE_NAME_PRINT(PRTE_PROC_MY_NAME)));
                PRTE_ACTIVATE_JOB_STATE(NULL, PRTE_JOB_STATE_DAEMONS_TERMINATED);
            }
            /* no need to alert the HNP - we are already on our way out */
            goto cleanup;
        }

    keep_going:
        /* if the job hasn't completed and the state is abnormally
         * terminated, then we need to alert the HNP right away - but
         * only do this once!
         */
        if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_FAIL_NOTIFIED, NULL, PMIX_BOOL)) {
            PMIX_DATA_BUFFER_CREATE(alert);
            /* pack update state command */
            cmd = PRTE_PLM_UPDATE_PROC_STATE;
            rc = PMIx_Data_pack(NULL, alert, &cmd, 1, PMIX_UINT8);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(alert);
                return;
            }
            /* pack only the data for this proc - have to start with the jobid
             * so the receiver can unpack it correctly
             */
            rc = PMIx_Data_pack(NULL, alert, &proc->nspace, 1, PMIX_PROC_NSPACE);
            if (PMIX_SUCCESS != rc) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(alert);
                return;
            }
            child->state = state;
            /* now pack the child's info */
            if (PMIX_SUCCESS != (rc = pack_state_for_proc(alert, child))) {
                PMIX_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(alert);
                return;
            }
            pmix_output_verbose(5, prte_errmgr_base_framework.framework_output,
                                "%s errmgr:prted reporting proc %s aborted to HNP (local procs = %d)",
                                PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_NAME_PRINT(&child->name),
                                jdata->num_local_procs);
            /* send it */
            PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, alert, PRTE_RML_TAG_PLM);
            if (PRTE_SUCCESS != rc) {
                PRTE_ERROR_LOG(rc);
                PMIX_DATA_BUFFER_RELEASE(alert);
            }
            /* mark that we reported termination of this proc so we
             * don't do it again */
            PRTE_FLAG_SET(child, PRTE_PROC_FLAG_TERM_REPORTED);
            /* mark that we notified the HNP for this job so we don't do it again;
             * recoverable jobs need to receive every notifications, though. */
            if (!prte_get_attribute(&jdata->attributes, PRTE_JOB_RECOVERABLE, NULL, PMIX_BOOL)) {
                prte_set_attribute(&jdata->attributes, PRTE_JOB_FAIL_NOTIFIED,
                                   PRTE_ATTR_LOCAL, NULL, PMIX_BOOL);
            }
        }
        /* if the proc has terminated, notify the state machine */
        if (PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_IOF_COMPLETE) &&
            PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_WAITPID) &&
            !PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_RECORDED)) {
            PRTE_ACTIVATE_PROC_STATE(proc, PRTE_PROC_STATE_TERMINATED);
        }
        goto cleanup;
    }

    /* only other state is terminated - see if anyone is left alive */
    if (!any_live_children(proc->nspace)) {
        PMIX_DATA_BUFFER_CREATE(alert);
        /* pack update state command */
        cmd = PRTE_PLM_UPDATE_PROC_STATE;
        rc = PMIx_Data_pack(NULL, alert, &cmd, 1, PMIX_UINT8);
        if (PMIX_SUCCESS != rc) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(alert);
            return;
        }
        /* pack the data for the job */
        if (PMIX_SUCCESS != (rc = pack_state_update(alert, jdata))) {
            PMIX_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(alert);
            return;
        }

        PMIX_OUTPUT_VERBOSE((5, prte_errmgr_base_framework.framework_output,
                             "%s errmgr:prted reporting all procs in %s terminated",
                             PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jdata->nspace)));

        /* remove all of this job's children from the global list */
        for (i = 0; i < prte_local_children->size; i++) {
            ptr = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i);
            if (NULL == ptr) {
                continue;
            }
            if (PMIX_CHECK_NSPACE(jdata->nspace, ptr->name.nspace)) {
                pmix_pointer_array_set_item(prte_local_children, i, NULL);
                PMIX_RELEASE(ptr);
            }
        }

        /* remove this job from our local job data since it is complete */
        PMIX_RELEASE(jdata);

        /* send it */
        PRTE_RML_SEND(rc, PRTE_PROC_MY_HNP->rank, alert, PRTE_RML_TAG_PLM);
        if (PRTE_SUCCESS != rc) {
            PRTE_ERROR_LOG(rc);
            PMIX_DATA_BUFFER_RELEASE(alert);
        }
        return;
    }

cleanup:
    PMIX_RELEASE(caddy);
}

/*****************
 * Local Functions
 *****************/
static bool any_live_children(pmix_nspace_t job)
{
    int i;
    prte_proc_t *child;

    for (i = 0; i < prte_local_children->size; i++) {
        if (NULL == (child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i))) {
            continue;
        }
        /* is this child part of the specified job? */
        if ((PMIX_NSPACE_INVALID(job) || PMIX_CHECK_NSPACE(job, child->name.nspace))
            && PRTE_FLAG_TEST(child, PRTE_PROC_FLAG_ALIVE)) {
            return true;
        }
    }

    /* if we get here, then nobody is left alive from that job */
    return false;
}

static int pack_state_for_proc(pmix_data_buffer_t *alert, prte_proc_t *child)
{
    int rc;

    /* pack the child's vpid */
    rc = PMIx_Data_pack(NULL, alert, &child->name.rank, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack the pid */
    rc = PMIx_Data_pack(NULL, alert, &child->pid, 1, PMIX_PID);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack its state */
    rc = PMIx_Data_pack(NULL, alert, &child->state, 1, PMIX_UINT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    /* pack its exit code */
    rc = PMIx_Data_pack(NULL, alert, &child->exit_code, 1, PMIX_INT32);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PRTE_SUCCESS;
}

static int pack_state_update(pmix_data_buffer_t *alert, prte_job_t *jobdat)
{
    int rc, i;
    prte_proc_t *child;
    pmix_rank_t null = PMIX_RANK_INVALID;

    /* pack the jobid */

    rc = PMIx_Data_pack(NULL, alert, &jobdat->nspace, 1, PMIX_PROC_NSPACE);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }
    for (i = 0; i < prte_local_children->size; i++) {
        if (NULL == (child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i))) {
            continue;
        }
        /* if this child is part of the job... */
        if (PMIX_CHECK_NSPACE(child->name.nspace, jobdat->nspace)) {
            if (PMIX_SUCCESS != (rc = pack_state_for_proc(alert, child))) {
                PMIX_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    /* flag that this job is complete so the receiver can know */
    rc = PMIx_Data_pack(NULL, alert, &null, 1, PMIX_PROC_RANK);
    if (PMIX_SUCCESS != rc) {
        PMIX_ERROR_LOG(rc);
        return rc;
    }

    return PRTE_SUCCESS;
}

static void failed_start(prte_job_t *jobdat)
{
    int i;
    prte_proc_t *child;

    /* set the state */
    jobdat->state = PRTE_JOB_STATE_FAILED_TO_START;

    for (i = 0; i < prte_local_children->size; i++) {
        if (NULL == (child = (prte_proc_t *) pmix_pointer_array_get_item(prte_local_children, i))) {
            continue;
        }
        /* is this child part of the specified job? */
        if (PMIX_CHECK_NSPACE(child->name.nspace, jobdat->nspace)) {
            if (PRTE_PROC_STATE_FAILED_TO_START == child->state) {
                /* this proc never launched - flag that the iof
                 * is complete or else we will hang waiting for
                 * pipes to close that were never opened
                 */
                PRTE_FLAG_SET(child, PRTE_PROC_FLAG_IOF_COMPLETE);
                /* ditto for waitpid */
                PRTE_FLAG_SET(child, PRTE_PROC_FLAG_WAITPID);
                PRTE_ACTIVATE_PROC_STATE(&child->name, PRTE_PROC_STATE_TERMINATED);
            }
        }
    }
    PMIX_OUTPUT_VERBOSE((1, prte_errmgr_base_framework.framework_output,
                         "%s errmgr:hnp: job %s reported incomplete start",
                         PRTE_NAME_PRINT(PRTE_PROC_MY_NAME), PRTE_JOBID_PRINT(jobdat->nspace)));
    return;
}

static void killprocs(pmix_nspace_t job, pmix_rank_t vpid)
{
    pmix_pointer_array_t cmd;
    prte_proc_t proc;
    int rc;

    if (PMIX_NSPACE_INVALID(job) && PMIX_RANK_WILDCARD == vpid) {
        if (PRTE_SUCCESS != (rc = prte_odls.kill_local_procs(NULL))) {
            PRTE_ERROR_LOG(rc);
        }
        return;
    }

    PMIX_CONSTRUCT(&cmd, pmix_pointer_array_t);
    PMIX_CONSTRUCT(&proc, prte_proc_t);
    PMIX_LOAD_PROCID(&proc.name, job, vpid);
    pmix_pointer_array_add(&cmd, &proc);
    if (PRTE_SUCCESS != (rc = prte_odls.kill_local_procs(&cmd))) {
        PRTE_ERROR_LOG(rc);
    }
    PMIX_DESTRUCT(&cmd);
    PMIX_DESTRUCT(&proc);
}
