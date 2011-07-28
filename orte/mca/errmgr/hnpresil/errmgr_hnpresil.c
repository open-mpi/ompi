/*
 * Copyright (c) 2009-2011 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif  /* HAVE_UNISTD_H */
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include "opal/util/output.h"
#include "opal/util/opal_sos.h"
#include "opal/dss/dss.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/plm/base/plm_private.h"
#include "orte/mca/plm/plm.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/debugger/base/base.h"
#include "orte/mca/notifier/notifier.h"
#include "orte/mca/grpcomm/grpcomm.h"
#include "orte/mca/ess/ess.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/util/nidmap.h"

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_locks.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_hnpresil.h"

/**********************
 * C/R Mgr Components
 * Global: HNP
 **********************/
static orte_errmgr_base_module_t global_module = {
    /** Initialization Function */
    orte_errmgr_hnpresil_global_module_init,
    /** Finalization Function */
    orte_errmgr_hnpresil_global_module_finalize,
    /** Error Log */
    orte_errmgr_base_log,
    /** Forced Abort */
    orte_errmgr_base_abort,
    /** Peer Force Abort */
    orte_errmgr_base_abort_peers,
    /** Update State */
    orte_errmgr_hnpresil_global_update_state,
    /* Predicted Fault */
    orte_errmgr_hnpresil_global_predicted_fault,
    /* Suggest proc to node mapping */
    orte_errmgr_hnpresil_global_suggest_map_targets,
    /* FT Event hook  */
    orte_errmgr_hnpresil_global_ft_event,
    orte_errmgr_base_register_migration_warning,
    /* Post-startup */
    orte_errmgr_hnpresil_global_post_startup,
    /* Pre-shutdown */
    orte_errmgr_hnpresil_global_pre_shutdown,
    /* Mark as dead */
    orte_errmgr_hnpresil_global_mark_processes_as_dead,
    /* Set the callback */
    orte_errmgr_base_set_fault_callback,
    /* Receive failure notification */
    orte_errmgr_hnpresil_global_failure_notification
};


/*
 * Local functions
 */
static void hnp_abort(orte_jobid_t job, orte_exit_code_t exit_code);
static void failed_start(orte_job_t *jdata);
static void update_local_procs_in_job(orte_job_t *jdata, orte_job_state_t jobstate,
                                      orte_proc_state_t state, orte_exit_code_t exit_code);
static void check_job_complete(orte_job_t *jdata);
static void killprocs(orte_jobid_t job, orte_vpid_t vpid, orte_epoch_t epoch);
static int hnp_relocate(orte_job_t *jdata, orte_process_name_t *proc,
                        orte_proc_state_t state, orte_exit_code_t exit_code);
static orte_odls_child_t* proc_is_local(orte_process_name_t *proc);
static int send_to_local_applications(opal_pointer_array_t *dead_names);

/************************
 * API Definitions
 ************************/
int orte_errmgr_hnpresil_component_query(mca_base_module_t **module, int *priority)
{
    opal_output_verbose(10, mca_errmgr_hnpresil_component.super.output_handle,
                        "errmgr:hnp:component_query()");

    if( ORTE_PROC_IS_HNP ) {
        *priority = mca_errmgr_hnpresil_component.super.priority;
        *module = (mca_base_module_t *)&global_module;
    }
    /* Daemons and Apps have their own components */
    else {
        *module = NULL;
        *priority = -1;
    }

    return ORTE_SUCCESS;
}

/*******************
 * Global Functions
 ********************/
int orte_errmgr_hnpresil_global_module_init(void)
{
    int ret, exit_status = ORTE_SUCCESS;

#if OPAL_ENABLE_FT_CR
    if( mca_errmgr_hnpresil_component.crmig_enabled ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_crmig_global_module_init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        /* Still need the tool listener so we can tell it that we cannot do
         * anything if they ask.
         */
        if( ORTE_SUCCESS != (ret = orte_errmgr_base_tool_init()) ) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    if( mca_errmgr_hnpresil_component.autor_enabled ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_autor_global_module_init()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
#endif /* OPAL_ENABLE_FT_CR */

    if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_base_global_init()) ) {
        exit_status = ret;
        goto cleanup;
    }

cleanup:
    return exit_status;
}

int orte_errmgr_hnpresil_global_module_finalize(void)
{
    int ret, exit_status = ORTE_SUCCESS;

#if OPAL_ENABLE_FT_CR
    if( mca_errmgr_hnpresil_component.crmig_enabled ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_crmig_global_module_finalize()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    else {
        /* Still need the tool listener so we can tell it that we cannot do
         * anything if they ask.
         */
        if( ORTE_SUCCESS != (ret = orte_errmgr_base_tool_finalize()) ) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
    }

    if( mca_errmgr_hnpresil_component.autor_enabled ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_autor_global_module_finalize()) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
#endif /* OPAL_ENABLE_FT_CR */

    if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_base_global_finalize()) ) {
        exit_status = ret;
        goto cleanup;
    }

cleanup:
    return exit_status;
}

int orte_errmgr_hnpresil_global_update_state(orte_jobid_t job,
                                          orte_job_state_t jobstate,
                                          orte_process_name_t *proc_name,
                                          orte_proc_state_t state,
                                          pid_t pid,
                                          orte_exit_code_t exit_code)
{
    int ret, exit_status = ORTE_SUCCESS;

    mca_errmgr_hnpresil_component.ignore_current_update = false;

    if (orte_finalizing ||
        orte_job_term_ordered ||
        ORTE_PROC_STATE_TERMINATED == state ) {
        mca_errmgr_hnpresil_component.term_in_progress  = true;
    }

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                         "errmgr:hnp:update_state() %s) "
                         "------- %s state updated for process %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ((NULL == proc_name) ? "App. Process" : 
                          (proc_name->jobid == ORTE_PROC_MY_HNP->jobid ? "Daemon" : "App. Process")),
                         (NULL == proc_name) ? "NULL" : ORTE_NAME_PRINT(proc_name)));

#if OPAL_ENABLE_FT_CR
    if( mca_errmgr_hnpresil_component.crmig_enabled &&
        !mca_errmgr_hnpresil_component.autor_in_progress) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_crmig_global_update_state(job,
                                                                               jobstate,
                                                                               proc_name,
                                                                               state,
                                                                               pid,
                                                                               exit_code)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    if( mca_errmgr_hnpresil_component.autor_enabled &&
        !mca_errmgr_hnpresil_component.crmig_in_progress) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_autor_global_update_state(job,
                                                                               jobstate,
                                                                               proc_name,
                                                                               state,
                                                                               pid,
                                                                               exit_code)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
#endif /* OPAL_ENABLE_FT_CR */

    if( !mca_errmgr_hnpresil_component.ignore_current_update ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_base_global_update_state(job,
                                                                             jobstate,
                                                                             proc_name,
                                                                             state,
                                                                             pid,
                                                                             exit_code)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

cleanup:
    return exit_status;
}

int orte_errmgr_hnpresil_global_predicted_fault(opal_list_t *proc_list,
                                             opal_list_t *node_list,
                                             opal_list_t *suggested_map)
{
#if OPAL_ENABLE_FT_CR
    int ret, exit_status = ORTE_SUCCESS;

    if( mca_errmgr_hnpresil_component.crmig_enabled ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_crmig_global_predicted_fault(proc_list,
                                                                                  node_list,
                                                                                  suggested_map)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
    /*
     * If Process migration is not enabled, then return an error the tool
     * which will print an appropriate message for the user.
     */
    else {
        OPAL_OUTPUT_VERBOSE((10, mca_errmgr_hnpresil_component.super.output_handle,
                             "errmgr:hnp:predicted_fault() Command line asked for a migration, but it is not enabled\n"));
        orte_errmgr_base_migrate_update(ORTE_ERRMGR_MIGRATE_STATE_ERROR);
        exit_status = ORTE_ERR_NOT_IMPLEMENTED;
        goto cleanup;
    }

cleanup:
    return exit_status;
#else
    return ORTE_ERR_NOT_IMPLEMENTED;
#endif /* OPAL_ENABLE_FT_CR */
}

int orte_errmgr_hnpresil_global_suggest_map_targets(orte_proc_t *proc,
                                                 orte_node_t *oldnode,
                                                 opal_list_t *node_list)
{
#if OPAL_ENABLE_FT_CR
    int ret, exit_status = ORTE_ERR_NOT_IMPLEMENTED;

    if( mca_errmgr_hnpresil_component.crmig_enabled &&
        !mca_errmgr_hnpresil_component.autor_in_progress ) {
        exit_status = ORTE_SUCCESS;
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_crmig_global_suggest_map_targets(proc,
                                                                                      oldnode,
                                                                                      node_list)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    if( mca_errmgr_hnpresil_component.autor_enabled &&
        !mca_errmgr_hnpresil_component.crmig_in_progress ) {
        exit_status = ORTE_SUCCESS;
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_autor_global_suggest_map_targets(proc,
                                                                                      oldnode,
                                                                                      node_list)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

cleanup:
    return exit_status;
#else
    return ORTE_ERR_NOT_IMPLEMENTED;
#endif /* OPAL_ENABLE_FT_CR */
}

int orte_errmgr_hnpresil_global_ft_event(int state)
{
    int ret, exit_status = ORTE_SUCCESS;

#if OPAL_ENABLE_FT_CR
    if( !mca_errmgr_hnpresil_component.crmig_enabled ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_crmig_global_ft_event(state)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }

    if( !mca_errmgr_hnpresil_component.autor_enabled ) {
        if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_autor_global_ft_event(state)) ) {
            exit_status = ret;
            goto cleanup;
        }
    }
#endif /* OPAL_ENABLE_FT_CR */

    if( ORTE_SUCCESS != (ret = orte_errmgr_hnpresil_base_global_ft_event(state)) ) {
        exit_status = ret;
        goto cleanup;
    }

cleanup:
    return exit_status;
}


/**********************
 * From HNP
 **********************/
int orte_errmgr_hnpresil_base_global_init(void)
{
    return ORTE_SUCCESS;
}

int orte_errmgr_hnpresil_base_global_finalize(void)
{
    return ORTE_SUCCESS;
}

int orte_errmgr_hnpresil_base_global_update_state(orte_jobid_t job,
                                             orte_job_state_t jobstate,
                                             orte_process_name_t *proc,
                                             orte_proc_state_t state,
                                             pid_t pid,
                                             orte_exit_code_t exit_code)
{
    orte_job_t *jdata;
    orte_exit_code_t sts;
    orte_odls_child_t *child;
    int rc;
    orte_app_context_t *app;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:hnp: job %s reported state %s"
                         " for proc %s state %s pid %d exit_code %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job),
                         orte_job_state_to_str(jobstate),
                         (NULL == proc) ? "NULL" : ORTE_NAME_PRINT(proc),
                         orte_proc_state_to_str(state), pid, exit_code));
    
    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_finalizing) {
        return ORTE_SUCCESS;
    }

    if (NULL == proc) {
        /* this is an update for an entire local job */
        if (ORTE_JOBID_INVALID == job) {
            /* whatever happened, we don't know what job
             * it happened to
             */
            if (ORTE_JOB_STATE_NEVER_LAUNCHED == jobstate) {
                orte_never_launched = true;
            }
            orte_show_help("help-orte-errmgr-hnp.txt", "errmgr-hnp:unknown-job-error",
                           true, orte_job_state_to_str(jobstate));
            hnp_abort(job, exit_code);
            return ORTE_SUCCESS;
        }

        /* get the job object */
        if (NULL == (jdata = orte_get_job_data_object(job))) {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
        /* update the state */
        jdata->state = jobstate;

        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                             "%s errmgr:hnp: job %s reported state %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid),
                             orte_job_state_to_str(jobstate)));

        switch (jobstate) {
        case ORTE_JOB_STATE_TERMINATED:
            /* support batch-operated jobs */
            update_local_procs_in_job(jdata, jobstate, ORTE_PROC_STATE_TERMINATED, 0);
            jdata->num_terminated = jdata->num_procs;
            check_job_complete(jdata);
            break;

        case ORTE_JOB_STATE_ABORTED:
            /* support batch-operated jobs */
            update_local_procs_in_job(jdata, jobstate, ORTE_PROC_STATE_ABORTED, exit_code);
            jdata->num_terminated = jdata->num_procs;
            check_job_complete(jdata);
            break;

        case ORTE_JOB_STATE_FAILED_TO_START:
            failed_start(jdata);
            check_job_complete(jdata);  /* set the local proc states */
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(job))) {
                sts = exit_code;
                if (ORTE_PROC_MY_NAME->jobid == job && !orte_abnormal_term_ordered) {
                    /* set the flag indicating that a daemon failed so we use the proper
                     * methods for attempting to shutdown the rest of the system
                     */
                    orte_abnormal_term_ordered = true;
                    if (WIFSIGNALED(exit_code)) { /* died on signal */
#ifdef WCOREDUMP
                        if (WCOREDUMP(exit_code)) {
                            orte_show_help("help-plm-base.txt", "daemon-died-signal-core", true,
                                           WTERMSIG(exit_code));
                            sts = WTERMSIG(exit_code);
                        } else {
                            orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                                           WTERMSIG(exit_code));
                            sts = WTERMSIG(exit_code);
                        }
#else
                        orte_show_help("help-plm-base.txt", "daemon-died-signal", true,
                                       WTERMSIG(exit_code));
                        sts = WTERMSIG(exit_code);
#endif /* WCOREDUMP */
                    } else {
                        orte_show_help("help-plm-base.txt", "daemon-died-no-signal", true,
                                       WEXITSTATUS(exit_code));
                        sts = WEXITSTATUS(exit_code);
                    }
                }
                hnp_abort(jdata->jobid, sts);
            }
            break;
        case ORTE_JOB_STATE_RUNNING:
            /* update all procs in job */
            update_local_procs_in_job(jdata, jobstate, ORTE_PROC_STATE_RUNNING, 0);
            /* record that we reported */
            jdata->num_daemons_reported++;
            /* report if requested */
            if (orte_report_launch_progress) {
                if (0 == jdata->num_daemons_reported % 100 || jdata->num_daemons_reported == orte_process_info.num_procs) {
                    opal_output(orte_clean_output, "Reported: %d (out of %d) daemons - %d (out of %d) procs",
                                (int)jdata->num_daemons_reported, (int)orte_process_info.num_procs,
                                (int)jdata->num_launched, (int)jdata->num_procs);
                }
            }
            break;
        case ORTE_JOB_STATE_NEVER_LAUNCHED:
            orte_never_launched = true;
            jdata->num_terminated = jdata->num_procs;
            check_job_complete(jdata);  /* set the local proc states */
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(job))) {
                hnp_abort(jdata->jobid, exit_code);
            }
            break;
        case ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED:
            /* update all procs in job */
            update_local_procs_in_job(jdata, jobstate,
                                      ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED,
                                      exit_code);
            /* order all local procs for this job to be killed */
            killprocs(jdata->jobid, ORTE_VPID_WILDCARD, ORTE_EPOCH_WILDCARD);
            check_job_complete(jdata);  /* set the local proc states */
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(job))) {
                hnp_abort(jdata->jobid, exit_code);
            }
            break;
        case ORTE_JOB_STATE_COMM_FAILED:
            /* order all local procs for this job to be killed */
            killprocs(jdata->jobid, ORTE_VPID_WILDCARD, ORTE_EPOCH_WILDCARD);
            check_job_complete(jdata);  /* set the local proc states */
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(job))) {
                hnp_abort(jdata->jobid, exit_code);
            }
            break;
        case ORTE_JOB_STATE_HEARTBEAT_FAILED:
            /* order all local procs for this job to be killed */
            killprocs(jdata->jobid, ORTE_VPID_WILDCARD, ORTE_EPOCH_WILDCARD);
            check_job_complete(jdata);  /* set the local proc states */
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(job))) {
                hnp_abort(jdata->jobid, exit_code);
            }
            break;

        default:
            break;
        }
        return ORTE_SUCCESS;
    }
    
    /* get the job object */
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        /* if the orteds are terminating, check job complete */
        if (orte_orteds_term_ordered) {
            opal_output(0, "TERM ORDERED - CHECKING COMPLETE");
            check_job_complete(NULL);
            return ORTE_SUCCESS;
        } else {
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            return ORTE_ERR_NOT_FOUND;
        }
    }

#if OPAL_ENABLE_FT_CR
    /* Notify the process state to the notifier framework if it is
       active and selected. */
    orte_errmgr_base_proc_state_notify(state, proc);
#endif

    /* update is for a specific proc */
    switch (state) {
    case ORTE_PROC_STATE_ABORTED:
    case ORTE_PROC_STATE_ABORTED_BY_SIG:
    case ORTE_PROC_STATE_TERM_WO_SYNC:
        if( jdata->enable_recovery ) {
            /* is this a local proc */
            if (NULL != (child = proc_is_local(proc))) {
                /* local proc - see if it has reached its restart limit */
                app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, child->app_idx);
                if (child->restarts < app->max_restarts) {
                    child->restarts++;
                    if (ORTE_SUCCESS == (rc = orte_odls.restart_proc(child))) {
                        return ORTE_SUCCESS;
                    }
                    /* reset the child's state as restart_proc would
                     * have cleared it
                     */
                    child->state = state;
                    /* see if we can relocate it somewhere else */
                    if (ORTE_SUCCESS == hnp_relocate(jdata, proc, state, exit_code)) {
                        return ORTE_SUCCESS;
                    }
                    /* let it fall thru to abort */
                }
            } else {
                /* this is a remote process - see if we can relocate it */
                if (ORTE_SUCCESS == hnp_relocate(jdata, proc, state, exit_code)) {
                    return ORTE_SUCCESS;
                }
                /* guess not - let it fall thru to abort */
            }
        }

        if (ORTE_PROC_STATE_ABORTED_BY_SIG == state) {
            exit_code = 0;
        }

        orte_errmgr_hnpresil_update_proc(jdata, proc, state, pid, exit_code);
        check_job_complete(jdata);  /* need to set the job state */
        /* the job object for this job will have been NULL'd
         * in the array if the job was solely local. If it isn't
         * NULL, then we need to tell everyone else to die
         */
        if (NULL != (jdata = orte_get_job_data_object(proc->jobid))) {
            hnp_abort(jdata->jobid, exit_code);
        }
        break;

        case ORTE_PROC_STATE_FAILED_TO_START:
        case ORTE_PROC_STATE_CALLED_ABORT:
            orte_errmgr_hnpresil_update_proc(jdata, proc, state, pid, exit_code);
            check_job_complete(jdata);
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(proc->jobid))) {
                hnp_abort(jdata->jobid, exit_code);
            }
            break;

        case ORTE_PROC_STATE_REGISTERED:
        case ORTE_PROC_STATE_RUNNING:
            orte_errmgr_hnpresil_update_proc(jdata, proc, state, pid, exit_code);
            break;

        case ORTE_PROC_STATE_LAUNCHED:
            /* record the pid for this child */
            orte_errmgr_hnpresil_update_proc(jdata, proc, state, pid, exit_code);
            break;

        case ORTE_PROC_STATE_TERMINATED:
        case ORTE_PROC_STATE_TERM_NON_ZERO:
        case ORTE_PROC_STATE_KILLED_BY_CMD:
            orte_errmgr_hnpresil_update_proc(jdata, proc, state, pid, exit_code);
            check_job_complete(jdata);
            break;

        case ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED:
            if (jdata->enable_recovery) {
                killprocs(proc->jobid, proc->vpid, proc->epoch);
                /* is this a local proc */
                if (NULL != (child = proc_is_local(proc))) {
                    /* local proc - see if it has reached its restart limit */
                    app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, child->app_idx);
                    if (child->restarts < app->max_restarts) {
                        child->restarts++;
                        if (ORTE_SUCCESS == (rc = orte_odls.restart_proc(child))) {
                            return ORTE_SUCCESS;
                        }
                        /* reset the child's state as restart_proc would
                         * have cleared it
                         */
                        child->state = state;
                        /* see if we can relocate it somewhere else */
                        if (ORTE_SUCCESS == hnp_relocate(jdata, proc, state, exit_code)) {
                            return ORTE_SUCCESS;
                        }
                        /* let it fall thru to abort */
                    }
                } else {
                    /* this is a remote process - see if we can relocate it */
                    if (ORTE_SUCCESS == hnp_relocate(jdata, proc, state, exit_code)) {
                        return ORTE_SUCCESS;
                    }
                    /* guess not - let it fall thru to abort */
                }
            }
            /* kill all jobs */
            orte_errmgr_hnpresil_update_proc(jdata, proc, state, pid, exit_code);
            check_job_complete(jdata);  /* need to set the job state */
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(proc->jobid))) {
                hnp_abort(jdata->jobid, exit_code);
            }
            break;

        case ORTE_PROC_STATE_COMM_FAILED:
            /* is this to a daemon? */
            if (ORTE_PROC_MY_NAME->jobid == proc->jobid) {
                /* if this is my own connection, ignore it */
                if (ORTE_PROC_MY_NAME->vpid == proc->vpid) {
                    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                "%s My own connection - ignoring it",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
                    break;
                }
                /* if we have ordered orteds to terminate, record it */
                if (orte_orteds_term_ordered) {
                    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                "%s Daemons terminating - recording daemon %s as gone",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(proc)));
                    /* remove from dependent routes, if it is one */
                    orte_routed.route_lost(proc);
                    /* update daemon job */
                    orte_errmgr_hnpresil_record_dead_process(proc);
                    /* We'll check if the job was complete when we get the
                     * message back from the HNP notifying us of the dead
                     * process
                     */
                    check_job_complete(jdata);
                    break;
                }
                /* if abort is in progress, see if this one failed to tell
                 * us it had terminated
                 */
                if (orte_abnormal_term_ordered) {
                    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                "%s Abort in progress - recording daemon %s as gone",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(proc)));
                    /* remove from dependent routes, if it is one */
                    orte_routed.route_lost(proc);
                    /* update daemon job */
                    orte_errmgr_hnpresil_record_dead_process(proc);
                    /* We'll check if the job was complete when we get the
                     * message back from the HNP notifying us of the dead
                     * process
                     */
                    check_job_complete(jdata);
                    break;
                }

                /* delete the route */
                orte_routed.delete_route(proc);
                /* purge the oob */
                orte_rml.purge(proc);

                if( orte_enable_recovery ) {
                    /* relocate its processes */
                    if (ORTE_SUCCESS != (rc = hnp_relocate(jdata, proc, state, exit_code))) {
                        /* unable to relocate for some reason */
                        opal_output(0, "%s UNABLE TO RELOCATE PROCS FROM FAILED DAEMON %s",
                                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(proc));
                        /* kill all local procs */
                        killprocs(ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD, ORTE_EPOCH_WILDCARD);
                        /* kill all jobs */
                        hnp_abort(ORTE_JOBID_WILDCARD, exit_code);
                        /* check if all is complete so we can terminate */
                        check_job_complete(jdata);
                    }
                } else {
                    if (ORTE_SUCCESS != orte_errmgr_hnpresil_record_dead_process(proc)) {
                        /* The process is already dead so don't keep trying to do
                         * this stuff. */
                        return ORTE_SUCCESS;
                    }
                    /* We'll check if the job was complete when we get the
                     * message back from the HNP notifying us of the dead
                     * process */
                }
            }
            break;

        case ORTE_PROC_STATE_HEARTBEAT_FAILED:
            /* heartbeats are only from daemons */
            if( orte_enable_recovery ) {
                /* relocate its processes */
            } else {
                orte_errmgr_hnpresil_record_dead_process(proc);
                /* kill all local procs */
                killprocs(ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD, ORTE_EPOCH_WILDCARD);
                /* kill all jobs */
                hnp_abort(ORTE_JOBID_WILDCARD, exit_code);
                return ORTE_ERR_UNRECOVERABLE;
            }
            break;

        default:
            break;
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_hnpresil_base_global_ft_event(int state)
{
    return ORTE_SUCCESS;
}

int orte_errmgr_hnpresil_global_post_startup(void) {
    return ORTE_SUCCESS;
}

int orte_errmgr_hnpresil_global_pre_shutdown(void) {
    return ORTE_SUCCESS;
}

int orte_errmgr_hnpresil_global_failure_notification(orte_process_name_t *sender, opal_buffer_t *buffer) {
    orte_std_cntr_t n;
    int ret = ORTE_SUCCESS, num_failed;
    opal_pointer_array_t *dead_names;
    int32_t i;
    orte_process_name_t *name_item;
    orte_epoch_t epoch;
    orte_job_t *jdat;
    orte_proc_t *pdat, *pdat2;
    opal_buffer_t *answer;
    orte_daemon_cmd_flag_t command;
    
    if (orte_debug_daemons_flag) {
        opal_output(0, "%s errmgr:hnp HNP received process failed from orted %s",
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                    ORTE_NAME_PRINT(sender));
    }
    
    n = 1;
    /* Get the number of failed procs */
    if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, &num_failed, &n, ORTE_VPID))) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    dead_names = OBJ_NEW(opal_pointer_array_t);
    
    for (i = 0; i < num_failed; i++) {
        name_item = (orte_process_name_t *) malloc(sizeof(orte_process_name_t));
        
        /* Unpack the buffer to get the dead process' name. */
        n = 1;
        if (ORTE_SUCCESS != (ret = opal_dss.unpack(buffer, name_item, &n, ORTE_NAME))) {
            ORTE_ERROR_LOG(ret);
            return ret;
        }
        
        /* Check to see if the message is telling us about an old epoch.  
         * If so ignore the message.
         */
        epoch = orte_util_lookup_epoch(name_item);
        if (name_item->epoch < epoch) {
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s errmgr:hnp HNP ignoring duplicate notification for %s failure (reported epoch: %s local epoch: %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(name_item),
                            ORTE_EPOCH_PRINT(name_item->epoch),
                            ORTE_EPOCH_PRINT(epoch));
            }
            free(name_item);
            continue;
        } else {
            if (orte_debug_daemons_flag) {
                opal_output(0, "%s errmgr:hnp HNP received notification for %s failure (reported epoch: %s local epoch: %s",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(name_item),
                            ORTE_EPOCH_PRINT(name_item->epoch),
                            ORTE_EPOCH_PRINT(epoch));
            }
        }
        
        opal_pointer_array_add(dead_names, name_item);
        
        /* Check to see if the message is telling us about an orted and
         * it is from another orted. Orteds don't have the list of all
         * the application processes so they don't know if there were
         * any child processes on the nodes that they are reporting. */
        if (OPAL_EQUAL != orte_util_compare_name_fields(ORTE_NS_CMP_ALL, sender, ORTE_PROC_MY_NAME)) {
            if (NULL == (jdat = orte_get_job_data_object(name_item->jobid))) {
                continue;
            } else if (NULL == (pdat = (orte_proc_t *) opal_pointer_array_get_item(jdat->procs, name_item->vpid))) {
                continue;
            } else if (NULL == pdat->node) {
                continue;
            }
            
            if (ORTE_PROC_MY_NAME->jobid == name_item->jobid) {
                for (i = 0; i < opal_pointer_array_get_size(pdat->node->procs); i++) {
                    if (NULL == (pdat2 = (orte_proc_t *) opal_pointer_array_get_item(pdat->node->procs, i))) {
                        continue;
                    }
                    
                    /* ignore this process if it has already terminated */
                    if (ORTE_PROC_STATE_TERMINATED <= pdat2->state) {
                        continue;
                    }

                    /* the proc must have been alive, so notify everyone that it died */
                    name_item = (orte_process_name_t *) malloc(sizeof(orte_process_name_t));
                        
                    name_item->jobid = pdat2->name.jobid;
                    name_item->vpid = pdat2->name.vpid;
                    name_item->epoch = orte_util_lookup_epoch(&(pdat2->name));
                        
                    opal_pointer_array_add(dead_names, name_item);
                }
            }
        }
        
    }
    
    /* Update the number of failed process so any duplicates don't get
     * re-reported.
     */
    num_failed = opal_pointer_array_get_size(dead_names);
    
    if (num_failed > 0) {
        orte_errmgr.mark_processes_as_dead(dead_names);
        
        if (!orte_orteds_term_ordered) {
            /* Send a message out to all the orteds to inform them that the
             * process is dead. Long live the process (or not if it is so 
             * decided)!
             */
            answer = OBJ_NEW(opal_buffer_t);
            command = ORTE_PROCESS_FAILED_NOTIFICATION;
            
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &command, 1, ORTE_DAEMON_CMD))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                return ret;
            }
            
            if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, &num_failed, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                return ret;
            }
            
            for (i = 0; i < opal_pointer_array_get_size(dead_names); i++) {
                if (NULL != (name_item = (orte_process_name_t *) opal_pointer_array_get_item(dead_names, i))) {
                    if (ORTE_SUCCESS != (ret = opal_dss.pack(answer, name_item, 1, ORTE_NAME))) {
                        ORTE_ERROR_LOG(ret);
                        OBJ_RELEASE(answer);
                        return ret;
                    }
                }
            }
            
            if (ORTE_SUCCESS != (ret = orte_grpcomm.xcast(ORTE_PROC_MY_NAME->jobid, answer, ORTE_RML_TAG_DAEMON))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(answer);
                return ret;
            }
            
            /* Tell the applications' ORTE layers that there is a failure. */
            if (ORTE_SUCCESS != (ret = send_to_local_applications(dead_names))) {
                return ret;
            }
        }
        
        for (i = 0; i < num_failed; i++) {
            name_item = (orte_process_name_t *) opal_pointer_array_get_item(dead_names, i);
            free(name_item);
        }
    }
    
    OBJ_RELEASE(dead_names);
    
    return ret;
}

/*****************
 * Local Functions
 *****************/
static void hnp_abort(orte_jobid_t job, orte_exit_code_t exit_code)
{
    int rc;

    /* if we are already in progress, then ignore this call */
    if (!opal_atomic_trylock(&orte_abort_inprogress_lock)) { /* returns 1 if already locked */
        OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                             "%s errmgr:hnp: abort in progress, ignoring abort on job %s with status %d",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(job), exit_code));
        return;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:hnp: abort called on job %s with status %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job), exit_code));
    
    /* if debuggers are running, clean up */
    orte_debugger.finalize();

    /* set control params to indicate we are terminating */
    orte_job_term_ordered = true;
    orte_abnormal_term_ordered = true;
    orte_enable_recovery = false;

    /* set the exit status, just in case whomever called us failed
     * to do so - it can only be done once, so we are protected
     * from overwriting it
     */
    ORTE_UPDATE_EXIT_STATUS(exit_code);    

    /* tell the plm to terminate the orteds - they will automatically
     * kill their local procs
     */
    if (ORTE_SUCCESS != (rc = orte_plm.terminate_orteds())) {
        ORTE_ERROR_LOG(rc);
    }
}

static void failed_start(orte_job_t *jdata)
{
    opal_list_item_t *item, *next;
    orte_odls_job_t *jobdat;
    orte_odls_child_t *child;
    orte_proc_t *proc;

    /* lookup the local jobdat for this job */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;

        /* is this the specified job? */
        if (jobdat->jobid == jdata->jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        /* race condition - may not have been formed yet */
        return;
    }
    jobdat->state = ORTE_JOB_STATE_FAILED_TO_START;

    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = next) {
        next = opal_list_get_next(item);
        child = (orte_odls_child_t*)item;
        if (child->name->jobid == jobdat->jobid) {
            if (ORTE_PROC_STATE_LAUNCHED > child->state ||
                ORTE_PROC_STATE_UNTERMINATED < child->state) {
                /* get the master proc object */
                proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, child->name->vpid);
                proc->state = child->state;
                proc->exit_code = child->exit_code;
                /* update the counter so we can terminate */
                jdata->num_terminated++;
                /* remove the child from our list */
                opal_list_remove_item(&orte_local_children, &child->super);
                OBJ_RELEASE(child);
                jobdat->num_local_procs--;
            }
        }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:hnp: job %s reported incomplete start",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
}

static void update_local_procs_in_job(orte_job_t *jdata, orte_job_state_t jobstate,
                                      orte_proc_state_t state, orte_exit_code_t exit_code)
{
    opal_list_item_t *item, *next;
    orte_odls_job_t *jobdat;
    orte_odls_child_t *child;
    orte_proc_t *proc;

    /* lookup the local jobdat for this job */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;

        /* is this the specified job? */
        if (jobdat->jobid == jdata->jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        /* race condition - may not have been formed yet */
        return;
    }
    jobdat->state = jobstate;
    jdata->state = jobstate;
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = next) {
        next = opal_list_get_next(item);
        child = (orte_odls_child_t*)item;
        if (jdata->jobid == child->name->jobid) {
            child->state = state;
            proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, child->name->vpid);
            proc->state = state;
            if (proc->exit_code < exit_code) {
                proc->exit_code = exit_code;
            }
            if (ORTE_PROC_STATE_UNTERMINATED < state) {
                opal_list_remove_item(&orte_local_children, &child->super);
                OBJ_RELEASE(child);
                jdata->num_terminated++;
                jobdat->num_local_procs--;
            } else if (ORTE_PROC_STATE_RUNNING) {
                jdata->num_launched++;
            } else if (ORTE_PROC_STATE_REGISTERED == state) {
                jdata->num_reported++;
                if (jdata->dyn_spawn_active &&
                    jdata->num_reported == jdata->num_procs) {
                    OPAL_RELEASE_THREAD(&jdata->dyn_spawn_lock,
                                        &jdata->dyn_spawn_cond,
                                        &jdata->dyn_spawn_active);
                }
            }
        }
    }
}

void orte_errmgr_hnpresil_update_proc(orte_job_t *jdata,
                                   orte_process_name_t *proc,
                                   orte_proc_state_t state,
                                   pid_t pid,
                                   orte_exit_code_t exit_code)
{
    opal_list_item_t *item, *next;
    orte_odls_child_t *child;
    orte_proc_t *proct;
    orte_odls_job_t *jobdat, *jdat;
    int i;

    jobdat = NULL;
    for (item  = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item  = opal_list_get_next(item)) {
        jdat = (orte_odls_job_t*)item;
        if (jdat->jobid == jdata->jobid) {
            jobdat = jdat;
            break;
        }
    }
    if (NULL == jobdat) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
    }

    /***   UPDATE LOCAL CHILD   ***/
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = next) {
        next = opal_list_get_next(item);
        child = (orte_odls_child_t*)item;
        if (child->name->jobid == proc->jobid) {
            if (child->name->vpid == proc->vpid) {
                child->state = state;
                if (0 < pid) {
                    child->pid = pid;
                }
                child->exit_code = exit_code;
                proct = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, child->name->vpid);
                proct->state = state;
                if (0 < pid) {
                    proct->pid = pid;
                }
                proct->exit_code = exit_code;
                if (ORTE_PROC_STATE_UNTERMINATED < state) {
                    if (!jdata->enable_recovery) {
                        opal_list_remove_item(&orte_local_children, &child->super);
                        OBJ_RELEASE(child);
                        if (NULL != jobdat) {
                            jobdat->num_local_procs--;
                        }
                    }
                    jdata->num_terminated++;
                } else if (ORTE_PROC_STATE_RUNNING == state) {
                    jdata->num_launched++;
                    if (jdata->num_launched == jdata->num_procs) {
                        jdata->state = ORTE_JOB_STATE_RUNNING;
                    }
                } else if (ORTE_PROC_STATE_REGISTERED == state) {
                    jdata->num_reported++;
                    if (jdata->dyn_spawn_active &&
                        jdata->num_reported == jdata->num_procs) {
                        OPAL_RELEASE_THREAD(&jdata->dyn_spawn_lock,
                                            &jdata->dyn_spawn_cond,
                                            &jdata->dyn_spawn_active);
                    }
                }
                return;
            }
        }
    }

    /***   UPDATE REMOTE CHILD   ***/
    for (i=0; i < jdata->procs->size; i++) {
        if (NULL == (proct = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            continue;
        }
        if (proct->name.jobid != proc->jobid ||
            proct->name.vpid != proc->vpid) {
            continue;
        }
        proct->state = state;
        if (0 < pid) {
            proct->pid = pid;
        }
        proct->exit_code = exit_code;
        if (ORTE_PROC_STATE_REGISTERED == state) {
            jdata->num_reported++;
            if (jdata->dyn_spawn_active &&
                jdata->num_reported == jdata->num_procs) {
                OPAL_RELEASE_THREAD(&jdata->dyn_spawn_lock,
                                    &jdata->dyn_spawn_cond,
                                    &jdata->dyn_spawn_active);
            }
        } else if (ORTE_PROC_STATE_UNTERMINATED < state) {
            /* update the counter so we can terminate */
            jdata->num_terminated++;
        } else if (ORTE_PROC_STATE_RUNNING == state) {
            jdata->num_launched++;
            if (jdata->num_launched == jdata->num_procs) {
                jdata->state = ORTE_JOB_STATE_RUNNING;
            }
        }
        return;
    }
}

static void check_job_complete(orte_job_t *jdata)
{
    orte_proc_t *proc;
    int i;
    orte_std_cntr_t j;
    orte_job_t *job;
    orte_node_t *node;
    orte_job_map_t *map;
    orte_std_cntr_t index;
    bool one_still_alive;
    orte_vpid_t non_zero=0, lowest=0;
    char *msg;

#if 0
    /* Check if FileM is active. If so then keep processing. */
    OPAL_ACQUIRE_THREAD(&orte_filem_base_lock, &orte_filem_base_cond, &orte_filem_base_is_active);
#endif
    if (NULL == jdata) {
        /* just check to see if the daemons are complete */
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                             "%s errmgr:hnp:check_job_complete - received NULL job, checking daemons",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        goto CHECK_DAEMONS;
    }

    for (i=0; i < jdata->procs->size && !jdata->abort; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            /* the proc array may no longer be left justified, so
             * we need to check everything
             */
            continue;
        }

        if (0 != proc->exit_code) {
            non_zero++;
            if (0 == lowest) {
                lowest = proc->exit_code;
            }
        }

        switch (proc->state) {
        case ORTE_PROC_STATE_KILLED_BY_CMD:
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr:hnp:check_job_completed proc %s killed by cmd",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            /* we ordered this proc to die, so it isn't an abnormal termination
             * and we don't flag it as such - just check the remaining jobs to
             * see if anyone is still alive
             */
            if (jdata->num_terminated >= jdata->num_procs) {
                /* this job has terminated - now we need to check to see if ALL
                 * the other jobs have also completed and wakeup if that is true
                 */
                if (!jdata->abort) {
                    jdata->state = ORTE_JOB_STATE_KILLED_BY_CMD;
                }
            }
            goto CHECK_ALIVE;
            break;
        case ORTE_PROC_STATE_ABORTED:
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr:hnp:check_job_completed proc %s aborted",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_ABORTED;
                /* point to the lowest rank to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            }
            break;
        case ORTE_PROC_STATE_FAILED_TO_START:
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr_hnpresil:check_job_completed proc %s failed to start",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_FAILED_TO_START;
                /* point to the lowest rank to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            }
            break;
#if 0
        case ORTE_PROC_STATE_ABORTED_BY_SIG:
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr:hnp:check_job_completed proc %s aborted by signal",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_ABORTED_BY_SIG;
                /* point to the lowest rank to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            }
            break;
#endif
        case ORTE_PROC_STATE_TERM_WO_SYNC:
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr:hnp:check_job_completed proc %s terminated without sync",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(&proc->name)));
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_ABORTED_WO_SYNC;
                /* point to the lowest rank to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                /* now treat a special case - if the proc exit'd without a required
                 * sync, it may have done so with a zero exit code. We want to ensure
                 * that the user realizes there was an error, so in this -one- case,
                 * we overwrite the process' exit code with the default error code
                 */
                ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
            }
            break;
        case ORTE_PROC_STATE_COMM_FAILED:
#if 0
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_COMM_FAILED;
                /* point to the lowest rank to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            }
#endif
            break;
        case ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED:
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED;
                /* point to the lowest rank to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            }
            break;
        case ORTE_PROC_STATE_CALLED_ABORT:
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_CALLED_ABORT;
                /* point to the first proc to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            }
            break;
        case ORTE_PROC_STATE_HEARTBEAT_FAILED:
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_HEARTBEAT_FAILED;
                /* point to the lowest rank to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            }
            break;
        case ORTE_PROC_STATE_TERM_NON_ZERO:
            ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
            if (orte_abort_non_zero_exit) {
                if (!jdata->abort) {
                    jdata->state = ORTE_JOB_STATE_NON_ZERO_TERM;
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = proc;
                    /* retain the object so it doesn't get free'd */
                    OBJ_RETAIN(proc);
                    jdata->abort = true;
                }
            }
            break;

        default:
            if (ORTE_PROC_STATE_UNTERMINATED < proc->state &&
                jdata->controls & ORTE_JOB_CONTROL_CONTINUOUS_OP) {
                OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                     "%s errmgr:hnp:check_job_completed proc %s terminated and continuous",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name)));
                if (!jdata->abort) {
                    proc->state = ORTE_PROC_STATE_ABORTED;
                    jdata->state = ORTE_JOB_STATE_ABORTED;
                    /* point to the lowest rank to cause the problem */
                    jdata->aborted_proc = proc;
                    /* retain the object so it doesn't get free'd */
                    OBJ_RETAIN(proc);
                    jdata->abort = true;
                    ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                }
            }
            break;
        }
    }
    
    if (jdata->abort) {
        /* the job aborted - turn off any sensors on this job */
        orte_sensor.stop(jdata->jobid);
    }

    if (ORTE_JOB_STATE_UNTERMINATED > jdata->state &&
        jdata->num_terminated >= jdata->num_procs) {
        /* this job has terminated */
        jdata->state = ORTE_JOB_STATE_TERMINATED;

        /* turn off any sensor monitors on this job */
        orte_sensor.stop(jdata->jobid);

        if (0 < non_zero) {
            if (!orte_report_child_jobs_separately || 1 == ORTE_LOCAL_JOBID(jdata->jobid)) {
                /* update the exit code */
                ORTE_UPDATE_EXIT_STATUS(lowest);
            }

            /* warn user */
            opal_output(orte_clean_output,
                        "-------------------------------------------------------\n"
                        "While %s job %s terminated normally, %s %s. Further examination may be required.\n"
                        "-------------------------------------------------------",
                        (1 == ORTE_LOCAL_JOBID(jdata->jobid)) ? "the primary" : "child",
                        (1 == ORTE_LOCAL_JOBID(jdata->jobid)) ? "" : ORTE_LOCAL_JOBID_PRINT(jdata->jobid),
                        ORTE_VPID_PRINT(non_zero),
                        (1 == non_zero) ? "process returned\na non-zero exit code." : "processes returned\nnon-zero exit codes.");
        }
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                             "%s errmgr:hnp:check_job_completed declared job %s normally terminated - checking all jobs",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));
    }

    /* if this job is a continuously operating one, then don't do
     * anything further - just return here
     */
    if (NULL != jdata &&
        (ORTE_JOB_CONTROL_CONTINUOUS_OP & jdata->controls ||
         ORTE_JOB_CONTROL_RECOVERABLE & jdata->controls)) {
        goto CHECK_ALIVE;
    }

    /* if the job that is being checked is the HNP, then we are
     * trying to terminate the orteds. In that situation, we
     * do -not- check all jobs - we simply notify the HNP
     * that the orteds are complete. Also check special case
     * if jdata is NULL - we want
     * to definitely declare the job done if the orteds
     * have completed, no matter what else may be happening.
     * This can happen if a ctrl-c hits in the "wrong" place
     * while launching
     */
CHECK_DAEMONS:
    if (jdata == NULL || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        if ((jdata->num_procs - 1) <= jdata->num_terminated) { /* Subtract one for the HNP */
            /* orteds are done! */
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s orteds complete - exiting",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            if (NULL == jdata) {
                jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
            }
            jdata->state = ORTE_JOB_STATE_TERMINATED;
            orte_quit();
            return;
        }
        return;
    }

    /* Release the resources used by this job. Since some errmgrs may want
     * to continue using resources allocated to the job as part of their
     * fault recovery procedure, we only do this once the job is "complete".
     * Note that an aborted/killed job -is- flagged as complete and will
     * therefore have its resources released. We need to do this after
     * we call the errmgr so that any attempt to restart the job will
     * avoid doing so in the exact same place as the current job
     */
    if (NULL != jdata->map  && jdata->state == ORTE_JOB_STATE_TERMINATED) {
        map = jdata->map;
        for (index = 0; index < map->nodes->size; index++) {
            if (NULL == (node = (orte_node_t*)opal_pointer_array_get_item(map->nodes, index))) {
                continue;
            }
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s releasing procs from node %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 node->name));
            for (i = 0; i < node->procs->size; i++) {
                if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                    continue;
                }
                if (proc->name.jobid != jdata->jobid) {
                    /* skip procs from another job */
                    continue;
                }
                node->slots_inuse--;
                node->num_procs--;
                OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                     "%s releasing proc %s from node %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&proc->name), node->name));
                /* set the entry in the node array to NULL */
                opal_pointer_array_set_item(node->procs, i, NULL);
                /* release the proc once for the map entry */
                OBJ_RELEASE(proc);
            }
        }
        OBJ_RELEASE(map);
        jdata->map = NULL;
    }

CHECK_ALIVE:
    /* now check to see if all jobs are done - release this jdata
     * object when we find it
     */
    one_still_alive = false;
    for (j=1; j < orte_job_data->size; j++) {
        if (NULL == (job = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, j))) {
            /* since we are releasing jdata objects as we
             * go, we can no longer assume that the job_data
             * array is left justified
             */
            continue;
        }
        /* if this is the job we are checking AND it normally terminated,
         * then go ahead and release it. We cannot release it if it
         * abnormally terminated as mpirun needs the info so it can
         * report appropriately to the user
         *
         * NOTE: do not release the primary job (j=1) so we
         * can pretty-print completion message
         */
        if (NULL != jdata && job->jobid == jdata->jobid &&
            (jdata->state == ORTE_JOB_STATE_TERMINATED ||
             jdata->state == ORTE_JOB_STATE_KILLED_BY_CMD)) {
            /* release this object, ensuring that the
             * pointer array internal accounting
             * is maintained!
             */
            if (1 < j) {
                opal_pointer_array_set_item(orte_job_data, j, NULL);  /* ensure the array has a NULL */
                OBJ_RELEASE(jdata);
            }
            continue;
        }
        /* if the job is flagged to not be monitored, skip it */
        if (ORTE_JOB_CONTROL_DO_NOT_MONITOR & job->controls) {
            continue;
        }
        /* when checking for job termination, we must be sure to NOT check
         * our own job as it - rather obviously - has NOT terminated!
         */
        if (job->num_terminated < job->num_procs) {
            /* we have at least one job that is not done yet - we cannot
             * just return, though, as we need to ensure we cleanout the
             * job data for the job that just completed
             */
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr:hnp:check_job_completed job %s is not terminated (%d:%d)",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job->jobid),
                                 job->num_terminated, job->num_procs));
            one_still_alive = true;
        }
        else {
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr:hnp:check_job_completed job %s is terminated (%d vs %d [%s])",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job->jobid),
                                 job->num_terminated, job->num_procs,
                                 (NULL == jdata) ? "UNKNOWN" : orte_job_state_to_str(jdata->state) ));
        }
    }
    /* if a job is still alive, we just return */
    if (one_still_alive) {
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                             "%s errmgr:hnp:check_job_completed at least one job is not terminated",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return;
    }
    /* if we get here, then all jobs are done, so terminate */
    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                         "%s errmgr:hnp:check_job_completed all jobs terminated",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* set the exit status to 0 - this will only happen if it
     * wasn't already set by an error condition
     */
    ORTE_UPDATE_EXIT_STATUS(0);
    /* provide a notifier message if that framework is active - ignored otherwise */
    if (NULL != (job = (orte_job_t*)opal_pointer_array_get_item(orte_job_data, 1))) {
        if (NULL == job->name) {
            job->name = strdup(orte_process_info.nodename);
        }
        if (NULL == job->instance) {
            asprintf(&job->instance, "%d", orte_process_info.pid);
        }
        if (0 == orte_exit_status) {
            asprintf(&msg, "Job %s:%s complete", job->name, job->instance);
            orte_notifier.log(ORTE_NOTIFIER_INFO, 0, msg);
        } else {
            asprintf(&msg, "Job %s:%s terminated abnormally", job->name, job->instance);
            orte_notifier.log(ORTE_NOTIFIER_ALERT, orte_exit_status, msg);
        }
        free(msg);
        /* this job object will be release during finalize */
    }

    orte_jobs_complete();
    /* if I am the only daemon alive, then I can exit now */
    if (0 == orte_routed.num_routes()) {
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                             "%s orteds complete - exiting",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        orte_quit();
    }
}

static void killprocs(orte_jobid_t job, orte_vpid_t vpid, orte_epoch_t epoch)
{
    opal_pointer_array_t cmd;
    orte_proc_t proc;
    int rc;

    /* stop local sensors for this job */
    if (ORTE_VPID_WILDCARD == vpid) {
        orte_sensor.stop(job);
    }

    if (ORTE_JOBID_WILDCARD == job && ORTE_VPID_WILDCARD == vpid && ORTE_EPOCH_WILDCARD == epoch) {
        if (ORTE_SUCCESS != (rc = orte_odls.kill_local_procs(NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        return;
    }

    OBJ_CONSTRUCT(&cmd, opal_pointer_array_t);
    OBJ_CONSTRUCT(&proc, orte_proc_t);
    proc.name.jobid = job;
    proc.name.vpid = vpid;
    proc.name.epoch = epoch;
    opal_pointer_array_add(&cmd, &proc);
    if (ORTE_SUCCESS != (rc = orte_odls.kill_local_procs(&cmd))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    OBJ_DESTRUCT(&proc);
}

static int hnp_relocate(orte_job_t *jdata, orte_process_name_t *proc,
                        orte_proc_state_t state, orte_exit_code_t exit_code)
{
    orte_job_t *jdat;
    orte_proc_t *pdata, *pdt, *pdt2;
    orte_node_t *node, *nd;
    orte_app_context_t *app;
    char *app_name;
    int rc, i, n;

    OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base.output,
                         "%s CHECKING ON RELOCATE FOR APP %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));

    /* get the proc_t object for this process */
    pdata = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, proc->vpid);
    if (NULL == pdata) {
        opal_output(0, "Data for proc %s could not be found", ORTE_NAME_PRINT(proc));
        return ORTE_ERR_NOT_FOUND;
    }

    /* set the state */
    pdata->state = state;

    /* retain the node id */
    node = pdata->node;

    /* if it is a daemon that died, we need to flag all of its procs
     * to be relocated
     */
    if (ORTE_PROC_MY_NAME->jobid == proc->jobid) {
        /* remove this proc from the daemon job */
        orte_errmgr_hnpresil_record_dead_process(proc);
        /* check to see if any other nodes are "alive" */
        if (!orte_hnp_is_allocated && jdata->num_procs == 1) {
            return ORTE_ERR_FATAL;
        }
        app_name = "orted";
        /* scan the procs looking for each unique jobid on the node */
        for (i=0; i < node->procs->size; i++) {
            if (NULL == (pdt = (orte_proc_t*)opal_pointer_array_get_item(node->procs, i))) {
                continue;
            }
            /* get the job data object for this process */
            if (NULL == (jdat = orte_get_job_data_object(pdt->name.jobid))) {
                /* major problem */
                ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
                continue;
            }
            /* since the node was used in this job's map, release
             * it so that accounting is maintained
             */
            OBJ_RELEASE(node);
            /* mark this proc as dead so it will be restarted */
            pdt->state = ORTE_PROC_STATE_ABORTED;
            /* remove this proc from the node */
            OBJ_RELEASE(pdt);   /* maintains accounting */
            opal_pointer_array_set_item(node->procs, i, NULL);
            /* maintain accounting on num procs alive in case this can't restart */
            jdat->num_terminated++;
            /* look for all other procs on this node from the same job */
            for (n=0; n < node->procs->size; n++) {
                if (NULL == (pdt2 = (orte_proc_t*)opal_pointer_array_get_item(node->procs, n))) {
                    continue;
                }
                if (pdt2->name.jobid == pdt->name.jobid) {
                    /* mark this proc as having aborted */
                    pdt2->state = ORTE_PROC_STATE_ABORTED;
                    /* remove it from the node */
                    OBJ_RELEASE(pdt2);
                    opal_pointer_array_set_item(node->procs, n, NULL);
                    /* maintain accounting on num procs alive */
                    jdat->num_terminated++;
                }
            }
            /* and remove the node from the map */
            for (n=0; n < jdat->map->nodes->size; n++) {
                if (NULL == (nd = (orte_node_t*)opal_pointer_array_get_item(jdat->map->nodes, n))) {
                    continue;
                }
                if (nd->index == node->index) {
                    opal_pointer_array_set_item(jdat->map->nodes, n, NULL);
                    OBJ_RELEASE(node);  /* maintain accounting */
                    break;
                }
            }
            /* reset the job params for this job */
            orte_plm_base_reset_job(jdat);

            /* relaunch the job */
            opal_output(0, "%s RELOCATING APPS FOR JOB %s FROM NODE %s",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_JOBID_PRINT(jdat->jobid), node->name);
            if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdat))) {
                opal_output(0, "FAILED TO RESTART APP %s on error %s", app_name, ORTE_ERROR_NAME(rc));
                return rc;
            }
        }

        return ORTE_SUCCESS;
    }

    /* otherwise, we are an app -  try to relocate us to another node */
    app = (orte_app_context_t*)opal_pointer_array_get_item(jdata->apps, pdata->app_idx);
    if (NULL == app) {
        /* no way to restart this job */
        orte_show_help("help-orte-errmgr-hnp.txt", "errmgr-hnp:cannot-relocate", true,
                       ORTE_NAME_PRINT(proc));
        return ORTE_ERR_NOT_FOUND;
    }
    app_name = app->app;
    /* track that we are attempting to restart */
    pdata->restarts++;
    /* have we exceeded the number of restarts for this proc? */
    if (app->max_restarts < pdata->restarts) {
        return ORTE_ERR_RESTART_LIMIT_EXCEEDED;
    }

    /* reset the job params for restart */
    orte_plm_base_reset_job(jdata);

    /* flag the current node as not-to-be-used */
    pdata->node->state = ORTE_NODE_STATE_DO_NOT_USE;

    /* restart the job - the spawn function will remap and
     * launch the replacement proc(s)
     */
    OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base.output,
                         "%s RELOCATING APP %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));
    
    if (ORTE_SUCCESS != (rc = orte_plm.spawn(jdata))) {
        opal_output(0, "FAILED TO RESTART APP %s on error %s", app_name, ORTE_ERROR_NAME(rc));
        return rc;
    }

    return ORTE_SUCCESS;
}

static orte_odls_child_t* proc_is_local(orte_process_name_t *proc)
{
    orte_odls_child_t *child;
    opal_list_item_t *item;

    child = NULL;
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (child->name->jobid == proc->jobid &&
            child->name->vpid == proc->vpid) {
            return child;
        }
    }
    return NULL;
}

static void cbfunc(int status,
                   orte_process_name_t *peer,
                   opal_buffer_t *buffer,
                   orte_rml_tag_t tag,
                   void* cbdata) {
    OBJ_RELEASE(buffer);
}

int orte_errmgr_hnpresil_record_dead_process(orte_process_name_t *proc) {
    orte_job_t *jdat;
    orte_proc_t *pdat;
    opal_buffer_t *buffer;
    orte_daemon_cmd_flag_t command;
    int i, rc, num_failed;
    opal_pointer_array_t *dead_names;
    orte_process_name_t *name_item;
    orte_proc_t *proc_item;

    OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base.output,
                         "%s RECORDING DEAD PROCESS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc)));

    if (NULL == (jdat = orte_get_job_data_object(proc->jobid))) {
        opal_output(0, "Can't find job object");
        return ORTE_ERR_NOT_FOUND;
    }

    if (NULL != (pdat = (orte_proc_t*)opal_pointer_array_get_item(jdat->procs, proc->vpid)) &&
        ORTE_PROC_STATE_TERMINATED < pdat->state) {

        /* Make sure that the epochs match. */
        if (proc->epoch != pdat->name.epoch) {
            opal_output(1, "The epoch does not match the current epoch. Throwing the request out.");
            return ORTE_SUCCESS;
        }

        dead_names = OBJ_NEW(opal_pointer_array_t);

        if (ORTE_PROC_MY_NAME->jobid == proc->jobid) {
            opal_pointer_array_add(dead_names, &(pdat->name));

            for (i = 0; i < opal_pointer_array_get_size(pdat->node->procs); i++) {
                if (NULL == (proc_item = (orte_proc_t *) opal_pointer_array_get_item(pdat->node->procs, i))) {
                    continue;
                }

                opal_pointer_array_add(dead_names, &(proc_item->name));
            }
        }

        if (!orte_orteds_term_ordered) {
            /*
             * Send a message to the other daemons so they know that a daemon has
             * died.
             */
            buffer = OBJ_NEW(opal_buffer_t);
            command = ORTE_PROCESS_FAILED_NOTIFICATION;

            num_failed = opal_pointer_array_get_size(dead_names);

            if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(buffer);
            } else if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, &num_failed, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                OBJ_RELEASE(buffer);
            } else {

                /* Iterate of the list of dead procs and send them along with
                 * the rest. The HNP needs this info so it can tell the other
                 * ORTEDs and they can inform the appropriate applications.
                 */
                for (i = 0; i < num_failed; i++) {
                    if (NULL != (name_item = (orte_process_name_t *) opal_pointer_array_get_item(dead_names, i))) {
                        if (ORTE_SUCCESS != (rc = opal_dss.pack(buffer, name_item, 1, ORTE_NAME))) {
                            ORTE_ERROR_LOG(rc);
                            OBJ_RELEASE(buffer);
                        }
                    }
                } 

                OBJ_RELEASE(dead_names);

                OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base.output, 
                                    "%s SENDING DEAD PROCESS MESSAGE TO HNP", 
                                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

                orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, buffer, ORTE_RML_TAG_DAEMON, 0, cbfunc, NULL);
            }
        } else {
            orte_errmgr_hnpresil_global_mark_processes_as_dead(dead_names);
        }
    }

    return ORTE_SUCCESS;
}

int orte_errmgr_hnpresil_global_mark_processes_as_dead(opal_pointer_array_t *dead_procs) {
    int i;
    orte_process_name_t *name_item;
    orte_job_t *jdat;
    orte_proc_t *pdat;
    orte_node_t *node;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                "HNP %s marking procs as dead",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* Iterate over the list of processes */
    for (i = 0; i < opal_pointer_array_get_size(dead_procs); i++) {
        if (NULL == (name_item = (orte_process_name_t *) opal_pointer_array_get_item(dead_procs, i))) {
            opal_output(1, "NULL found in dead process list.");
            continue;
        }

        if (NULL == (jdat = orte_get_job_data_object(name_item->jobid))) {
            OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                        "%s Job data not found.",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            return ORTE_ERR_NOT_FOUND;
        }

        if (NULL != (pdat = (orte_proc_t *) opal_pointer_array_get_item(jdat->procs, name_item->vpid)) &&
            pdat->state < ORTE_PROC_STATE_TERMINATED) {

            OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                        "HNP %s marking %s as dead",
                        ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                        ORTE_NAME_PRINT(&pdat->name)));

            /* Make sure the epochs match, if not it probably means that we
             * already reported this failure. */
            if (name_item->epoch != pdat->name.epoch) {
                continue;
            }

            orte_util_set_epoch(name_item, name_item->epoch + 1);

            /* Remove it from the job array */
            opal_pointer_array_set_item(jdat->procs, name_item->vpid, NULL);
            orte_process_info.num_procs--;
            jdat->num_procs--;

            /* Check if this is an ORTED */
            if (ORTE_PROC_MY_NAME->jobid == name_item->jobid) {
                /* Mark the node as down so it won't be used in mapping anymore. */
                node = pdat->node;
                node->state = ORTE_NODE_STATE_DOWN;
                node->daemon = NULL;
            }

            OBJ_RELEASE(pdat);

            /* Create a new proc object that will keep track of the epoch
             * information */
            pdat = OBJ_NEW(orte_proc_t);
            pdat->name.jobid = jdat->jobid;
            pdat->name.vpid = name_item->vpid;
            pdat->name.epoch = name_item->epoch + 1;

            /* Set the state as terminated so we'll know the process isn't
             * actually there. */
            pdat->state = ORTE_PROC_STATE_TERMINATED;

            opal_pointer_array_set_item(jdat->procs, name_item->vpid, pdat);
            jdat->num_procs++;
            jdat->num_terminated++;
        } else {
            opal_output(0, "Proc data not found for %s", ORTE_NAME_PRINT(name_item));
            /* Create a new proc object that will keep track of the epoch
             * information */
            pdat = OBJ_NEW(orte_proc_t);
            pdat->name.jobid = jdat->jobid;
            pdat->name.vpid = name_item->vpid;
            pdat->name.epoch = name_item->epoch + 1;

            /* Set the state as terminated so we'll know the process isn't
             * actually there. */
            pdat->state = ORTE_PROC_STATE_TERMINATED;

            opal_pointer_array_set_item(jdat->procs, name_item->vpid, pdat);
            jdat->num_procs++;
            jdat->num_terminated++;
        }

        check_job_complete(jdat);
    }

    if (!orte_orteds_term_ordered) {
        /* Need to update the orted routing module. */
        orte_routed.update_routing_tree(ORTE_PROC_MY_NAME->jobid);

        if (NULL != fault_cbfunc) {
            (*fault_cbfunc)(dead_procs);
        }
    }

    return ORTE_SUCCESS;
}

int send_to_local_applications(opal_pointer_array_t *dead_names) {
    opal_buffer_t *buf;
    int ret = ORTE_SUCCESS;
    orte_process_name_t *name_item;
    int size, i;

    OPAL_OUTPUT_VERBOSE((10, orte_errmgr_base.output,
                "%s Sending failure to local applications.",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        
    buf = OBJ_NEW(opal_buffer_t);
        
    size = opal_pointer_array_get_size(dead_names);
        
    if (ORTE_SUCCESS != (ret = opal_dss.pack(buf, &size, 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(buf);
        return ret;
    }
        
    for (i = 0; i < size; i++) {
        if (NULL != (name_item = (orte_process_name_t *) opal_pointer_array_get_item(dead_names, i))) {
            if (ORTE_SUCCESS != (ret = opal_dss.pack(buf, name_item, 1, ORTE_NAME))) {
                ORTE_ERROR_LOG(ret);
                OBJ_RELEASE(buf);
                return ret;
            }
        }
    }
        
    if (ORTE_SUCCESS != (ret = orte_odls.deliver_message(ORTE_JOBID_WILDCARD, buf, ORTE_RML_TAG_EPOCH_CHANGE))) {
        ORTE_ERROR_LOG(ret);
        OBJ_RELEASE(buf);
        return ret;
    }
        
    OBJ_RELEASE(buf);

    return ret;
}
