/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 *
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
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

#include "opal/util/output.h"
#include "opal/dss/dss.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_locks.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/plm/base/base.h"
#include "orte/mca/rmaps/rmaps_types.h"
#if ORTE_ENABLE_SENSORS
#include "orte/mca/sensor/sensor.h"
#endif
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"
#include "errmgr_hnp.h"

/* Local functions */
static void hnp_abort(orte_jobid_t job, orte_exit_code_t exit_code);
static void failed_start(orte_job_t *jdata, orte_exit_code_t exit_code);
static void update_local_procs_in_job(orte_job_t *jdata, orte_job_state_t jobstate, orte_proc_state_t state);
static void update_proc(orte_job_t *jdata, orte_process_name_t *proc,
                        orte_proc_state_t state,
                        orte_exit_code_t exit_code);
static void check_job_complete(orte_job_t *jdata);
static void killprocs(orte_jobid_t job, orte_vpid_t vpid);

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

static int update_state(orte_jobid_t job,
                        orte_job_state_t jobstate,
                        orte_process_name_t *proc_name,
                        orte_proc_state_t state,
                        orte_exit_code_t exit_code,
                        orte_errmgr_stack_state_t *stack_state);

static int predicted_fault(char ***proc_list,
                           char ***node_list,
                           char ***suggested_nodes,
                           orte_errmgr_stack_state_t *stack_state);

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list,
                               orte_errmgr_stack_state_t *stack_state);

static int ft_event(int state);



/******************
 * ORCM module
 ******************/
orte_errmgr_base_module_t orte_errmgr_hnp_module = {
    init,
    finalize,
    update_state,
    predicted_fault,
    suggest_map_targets,
    ft_event
};

/************************
 * API Definitions
 ************************/
static int init(void)
{
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static int update_state(orte_jobid_t job,
                        orte_job_state_t jobstate,
                        orte_process_name_t *proc,
                        orte_proc_state_t state,
                        orte_exit_code_t exit_code,
                        orte_errmgr_stack_state_t *stack_state)
{
    orte_job_t *jdata;
    orte_exit_code_t sts;
    orte_odls_child_t *child;
    opal_list_item_t *item;
    int rc;
    
    /* indicate that this is the end of the line */
    *stack_state |= ORTE_ERRMGR_STACK_STATE_COMPLETE;
    
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:hnp: job %s reported state %s"
                         " for proc %s state %s exit_code %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(job),
                         orte_job_state_to_str(jobstate),
                         (NULL == proc) ? "NULL" : ORTE_NAME_PRINT(proc),
                         orte_proc_state_to_str(state), exit_code));
    
    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_errmgr_base.shutting_down) {
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
            case ORTE_JOB_STATE_FAILED_TO_START:
                failed_start(jdata, exit_code);
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
                update_local_procs_in_job(jdata, jobstate, ORTE_PROC_STATE_RUNNING);
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
                update_local_procs_in_job(jdata, jobstate, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED);
                /* order all local procs for this job to be killed */
                killprocs(jdata->jobid, ORTE_VPID_WILDCARD);
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
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        return ORTE_ERR_NOT_FOUND;
    }

    /* update is for a specific proc */
    switch (state) {
        case ORTE_PROC_STATE_ABORTED:
        case ORTE_PROC_STATE_ABORTED_BY_SIG:
        case ORTE_PROC_STATE_TERM_WO_SYNC:
        case ORTE_PROC_STATE_COMM_FAILED:
            if (jdata->enable_recovery) {
                /* is this a local proc */
                child = NULL;
                for (item = opal_list_get_first(&orte_local_children);
                     item != opal_list_get_end(&orte_local_children);
                     item = opal_list_get_next(item)) {
                    child = (orte_odls_child_t*)item;
                    if (child->name->jobid == proc->jobid &&
                        child->name->vpid == proc->vpid) {
                        break;
                    }
                }
                if (NULL != child) {
                    /* see if this child has reached its local restart limit */
                    if (child->restarts < jdata->max_local_restarts) {
                        child->restarts++;
                        if (ORTE_SUCCESS == (rc = orte_odls.restart_proc(child))) {
                            return ORTE_SUCCESS;
                        }
                        /* let it fall thru to abort */
                    }
                }
            }
            update_proc(jdata, proc, state, exit_code);
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
            update_proc(jdata, proc, state, exit_code);
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
            update_proc(jdata, proc, state, exit_code);
            break;

        case ORTE_PROC_STATE_TERMINATED:
        case ORTE_PROC_STATE_KILLED_BY_CMD:
            update_proc(jdata, proc, state, exit_code);
            check_job_complete(jdata);
            break;

        case ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED:
            update_proc(jdata, proc, state, exit_code);
            killprocs(proc->jobid, proc->vpid);
            check_job_complete(jdata);  /* need to set the job state */
            /* the job object for this job will have been NULL'd
             * in the array if the job was solely local. If it isn't
             * NULL, then we need to tell everyone else to die
             */
            if (NULL != (jdata = orte_get_job_data_object(proc->jobid))) {
                hnp_abort(jdata->jobid, exit_code);
            }
            break;
            
        default:
            break;
    }

    return ORTE_SUCCESS;
}

static int predicted_fault(char ***proc_list,
                           char ***node_list,
                           char ***suggested_nodes,
                           orte_errmgr_stack_state_t *stack_state)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list,
                               orte_errmgr_stack_state_t *stack_state)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

int ft_event(int state)
{
    return ORTE_SUCCESS;
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
    
    orte_job_term_ordered = true;
    
    /* tell the plm to terminate all jobs */
    if (ORTE_SUCCESS != (rc = orte_plm.terminate_job(ORTE_JOBID_WILDCARD))) {
        ORTE_ERROR_LOG(rc);
    }
    
    /* set the exit status, just in case whomever called us failed
     * to do so - it can only be done once, so we are protected
     * from overwriting it
     */
    ORTE_UPDATE_EXIT_STATUS(exit_code);    
}

static void failed_start(orte_job_t *jdata, orte_exit_code_t exit_code)
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
                proc->exit_code = exit_code;
                /* update the counter so we can terminate */
                jdata->num_terminated++;
                /* remove the child from our list */
                opal_list_remove_item(&orte_local_children, &child->super);
                OBJ_RELEASE(child);
            }
        }
    }
    
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:hnp: job %s reported incomplete start",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid)));
}

static void update_local_procs_in_job(orte_job_t *jdata, orte_job_state_t jobstate, orte_proc_state_t state)
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
            if (ORTE_PROC_STATE_UNTERMINATED < state) {
                opal_list_remove_item(&orte_local_children, &child->super);
                OBJ_RELEASE(child);
                jdata->num_terminated++;
            } else if (ORTE_PROC_STATE_RUNNING) {
                jdata->num_launched++;
            } else if (ORTE_PROC_STATE_REGISTERED == state) {
                jdata->num_reported++;
                if (jdata->num_reported == jdata->num_procs) {
                    OPAL_RELEASE_THREAD(&jdata->reported_lock,
                                        &jdata->reported_cond,
                                        &jdata->not_reported);
                }
            }
        }
    }
}

static void update_proc(orte_job_t *jdata,
                        orte_process_name_t *proc,
                        orte_proc_state_t state,
                        orte_exit_code_t exit_code)
{
    opal_list_item_t *item, *next;
    orte_odls_child_t *child;
    orte_proc_t *proct;
    int i;
    
    /***   UPDATE LOCAL CHILD   ***/
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = next) {
        next = opal_list_get_next(item);
        child = (orte_odls_child_t*)item;
        if (child->name->jobid == proc->jobid) {
            if (child->name->vpid == proc->vpid) {
                child->state = state;
                child->exit_code = exit_code;
                proct = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, child->name->vpid);
                proct->state = state;
                proct->exit_code = exit_code;
                if (ORTE_PROC_STATE_UNTERMINATED < state) {
                    if (!jdata->enable_recovery) {
                        opal_list_remove_item(&orte_local_children, &child->super);
                        OBJ_RELEASE(child);
                    }
                    jdata->num_terminated++;
                } else if (ORTE_PROC_STATE_RUNNING == state) {
                    jdata->num_launched++;
                    if (jdata->num_launched == jdata->num_procs) {
                        jdata->state = ORTE_JOB_STATE_RUNNING;
                    }
                } else if (ORTE_PROC_STATE_REGISTERED == state) {
                    jdata->num_reported++;
                    if (jdata->num_reported == jdata->num_procs) {
                        OPAL_RELEASE_THREAD(&jdata->reported_lock,
                                            &jdata->reported_cond,
                                            &jdata->not_reported);
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
        proct->exit_code = exit_code;
        if (ORTE_PROC_STATE_REGISTERED == state) {
            jdata->num_reported++;
            if (jdata->num_reported == jdata->num_procs) {
                OPAL_RELEASE_THREAD(&jdata->reported_lock,
                                    &jdata->reported_cond,
                                    &jdata->not_reported);
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
    orte_vpid_t non_zero=0;
    
#if 0
    /* Check if FileM is active. If so then keep processing. */
    OPAL_ACQUIRE_THREAD(&orte_filem_base_lock, &orte_filem_base_cond, &orte_filem_base_is_active);
#endif
    
    for (i=0; i < jdata->procs->size; i++) {
        if (NULL == (proc = (orte_proc_t*)opal_pointer_array_get_item(jdata->procs, i))) {
            /* the proc array may no longer be left justified, so
             * we need to check everything
             */
            continue;
        }
        
        if (0 != proc->exit_code) {
            non_zero++;
        }
        
        /*
         * Determine how the process state affects the job state
         */
        if (ORTE_PROC_STATE_FAILED_TO_START == proc->state) {
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr_hnp:check_job_completed proc %s failed to start",
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
                break;
            }
        } else if (ORTE_PROC_STATE_ABORTED == proc->state) {
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
                break;
            }
        } else if (ORTE_PROC_STATE_ABORTED_BY_SIG == proc->state) {
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
                break;
            }
        } else if (ORTE_PROC_STATE_TERM_WO_SYNC == proc->state) {
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
                break;
            }
        } else if (ORTE_PROC_STATE_KILLED_BY_CMD == proc->state) {
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
        } else if (ORTE_PROC_STATE_CALLED_ABORT == proc->state) {
            if (!jdata->abort) {
                jdata->state = ORTE_JOB_STATE_CALLED_ABORT;
                /* point to the first proc to cause the problem */
                jdata->aborted_proc = proc;
                /* retain the object so it doesn't get free'd */
                OBJ_RETAIN(proc);
                jdata->abort = true;
                ORTE_UPDATE_EXIT_STATUS(proc->exit_code);
                break;
           }
        } else if (ORTE_PROC_STATE_UNTERMINATED < proc->state &&
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
                break;
            }
        }
    }

#if ORTE_ENABLE_SENSORS
    if (jdata->abort) {
        /* the job aborted - turn off any sensors on this job */
        orte_sensor.stop(jdata->jobid);
    }
#endif

    if (ORTE_JOB_STATE_UNTERMINATED > jdata->state &&
        jdata->num_terminated >= jdata->num_procs) {
        /* this job has terminated */
        jdata->state = ORTE_JOB_STATE_TERMINATED;
#if ORTE_ENABLE_SENSORS
        /* turn off any sensor monitors on this job */
        orte_sensor.stop(jdata->jobid);
#endif
        if (0 < non_zero) {
            /* warn user */
            opal_output(orte_clean_output,
                        "-----------------------------------------------------\n\n"
                        "While job %s terminated normally, %s processes returned\n"
                        "non-zero exit codes. Further examination may be required.\n\n"
                        "-----------------------------------------------------",
                        ORTE_JOBID_PRINT(jdata->jobid), ORTE_VPID_PRINT(non_zero));
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
    if (jdata == NULL || jdata->jobid == ORTE_PROC_MY_NAME->jobid) {
        jdata = orte_get_job_data_object(ORTE_PROC_MY_NAME->jobid);
        if (jdata->num_terminated >= jdata->num_procs) {
            /* orteds are done! */
            jdata->state = ORTE_JOB_STATE_TERMINATED;
            orte_trigger_event(&orteds_exit);
            return;
        }
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
         */
        if (NULL != jdata && job->jobid == jdata->jobid &&
            (jdata->state == ORTE_JOB_STATE_TERMINATED ||
             jdata->state == ORTE_JOB_STATE_KILLED_BY_CMD)) {
                /* release this object, ensuring that the
                 * pointer array internal accounting
                 * is maintained!
                 */
                opal_pointer_array_set_item(orte_job_data, j, NULL);  /* ensure the array has a NULL */
                OBJ_RELEASE(jdata);
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
                                 "%s errmgr:hnp:check_job_completed job %s is terminated (%d vs %d [0x%x])",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_JOBID_PRINT(job->jobid),
                                 job->num_terminated, job->num_procs, jdata->state ));
        }
    }
    /* if a job is still alive, we just return */
    if (one_still_alive) {
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                             "%s errmgr:hnp:check_job_completed at least one job is not terminated",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return;
    }
    /* if we get here, then all jobs are done, so wakeup */
    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                         "%s errmgr:hnp:check_job_completed all jobs terminated - waking up",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* set the exit status to 0 - this will only happen if it
     * wasn't already set by an error condition
     */
    ORTE_UPDATE_EXIT_STATUS(0);
    orte_trigger_event(&orte_exit);
}

static void killprocs(orte_jobid_t job, orte_vpid_t vpid)
{
    opal_pointer_array_t cmd;
    orte_proc_t proc;
    int rc;
    
    OBJ_CONSTRUCT(&cmd, opal_pointer_array_t);
    OBJ_CONSTRUCT(&proc, orte_proc_t);
    proc.name.jobid = job;
    proc.name.vpid = vpid;
    opal_pointer_array_add(&cmd, &proc);
    if (ORTE_SUCCESS != (rc = orte_odls.kill_local_procs(&cmd))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&cmd);
    OBJ_DESTRUCT(&proc);
}

