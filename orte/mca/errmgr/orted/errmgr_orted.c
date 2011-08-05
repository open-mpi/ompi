/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
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
#include "opal/util/opal_sos.h"
#include "opal/dss/dss.h"

#include "orte/util/error_strings.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/runtime/orte_quit.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_orted.h"

/* Local functions */
static bool any_live_children(orte_jobid_t job);
static int pack_state_update(opal_buffer_t *alert, orte_odls_job_t *jobdat);
static int pack_state_for_proc(opal_buffer_t *alert, orte_odls_child_t *child);
static bool all_children_registered(orte_jobid_t job);
static int pack_child_contact_info(orte_jobid_t job, opal_buffer_t *buf);
static void failed_start(orte_odls_job_t *jobdat, orte_exit_code_t exit_code);
static void update_local_children(orte_odls_job_t *jobdat,
                                  orte_job_state_t jobstate,
                                  orte_proc_state_t state);
static void killprocs(orte_jobid_t job, orte_vpid_t vpid);


/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

static int predicted_fault(opal_list_t *proc_list,
                           opal_list_t *node_list,
                           opal_list_t *suggested_map);

static int update_state(orte_jobid_t job,
                        orte_job_state_t jobstate,
                        orte_process_name_t *proc,
                        orte_proc_state_t state,
                        pid_t pid,
                        orte_exit_code_t exit_code);

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list);

static int ft_event(int state);



/******************
 * ORTED module
 ******************/
orte_errmgr_base_module_t orte_errmgr_orted_module = {
    init,
    finalize,
    orte_errmgr_base_log,
    orte_errmgr_base_abort,
    orte_errmgr_base_abort_peers,
    update_state,
    predicted_fault,
    suggest_map_targets,
    ft_event,
    orte_errmgr_base_register_migration_warning,
    NULL, /* post_startup */
    NULL, /* pre_shutdown */
    NULL, /* mark_processes_as_dead */
    NULL, /* set_fault_callback */
    NULL  /* failure_notification */
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
                        pid_t pid,
                        orte_exit_code_t exit_code)
{
    opal_list_item_t *item, *next;
    orte_odls_job_t *jobdat = NULL;
    orte_odls_child_t *child;
    opal_buffer_t alert;
    orte_plm_cmd_flag_t cmd;
    int rc=ORTE_SUCCESS;
    orte_vpid_t null=ORTE_VPID_INVALID;
    orte_app_context_t *app;
    
    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_finalizing) {
        return ORTE_SUCCESS;
    }
    
    /* if this is a heartbeat failure, let the HNP handle it */
    if (ORTE_JOB_STATE_HEARTBEAT_FAILED == jobstate ||
        ORTE_PROC_STATE_HEARTBEAT_FAILED == state) {
        return ORTE_SUCCESS;
    }
    
    /***   UPDATE COMMAND FOR A JOB   ***/
    if (NULL == proc) {
        /* this is an update for an entire job */
        if (ORTE_JOBID_INVALID == job) {
            /* whatever happened, we don't know what job
             * it happened to
             */
            orte_show_help("help-orte-errmgr-orted.txt", "errmgr-orted:unknown-job-error",
                           true, orte_job_state_to_str(jobstate));
            OBJ_CONSTRUCT(&alert, opal_buffer_t);
            /* pack update state command */
            cmd = ORTE_PLM_UPDATE_PROC_STATE;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* pack the "invalid" jobid */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &job, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
                ORTE_ERROR_LOG(rc);
            } else {
                rc = ORTE_SUCCESS;
            }
            OBJ_DESTRUCT(&alert);
            return rc;
        }

        /* lookup the local jobdat for this job */
        jobdat = NULL;
        for (item = opal_list_get_first(&orte_local_jobdata);
             item != opal_list_get_end(&orte_local_jobdata);
             item = opal_list_get_next(item)) {
            jobdat = (orte_odls_job_t*)item;
            
            /* is this the specified job? */
            if (jobdat->jobid == job) {
                break;
            }
        }
        if (NULL == jobdat) {
            return ORTE_ERR_NOT_FOUND;
        }
        
        switch (jobstate) {
        case ORTE_JOB_STATE_FAILED_TO_START:
            failed_start(jobdat, exit_code);
            break;
        case ORTE_JOB_STATE_RUNNING:
            /* update all local child states */
            update_local_children(jobdat, jobstate, ORTE_PROC_STATE_RUNNING);
            break;
        case ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED:
            /* update all procs in job */
            update_local_children(jobdat, jobstate, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED);
            /* order all local procs for this job to be killed */
            killprocs(jobdat->jobid, ORTE_VPID_WILDCARD);
        case ORTE_JOB_STATE_COMM_FAILED:
            /* kill all local procs */
            killprocs(ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD);
            /* tell the caller we can't recover */
            return ORTE_ERR_UNRECOVERABLE;
            break;
        case ORTE_JOB_STATE_HEARTBEAT_FAILED:
            /* let the HNP handle this */
            return ORTE_SUCCESS;
            break;

        default:
            break;
        }
        OBJ_CONSTRUCT(&alert, opal_buffer_t);
        /* pack update state command */
        cmd = ORTE_PLM_UPDATE_PROC_STATE;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto FINAL_CLEANUP;
        }
        /* pack the job info */
        if (ORTE_SUCCESS != (rc = pack_state_update(&alert, jobdat))) {
            ORTE_ERROR_LOG(rc);
        }
        /* send it */
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
            ORTE_ERROR_LOG(rc);
        } else {
            rc = ORTE_SUCCESS;
        }
        OBJ_DESTRUCT(&alert);
        return rc;
    }

    /* if this was a failed comm, then see if it was to our
     * lifeline
     */
    if (ORTE_PROC_STATE_COMM_FAILED == state) {
        /* if it is our own connection, ignore it */
        if (ORTE_PROC_MY_NAME->jobid == proc->jobid &&
            ORTE_PROC_MY_NAME->vpid == proc->vpid) {
            return ORTE_SUCCESS;
        }
        /* see if this was a lifeline */
        if (ORTE_SUCCESS != orte_routed.route_lost(proc)) {
            /* kill our children */
            killprocs(ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD);
            /* terminate - our routed children will see
             * us leave and automatically die
             */
            orte_quit();
        }
        /* purge the oob */
        orte_rml.purge(proc);
        /* was it a daemon that failed? */
        if (proc->jobid == ORTE_PROC_MY_NAME->jobid) {
            /* if all my routes are gone, then terminate ourselves */
            if (0 == orte_routed.num_routes()) {
                orte_quit();
            }
        }
        /* if not, then indicate we can continue */
        return ORTE_SUCCESS;
    }
    
    /* lookup the local jobdat for this job */
    jobdat = NULL;
    for (item = opal_list_get_first(&orte_local_jobdata);
         item != opal_list_get_end(&orte_local_jobdata);
         item = opal_list_get_next(item)) {
        jobdat = (orte_odls_job_t*)item;
        
        /* is this the specified job? */
        if (jobdat->jobid == proc->jobid) {
            break;
        }
    }
    if (NULL == jobdat) {
        /* must already be complete */
        return ORTE_SUCCESS;
    }
    
    /* if there are no local procs for this job, we can
     * ignore this call
     */
    if (0 == jobdat->num_local_procs) {
        return ORTE_SUCCESS;
    }

    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                         "%s errmgr:orted got state %s for proc %s pid %d",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_proc_state_to_str(state),
                         ORTE_NAME_PRINT(proc), pid));
 
    /***  UPDATE COMMAND FOR A SPECIFIC PROCESS ***/
    if (ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED == state) {
        /* find this proc in the local children */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (child->name->jobid == proc->jobid &&
                child->name->vpid == proc->vpid) {
                if (ORTE_PROC_STATE_UNTERMINATED > child->state) {
                    child->state = state;
                    child->exit_code = exit_code;
                    /* Decrement the number of local procs */
                    jobdat->num_local_procs--;
                    /* kill this proc */
                    killprocs(proc->jobid, proc->vpid);
                }
                app = (orte_app_context_t*)opal_pointer_array_get_item(&jobdat->apps, child->app_idx);
                if( jobdat->enable_recovery && child->restarts < app->max_restarts ) {
                    child->restarts++;
                    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                         "%s errmgr:orted restarting proc %s for the %d time",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         ORTE_NAME_PRINT(proc), child->restarts));
                    rc = orte_odls.restart_proc(child);
                }
                return rc;
            }
        }
    }

    if (ORTE_PROC_STATE_TERM_NON_ZERO == state) {
        if (orte_abort_non_zero_exit) {
            /* treat this as an abnormal
             * termination - no recovery allowed
             */
            goto REPORT_ABORT;
        }
        /* treat this as normal termination */
        goto REPORT_STATE;
    }
    
    if (ORTE_PROC_STATE_TERMINATED < state) {
        if( jobdat->enable_recovery ) {
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s RECOVERY ENABLED",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            /* find this proc in the local children */
            for (item = opal_list_get_first(&orte_local_children);
                 item != opal_list_get_end(&orte_local_children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (child->name->jobid == proc->jobid &&
                    child->name->vpid == proc->vpid) {
                    /* see if this child has reached its local restart limit */
                    app = (orte_app_context_t*)opal_pointer_array_get_item(&jobdat->apps, child->app_idx);
                    OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                         "%s CHECKING RESTARTS %d VS MAX %d",
                                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                         child->restarts, app->max_restarts));
                    if (child->restarts < app->max_restarts ) {
                        /*  attempt to restart it locally */
                        child->restarts++;
                        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                             "%s errmgr:orted restarting proc %s for the %d time",
                                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                             ORTE_NAME_PRINT(child->name), child->restarts));
                        if (ORTE_SUCCESS != (rc = orte_odls.restart_proc(child))) {
                            /* reset the child's state as restart_proc would
                             * have cleared it
                             */
                            child->state = state;
                            ORTE_ERROR_LOG(rc);
                            goto REPORT_ABORT;
                        }
                        return ORTE_SUCCESS;
                    }
                }
            }
        }
        
    REPORT_ABORT:
        /* if the job hasn't completed and the state is abnormally
         * terminated, then we need to alert the HNP right away
         */
        OBJ_CONSTRUCT(&alert, opal_buffer_t);
        /* pack update state command */
        cmd = ORTE_PLM_UPDATE_PROC_STATE;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto FINAL_CLEANUP;
        }
        /* pack only the data for this proc - have to start with the jobid
         * so the receiver can unpack it correctly
         */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &proc->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* find this proc in the local children */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = opal_list_get_next(item)) {
            child = (orte_odls_child_t*)item;
            if (child->name->jobid == proc->jobid &&
                child->name->vpid == proc->vpid) {
                if (ORTE_PROC_STATE_UNTERMINATED > child->state) {
                    child->state = state;
                    child->exit_code = exit_code;
                }
                /* now pack the child's info */
                if (ORTE_SUCCESS != (rc = pack_state_for_proc(&alert, child))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
                /* remove the child from our local list as it is no longer alive */
                opal_list_remove_item(&orte_local_children, &child->super);
                /* Decrement the number of local procs */
                jobdat->num_local_procs--;
                
                OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                     "%s errmgr:orted reporting proc %s aborted to HNP (local procs = %d)",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(child->name),
                                     jobdat->num_local_procs));
                
                /* release the child object */
                OBJ_RELEASE(child);
                /* done with loop */
                break;
            }
        }
        /* send it */
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
            ORTE_ERROR_LOG(rc);
        } else {
            rc = ORTE_SUCCESS;
        }
        OBJ_DESTRUCT(&alert);
        return rc;
    }
    
REPORT_STATE:
    /* find this proc in the local children so we can update its state */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (child->name->jobid == proc->jobid &&
            child->name->vpid == proc->vpid) {
            if (ORTE_PROC_STATE_UNTERMINATED > child->state) {
                child->state = state;
                if (0 < pid) {
                    child->pid = pid;
                }
                child->exit_code = exit_code;
            }
            /* done with loop */
            break;
        }
    }

    if (ORTE_PROC_STATE_REGISTERED == state) {
        /* see if everyone in this job has registered */
        if (all_children_registered(proc->jobid)) {
            /* once everyone registers, send their contact info to
             * the HNP so it is available to debuggers and anyone
             * else that needs it
             */
            
            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                                 "%s errmgr:orted: sending contact info to HNP",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            OBJ_CONSTRUCT(&alert, opal_buffer_t);
            /* pack init routes command */
            cmd = ORTE_PLM_INIT_ROUTES_CMD;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
                ORTE_ERROR_LOG(rc);
                goto FINAL_CLEANUP;
            }
            /* pack the jobid */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &proc->jobid, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                goto FINAL_CLEANUP;
            }
            /* pack all the local child vpids */
            for (item = opal_list_get_first(&orte_local_children);
                 item != opal_list_get_end(&orte_local_children);
                 item = opal_list_get_next(item)) {
                child = (orte_odls_child_t*)item;
                if (child->name->jobid == proc->jobid) {
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &child->name->vpid, 1, ORTE_VPID))) {
                        ORTE_ERROR_LOG(rc);
                        goto FINAL_CLEANUP;
                    }
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &child->name->epoch, 1, ORTE_EPOCH))) {
                        ORTE_ERROR_LOG(rc);
                        goto FINAL_CLEANUP;
                    }
                }
            }
            /* pack an invalid marker */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &null, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto FINAL_CLEANUP;
            }
            /* add in contact info for all procs in the job */
            if (ORTE_SUCCESS != (rc = pack_child_contact_info(proc->jobid, &alert))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&alert);
                return rc;
            }
            /* send it */
            if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
                ORTE_ERROR_LOG(rc);
            } else {
                rc = ORTE_SUCCESS;
            }
            OBJ_DESTRUCT(&alert);
        }        
        return rc;
    }
    
    /* only other state is terminated - see if anyone is left alive */
    if (!any_live_children(proc->jobid)) {
        /* lookup the local jobdat for this job */
        jobdat = NULL;
        for (item = opal_list_get_first(&orte_local_jobdata);
             item != opal_list_get_end(&orte_local_jobdata);
             item = opal_list_get_next(item)) {
            jobdat = (orte_odls_job_t*)item;
        
            /* is this the specified job? */
            if (jobdat->jobid == proc->jobid) {
                break;
            }
        }
        if (NULL == jobdat) {
            /* race condition - may not have been formed yet */
            return ORTE_SUCCESS;
        }

        OBJ_CONSTRUCT(&alert, opal_buffer_t);
        /* pack update state command */
        cmd = ORTE_PLM_UPDATE_PROC_STATE;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(&alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            goto FINAL_CLEANUP;
        }
        /* pack the data for the job */
        if (ORTE_SUCCESS != (rc = pack_state_update(&alert, jobdat))) {
            ORTE_ERROR_LOG(rc);
        }
        
    FINAL_CLEANUP:
        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base.output,
                             "%s errmgr:orted reporting all procs in %s terminated",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jobdat->jobid)));
        
        /* remove all of this job's children from the global list - do not lock
         * the thread as we are already locked
         */
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = next) {
            child = (orte_odls_child_t*)item;
            next = opal_list_get_next(item);
            
            if (jobdat->jobid == child->name->jobid) {
                opal_list_remove_item(&orte_local_children, &child->super);
                OBJ_RELEASE(child);
            }
        }

        /* ensure the job's local session directory tree is removed */
        orte_session_dir_cleanup(jobdat->jobid);
        
        /* remove this job from our local job data since it is complete */
        opal_list_remove_item(&orte_local_jobdata, &jobdat->super);
        OBJ_RELEASE(jobdat);
        
        /* send it */
        if (0 > (rc = orte_rml.send_buffer(ORTE_PROC_MY_HNP, &alert, ORTE_RML_TAG_PLM, 0))) {
            ORTE_ERROR_LOG(rc);
        } else {
            rc = ORTE_SUCCESS;
        }
        OBJ_DESTRUCT(&alert);
        /* indicate that the job is complete */
        return rc;
    }
    return ORTE_SUCCESS;
}

static int predicted_fault(opal_list_t *proc_list,
                           opal_list_t *node_list,
                           opal_list_t *suggested_map)
{
    return ORTE_ERR_NOT_IMPLEMENTED;
}

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list)
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
static bool any_live_children(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* the thread is locked elsewhere - don't try to do it again here */
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if ((job == child->name->jobid || ORTE_JOBID_WILDCARD == job) &&
            child->alive) {
            return true;
        }
    }

    /* if we get here, then nobody is left alive from that job */
    return false;
    
}

static int pack_state_for_proc(opal_buffer_t *alert, orte_odls_child_t *child)
{
    int rc;
    
    /* pack the child's vpid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &(child->name->vpid), 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack the pid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->pid, 1, OPAL_PID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* if we are timing things, pack the time the proc was launched */
    if (orte_timing) {
        int64_t tmp;
        tmp = child->starttime.tv_sec;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        tmp = child->starttime.tv_usec;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    /* pack its state */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->state, 1, ORTE_PROC_STATE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack its exit code */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->exit_code, 1, ORTE_EXIT_CODE))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}

    static int pack_state_update(opal_buffer_t *alert, orte_odls_job_t *jobdat)
    {
        int rc;
        opal_list_item_t *item, *next;
        orte_odls_child_t *child;
        orte_vpid_t null=ORTE_VPID_INVALID;
    
        /* pack the jobid */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &jobdat->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        /* if we are timing things, pack the time the launch msg for this job was recvd */
        if (orte_timing) {
            int64_t tmp;
            tmp = jobdat->launch_msg_recvd.tv_sec;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            tmp = jobdat->launch_msg_recvd.tv_usec;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &tmp, 1, OPAL_INT64))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        for (item = opal_list_get_first(&orte_local_children);
             item != opal_list_get_end(&orte_local_children);
             item = next) {
            child = (orte_odls_child_t*)item;
            next = opal_list_get_next(item);
            /* if this child is part of the job... */
            if (child->name->jobid == jobdat->jobid) {
                if (ORTE_SUCCESS != (rc = pack_state_for_proc(alert, child))) {
                    ORTE_ERROR_LOG(rc);
                    return rc;
                }
            }
        }
        /* flag that this job is complete so the receiver can know */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &null, 1, ORTE_VPID))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    
        return ORTE_SUCCESS;
    }

static bool all_children_registered(orte_jobid_t job)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* the thread is locked elsewhere - don't try to do it again here */
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (OPAL_EQUAL == opal_dss.compare(&child->name->jobid, &job, ORTE_JOBID)) {
            /* if this child has terminated, we consider it as having
             * registered for the purposes of this function. If it never
             * did register, then we will send a NULL rml_uri back to
             * the HNP, which will then know that the proc did not register.
             * If other procs did register, then the HNP can declare an
             * abnormal termination
             */
            if (ORTE_PROC_STATE_UNTERMINATED < child->state) {
                /* this proc has terminated somehow - consider it
                 * as registered for now
                 */
                continue;
            }
            /* if this child is *not* registered yet, return false */
            if (!child->init_recvd) {
                return false;
            }
            /* if this child has registered a finalize, return false */
            if (child->fini_recvd) {
                return false;
            }
        }
    }
    
    /* if we get here, then everyone in the job is currently registered */
    return true;
    
}

static int pack_child_contact_info(orte_jobid_t job, opal_buffer_t *buf)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    int rc;
    
    /* the thread is locked elsewhere - don't try to do it again here */
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        /* is this child part of the specified job? */
        if (OPAL_EQUAL == opal_dss.compare(&child->name->jobid, &job, ORTE_JOBID)) {
            /* pack the child's vpid - must be done in case rml_uri is NULL */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &(child->name->vpid), 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }            
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &(child->name->epoch), 1, ORTE_EPOCH))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }            
            /* pack the contact info */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &child->rml_uri, 1, OPAL_STRING))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    return ORTE_SUCCESS;
    
}

static void failed_start(orte_odls_job_t *jobdat, orte_exit_code_t exit_code)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* set the state */
    jobdat->state = ORTE_JOB_STATE_FAILED_TO_START;
    
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (child->name->jobid == jobdat->jobid) {
            if (ORTE_PROC_STATE_LAUNCHED > child->state ||
                ORTE_PROC_STATE_FAILED_TO_START == child->state) {
                /* this proc never launched - flag that the iof
                 * is complete or else we will hang waiting for
                 * pipes to close that were never opened
                 */
                child->iof_complete = true;
                /* ditto for waitpid */
                child->waitpid_recvd = true;
            }
        }
    }
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base.output,
                         "%s errmgr:orted: job %s reported incomplete start",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobdat->jobid)));
    return;
}

static void update_local_children(orte_odls_job_t *jobdat, orte_job_state_t jobstate, orte_proc_state_t state)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    
    /* update job state */
    jobdat->state = jobstate;
    /* update children */
    for (item = opal_list_get_first(&orte_local_children);
         item != opal_list_get_end(&orte_local_children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        if (jobdat->jobid == child->name->jobid) {
            child->state = state;
        }
    }
}

static void killprocs(orte_jobid_t job, orte_vpid_t vpid)
{
    opal_pointer_array_t cmd;
    orte_proc_t proc;
    int rc;
    
    /* stop local sensors for this job */
    if (ORTE_VPID_WILDCARD == vpid) {
        orte_sensor.stop(job);
    }
    
    if (ORTE_JOBID_WILDCARD == job && ORTE_VPID_WILDCARD == vpid) {
        if (ORTE_SUCCESS != (rc = orte_odls.kill_local_procs(NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        return;
    }
    
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
