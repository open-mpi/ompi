/*
 * Copyright (c) 2009-2010 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
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
#include "orte/util/session_dir.h"
#include "orte/util/show_help.h"
#include "orte/util/nidmap.h"

#include "orte/mca/rml/rml.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/sensor/sensor.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/state/state.h"

#include "orte/runtime/orte_quit.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/data_type_support/orte_dt_support.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_default_orted.h"

/*
 * Module functions: Global
 */
static int init(void);
static int finalize(void);

static int predicted_fault(opal_list_t *proc_list,
                           opal_list_t *node_list,
                           opal_list_t *suggested_map);

static int suggest_map_targets(orte_proc_t *proc,
                               orte_node_t *oldnode,
                               opal_list_t *node_list);

static int ft_event(int state);


/******************
 * default_orted module
 ******************/
orte_errmgr_base_module_t orte_errmgr_default_orted_module = {
    init,
    finalize,
    orte_errmgr_base_log,
    orte_errmgr_base_abort,
    orte_errmgr_base_abort_peers,
    predicted_fault,
    suggest_map_targets,
    ft_event,
    orte_errmgr_base_register_migration_warning,
    NULL,
    orte_errmgr_base_execute_error_callbacks
};

/* Local functions */
static bool any_live_children(orte_jobid_t job);
static int pack_state_update(opal_buffer_t *alert, orte_job_t *jobdat);
static int pack_state_for_proc(opal_buffer_t *alert, orte_proc_t *child);
static bool all_children_registered(orte_jobid_t job);
static int pack_child_contact_info(orte_jobid_t job, opal_buffer_t *buf);
static void failed_start(orte_job_t *jobdat);
static void update_local_children(orte_job_t *jobdat,
                                  orte_job_state_t jobstate,
                                  orte_proc_state_t state);
static void killprocs(orte_jobid_t job, orte_vpid_t vpid);

static void job_errors(int fd, short args, void *cbdata);
static void proc_errors(int fd, short args, void *cbdata);

/************************
 * API Definitions
 ************************/
static int init(void)
{
    /* setup state machine to trap job errors */
    orte_state.add_job_state(ORTE_JOB_STATE_ERROR, job_errors, ORTE_ERROR_PRI);

    /* set the lost connection state to run at MSG priority so
     * we can process any last messages from the proc
     */
    orte_state.add_proc_state(ORTE_PROC_STATE_COMM_FAILED, proc_errors, ORTE_MSG_PRI);

    /* setup state machine to trap proc errors */
    orte_state.add_proc_state(ORTE_PROC_STATE_ERROR, proc_errors, ORTE_ERROR_PRI);

    return ORTE_SUCCESS;
}

static int finalize(void)
{
    return ORTE_SUCCESS;
}

static void job_errors(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_job_state_t jobstate;
    int rc;
    orte_plm_cmd_flag_t cmd;
    opal_buffer_t *alert;

    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_finalizing) {
        return;
    }

    /* if the jdata is NULL, then we abort as this
     * is reporting an unrecoverable error
     */
    if (NULL == caddy->jdata) {
        ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_FORCED_EXIT);
        OBJ_RELEASE(caddy);
        return;
    }

    /* update the state */
    jdata = caddy->jdata;
    jobstate = caddy->job_state;
    jdata->state = jobstate;

    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_framework.framework_output,
                         "%s errmgr:default_orted: job %s reported error state %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jdata->jobid),
                         orte_job_state_to_str(jobstate)));

    switch (jobstate) {
    case ORTE_JOB_STATE_FAILED_TO_START:
        failed_start(jdata);
        break;
    case ORTE_JOB_STATE_SENSOR_BOUND_EXCEEDED:
        /* update all procs in job */
        update_local_children(jdata, jobstate, ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED);
        /* order all local procs for this job to be killed */
        killprocs(jdata->jobid, ORTE_VPID_WILDCARD);
        break;
    case ORTE_JOB_STATE_COMM_FAILED:
        /* kill all local procs */
        killprocs(ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD);
        /* order termination */
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto cleanup;
        break;
    case ORTE_JOB_STATE_HEARTBEAT_FAILED:
        /* let the HNP handle this */
        goto cleanup;
        break;

    default:
        break;
    }
    alert = OBJ_NEW(opal_buffer_t);
    /* pack update state command */
    cmd = ORTE_PLM_UPDATE_PROC_STATE;
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(alert);
        goto cleanup;
    }
    /* pack the job info */
    if (ORTE_SUCCESS != (rc = pack_state_update(alert, jdata))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(alert);
        goto cleanup;
    }
    /* send it */
    if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                          ORTE_RML_TAG_PLM,
                                          orte_rml_send_callback, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(alert);
    }

 cleanup:
    OBJ_RELEASE(caddy);
}

static void proc_errors(int fd, short args, void *cbdata)
{
    orte_state_caddy_t *caddy = (orte_state_caddy_t*)cbdata;
    orte_job_t *jdata;
    orte_process_name_t *proc = &caddy->name;
    orte_proc_state_t state = caddy->proc_state;

    orte_proc_t *child, *ptr;
    opal_buffer_t *alert;
    orte_plm_cmd_flag_t cmd;
    int rc=ORTE_SUCCESS;
    orte_vpid_t null=ORTE_VPID_INVALID;
    orte_ns_cmp_bitmask_t mask=ORTE_NS_CMP_ALL;
    int i;

    /*
     * if orte is trying to shutdown, just let it
     */
    if (orte_finalizing) {
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_framework.framework_output,
                         "%s errmgr:default_orted:proc_errors process %s error state %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(proc),
                         orte_proc_state_to_str(state)));

    /* if this is a heartbeat failure, let the HNP handle it */
    if (ORTE_PROC_STATE_HEARTBEAT_FAILED == state) {
        goto cleanup;
    }

    /* if this was a failed comm, then see if it was to our
     * lifeline
     */
    if (ORTE_PROC_STATE_LIFELINE_LOST == state ||
        ORTE_PROC_STATE_UNABLE_TO_SEND_MSG == state) {
        OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:orted lifeline lost - exiting",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        /* set our exit status */
        ORTE_UPDATE_EXIT_STATUS(ORTE_ERROR_DEFAULT_EXIT_CODE);
        /* kill our children */
        killprocs(ORTE_JOBID_WILDCARD, ORTE_VPID_WILDCARD);
        /* terminate - our routed children will see
         * us leave and automatically die
         */
        ORTE_FORCED_TERMINATE(ORTE_ERROR_DEFAULT_EXIT_CODE);
        goto cleanup;
    }

    if (ORTE_PROC_STATE_COMM_FAILED == state) {
        /* if it is our own connection, ignore it */
        if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, ORTE_PROC_MY_NAME, proc)) {
            goto cleanup;
        }
        /* was it a daemon? */
        if (proc->jobid != ORTE_PROC_MY_NAME->jobid) {
            /* nope - ignore */
            goto cleanup;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:default:orted daemon %s exited",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(proc)));
        /* are any of my children still alive */
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL != (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                if (child->alive && child->state < ORTE_PROC_STATE_UNTERMINATED) {
                    goto cleanup;
                }
            }
        }
        /* if all my routes and children are gone, then terminate
           ourselves nicely (i.e., this is a normal termination) */
        if (0 == orte_routed.num_routes()) {
            OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_framework.framework_output,
                                 "%s errmgr:default:orted all routes gone - exiting",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            ORTE_ACTIVATE_JOB_STATE(NULL, ORTE_JOB_STATE_DAEMONS_TERMINATED);
        } else {
            OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_framework.framework_output,
                                 "%s errmgr:default:orted not exiting, num_routes() == %d",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 (int)orte_routed.num_routes()));
        }
        /* if not, then we can continue */
        goto cleanup;
    }

    /* get the job object */
    if (NULL == (jdata = orte_get_job_data_object(proc->jobid))) {
        /* must already be complete */
        goto cleanup;
    }

    /* if there are no local procs for this job, we can
     * ignore this call
     */
    if (0 == jdata->num_local_procs) {
        goto cleanup;
    }

    /* find this proc in the local children */
    child = NULL;
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (ptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        if (OPAL_EQUAL == orte_util_compare_name_fields(mask, &ptr->name, proc)) {
            child = ptr;
            break;
        }
    }
    if (NULL == child) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        goto cleanup;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_errmgr_base_framework.framework_output,
                         "%s errmgr:default_orted got state %s for proc %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         orte_proc_state_to_str(state),
                         ORTE_NAME_PRINT(proc)));
 
    if (ORTE_PROC_STATE_SENSOR_BOUND_EXCEEDED == state) {
        child->state = state;
        /* Decrement the number of local procs */
        jdata->num_local_procs--;
        /* kill this proc */
        killprocs(proc->jobid, proc->vpid);
        goto cleanup;
    }

    if (ORTE_PROC_STATE_TERM_NON_ZERO == state) {
        if (!orte_abort_non_zero_exit) {
            /* leave the child in orte_local_children so we can
             * later send the state info after full job termination
             */
            child->state = state;
            child->waitpid_recvd = true;
            if (child->iof_complete) {
                /* the proc has terminated */
                child->alive = false;
                /* Clean up the session directory as if we were the process
                 * itself.  This covers the case where the process died abnormally
                 * and didn't cleanup its own session directory.
                 */
                orte_session_dir_finalize(&child->name);
                /* track job status */
                jdata->num_terminated++;
            }
            /* treat this as normal termination */
            goto REPORT_STATE;
        }
        /* report this as abnormal termination to the HNP */
        alert = OBJ_NEW(opal_buffer_t);
        /* pack update state command */
        cmd = ORTE_PLM_UPDATE_PROC_STATE;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack only the data for this proc - have to start with the jobid
         * so the receiver can unpack it correctly
         */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &proc->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return;
        }

        child->state = state;
        /* now pack the child's info */
        if (ORTE_SUCCESS != (rc = pack_state_for_proc(alert, child))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* remove the child from our local array as it is no longer alive */
        opal_pointer_array_set_item(orte_local_children, i, NULL);
        /* Decrement the number of local procs */
        jdata->num_local_procs--;

        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:default_orted reporting proc %s abnormally terminated with non-zero status (local procs = %d)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&child->name),
                             jdata->num_local_procs));
        
        /* release the child object */
        OBJ_RELEASE(child);

        /* send it */
        if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                              ORTE_RML_TAG_PLM,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(alert);
        }
        return;
    }

    if (ORTE_PROC_STATE_FAILED_TO_START == state ||
        ORTE_PROC_STATE_FAILED_TO_LAUNCH == state) {
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
        goto cleanup;
    }

    if (ORTE_PROC_STATE_TERMINATED < state) {
        /* if the job hasn't completed and the state is abnormally
         * terminated, then we need to alert the HNP right away
         */
        alert = OBJ_NEW(opal_buffer_t);
        /* pack update state command */
        cmd = ORTE_PLM_UPDATE_PROC_STATE;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack only the data for this proc - have to start with the jobid
         * so the receiver can unpack it correctly
         */
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &proc->jobid, 1, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            return;
        }

        child->state = state;
        /* now pack the child's info */
        if (ORTE_SUCCESS != (rc = pack_state_for_proc(alert, child))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* remove the child from our local array as it is no longer alive */
        opal_pointer_array_set_item(orte_local_children, i, NULL);
        /* Decrement the number of local procs */
        jdata->num_local_procs--;

        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:default_orted reporting proc %s aborted to HNP (local procs = %d)",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&child->name),
                             jdata->num_local_procs));
        
        /* release the child object */
        OBJ_RELEASE(child);

        /* send it */
        if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                              ORTE_RML_TAG_PLM,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        return;
    }

 REPORT_STATE:
    if (ORTE_PROC_STATE_REGISTERED == state) {
        /* see if everyone in this job has registered */
        if (all_children_registered(proc->jobid)) {
            /* once everyone registers, send their contact info to
             * the HNP so it is available to debuggers and anyone
             * else that needs it
             */

            OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                                 "%s errmgr:default_orted: sending contact info to HNP",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
            
            alert = OBJ_NEW(opal_buffer_t);
            /* pack init routes command */
            cmd = ORTE_PLM_INIT_ROUTES_CMD;
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            /* pack the jobid */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &proc->jobid, 1, ORTE_JOBID))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            /* pack all the local child vpids */
            for (i=0; i < orte_local_children->size; i++) {
                if (NULL == (ptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                    continue;
                }
                if (ptr->name.jobid == proc->jobid) {
                    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &ptr->name.vpid, 1, ORTE_VPID))) {
                        ORTE_ERROR_LOG(rc);
                        return;
                    }
                }
            }
            /* pack an invalid marker */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &null, 1, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                return;
            }
            /* add in contact info for all procs in the job */
            if (ORTE_SUCCESS != (rc = pack_child_contact_info(proc->jobid, alert))) {
                ORTE_ERROR_LOG(rc);
                OBJ_DESTRUCT(&alert);
                return;
            }
            /* send it */
            if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                                  ORTE_RML_TAG_PLM,
                                                  orte_rml_send_callback, NULL))) {
                ORTE_ERROR_LOG(rc);
            }
        }        
        return;
    }

    /* only other state is terminated - see if anyone is left alive */
    if (!any_live_children(proc->jobid)) {
        alert = OBJ_NEW(opal_buffer_t);
        /* pack update state command */
        cmd = ORTE_PLM_UPDATE_PROC_STATE;
        if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &cmd, 1, ORTE_PLM_CMD))) {
            ORTE_ERROR_LOG(rc);
            return;
        }
        /* pack the data for the job */
        if (ORTE_SUCCESS != (rc = pack_state_update(alert, jdata))) {
            ORTE_ERROR_LOG(rc);
            return;
        }

        OPAL_OUTPUT_VERBOSE((5, orte_errmgr_base_framework.framework_output,
                             "%s errmgr:default_orted reporting all procs in %s terminated",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid)));
        
        /* remove all of this job's children from the global list - do not lock
         * the thread as we are already locked
         */
        for (i=0; i < orte_local_children->size; i++) {
            if (NULL == (ptr = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
                continue;
            }
            if (jdata->jobid == ptr->name.jobid) {
                opal_pointer_array_set_item(orte_local_children, i, NULL);
                OBJ_RELEASE(ptr);
            }
        }

        /* ensure the job's local session directory tree is removed */
        orte_session_dir_cleanup(jdata->jobid);

        /* remove this job from our local job data since it is complete */
        opal_pointer_array_set_item(orte_job_data, ORTE_LOCAL_JOBID(jdata->jobid), NULL);
        OBJ_RELEASE(jdata);

        /* send it */
        if (0 > (rc = orte_rml.send_buffer_nb(ORTE_PROC_MY_HNP, alert,
                                              ORTE_RML_TAG_PLM,
                                              orte_rml_send_callback, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        return;
    }

 cleanup:
    OBJ_RELEASE(caddy);
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

static int ft_event(int state)
{
    return ORTE_SUCCESS;
}


/*****************
 * Local Functions
 *****************/
static bool any_live_children(orte_jobid_t job)
{
    int i;
    orte_proc_t *child;

    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        /* is this child part of the specified job? */
        if ((job == child->name.jobid || ORTE_JOBID_WILDCARD == job) &&
            child->alive) {
            return true;
        }
    }

    /* if we get here, then nobody is left alive from that job */
    return false;

}

static int pack_state_for_proc(opal_buffer_t *alert, orte_proc_t *child)
{
    int rc;

    /* pack the child's vpid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &(child->name.vpid), 1, ORTE_VPID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* pack the pid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &child->pid, 1, OPAL_PID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
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

static int pack_state_update(opal_buffer_t *alert, orte_job_t *jobdat)
{
    int rc, i;
    orte_proc_t *child;
    orte_vpid_t null=ORTE_VPID_INVALID;

    /* pack the jobid */
    if (ORTE_SUCCESS != (rc = opal_dss.pack(alert, &jobdat->jobid, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        /* if this child is part of the job... */
        if (child->name.jobid == jobdat->jobid) {
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
    int i;
    orte_proc_t *child;

    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        /* is this child part of the specified job? */
        if (job == child->name.jobid || ORTE_JOBID_WILDCARD == job) {
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
            /* if this child has *not* registered yet, return false */
            if (!child->registered) {
                return false;
            }
        }
    }

    /* if we get here, then everyone in the job has registered */
    return true;

}

static int pack_child_contact_info(orte_jobid_t job, opal_buffer_t *buf)
{
    orte_proc_t *child;
    int rc, i;

    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        /* is this child part of the specified job? */
        if (job == child->name.jobid || ORTE_JOBID_WILDCARD == job) {
            /* pack the child's vpid - must be done in case rml_uri is NULL */
            if (ORTE_SUCCESS != (rc = opal_dss.pack(buf, &(child->name.vpid), 1, ORTE_VPID))) {
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

static void failed_start(orte_job_t *jobdat)
{
    int i;
    orte_proc_t *child;

    /* set the state */
    jobdat->state = ORTE_JOB_STATE_FAILED_TO_START;

    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        /* is this child part of the specified job? */
        if (child->name.jobid == jobdat->jobid) {
            if (ORTE_PROC_STATE_FAILED_TO_START == child->state) {
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
    OPAL_OUTPUT_VERBOSE((1, orte_errmgr_base_framework.framework_output,
                         "%s errmgr:hnp: job %s reported incomplete start",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_JOBID_PRINT(jobdat->jobid)));
    return;
}

static void update_local_children(orte_job_t *jobdat, orte_job_state_t jobstate, orte_proc_state_t state)
{
    int i;
    orte_proc_t *child;

    /* update job state */
    jobdat->state = jobstate;
    /* update children */
    for (i=0; i < orte_local_children->size; i++) {
        if (NULL == (child = (orte_proc_t*)opal_pointer_array_get_item(orte_local_children, i))) {
            continue;
        }
        /* is this child part of the specified job? */
        if (jobdat->jobid == child->name.jobid) {
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

    if (ORTE_JOBID_WILDCARD == job 
        && ORTE_VPID_WILDCARD == vpid) {
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
