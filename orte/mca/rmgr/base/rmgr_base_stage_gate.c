/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

/*
 * includes
 */
#include "orte_config.h"

#include <string.h>

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/class/orte_pointer_array.h"
#include "orte/dss/dss.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/odls.h"
#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/smr/smr.h"
#include "orte/runtime/runtime.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

int orte_rmgr_base_proc_stage_gate_init(orte_jobid_t job)
{
    int rc;

    /* init the stage gates */
    if (ORTE_SUCCESS != (rc = orte_smr.init_job_stage_gates(job, orte_rmgr_base_proc_stage_gate_mgr, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
 
    return ORTE_SUCCESS;
}


int orte_rmgr_base_proc_stage_gate_mgr(orte_gpr_notify_message_t *msg)
{
    int rc;
    orte_jobid_t job;
    orte_buffer_t *buffer;

    OPAL_TRACE(1);

    /* All stage gate triggers are named, so we can extract the jobid
      * directly from the trigger name
      */
     if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_std_trigger_name(&job, msg->target))) {
         ORTE_ERROR_LOG(rc);
         return rc;
     }

    OPAL_TRACE_ARG1(1, job);
            
    /* set the job state to the appropriate level */
    if (orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_INIT_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_INIT))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_STARTUP_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_ORTE_STARTUP_COMPLETE))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_LAUNCHED_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_LAUNCHED))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_RUNNING_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_RUNNING))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_STG1_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_AT_STG1))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_STG2_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_AT_STG2))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_STG3_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_AT_STG3))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_FINALIZED_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_FINALIZED))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_TERMINATED_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_TERMINATED))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_NUM_ABORTED_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_ABORTED))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_FAILED_TO_START_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(job, ORTE_JOB_STATE_FAILED_TO_START))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }

    /* check to see if this came from a trigger that does not require we send
     * out a message
     */
    if (!orte_schema.check_std_trigger_name(msg->target, ORTE_STARTUP_TRIGGER) &&
        !orte_schema.check_std_trigger_name(msg->target, ORTE_STG1_TRIGGER) &&
        !orte_schema.check_std_trigger_name(msg->target, ORTE_STG2_TRIGGER) &&
        !orte_schema.check_std_trigger_name(msg->target, ORTE_STG3_TRIGGER) &&
        !orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_FINALIZED_TRIGGER)) {
        return ORTE_SUCCESS;
    }
    
    /* set the message type to SUBSCRIPTION. When we give this to the processes, we want
     * them to break the message down and deliver it to the various subsystems.
     */
    msg->msg_type = ORTE_GPR_SUBSCRIPTION_MSG;
    msg->id = ORTE_GPR_TRIGGER_ID_MAX;

    /* setup the buffer */
    buffer = OBJ_NEW(orte_buffer_t);
    
    /* load the payload */
    if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &msg, 1, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(buffer);
        return rc;
    }

    /* send the message to the xcast_barrier tag for handling - this is the
     * destination here since these messages are intended to release
     * a process from an xcast gate
     */
    if (ORTE_SUCCESS != (rc = orte_rml.xcast(job, buffer, ORTE_RML_TAG_XCAST_BARRIER))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(buffer);

CLEANUP:
    
    return rc;
}


int orte_rmgr_base_orted_stage_gate_init(orte_jobid_t job)
{
    orte_job_map_t *map;
    int rc;
    
    /* get the map for this job */
    if (ORTE_SUCCESS != (rc = orte_rmaps.get_job_map(&map, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the orted triggers for passing their launch info */
    if (ORTE_SUCCESS != (rc = orte_smr.init_orted_stage_gates(map->job, map->num_new_daemons,
                                                              orte_rmgr_base_orted_stage_gate_mgr, NULL))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}


int orte_rmgr_base_orted_stage_gate_mgr(orte_gpr_notify_message_t *msg)
{
    int rc;
    orte_jobid_t job, *jptr;
    orte_job_map_t *map;
    orte_daemon_cmd_flag_t command;
    orte_buffer_t *buffer;
    orte_gpr_notify_data_t *launch_data;
    char* keys[] = {
        ORTE_JOB_BEING_LAUNCHED_KEY,
        NULL
    };
    char* tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    orte_gpr_value_t **values=NULL;
    orte_std_cntr_t num_values=0;
    
    OPAL_TRACE(1);

    /* set the job state to the appropriate level */
    if (orte_schema.check_std_trigger_name(msg->target, ORTE_STARTUP_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(0, ORTE_JOB_ORTE_STARTUP_COMPLETE))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_RUNNING_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_smr.set_job_state(0, ORTE_JOB_STATE_RUNNING))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* check to see if this came from a trigger that requires we send a message - if
     * so, then handle it as required
     */
    if (orte_schema.check_std_trigger_name(msg->target, ORTE_STARTUP_TRIGGER)) {
        /* the startup trigger is intended for the sharing of contact info
         * across all orteds. The new orteds will be sitting at their startup
         * stage gate and need an xcast_barrier message to release them, so we
         * send the message to that RML tag. Orteds that have already started will
         * have posted a persistent RML receive on that tag so they can "catch"
         * this message as well - they use that mechanism to update their RML
         * contact info so they can talk to the other daemons since the xcast
         * goes to ALL members of the specified job.
         */
        
        /* set the message type to SUBSCRIPTION. When we give this to the processes, we want
         * them to break the message down and deliver it to the various subsystems.
         */
        msg->msg_type = ORTE_GPR_SUBSCRIPTION_MSG;
        msg->id = ORTE_GPR_TRIGGER_ID_MAX;
        
        /* setup the buffer */
        buffer = OBJ_NEW(orte_buffer_t);

        /* pack the msg to be delivered */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &msg, 1, ORTE_GPR_NOTIFY_MSG))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buffer);
            return rc;
        }
        
        /* send the message to the xcast_barrier since the orte_startup trigger
         * is a blocking action
         */
        if (ORTE_SUCCESS != (rc = orte_rml.xcast(0, buffer, ORTE_RML_TAG_XCAST_BARRIER))) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_RELEASE(buffer);
        
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_ALL_RUNNING_TRIGGER)) {
        /* the running trigger indicates that we are ready to launch a job - get the
         * job being launched from the registry, get the launch data, and then send it out to
         * all the orteds
         */
        if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
                                               "orte-job-0", tokens, keys,
                                               &num_values, &values))) {
            
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        
        if (1 != num_values || 1 != values[0]->cnt) { /* can only be one value returned */
            ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
            goto cleanup;
        }
        
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&jptr, values[0]->keyvals[0]->value, ORTE_JOBID))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        job = *jptr;
        OBJ_RELEASE(values[0]);
        
        /* get the job map */
        if (ORTE_SUCCESS != (rc = orte_rmaps.get_job_map(&map, job))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        
        /* let the local launcher provide its required data */
        if (ORTE_SUCCESS != (rc = orte_odls.get_add_procs_data(&launch_data, map))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        OBJ_RELEASE(map);  /* done with this */
        
        /* setup the buffer */
        buffer = OBJ_NEW(orte_buffer_t);
        /* pack the add_local_procs command */
        command = ORTE_DAEMON_ADD_LOCAL_PROCS;
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &command, 1, ORTE_DAEMON_CMD))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buffer);
            return rc;
        }
        
        /* pack the launch data */
        if (ORTE_SUCCESS != (rc = orte_dss.pack(buffer, &launch_data, 1, ORTE_GPR_NOTIFY_DATA))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(buffer);
            return rc;
        }
   
        /* send the command to the daemon */
        if (ORTE_SUCCESS != (rc = orte_rml.xcast(0, buffer, ORTE_RML_TAG_DAEMON))) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_RELEASE(buffer);
        
cleanup:
        if (NULL != values) free(values);
    }
    
    return ORTE_SUCCESS;
}

