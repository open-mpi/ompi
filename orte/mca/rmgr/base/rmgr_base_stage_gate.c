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

