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

#include "orte/include/orte_constants.h"
#include "orte/include/orte_types.h"

#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/soh/soh.h"

#include "orte/mca/rmgr/base/base.h"


int orte_rmgr_base_proc_stage_gate_init(orte_jobid_t job)
{
    size_t i, num_counters=6, num_named_trigs=5;
    size_t zero=0;
    int rc;
    orte_gpr_value_t *value;
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED,
        ORTE_PROC_NUM_TERMINATED,
        ORTE_PROC_NUM_ABORTED
    };
    char* trig_names[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_STG1_TRIGGER,
        ORTE_STG2_TRIGGER,
        ORTE_STG3_TRIGGER,
        ORTE_NUM_FINALIZED_TRIGGER,
        ORTE_NUM_TERMINATED_TRIGGER
    };
    char *segment, *trig_name, *tokens[2], *trig_keys[2];
    orte_gpr_trigger_id_t id;
    size_t trig_level;

    OPAL_TRACE(1);

    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the counters */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value,
                             ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                             segment, num_counters, 1))) {
                                 
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS); /* put counters in the job's globals container */
    
    for (i=0; i < num_counters; i++) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[i]), keys[i], ORTE_SIZE, &zero))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return rc;
        }
    }

    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    OBJ_RELEASE(value);

    /*** DEFINE STAGE GATE STANDARD TRIGGERS ***/
    /* The standard triggers will return the trigger counters so that we
     * can get required information for notifying processes. Other
     * subscriptions will then attach to them.
     */
    tokens[0] = strdup(ORTE_JOB_GLOBALS);
    tokens[1] = NULL;

    trig_keys[0] = strdup(ORTE_JOB_SLOTS_KEY);
    for (i=0; i < num_named_trigs; i++) {
        trig_keys[1] = strdup(keys[i]);
        if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&trig_name,
                                        trig_names[i], job))) {
            ORTE_ERROR_LOG(rc);
            free(tokens[0]);
            free(segment);
            free(trig_keys[0]);
            free(trig_keys[1]);
            return rc;
        }
        if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id, trig_name,
                 ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS | ORTE_GPR_TRIG_ONE_SHOT |
                 ORTE_GPR_TRIG_ROUTE_DATA_THRU_ME | ORTE_GPR_TRIG_CMP_LEVELS,
                 ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                 segment, tokens, 2, trig_keys,
                 orte_rmgr_base_proc_stage_gate_mgr, NULL))) {
            ORTE_ERROR_LOG(rc);
            free(tokens[0]);
            free(segment);
            free(trig_name);
            free(trig_keys[0]);
            free(trig_keys[1]);
            return rc;
        }
        free(trig_name);
        free(trig_keys[1]);
    }
    free(trig_keys[0]);

    /* Now define the abort trigger. Again, only the trigger counter needs
     * to be returned, so we don't need to setup a subscription to get
     * other information
     */
    trig_keys[0] = strdup(ORTE_PROC_NUM_ABORTED);
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&trig_name,
                                    ORTE_NUM_ABORTED_TRIGGER, job))) {
        ORTE_ERROR_LOG(rc);
        free(tokens[0]);
        free(segment);
        free(trig_keys[0]);
        return rc;
    }
    trig_level = 1;
    if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger_level(&id, trig_name,
             ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS | ORTE_GPR_TRIG_ONE_SHOT |
             ORTE_GPR_TRIG_AT_LEVEL,
             ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
             segment, tokens, 1, trig_keys, &trig_level,
             orte_rmgr_base_proc_stage_gate_mgr_abort, NULL))) {
        ORTE_ERROR_LOG(rc);
        free(tokens[0]);
        free(segment);
        free(trig_name);
        free(trig_keys[0]);
        return rc;
    }
    free(tokens[0]);
    free(segment);
    free(trig_name);
    free(trig_keys[0]);

    /* set the job state to "launched" */
    if (ORTE_SUCCESS != (rc = orte_soh.set_job_soh(job, ORTE_JOB_STATE_LAUNCHED))) {
        ORTE_ERROR_LOG(rc);
    }

    return rc;
}


int orte_rmgr_base_proc_stage_gate_mgr(orte_gpr_notify_message_t *msg)
{
    orte_buffer_t buffer;
    orte_process_name_t *recipients=NULL;
    size_t n=0;
    int rc;
    orte_jobid_t job;

    OPAL_TRACE(1);

    /* check to see if this came from terminate. If so, we ignore it because
     * that stage gate does NOT set an xcast barrier - processes simply
     * record their state and continue processing
      */
    if (orte_schema.check_std_trigger_name(msg->target, ORTE_NUM_TERMINATED_TRIGGER)) {
        return ORTE_SUCCESS;
     }

     /* All stage gate triggers are named, so we can extract the jobid
      * directly from the trigger name
      */
     if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_std_trigger_name(&job, msg->target))) {
         ORTE_ERROR_LOG(rc);
         return rc;
     }

    /* need the list of peers for this job so we can send them the xcast.
     * obtain this list from the name service's get_job_peers function
     */
    if (ORTE_SUCCESS != (rc = orte_ns.get_job_peers(&recipients, &n, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* set the job state to the appropriate level */
    if (orte_schema.check_std_trigger_name(msg->target, ORTE_STG1_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_soh.set_job_soh(job, ORTE_JOB_STATE_AT_STG1))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_STG2_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_soh.set_job_soh(job, ORTE_JOB_STATE_AT_STG2))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_STG3_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_soh.set_job_soh(job, ORTE_JOB_STATE_AT_STG3))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    } else if (orte_schema.check_std_trigger_name(msg->target, ORTE_NUM_FINALIZED_TRIGGER)) {
        if (ORTE_SUCCESS != (rc = orte_soh.set_job_soh(job, ORTE_JOB_STATE_FINALIZED))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }

    /* set the message type to SUBSCRIPTION. When we give this to the processes, we want
     * them to break the message down and deliver it to the various subsystems.
     */
    msg->msg_type = ORTE_GPR_SUBSCRIPTION_MSG;
    msg->id = ORTE_GPR_TRIGGER_ID_MAX;

    /* need to pack the msg for sending */
    OBJ_CONSTRUCT(&buffer, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dss.pack(&buffer, &msg, 1, ORTE_GPR_NOTIFY_MSG))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&buffer);
        goto CLEANUP;
    }

    /* send the message */
    if (ORTE_SUCCESS != (rc = orte_rml.xcast(orte_process_info.my_name, recipients,
                                        n, &buffer, NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_DESTRUCT(&buffer);

CLEANUP:
    if (NULL != recipients) free(recipients);
    return rc;
}

int orte_rmgr_base_proc_stage_gate_mgr_abort(orte_gpr_notify_message_t *msg)
{
    orte_jobid_t job;
    int rc;

    OPAL_TRACE(1);

     /* All stage gate triggers are named, so we can extract the jobid
      * directly from the trigger name
      */
     if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_std_trigger_name(&job, msg->target))) {
         ORTE_ERROR_LOG(rc);
         return rc;
     }

    /* set the job status to "aborted" */

    if (ORTE_SUCCESS != (rc = orte_soh.set_job_soh(job, ORTE_JOB_STATE_ABORTED))) {
        ORTE_ERROR_LOG(rc);
    }

    orte_errmgr.incomplete_start(job);

    return ORTE_SUCCESS;
}


/*
 * Routine that tools such as orterun can use to subscribe
 * to events on all counters.
 */

int orte_rmgr_base_proc_stage_gate_subscribe(orte_jobid_t job, orte_gpr_notify_cb_fn_t cbfunc, void* cbdata, int type)
{
    size_t i;
    int rc;
    char *segment, *trig_name, *tokens[2];
    orte_gpr_subscription_id_t id;
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED,
        ORTE_PROC_NUM_TERMINATED,
        ORTE_PROC_NUM_ABORTED
    };
    char* trig_names[] = {
        /* changes to this ordering need to be reflected in code below
         * number of entries MUST match those above
         */
        ORTE_STG1_TRIGGER,
        ORTE_STG2_TRIGGER,
        ORTE_STG3_TRIGGER,
        ORTE_NUM_FINALIZED_TRIGGER,
        ORTE_NUM_TERMINATED_TRIGGER,
        ORTE_NUM_ABORTED_TRIGGER
    };
    size_t num_counters = sizeof(keys)/sizeof(keys[0]);

    OPAL_TRACE(1);

    /* identify the segment for this job */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* setup the tokens */
    tokens[0]=ORTE_JOB_GLOBALS;
    tokens[1]=NULL;

    for (i=0; i < num_counters; i++) {
        if (ORTE_STAGE_GATE_TERMINATION == type) {
            if ( ORTE_PROC_NUM_TERMINATED != keys[i] &&
                 ORTE_PROC_NUM_ABORTED    != keys[i])
                continue;
        }
        else if (ORTE_STAGE_GATE_STAGES == type) {
            if (ORTE_PROC_NUM_AT_STG1   != keys[i] &&
                ORTE_PROC_NUM_AT_STG2   != keys[i] &&
                ORTE_PROC_NUM_AT_STG3   != keys[i] &&
                ORTE_PROC_NUM_FINALIZED != keys[i] )
                continue;
        }
        else if (ORTE_STAGE_GATE_ALL != type) {
            ORTE_ERROR_LOG(ORTE_ERROR);
            printf("Invalid argument (%d)\n", type);
            return ORTE_ERROR;
        }

        /* attach ourselves to the appropriate standard trigger */
        if (ORTE_SUCCESS !=
            (rc = orte_schema.get_std_trigger_name(&trig_name, trig_names[i], job))) {
            ORTE_ERROR_LOG(rc);
            free(segment);
            return rc;
        }

        if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&id, trig_name, NULL,
                                    ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG,
                                    ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                    segment, tokens, keys[i],
                                    cbfunc, cbdata))) {
            ORTE_ERROR_LOG(rc);
            free(segment);
            free(trig_name);
            return rc;
        }
        free(trig_name);
    }
    free(segment);

    return ORTE_SUCCESS;
}


