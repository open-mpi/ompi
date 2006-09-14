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

#include "orte/dss/dss.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/smr/base/smr_private.h"


int orte_smr_base_init_job_stage_gates(orte_jobid_t job,
                                        orte_gpr_trigger_cb_fn_t cbfunc,
                                        void *user_tag)
{
    orte_std_cntr_t i, num_counters, num_named_trigs;
    orte_std_cntr_t zero=0;
    int rc, num_start_routing;
    orte_gpr_value_t *value;
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        /* We need to set up counters for all the defined ORTE process states, even though
         * the launch system doesn't actually use them all. This must be done so that
         * user-defined callbacks can be generated - otherwise, they won't happen!
         */
        ORTE_PROC_NUM_AT_INIT,
        ORTE_PROC_NUM_LAUNCHED,
        ORTE_PROC_NUM_RUNNING,
        ORTE_PROC_NUM_TERMINATED,
        /* the following stage gates need data routed through them */
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED
    };
    char* trig_names[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_ALL_INIT_TRIGGER,
        ORTE_ALL_LAUNCHED_TRIGGER,
        ORTE_ALL_RUNNING_TRIGGER,
        ORTE_NUM_TERMINATED_TRIGGER,
        /* the following triggers need data routed through them */
        ORTE_STG1_TRIGGER,
        ORTE_STG2_TRIGGER,
        ORTE_STG3_TRIGGER,
        ORTE_NUM_FINALIZED_TRIGGER,
    };
    char *segment, *trig_name, *tokens[2], *trig_keys[2];
    orte_gpr_trigger_id_t id;
    orte_gpr_trigger_action_t trig_mode, trig_mode_routed;

    num_counters = sizeof(keys)/sizeof(keys[0]);
    num_named_trigs= sizeof(trig_names)/sizeof(trig_names[0]);
    num_start_routing = 4;

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
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[i]), keys[i], ORTE_STD_CNTR, &zero))) {
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
    trig_mode = ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS | ORTE_GPR_TRIG_ONE_SHOT |
                ORTE_GPR_TRIG_CMP_LEVELS;
    trig_mode_routed = trig_mode | ORTE_GPR_TRIG_ROUTE_DATA_THRU_ME;
    
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
        if (i < num_start_routing) {
            /* the first set of triggers do NOT have anything routed to them.
             * They are setup here strictly for users to attach to them.
             * Hence, we do not pass a trigger callback function and
             * leave the trig actionso not route data through me
             */
            if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id, trig_name, trig_mode,
                                                              ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                                                              segment, tokens, 2, trig_keys,
                                                              NULL, NULL))) {
                ORTE_ERROR_LOG(rc);
                free(tokens[0]);
                free(segment);
                free(trig_name);
                free(trig_keys[0]);
                free(trig_keys[1]);
                return rc;
            }
        } else {
            if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id, trig_name, trig_mode_routed,
                     ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                     segment, tokens, 2, trig_keys,
                     cbfunc, user_tag))) {
                ORTE_ERROR_LOG(rc);
                free(tokens[0]);
                free(segment);
                free(trig_name);
                free(trig_keys[0]);
                free(trig_keys[1]);
                return rc;
            }
        }
        free(trig_name);
        free(trig_keys[1]);
    }
    free(trig_keys[0]);
    free(tokens[0]);
    free(segment);

    return ORTE_SUCCESS;
}


/*
 * Setup orted-specific stage gates
 * setup the orted trigger to fire when the specified number of orteds have been launched
 */
int orte_smr_base_init_orted_stage_gates(orte_jobid_t job,
                                         orte_std_cntr_t num_orteds,
                                         orte_gpr_trigger_cb_fn_t cbfunc,
                                         void *user_tag)
{
    char *segment;
    char *trig_name;
    orte_gpr_value_t *value;
    orte_std_cntr_t zero=0;
    char *trig_tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    char *trig_keys[] = {
        ORTED_NUM_TO_BE_LAUNCHED,
        ORTED_LAUNCH_STAGE_GATE_CNTR,
        NULL
    };
    int rc;
    orte_gpr_trigger_id_t id;
    
    /** get the segment name where all of this is taking place */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup to store two values - the number of orteds and the counter */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value,
                                                    ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                                                    segment, 2, 1))) {
        
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS); /* put counters in the job's globals container */
    
    /** store the number of orteds */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]),
                                                     ORTED_NUM_TO_BE_LAUNCHED, ORTE_STD_CNTR, &num_orteds))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        OBJ_RELEASE(value);
        return rc;
    }
    /** initialize the counter to zero */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTED_LAUNCH_STAGE_GATE_CNTR, ORTE_STD_CNTR, &zero))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        OBJ_RELEASE(value);
        return rc;
    }
    
    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        OBJ_RELEASE(value);
        return rc;
    }
    OBJ_RELEASE(value);
    
    /* now define a trigger based on those counters */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&trig_name,
                                                               ORTED_LAUNCH_STAGE_GATE_TRIGGER, job))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger(&id, trig_name,
                                                      ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS | ORTE_GPR_TRIG_ONE_SHOT |
                                                      ORTE_GPR_TRIG_CMP_LEVELS,
                                                      ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                                                      segment, trig_tokens, 2, trig_keys,
                                                      cbfunc, user_tag))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        free(trig_name);
        return rc;
    }
    
    free(segment);
    free(trig_name);
    return ORTE_SUCCESS;    
}


/*
 * Setup an alert monitor
 */
int orte_smr_base_define_alert_monitor(orte_jobid_t job,
                                       char *trigger_name,
                                       char *counter_key,
                                       orte_std_cntr_t init_value,
                                       orte_std_cntr_t alert_value,
                                       bool one_shot,
                                       orte_gpr_trigger_cb_fn_t cbfunc,
                                       void *user_tag)
{
    int rc;
    orte_gpr_value_t *value;
    orte_gpr_trigger_action_t trig_mode;
    char *segment, *trig_name;
    char *tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    orte_gpr_trigger_id_t id;
    
    /* get the job's segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the counters */
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value,
                                                    ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                                                    segment, 1, 1))) {
        
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }    
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS); /* put counters in the job's globals container */
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), counter_key, ORTE_STD_CNTR, &init_value))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    
    /* put the counter on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        free(segment);
        return rc;
    }
    OBJ_RELEASE(value);
    
    
    /* define the trigger to fire at specified level */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&trig_name,
                                                               trigger_name, job))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    trig_mode = ORTE_GPR_TRIG_INCLUDE_TRIG_CNTRS | ORTE_GPR_TRIG_AT_LEVEL;
    if (one_shot) {
        trig_mode = trig_mode | ORTE_GPR_TRIG_ONE_SHOT;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.define_trigger_level(&id, trig_name,
                                                            trig_mode,
                                                            ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR,
                                                            segment, tokens, 1, &counter_key, &alert_value,
                                                            cbfunc, user_tag))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        free(trig_name);
        return rc;
    }
    free(segment);
    free(trig_name);
    
    return ORTE_SUCCESS;    
}


/*
 * Routine that tools such as orterun can use to subscribe
 * to events on all counters.
 */

int orte_smr_base_job_stage_gate_subscribe(orte_jobid_t job,
                                           orte_gpr_notify_cb_fn_t cbfunc, void* cbdata,
                                           orte_proc_state_t cb_conditions)
{
    orte_std_cntr_t i;
    int rc;
    char *segment, *trig_name, *tokens[2];
    orte_proc_state_t conditions;
    orte_gpr_subscription_id_t id;
    /** the order of the next three definitions MUST match */
    orte_proc_state_t state[] = {
        ORTE_PROC_STATE_INIT,
        ORTE_PROC_STATE_LAUNCHED,
        ORTE_PROC_STATE_RUNNING,
        ORTE_PROC_STATE_AT_STG1,
        ORTE_PROC_STATE_AT_STG2,
        ORTE_PROC_STATE_AT_STG3,
        ORTE_PROC_STATE_FINALIZED,
        ORTE_PROC_STATE_TERMINATED
    };
    char* keys[] = {
        ORTE_PROC_NUM_AT_INIT,
        ORTE_PROC_NUM_LAUNCHED,
        ORTE_PROC_NUM_RUNNING,
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED,
        ORTE_PROC_NUM_TERMINATED
    };
    char* trig_names[] = {
        ORTE_ALL_INIT_TRIGGER,
        ORTE_ALL_LAUNCHED_TRIGGER,
        ORTE_ALL_RUNNING_TRIGGER,
        ORTE_STG1_TRIGGER,
        ORTE_STG2_TRIGGER,
        ORTE_STG3_TRIGGER,
        ORTE_NUM_FINALIZED_TRIGGER,
        ORTE_NUM_TERMINATED_TRIGGER
    };
    orte_std_cntr_t num_counters = sizeof(keys)/sizeof(keys[0]);

    /* identify the segment for this job */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* setup the tokens */
    tokens[0]=ORTE_JOB_GLOBALS;
    tokens[1]=NULL;

    conditions = cb_conditions;
    for (i=0; i < num_counters; i++) {
        if (state[i] & conditions) {
            /** want this one - attach ourselves to the appropriate standard trigger */
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
            /* clear the spot in the conditions so we can check that all were found */
            conditions = conditions & ~(state[i]);
        }
    }
    free(segment);

    return ORTE_SUCCESS;
}


