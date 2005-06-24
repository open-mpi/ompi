/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "util/output.h"

#include "dps/dps.h"
#include "mca/gpr/gpr.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"

#include "mca/rmgr/base/base.h"


int orte_rmgr_base_proc_stage_gate_init(orte_jobid_t job)
{
    size_t i, num_counters=6;
    int rc;
    orte_gpr_value_t *values, value, trigvalue, *trigvals;
    orte_gpr_trigger_t trig, *trigs;
    orte_gpr_subscription_t sub, *subs;
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED,
        ORTE_PROC_NUM_ABORTED,
        ORTE_PROC_NUM_TERMINATED
    };
    char* trig_names[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_STG1_TRIGGER,
        ORTE_STG2_TRIGGER,
        ORTE_STG3_TRIGGER
    };

    /* setup the counters */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    value.addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value.segment), job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* put counters in the job's globals container */
    value.num_tokens = 1;
    value.cnt = num_counters;
    value.keyvals = (orte_gpr_keyval_t**)malloc(num_counters * sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < num_counters; i++) {
        value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == value.keyvals[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&value);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        value.keyvals[i]->key = strdup(keys[i]);
        value.keyvals[i]->type = ORTE_SIZE;
        value.keyvals[i]->value.size = 0;
    }
    values = &value;
    
    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value);
    
    /* for the stage gate triggers, we want the counter values returned to us AND
     * information on VPID_START so we can generate the list of peers
     * to receive the xcast messages for barrier release.
     */
     
    /*** SUBSCRIPTIONS ***/
    /* the subscription object is used to define the values we want
     * returned to us. we'll enter the precise data
     * keys when we are ready to register the subscription - for now,
     * do all the basic stuff
     */
    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    /* we do not name the subscription - see explanation below. also, we do
     * not assign the subscription id here - it is assigned for us when the
     * registry "registers" the subscription and is returned in the
     * subscription object at that time
     */
    /*
     * set the action to delete the subscription after the trigger fires. this
     * subscription is solely for the purpose of returning stagegate information
     * to the resource manager - we don't need it after that happens
     */
    sub.action = ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG;
    /*
     * setup the value object to define the data to be returned to us
     */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    values = &value;
    sub.values = &values;
    sub.cnt = 1;
    
    /* set the address mode to identify a specific container (in this case,
     * the ORTE_JOB_GLOBALS container) and any keys within it
     */
    value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value.segment), job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        sub.values = NULL;
        OBJ_DESTRUCT(&sub);
        return rc;
    }
    /* define the tokens for the container */
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        sub.values = NULL;
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* the counters are in the job's globals container */
    value.num_tokens = 1;
    /* define the keys to be returned */
    value.cnt = 3;
    value.keyvals = (orte_gpr_keyval_t**)malloc(value.cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        sub.values = NULL;
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    for (i=0; i < value.cnt; i++) {
        value.keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == value.keyvals[i]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&value);
            sub.values = NULL;
            OBJ_DESTRUCT(&sub);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }
    /* the 0th entry will be defined below */
    value.keyvals[1]->key = strdup(ORTE_JOB_SLOTS_KEY);
    value.keyvals[2]->key = strdup(ORTE_JOB_VPID_START_KEY);
    /* we don't need to define the type and value for the keyvals - the subscribe
     * function ignores those fields
     */
     
    sub.cbfunc = orte_rmgr_base_proc_stage_gate_mgr;
    sub.user_tag = NULL;
    
    /*** TRIGGERS ***/
    /* setup the trigger information - initialize the common elements */
    OBJ_CONSTRUCT(&trig, orte_gpr_trigger_t);
    /* we WILL name the trig - see explanation below. we do
     * NOT assign the trigger id here - it is assigned for us when the
     * registry "registers" the trigger and is returned in the
     * trigger object at that time
     */
    /*
     * set the action to compare all specified counter levels. this will
     * "fire" the trigger when all counters are equal
     */
    trig.action = ORTE_GPR_TRIG_ALL_CMP;
    /*
     * setup the value object to define the data to be returned to us
     */
    OBJ_CONSTRUCT(&trigvalue, orte_gpr_value_t);
    trigvals = &trigvalue;
    trig.values = &trigvals;
    trig.cnt = 1;
    
    /* set the address mode to identify a specific container (in this case,
     * the ORTE_JOB_GLOBALS container) and any keys within it
     */
    trigvalue.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(trigvalue.segment), job))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* define the tokens for the container */
    trigvalue.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trigvalue.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    trigvalue.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* the counters are in the job's globals container */
    trigvalue.num_tokens = 1;
    /* define the keys that identify the counters */
    trigvalue.cnt = 2;
    trigvalue.keyvals = (orte_gpr_keyval_t**)malloc(trigvalue.cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == trigvalue.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    trigvalue.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trigvalue.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    trigvalue.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trigvalue.keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* setup the triggers for the three main stage gates - these all compare
     * their value to that in ORTE_JOB_SLOTS_KEY
     */
    trigvalue.keyvals[0]->key = strdup(ORTE_JOB_SLOTS_KEY);
    if (NULL == trigvalue.keyvals[0]->key) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    
    /* we don't need to define the type and value for the keyvals - the subscribe
     * function ignores those fields
     */
     
     
    /* do the three stage gate subscriptions.
     */
    for (i=0; i < 3; i++) {
        /*
         * NOTE: we do NOT name the subscriptions here as these are not
         * standard subscriptions that multiple processes should attach
         * themselves to - the subscriptions only have meaning to the
         * resource manager
         */
        value.keyvals[0]->key = strdup(keys[i]);
        if (NULL == value.keyvals[0]->key) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        /*
         * NOTE: we DO name the triggers as these will be standard triggers
         * that multiple processes will want to attach themselves to - for
         * example, a process may well want to receive some information when
         * it reaches STAGE_GATE_1, and so will "attach" itself to that
         * trigger as defined by us here
         */
        if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&(trig.name),
                                        trig_names[i], job))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        trigvalue.keyvals[1]->key = strdup(keys[i]);
        if (NULL == trigvalue.keyvals[1]->key) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        subs = &sub;
        trigs = &trig;
        rc = orte_gpr.subscribe(
             1, &subs,
             1, &trigs);
    
         if (ORTE_SUCCESS != rc) {
             ORTE_ERROR_LOG(rc);
             goto CLEANUP;
         }
         free(value.keyvals[0]->key);
         value.keyvals[0]->key = NULL;
         free(trig.name);
         free(trigvalue.keyvals[1]->key);
         trigvalue.keyvals[1]->key = NULL;
    }
    
    /* Next, setup the trigger that watches the NUM_ABORTED counter to see if
     * any process abnormally terminates - if so, then call the
     * stage_gate_mgr_abort function
     * so it can in turn order the job to be aborted
     */
    sub.cbfunc = orte_rmgr_base_proc_stage_gate_mgr_abort;
    value.keyvals[0]->key = strdup(ORTE_PROC_NUM_ABORTED);
    if (NULL == value.keyvals[0]->key) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    
    /* set the trigger name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&(trig.name),
                                    ORTE_NUM_ABORTED_TRIGGER, job))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    /* set the trigger action to fire at a specified level */
    trig.action = ORTE_GPR_TRIG_ALL_AT;
    /* cleanup the trigger keyvals that are no longer needed - we will
     * rebuild them as required
     */
    OBJ_RELEASE(trigvalue.keyvals[0]);
    OBJ_RELEASE(trigvalue.keyvals[1]);
    free(trigvalue.keyvals);
    /* we only need one trigger keyval here as we are not comparing
     * trigger levels - we are just asking to be notified when
     * a specific counter changes value to "1"
     */
    trigvalue.cnt = 1;
    trigvalue.keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t**));
    if (NULL == trigvalue.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    trigvalue.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trigvalue.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    trigvalue.keyvals[0]->key = strdup(ORTE_PROC_NUM_ABORTED);
    if (NULL == trigvalue.keyvals[0]->key) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }
    /* trigger on the first process that aborts */
    trigvalue.keyvals[0]->type = ORTE_SIZE;
    trigvalue.keyvals[0]->value.size = 1;
    
    subs = &sub;
    trigs = &trig;
    rc = orte_gpr.subscribe(
         1, &subs,
         1, &trigs);

    if (ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

CLEANUP:
    OBJ_DESTRUCT(&trigvalue);
    trig.values = NULL;
    OBJ_DESTRUCT(&trig);
    OBJ_DESTRUCT(&value);
    sub.values = NULL;
    OBJ_DESTRUCT(&sub);
    
    return rc;
}


void orte_rmgr_base_proc_stage_gate_mgr(orte_gpr_notify_data_t *data,
                                        void *user_tag)
{
    orte_gpr_value_t **values;
    orte_gpr_keyval_t **kvals;
    orte_process_name_t *recipients;
    size_t i, j, n=0;
    orte_vpid_t k=0;
    int rc;
    bool found_slots=false, found_start=false;
    orte_buffer_t msg;
    orte_jobid_t job;
    
    /* get the jobid from the segment name
     * we setup the stage gate triggers to return at least one value
     * to us. we use that value to extract the jobid for the returned
     * data
     */
    values = data->values;
    if (ORTE_SUCCESS != (rc =
            orte_schema.extract_jobid_from_segment_name(&job,
                    values[0]->segment))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* value returned will contain the counter, which contains the number of
     * procs in this job
     */
    values = data->values;
    for (i=0; i < data->cnt && (!found_slots || !found_start); i++) {
        kvals = values[i]->keyvals;
        /* check to see if ORTE_JOB_GLOBALS is the token */
        if (NULL != values[i]->tokens &&
            0 == strcmp(ORTE_JOB_GLOBALS, values[i]->tokens[0])) {
            /* find the ORTE_JOB_SLOTS_KEY and the ORTE_JOB_VPID_START_KEY keyval */
            for (j=0; j < values[i]->cnt && (!found_slots || !found_start); j++) {
                if (NULL != kvals[j] && !found_slots &&
                    0 == strcmp(ORTE_JOB_SLOTS_KEY, kvals[j]->key)) {
                    n = kvals[j]->value.size;
                    found_slots = true;
                }
                if (NULL != kvals[j] && !found_start &&
                    0 == strcmp(ORTE_JOB_VPID_START_KEY, kvals[j]->key)) {
                    k = kvals[j]->value.vpid;
                    found_start = true;
                }
            }
        }
    }
    
    if (!found_slots) {
        ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
        return;
    }
    
    if (!found_start) {
        ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
        return;
    }
    /* now can generate the list of recipients */
    recipients = (orte_process_name_t*)malloc(n * sizeof(orte_process_name_t));
    for (i=0; i < n; i++) {
        recipients[i].cellid = 0;
        recipients[i].jobid = job;
        recipients[i].vpid = (orte_vpid_t)(k + i);
    }
    
    /* for the purposes of the stage gate manager, we don't actually have
     * to determine anything from the message. All we have to do is respond
     * by sending an xcast to all processes. However, the buffer has to include
     * at least one piece of data for the RML to function, so pack something
     * meaningless.
     */
    
    OBJ_CONSTRUCT(&msg, orte_buffer_t);
    if (ORTE_SUCCESS != (rc = orte_dps.pack(&msg, &job, 1, ORTE_JOBID))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&msg);
        return;
    }
    
    if (ORTE_SUCCESS != (rc = orte_rml.xcast(orte_process_info.my_name, recipients,
                                        n, &msg, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&msg);
        return;
    }
    OBJ_DESTRUCT(&msg);
    free(recipients);
}

void orte_rmgr_base_proc_stage_gate_mgr_abort(orte_gpr_notify_data_t *data,
                                        void *user_tag)
{
    orte_gpr_value_t **values;
    orte_jobid_t job;
    int rc;
    
    /* get the jobid from the segment name
     * we setup the stage gate triggers to return at least one value
     * to us. we use that value to extract the jobid for the returned
     * data
     */
    values = data->values;
    if (ORTE_SUCCESS != (rc =
            orte_schema.extract_jobid_from_segment_name(&job,
                    values[0]->segment))) {
        ORTE_ERROR_LOG(rc);
        return;
    }
    
    orte_errmgr.incomplete_start(job);
}


/*
 * Routine that subscribes to events on all counters.
 */

int orte_rmgr_base_proc_stage_gate_subscribe(orte_jobid_t job, orte_gpr_notify_cb_fn_t cbfunc, void* cbdata)
{
    size_t i;
    int rc;
    orte_gpr_value_t value, *values;
    orte_gpr_trigger_t trig, *trigs;
    orte_gpr_subscription_t sub, *subs;
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED,
        ORTE_PROC_NUM_TERMINATED
    };
    char* trig_names[] = {
        /* changes to this ordering need to be reflected in code below
         * number of entries MUST match those above
         */
        ORTE_STG1_TRIGGER,
        ORTE_STG2_TRIGGER,
        ORTE_STG3_TRIGGER,
        ORTE_NUM_FINALIZED_TRIGGER,
        ORTE_NUM_TERMINATED_TRIGGER
    };
    size_t num_counters = sizeof(keys)/sizeof(keys[0]);

    /*** SUBSCRIPTIONS ***/
    /* the subscription object is used to define the values we want
     * returned to us. we'll enter the precise data
     * keys when we are ready to register the subscription - for now,
     * do all the basic stuff
     */
    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    /* we do not name the subscription - see explanation below. also, we do
     * not assign the subscription id here - it is assigned for us when the
     * registry "registers" the subscription and is returned in the
     * subscription object at that time
     */
    /*
     * set the action to delete the subscription after the trigger fires. this
     * subscription is solely for the purpose of returning stagegate information
     * to the resource manager - we don't need it after that happens
     */
    sub.action = ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG;
    /*
     * setup the value object to define the data to be returned to us
     */
    OBJ_CONSTRUCT(&value, orte_gpr_value_t);
    values = &value;
    sub.values = &values;
    sub.cnt = 1;
    
    /* set the address mode to identify a specific container (in this case,
     * the ORTE_JOB_GLOBALS container) and any keys within it
     */
    value.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value.segment), job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        sub.values = NULL;
        OBJ_DESTRUCT(&sub);
        return rc;
    }
    /* define the tokens for the container */
    value.tokens = (char**)malloc(sizeof(char*));
    if (NULL == value.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        sub.values = NULL;
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* the counters are in the job's globals container */
    value.num_tokens = 1;
    /* the keys describing the data to be returned will be defined later
     * for now, we simply allocate the space
     */
    value.keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    if (NULL == value.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        sub.values = NULL;
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        sub.values = NULL;
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value.cnt = 1;
    /* define the callback and associated data tag */
    sub.cbfunc = cbfunc;
    sub.user_tag = cbdata;
    
    /*** TRIGGERS ***/
    /* setup the trigger information - initialize the common elements */
    OBJ_CONSTRUCT(&trig, orte_gpr_trigger_t);
    /* since the named triggers have already been defined, we don't need
     * to replicate that here! all we need to do is refer to the
     * proper trigger name - we'll do that below
     */
    trig.action = ORTE_GPR_TRIG_ALL_CMP;

    /* do the trigger subscriptions */
    for (i=0; i < num_counters; i++) {
        /* insert the subscription key identifying the data to
         * be returned from this trigger
         */
        value.keyvals[0]->key = strdup(keys[i]);
        if (NULL == value.keyvals[0]->key) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto CLEANUP;
        }
        /* get the standard trigger name to which we are "attaching" */
        if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&(trig.name),
                                        trig_names[i], job))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        subs = &sub;
        trigs = &trig;
        rc = orte_gpr.subscribe(
             1, &subs,
             1, &trigs);
    
         if(ORTE_SUCCESS != rc) {
             ORTE_ERROR_LOG(rc);
             goto CLEANUP;
         }
         free(value.keyvals[0]->key);
         value.keyvals[0]->key = NULL;
         free(trig.name);
         trig.name = NULL;
    }
    
    /* Now do the abort trigger.
     * setup the subscription to return the number aborted\
     */
    value.keyvals[0]->key = strdup(ORTE_PROC_NUM_ABORTED);
    if (NULL == value.keyvals[0]->key) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        rc = ORTE_ERR_OUT_OF_RESOURCE;
        goto CLEANUP;
    }

    /* set the trigger action */
    trig.action = ORTE_GPR_TRIG_ALL_AT;
    /* get the standard "abort" trigger name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&(trig.name),
                                    ORTE_NUM_ABORTED_TRIGGER, job))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    subs = &sub;
    trigs = &trig;
    rc = orte_gpr.subscribe(
         1, &subs,
         1, &trigs);

     if (ORTE_SUCCESS != rc) {
         ORTE_ERROR_LOG(rc);
     }
     
CLEANUP:
    OBJ_DESTRUCT(&trig);
    OBJ_DESTRUCT(&value);
    sub.values = NULL;
    OBJ_DESTRUCT(&sub);
    
    return rc;
}


