/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "include/orte_constants.h"
#include "include/orte_types.h"

#include "dps/dps.h"
#include "mca/gpr/gpr.h"
#include "mca/errmgr/errmgr.h"
#include "mca/rml/rml.h"

#include "mca/rmgr/base/base.h"


int orte_rmgr_base_proc_stage_gate_init(orte_jobid_t job)
{
    int i, rc, num_counters=6;
    orte_gpr_value_t *values, value, trig, *trigs;
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
        value.keyvals[i]->type = ORTE_UINT32;
        value.keyvals[i]->value.i32 = 0;
    }
    values = &value;
    
    /* put the counters on the registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &values))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&value);
        return rc;
    }
    OBJ_DESTRUCT(&value);
    
    /* for the trigger, we want the counter values returned to us
     * setup subscriptions for that purpose. we'll enter the precise data
     * keys when we are ready to register the subscription - for now,
     * do all the basic stuff
     */
    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    sub.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(sub.segment), job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&sub);
        return rc;
    }
    sub.tokens = (char**)malloc(sizeof(char*));
    if (NULL == sub.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* the counters are in the job's globals container */
    sub.num_tokens = 1;
    sub.keys = (char**)malloc(sizeof(char*)*3);
    if (NULL == sub.keys) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.num_keys = 2;
    sub.keys[1] = strdup(ORTE_JOB_SLOTS_KEY);

    sub.cbfunc = orte_rmgr_base_proc_stage_gate_mgr;
    sub.user_tag = NULL;
    
    /* setup the trigger information - initialize the common elements */
    OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
    trig.addr_mode = ORTE_GPR_TOKENS_XAND;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(trig.segment), job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&trig);
        return rc;
    }
    trig.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trig.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.tokens[0] = strdup(ORTE_JOB_GLOBALS);
    trig.num_tokens = 1;
    trig.cnt = 2;
    trig.keyvals = (orte_gpr_keyval_t**)malloc(2*sizeof(orte_gpr_keyval_t*));
    if (NULL == trig.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Now do the individual triggers.
     * First, setup the triggers for the three main stage gates - these all compare
     * their value to that in ORTE_JOB_SLOTS_KEY
     */
    trig.keyvals[0]->key = strdup(ORTE_JOB_SLOTS_KEY);
    if (NULL == trig.keyvals[0]->key) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    trig.keyvals[0]->type = ORTE_NULL;
    trig.keyvals[1]->type = ORTE_NULL;
    
    /* do the three stage gate subscriptions.
     */
    for (i=0; i < 3; i++) {
        sub.keys[0] = strdup(keys[i]);
        if (NULL == sub.keys[0]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&sub);
            OBJ_DESTRUCT(&trig);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        trig.keyvals[1]->key = strdup(keys[i]);
        if (NULL == trig.keyvals[1]->key) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&value);
            OBJ_DESTRUCT(&trig);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        subs = &sub;
        trigs = &trig;
        rc = orte_gpr.subscribe(
             ORTE_GPR_TRIG_ALL_CMP,
             1, &subs,
             1, &trigs,
             &rc);
    
         if(ORTE_SUCCESS != rc) {
             ORTE_ERROR_LOG(rc);
             OBJ_DESTRUCT(&sub);
             OBJ_DESTRUCT(&trig);
             return rc;
         }
         free(sub.keys[0]);
         sub.keys[0] = NULL;
         free(trig.keyvals[1]->key);
         trig.keyvals[1]->key = NULL;
    }
    
    /* Next, setup the trigger that watches the NUM_ABORTED counter to see if
     * any process abnormally terminates - if so, then call the stage_gate_mgr_abort
     * so it can in turn order the job to be aborted
     */
    sub.cbfunc = orte_rmgr_base_proc_stage_gate_mgr_abort;
    sub.keys[0] = strdup(ORTE_PROC_NUM_ABORTED);
    if (NULL == sub.keys[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    OBJ_RELEASE(trig.keyvals[0]);
    OBJ_RELEASE(trig.keyvals[1]);
    free(trig.keyvals);
    trig.cnt = 1;
    trig.keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t**));
    if (NULL == trig.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0]->key = strdup(ORTE_PROC_NUM_ABORTED);
    if (NULL == trig.keyvals[0]->key) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&value);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0]->type = ORTE_INT32;
    trig.keyvals[0]->value.i32 = 1;  /* trigger on the first process that aborts */
    
    subs = &sub;
    trigs = &trig;
    rc = orte_gpr.subscribe(
         ORTE_GPR_TRIG_ALL_AT,
         1, &subs,
         1, &trigs,
         &rc);

     if (ORTE_SUCCESS != rc) {
         ORTE_ERROR_LOG(rc);
         OBJ_DESTRUCT(&sub);
         OBJ_DESTRUCT(&trig);
         return rc;
     }

    OBJ_DESTRUCT(&sub);
    OBJ_DESTRUCT(&trig);
    return ORTE_SUCCESS;
}


void orte_rmgr_base_proc_stage_gate_mgr(orte_gpr_notify_data_t *data,
                                        void *user_tag)
{
    orte_gpr_value_t **values;
    orte_gpr_keyval_t **kvals;
    orte_process_name_t *recipients;
    int i, j, n, k, rc;
    orte_buffer_t msg;
    orte_jobid_t job;
    
    /* get the jobid from the segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_segment_name(&job, data->segment))) {
        ORTE_ERROR_LOG(rc);
        return;
    }

    /* value returned will contain the counter, which contains the number of
     * procs in this job
     */
    values = data->values;
    n = -1; k = 0;
    for (i=0; i < data->cnt && (0 > n || 0 > k); i++) {
        kvals = values[i]->keyvals;
        /* check to see if ORTE_JOB_GLOBALS is the token */
        if (NULL != values[i]->tokens &&
            0 == strcmp(ORTE_JOB_GLOBALS, values[i]->tokens[0])) {
            /* find the ORTE_JOB_SLOTS_KEY and the ORTE_JOB_VPID_START_KEY keyval */
            for (j=0; j < values[i]->cnt && (0 > n); j++) {
                if (NULL != kvals[j] && 0 > n &&
                    0 == strcmp(ORTE_JOB_SLOTS_KEY, kvals[j]->key)) {
                    n = (int)(kvals[j]->value.ui32);
                }
                if (NULL != kvals[j] && 0 > k &&
                    0 == strcmp(ORTE_JOB_VPID_START_KEY, kvals[j]->key)) {
                    k = (int)(kvals[j]->value.ui32);
                }
            }
        }
    }
    
    if (0 > n) {
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
     * by sending an xcast to all processes
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
    orte_jobid_t job;
    int rc;
    
     /* get the jobid from the segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.extract_jobid_from_segment_name(&job, data->segment))) {
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
    int i, rc;
    orte_gpr_value_t trig, *trigs;
    orte_gpr_subscription_t sub, *subs;
    char* keys[] = {
        /* changes to this ordering need to be reflected in code below */
        ORTE_PROC_NUM_AT_STG1,
        ORTE_PROC_NUM_AT_STG2,
        ORTE_PROC_NUM_AT_STG3,
        ORTE_PROC_NUM_FINALIZED,
        ORTE_PROC_NUM_TERMINATED
    };
    int num_counters = sizeof(keys)/sizeof(keys[0]);

    /* for the trigger, we want the counter values returned to us
     * setup subscriptions for that purpose. we'll enter the precise data
     * keys when we are ready to register the subscription - for now,
     * do all the basic stuff
     */
    OBJ_CONSTRUCT(&sub, orte_gpr_subscription_t);
    sub.addr_mode = ORTE_GPR_TOKENS_XAND | ORTE_GPR_KEYS_OR;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(sub.segment), job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&sub);
        return rc;
    }
    sub.tokens = (char**)malloc(sizeof(char*));
    if (NULL == sub.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.tokens[0] = strdup(ORTE_JOB_GLOBALS); /* the counters are in the job's globals container */
    sub.num_tokens = 1;
    sub.keys = (char**)malloc(sizeof(char*)*1);
    if (NULL == sub.keys) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&sub);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    sub.num_keys = 1;
    sub.cbfunc = cbfunc;
    sub.user_tag = cbdata;
    
    /* setup the trigger information - initialize the common elements */
    OBJ_CONSTRUCT(&trig, orte_gpr_value_t);
    trig.addr_mode = ORTE_GPR_TOKENS_XAND;
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(trig.segment), job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_DESTRUCT(&trig);
        return rc;
    }
    trig.tokens = (char**)malloc(sizeof(char*));
    if (NULL == trig.tokens) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.tokens[0] = strdup(ORTE_JOB_GLOBALS);
    trig.num_tokens = 1;
    trig.cnt = 2;
    trig.keyvals = (orte_gpr_keyval_t**)malloc(2*sizeof(orte_gpr_keyval_t*));
    if (NULL == trig.keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    trig.keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == trig.keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    /* Now do the individual triggers.
     * First, setup the triggers for the three main stage gates - these all compare
     * their value to that in ORTE_JOB_SLOTS_KEY
     */
    trig.keyvals[0]->key = strdup(ORTE_JOB_SLOTS_KEY);
    if (NULL == trig.keyvals[0]->key) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_DESTRUCT(&trig);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    trig.keyvals[0]->type = ORTE_NULL;
    trig.keyvals[1]->type = ORTE_NULL;
    
    /* do the counter subscriptions. */
    for (i=0; i < num_counters; i++) {
        sub.keys[0] = strdup(keys[i]);
        if (NULL == sub.keys[0]) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&sub);
            OBJ_DESTRUCT(&trig);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        trig.keyvals[1]->key = strdup(keys[i]);
        if (NULL == trig.keyvals[1]->key) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            OBJ_DESTRUCT(&sub);
            OBJ_DESTRUCT(&trig);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        subs = &sub;
        trigs = &trig;
        rc = orte_gpr.subscribe(
             ORTE_GPR_TRIG_ALL_CMP,
             1, &subs,
             1, &trigs,
             &rc);
    
         if(ORTE_SUCCESS != rc) {
             ORTE_ERROR_LOG(rc);
             OBJ_DESTRUCT(&sub);
             OBJ_DESTRUCT(&trig);
             return rc;
         }
         free(sub.keys[0]);
         sub.keys[0] = NULL;
         free(trig.keyvals[1]->key);
         trig.keyvals[1]->key = NULL;
    }
    OBJ_DESTRUCT(&sub);
    OBJ_DESTRUCT(&trig);
    return ORTE_SUCCESS;
}


