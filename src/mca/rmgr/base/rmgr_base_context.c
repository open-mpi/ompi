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
#include "ompi_config.h"
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "include/constants.h"
#include "mca/rmgr/base/base.h"
#include "mca/rds/base/base.h"
#include "mca/ras/base/base.h"
#include "mca/rmaps/base/base.h"
#include "mca/pls/base/base.h"
#include "mca/gpr/gpr.h"
#include "mca/ns/ns.h"
#include "mca/errmgr/errmgr.h"


/*
 *  Create the job segment and initialize the application context.
 */

int orte_rmgr_base_put_app_context(
    orte_jobid_t jobid,
    orte_app_context_t** app_context,
    size_t num_context)
{
    orte_gpr_value_t* value;
    size_t i;
    size_t job_slots;
    int rc;

    rc = orte_rmgr_base_get_job_slots(jobid, &job_slots);
    if(ORTE_SUCCESS != rc) {
        return rc;
    }

    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->addr_mode = ORTE_GPR_OVERWRITE;
    
    /* put context info on the job segment of the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value->segment), jobid))) {
        OBJ_RELEASE(value);
        return rc;
    }
    
    value->num_tokens = 1;
    if(NULL == (value->tokens = (char**)malloc(sizeof(char*)*2))) {
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    value->tokens[1] = NULL;

    value->cnt = num_context;
    value->keyvals = (orte_gpr_keyval_t**)malloc(num_context * sizeof(orte_gpr_keyval_t*));
    if(NULL == value->keyvals) {
        OBJ_RELEASE(value);
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return OMPI_ERR_OUT_OF_RESOURCE;
    }
    memset(value->keyvals, 0, num_context * sizeof(orte_gpr_keyval_t*));

    for(i=0; i<num_context; i++) {
        orte_app_context_t* app = app_context[i];
        value->keyvals[i] = OBJ_NEW(orte_gpr_keyval_t);
        if (NULL == value->keyvals[i]) {
            rc = ORTE_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        (value->keyvals[i])->key = strdup(ORTE_JOB_APP_CONTEXT_KEY);
        (value->keyvals[i])->type = ORTE_APP_CONTEXT;
        (value->keyvals[i])->value.app_context = app;
        app->idx = i;
        job_slots += app->num_procs;
    }
            
    rc = orte_gpr.put(1, &value);
    if(ORTE_SUCCESS != rc) {
        goto cleanup;
    }
    rc = orte_rmgr_base_set_job_slots(jobid, job_slots);

cleanup:
    OBJ_RELEASE(value);
    return rc;
}


/*
 * Comparison function for sorting context by index.
 */

static int orte_rmgr_base_cmp_app_context(
    orte_app_context_t** app1,
    orte_app_context_t** app2)
{
    if ((*app1)->idx < (*app2)->idx) {
        return -1;
    } else if((*app1)->idx > (*app2)->idx) {
        return 1;
    }
    return 0;
}


/*
 *  Retreive the application context
 */

int orte_rmgr_base_get_app_context(
    orte_jobid_t jobid,
    orte_app_context_t*** app_context,
    size_t* num_context)
{
    char *segment;
    char *tokens[2];
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    int i, num_values = 0, index = 0;
    int rc;

    /* create the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid)))
        return rc;

    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    keys[0] = ORTE_JOB_APP_CONTEXT_KEY;
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_TOKENS_OR,
        segment,
        tokens,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS)
        goto cleanup;

    *num_context = 0;
    for(i=0; i<num_values; i++) {
        *num_context += values[i]->cnt;
    }

    *app_context = (orte_app_context_t**)malloc(*num_context * sizeof(orte_app_context_t*));
    for(i=0; i<num_values; i++) {
        orte_gpr_value_t* value = values[i];
        orte_gpr_keyval_t** keyvals = value->keyvals;
        int k;
        for(k=0; k < value->cnt; k++) {
            orte_gpr_keyval_t* keyval = keyvals[k];
            (*app_context)[index++] = keyval->value.app_context;
            keyval->value.app_context = NULL;
        }
    }
    qsort(*app_context, *num_context, sizeof(orte_app_context_t*), 
        (int (*)(const void*,const void*))orte_rmgr_base_cmp_app_context);

cleanup:
    for(i=0; i<num_values; i++) {
        OBJ_RELEASE(values[i]);
    }
    if(NULL != values) 
        free(values);
    free(segment);
    return rc;
}


/*
 * Query for the total number of process slots requested for the job.
 */

int orte_rmgr_base_get_job_slots(orte_jobid_t jobid, size_t* proc_slots)
{
    char *segment;
    char *tokens[2];
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    int i, num_values = 0;
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid)))
        return rc;

    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    keys[0] = ORTE_JOB_SLOTS_KEY;
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        segment,
        tokens,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(segment);
        return rc;
    }

    if(0 == num_values) {
        *proc_slots = 0;
        return ORTE_SUCCESS;
    }
    if(1 != num_values || values[0]->cnt != 1) {
        return ORTE_ERR_NOT_FOUND;
    }
    *proc_slots = values[0]->keyvals[0]->value.ui32;

    for(i=0; i<num_values; i++)
        OBJ_RELEASE(values[i]);
    free(segment);
    free(values);
    return ORTE_SUCCESS;
}


/*
 * Set the total number of process slots requested for the job.
 */

int orte_rmgr_base_set_job_slots(orte_jobid_t jobid, size_t proc_slots)
{
    char *segment;
    char *tokens[2];
    orte_gpr_value_t* values[1];
    orte_gpr_value_t value;
    orte_gpr_keyval_t keyval;
    orte_gpr_keyval_t* keyvals[1];
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid)))
        return rc;

    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;

    keyval.type = ORTE_UINT32;
    keyval.key = ORTE_JOB_SLOTS_KEY;
    keyval.value.ui32 = proc_slots;
    keyvals[0] = &keyval;

    value.addr_mode = ORTE_GPR_OVERWRITE;
    value.segment = segment;
    value.keyvals = keyvals;
    value.cnt = 1;
    value.tokens = tokens;
    value.num_tokens = 1;
    values[0] = &value;

    rc = orte_gpr.put(1, values);
    free(segment);
    return rc;
}


