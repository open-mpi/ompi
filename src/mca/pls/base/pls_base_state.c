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


#include "orte_config.h"
#include "include/orte_constants.h"
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pls/base/base.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"
#include "mca/soh/soh_types.h"
#include "mca/errmgr/errmgr.h"


/**
 * Set the process pid in the job segment and indicate the state
 * as being launched.
 */

int orte_pls_base_set_proc_pid(const orte_process_name_t* name, pid_t pid)
{
    orte_gpr_value_t* values[1];
    orte_gpr_value_t value;
    orte_gpr_keyval_t kv_pid = {{OBJ_CLASS(orte_gpr_keyval_t),0},ORTE_PROC_PID_KEY,ORTE_UINT32};
    orte_gpr_keyval_t kv_state = {{OBJ_CLASS(orte_gpr_keyval_t),0},ORTE_PROC_STATE_KEY,ORTE_PROC_STATE};
    orte_gpr_keyval_t* keyvals[2];
    int i, rc;

    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&value.segment, name->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if(ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&value.tokens, &value.num_tokens, (orte_process_name_t*)name))) {
        ORTE_ERROR_LOG(rc);
        free(value.segment);
        return rc;
    }

    kv_pid.value.ui32 = pid;
    kv_state.value.proc_state = ORTE_PROC_STATE_LAUNCHED;
    keyvals[0] = &kv_pid;
    keyvals[1] = &kv_state;
    
    value.keyvals = keyvals;
    value.cnt = 2;
    value.addr_mode = ORTE_GPR_OVERWRITE;
    values[0] = &value;
    
    rc = orte_gpr.put(1, values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

    free(value.segment);
    for(i=0; i<value.num_tokens; i++)
        free(value.tokens[i]);
    free(value.tokens);
    return rc;
}

/**
 *  Retreive a specified process pid from the registry.
 */
int orte_pls_base_get_proc_pid(const orte_process_name_t* name, pid_t* pid)
{
    char *segment;
    char **tokens;
    int num_tokens;
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    int i, num_values = 0;
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, name->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if(ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens, (orte_process_name_t*)name))) {
        free(segment);
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    keys[0] = ORTE_PROC_PID_KEY;
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
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    if(1 != num_values || values[0]->cnt != 1) {
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    *pid = values[0]->keyvals[0]->value.ui32;

cleanup:
    if(NULL != values) {
        for(i=0; i<num_values; i++) {
            if(NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        free(values);
    }
    free(segment);
    return rc;
}

/**
 *  Retreive all process pids for the specified job.
 */
int orte_pls_base_get_proc_pids(orte_jobid_t jobid, pid_t **pids, size_t* num_pids)
{
    char *segment;
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    int i, num_values = 0;
    int rc;

    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    keys[0] = ORTE_PROC_PID_KEY;
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        segment,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(segment);
        return rc;
    }

    if(0 == num_values) {
        *pids = NULL;
    } else {
        *pids = (pid_t*)malloc(sizeof(pid_t)*num_values);
        for(i=0; i<num_values; i++) {
            (*pids)[i] = values[i]->keyvals[0]->value.ui32;
        }
    } 
    *num_pids = num_values;

    if(NULL != values) {
        for(i=0; i<num_values; i++) {
            if(NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        free(values);
    }
    free(segment);
    return rc;
}


/**
 * Add a key-value to the node segment containing the process pid for
 * the daemons.
 */

int orte_pls_base_set_node_pid(orte_cellid_t cellid, char* node_name, orte_jobid_t jobid, pid_t pid)
{
    orte_gpr_value_t* values[1];
    orte_gpr_value_t value;
    orte_gpr_keyval_t kv_pid = {{OBJ_CLASS(orte_gpr_keyval_t),0},ORTE_PROC_PID_KEY,ORTE_UINT32};
    orte_gpr_keyval_t* keyvals[1];
    char* jobid_string;
    int i, rc;

    if(ORTE_SUCCESS != (rc = orte_schema.get_node_tokens(&value.tokens, &value.num_tokens, cellid, node_name)))
        return rc;

    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid)))
        goto cleanup;

    asprintf(&kv_pid.key, "%s-%s", ORTE_PROC_PID_KEY, jobid_string);
    free(jobid_string);

    kv_pid.value.ui32 = pid;
    keyvals[0] = &kv_pid;
    
    value.segment = ORTE_NODE_SEGMENT;
    value.keyvals = keyvals;
    value.cnt = 1;
    value.addr_mode = ORTE_GPR_OVERWRITE;
    values[0] = &value;
    
    rc = orte_gpr.put(1, values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    free(kv_pid.key);
    for(i=0; i<value.num_tokens; i++)
        free(value.tokens[i]);
    free(value.tokens);
    return rc;
}


/**
 *  Retreive all daemon pids for the specified job.
 */
int orte_pls_base_get_node_pids(orte_jobid_t jobid, pid_t **pids, size_t* num_pids)
{
    char *keys[2];
    orte_gpr_value_t** values = NULL;
    int i, num_values = 0;
    int rc;
    char *jobid_string;

    if(ORTE_SUCCESS != (rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid)))
        goto cleanup;

    asprintf(&keys[0], "%s-%s", ORTE_PROC_PID_KEY, jobid_string);
    free(jobid_string);
    keys[1] = NULL;

    rc = orte_gpr.get(
        ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_OR,
        ORTE_NODE_SEGMENT,
        NULL,
        keys,
        &num_values,
        &values
        );
    if(rc != ORTE_SUCCESS) {
        free(keys[0]);
        return rc;
    }

    if(0 == num_values) {
        *pids = NULL;
    } else {
        *pids = (pid_t*)malloc(sizeof(pid_t)*num_values);
        for(i=0; i<num_values; i++) {
            (*pids)[i] = values[i]->keyvals[0]->value.ui32;
        }
    }
    *num_pids = num_values;

cleanup:
    if(NULL != values) {
        for(i=0; i<num_values; i++)
            OBJ_RELEASE(values[i]);
        free(values);
    }
    free(keys[0]);
    return rc;
}


