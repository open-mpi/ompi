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

#include "include/orte_schema.h"

#include "mca/errmgr/errmgr.h"
#include "mca/gpr/gpr.h"
#include "mca/ns/ns.h"

#include "mca/soh/base/base.h"

int orte_soh_base_set_proc_soh(orte_process_name_t *proc,
                               orte_proc_state_t state,
                               int exit_status)
{
    orte_gpr_value_t *value;
    int rc;
    orte_jobid_t jobid;
    orte_vpid_t vpid;

    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    value->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND;
    
    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, proc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value->segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid(&vpid, proc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_VPID_MAX == vpid) {  /* wildcard case - set everyone on job segment to this status */
        value->tokens = NULL;
    } else {
        if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(value->tokens),
                                                        &(value->num_tokens), proc))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    value->cnt = 2;
    value->keyvals = (orte_gpr_keyval_t**)malloc(2 * sizeof(orte_gpr_keyval_t*));
    if (NULL == value->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (value->keyvals[0])->key = strdup(ORTE_PROC_STATE_KEY);
    (value->keyvals[0])->type = ORTE_PROC_STATE;
    (value->keyvals[0])->value.proc_state = state;
    
    value->keyvals[1] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (value->keyvals[1])->key = strdup(ORTE_PROC_EXIT_CODE_KEY);
    (value->keyvals[1])->type = ORTE_EXIT_CODE;
    (value->keyvals[1])->value.exit_code = exit_status;

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(value);

    /* check to see if we need to increment orte-standard counters */
    /* first, cleanup value so it can be used for that purpose */

    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->addr_mode = ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_AND;
    
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&(value->segment), jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(NULL == (value->tokens = (char**)malloc(sizeof(char*)))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    value->num_tokens = 1;
    
    value->keyvals = (orte_gpr_keyval_t**)malloc(sizeof(orte_gpr_keyval_t*));
    if (NULL == value->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->cnt = 1;
    value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    (value->keyvals[0])->type = ORTE_NULL;
    
    /* see which state we are in - let that determine the counter, if any */
    switch (state) {
        case ORTE_PROC_STATE_AT_STG1:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_AT_STG1);
            break;
            
        case ORTE_PROC_STATE_AT_STG2:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_AT_STG2);
            break;
            
        case ORTE_PROC_STATE_AT_STG3:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_AT_STG3);
            break;
            
        case ORTE_PROC_STATE_FINALIZED:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_FINALIZED);
            break;
            
        case ORTE_PROC_STATE_TERMINATED:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_TERMINATED);
            break;
            
        case ORTE_PROC_STATE_ABORTED:
            (value->keyvals[0])->key = strdup(ORTE_PROC_NUM_ABORTED);
            break;
    }
    if (NULL != (value->keyvals[0])->key) { /* need to increment a counter */
        if (ORTE_SUCCESS != (rc = orte_gpr.increment_value(value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            return rc;
        }
    }
    
    /* all done */
    OBJ_RELEASE(value);
    
    return rc;
}
