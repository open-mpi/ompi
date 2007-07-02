/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
 *
 * Support functions for the RMGR subsystem
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>

#include "opal/util/output.h"
#include "opal/class/opal_list.h"

#include "orte/dss/dss.h"
#include "orte/mca/schema/schema.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmgr/base/rmgr_private.h"

int orte_rmgr_base_set_proc_info(const orte_process_name_t* name, pid_t pid, char *nodename) 
{
    orte_gpr_value_t *values[1];
    char *segment;
    int rc = ORTE_SUCCESS;

    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, name->jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if( NULL != nodename ) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&values[0],
                                                        ORTE_GPR_OVERWRITE,
                                                        segment,
                                                        2, 0))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(segment);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }
    else {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&values[0],
                                                        ORTE_GPR_OVERWRITE,
                                                        segment,
                                                        1, 0))) {
            ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
            free(segment);
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
    }

    if(ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(values[0]->tokens), &(values[0]->num_tokens), (orte_process_name_t*)name))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(segment);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[0]), ORTE_PROC_LOCAL_PID_KEY, ORTE_PID, &pid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(segment);
        return rc;
    }

    if( NULL != nodename ) {
        if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[1]), ORTE_NODE_NAME_KEY, ORTE_STRING, nodename))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(values[0]);
            free(nodename);
            return rc;
        }
    }

    rc = orte_gpr.put(1, values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(segment);
        return rc;
    }

    OBJ_RELEASE(values[0]);
    free(segment);

    return rc;
}

int orte_rmgr_base_get_proc_info(const orte_process_name_t* name, pid_t* pid, char **nodename)
{

    char **tokens;
    orte_std_cntr_t num_tokens;
    char *keys[3];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0;
    pid_t *pptr;
    char **nptr;
    int rc;
    char *segment;
    orte_std_cntr_t k;

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

    keys[0] = ORTE_PROC_LOCAL_PID_KEY;
    keys[1] = ORTE_NODE_NAME_KEY;
    keys[2] = NULL;

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
    for( k = 0; k < values[0]->cnt; ++k) {
        orte_gpr_keyval_t* keyval = values[0]->keyvals[k];
        if( 0 == strncmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY, strlen(ORTE_PROC_LOCAL_PID_KEY) ) ) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&pptr, values[0]->keyvals[0]->value, ORTE_PID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            *pid = *pptr;
        }
        else if( 0 == strncmp(keyval->key, ORTE_NODE_NAME_KEY, strlen(ORTE_NODE_NAME_KEY) ) ) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&nptr, values[0]->keyvals[1]->value, ORTE_STRING))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            *nodename = strdup(*nptr);
        }
    }

cleanup:
    if(NULL != values) {
        for(i=0; i<num_values; i++) {
            if(NULL != values[i]) {
                OBJ_RELEASE(values[i]);
            }
        }
        if (NULL != values) free(values);
    }
    free(segment);

    return rc;
}

int orte_rmgr_base_set_universe_size(orte_jobid_t job, orte_std_cntr_t univ_size)
{
    orte_gpr_value_t *value;
    char *segment;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                                    segment, 1, 1))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    free(segment);
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]),
                                                     ORTE_JOB_TOTAL_SLOTS_ALLOC_KEY, ORTE_STD_CNTR, &univ_size))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    
    /* put the value in the JOB_GLOBALS container */
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    
    /* do the insert */
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(value);
    
    return rc;
}

int orte_rmgr_base_get_universe_size(orte_jobid_t job, orte_std_cntr_t *univ_size)
{
    char *segment;
    orte_gpr_value_t **values;
    orte_std_cntr_t i, cnt, *sptr;
    char *glob_tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    char *glob_keys[] = {
        ORTE_JOB_TOTAL_SLOTS_ALLOC_KEY,
        NULL
    };
    int rc;
    
    /* set default error response */
    *univ_size = -1;
    
    /* get the segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the info from the job globals container first */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                           segment, glob_tokens, glob_keys, &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    free(segment);
    
    /* it is okay for this value not to be defined yet */
    if (0 == cnt) {
        return ORTE_SUCCESS;
    }
    
    /* can only be one value */
    if (1 != cnt || values[0]->cnt != 1) {
        ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
        rc = ORTE_ERR_GPR_DATA_CORRUPT;
        goto cleanup;
    }
    
    if (strcmp(values[0]->keyvals[0]->key, ORTE_JOB_TOTAL_SLOTS_ALLOC_KEY) == 0) {
        /* this can only occur once, so just store it */
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&sptr, values[0]->keyvals[0]->value, ORTE_STD_CNTR))) {
            ORTE_ERROR_LOG(rc);
            goto cleanup;
        }
        *univ_size = *sptr;
        rc = ORTE_SUCCESS;
        goto cleanup;
    }
    
cleanup:
    for (i=0; i < cnt; i++) {
        OBJ_RELEASE(values[i]);
    }
    if (NULL != values) free(values);
    
    return rc;
}

