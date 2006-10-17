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

/**
 *  Set the vpid start and range for a job/pset on the registry
 */

int orte_rmgr_base_set_vpid_range(orte_jobid_t jobid, orte_vpid_t start, orte_vpid_t range)
{
    orte_gpr_value_t *value;
    char *segment;
    int rc;
    
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE, segment, 2, 1))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    free(segment);
    value->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_JOB_VPID_START_KEY, ORTE_VPID, &start))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_JOB_VPID_RANGE_KEY, ORTE_VPID, &range))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }
    
    rc = orte_gpr.put(1, &value);
    if (ORTE_SUCCESS != rc) ORTE_ERROR_LOG(rc);
    
    OBJ_RELEASE(value);
    return rc;
}


/**
 *  Get the vpid start and range for a job/pset from the registry
 */

int orte_rmgr_base_get_vpid_range(orte_jobid_t jobid, orte_vpid_t *start, orte_vpid_t *range)
{
    char *segment;
    char *tokens[2];
    char *keys[3];
    orte_gpr_value_t** values = NULL;
    orte_std_cntr_t i, num_values = 0;
    orte_vpid_t *vptr;
    int rc;
    
    /* query the job segment on the registry */
    if(ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    tokens[0] = ORTE_JOB_GLOBALS;
    tokens[1] = NULL;
    
    keys[0] = ORTE_JOB_VPID_START_KEY;
    keys[1] = ORTE_JOB_VPID_RANGE_KEY;
    keys[2] = NULL;
    
    rc = orte_gpr.get(
                      ORTE_GPR_KEYS_AND|ORTE_GPR_TOKENS_OR,
                      segment,
                      tokens,
                      keys,
                      &num_values,
                      &values
                      );
    if(rc != ORTE_SUCCESS) {
        free(segment);
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    if(num_values != 1) {
        rc = ORTE_ERR_NOT_FOUND;
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    for(i=0; i<values[0]->cnt; i++) {
        if(strcmp(values[0]->keyvals[i]->key, ORTE_JOB_VPID_START_KEY) == 0) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, values[0]->keyvals[i]->value, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            *start = *vptr;
            continue;
        }
        if(strcmp(values[0]->keyvals[i]->key, ORTE_JOB_VPID_RANGE_KEY) == 0) {
            if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&vptr, values[0]->keyvals[i]->value, ORTE_VPID))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            *range = *vptr;
            continue;
        }
    }
    
cleanup:
    for(i=0; i<num_values; i++)
        OBJ_RELEASE(values[i]);
    free(segment);
    free(values);
    return rc;
}

