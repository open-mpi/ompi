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


#include "orte_config.h"
#include "orte/orte_constants.h"

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/schema/schema_types.h"

#include "orte/mca/ras/base/ras_private.h"

/*
 * Indicate that the allocation for this job is uncertain - therefore,
 * the oversubscribed condition that might results should be overridden
 * locally based on the actual available hardware on the node
 */
int orte_ras_base_set_oversubscribe_override(orte_jobid_t job)
{
    orte_gpr_addr_mode_t addr_mode;
    char *segment;
    char *tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    orte_data_value_t val = ORTE_DATA_VALUE_EMPTY;
    bool trueval = true;
    int rc;

    addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    
    /* get the job segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* setup the value to be true */
    val.type = ORTE_BOOL;
    val.data = &trueval;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.put_1(addr_mode, segment, tokens,
                                             ORTE_JOB_OVERSUBSCRIBE_OVERRIDE_KEY, &val))) {
        ORTE_ERROR_LOG(rc);
    }
    free(segment);
    
    return rc;
}

int orte_ras_base_get_oversubscribe_override(orte_jobid_t job, bool *flag)
{
    orte_gpr_addr_mode_t addr_mode;
    char *segment;
    char *tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    char *keys[] = {
        ORTE_JOB_OVERSUBSCRIBE_OVERRIDE_KEY,
        NULL
    };
    orte_gpr_value_t **values;
    orte_std_cntr_t i, cnt;
    bool *bptr;
    int rc;
    
    *flag = false; /* default if flag not set */
    
    addr_mode = ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR;
    
    /* get the job segment name */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the data */    
    if (ORTE_SUCCESS != (rc = orte_gpr.get(addr_mode, segment, tokens,
                                           keys,
                                           &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    free(segment);
    
    if (0 < cnt) {
        if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&bptr, values[0]->keyvals[0]->value, ORTE_BOOL))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
        *flag = *bptr;
    }

CLEANUP:
    for (i=0; i < cnt; i++) OBJ_RELEASE(values[i]);
    if (NULL != values) {
        free(values);
    }
    
    return ORTE_SUCCESS;
}
