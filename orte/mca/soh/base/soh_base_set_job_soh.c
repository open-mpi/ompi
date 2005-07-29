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

#include "mca/schema/schema.h"

#include "mca/errmgr/errmgr.h"
#include "mca/gpr/gpr.h"
#include "mca/ns/ns.h"

#include "mca/soh/base/base.h"

int orte_soh_base_set_job_soh(orte_jobid_t jobid,
                              orte_job_state_t state)
{
    orte_gpr_value_t *value;
    int rc;

    value = OBJ_NEW(orte_gpr_value_t);
    if (NULL == value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    value->addr_mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND;
    value->segment = strdup(ORTE_JOBINFO_SEGMENT);
    
    if (ORTE_JOBID_MAX == jobid) {  /* wildcard case - set everyone on job segment to this status */
        value->tokens = NULL;
    } else {
        if (ORTE_SUCCESS != (rc = orte_schema.get_job_tokens(&(value->tokens),
                                                &(value->num_tokens), jobid))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    value->cnt = 1;
    value->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt * sizeof(orte_gpr_keyval_t*));
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
    (value->keyvals[0])->key = strdup(ORTE_JOB_STATE_KEY);
    (value->keyvals[0])->type = ORTE_JOB_STATE;
    (value->keyvals[0])->value.job_state = state;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(value);

    /* all done */    
    return rc;
}
