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

int orte_soh_base_get_proc_soh(orte_proc_state_t *state,
                               int *exit_status,
                               orte_process_name_t *proc)
{
    orte_gpr_value_t **values;
    orte_gpr_keyval_t **keyvals;
    int rc, cnt, num_tokens, i, j;
    char *segment, **tokens, *keys[3];
    orte_jobid_t jobid;

    if (ORTE_SUCCESS != (rc = orte_ns.get_jobid(&jobid, proc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens, proc))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    keys[0] = strdup(ORTE_PROC_STATE_KEY);
    keys[1] = strdup(ORTE_PROC_EXIT_CODE_KEY);
    keys[2] = NULL;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_XAND, segment,
                                tokens, keys, &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }
    
    for (i=0; i < cnt; i++) {
        keyvals = values[i]->keyvals;
        if (NULL != keyvals) {
            for (j=0; j < values[i]->cnt; j++) {
                if (ORTE_PROC_STATE == keyvals[j]->type) {
                    *state = keyvals[j]->value.proc_state;
                } else if (ORTE_EXIT_CODE == keyvals[j]->type) {
                    *exit_status = keyvals[j]->value.exit_code;
                } else {
                    ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
                    rc = ORTE_ERR_GPR_DATA_CORRUPT;
                }
            }
        }
    }

CLEANUP:
    for (i=0; i < 3; i++) {
        if (NULL != keys[i]) free(keys[i]);
    }
    
    if (NULL != segment) free(segment);
    
    for (i=0; i < num_tokens; i++) {
        if (NULL != tokens[i]) free(tokens[i]);
    }
    free(tokens);
    
    if (NULL != values) OBJ_RELEASE(values);
    
    return rc;
}
