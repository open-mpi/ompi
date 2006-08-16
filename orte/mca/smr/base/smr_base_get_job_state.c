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
 */

/*
 * includes
 */
#include "orte_config.h"

#include <string.h>

#include "orte/mca/schema/schema.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/ns/ns.h"

#include "orte/mca/smr/base/smr_private.h"

int orte_smr_base_get_job_state(orte_job_state_t *state,
                              orte_jobid_t jobid)
{
    orte_gpr_value_t **values;
    orte_gpr_keyval_t **keyvals;
    int rc;
    orte_std_cntr_t cnt, num_tokens, i, j;
    char **tokens, *keys[2];
    orte_job_state_t *js;

    if (ORTE_SUCCESS != (rc = orte_schema.get_job_tokens(&tokens, &num_tokens, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    keys[0] = strdup(ORTE_JOB_STATE_KEY);
    keys[1] = NULL;

    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_XAND, ORTE_JOBINFO_SEGMENT,
                                tokens, keys, &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    for (i=0; i < cnt; i++) {
        keyvals = values[i]->keyvals;
        if (NULL != keyvals) {
            for (j=0; j < values[i]->cnt; j++) {
                if (ORTE_JOB_STATE == keyvals[j]->value->type) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&js, keyvals[j]->value, ORTE_JOB_STATE))) {
                        ORTE_ERROR_LOG(rc);
                    }
                    *state = *js;
                    goto CLEANUP;
                }
            }
        }
    }

    /* if we get here, then we never found the job state data - something is wrong */
    ORTE_ERROR_LOG(ORTE_ERR_GPR_DATA_CORRUPT);
    rc = ORTE_ERR_GPR_DATA_CORRUPT;
    
CLEANUP:
    for (i=0; i < 2; i++) {
        if (NULL != keys[i]) free(keys[i]);
    }

    for (i=0; i < num_tokens; i++) {
        if (NULL != tokens[i]) free(tokens[i]);
    }
    free(tokens);

    if (NULL != values) {
        for (i=0; i < cnt; i++) {
            OBJ_RELEASE(values[i]);
        }
        free(values);
    }

    return rc;
}
