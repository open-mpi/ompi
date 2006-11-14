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

int orte_smr_base_get_proc_state(orte_proc_state_t *state,
                               int *exit_status,
                               orte_process_name_t *proc)
{
    orte_gpr_value_t **values;
    orte_gpr_keyval_t **keyvals;
    int rc;
    orte_std_cntr_t cnt, num_tokens, i, j;
    char *segment, **tokens, *keys[3];
    orte_proc_state_t *ps;
    orte_exit_code_t *ecptr;
    orte_jobid_t jobid;
    bool found1=false, found2=false;

    jobid = proc->jobid;

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

    /** there should be one - and only one - value returned. if cnt is anything else,
     * we have a problem
     */
    if (1 != cnt) {
        if (0 == cnt) {  /** check for special case - didn't find the process container */
            ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
            rc = ORTE_ERR_NOT_FOUND;
            goto CLEANUP;
        }
        /** if not 0, then we have too many - report that */
        ORTE_ERROR_LOG(ORTE_ERR_INDETERMINATE_STATE_INFO);
        rc = ORTE_ERR_INDETERMINATE_STATE_INFO;
        goto CLEANUP;
    }

    for (i=0; i < cnt; i++) {
        keyvals = values[i]->keyvals;
        if (NULL != keyvals) {
            for (j=0; j < values[i]->cnt; j++) {
                if (ORTE_PROC_STATE == keyvals[j]->value->type) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&ps, keyvals[j]->value, ORTE_PROC_STATE))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    *state = *ps;
                    found1 = true;
                } else if (ORTE_EXIT_CODE == keyvals[j]->value->type) {
                    if (ORTE_SUCCESS != (rc = orte_dss.get((void**)&ecptr, keyvals[j]->value, ORTE_EXIT_CODE))) {
                        ORTE_ERROR_LOG(rc);
                        goto CLEANUP;
                    }
                    *exit_status = (int)(*ecptr);
                    found2 = true;
                }
            }
        }
    }

    /* see if we found everything */
    if (!found1 || !found2) {
        if (found1) { /** we found the proc state, so we are missing the exit status */
            ORTE_ERROR_LOG(ORTE_ERR_PROC_EXIT_STATUS_MISSING);
            rc = ORTE_ERR_PROC_EXIT_STATUS_MISSING;
            goto CLEANUP;
        }
        if (found2) { /** missing the proc state */
            ORTE_ERROR_LOG(ORTE_ERR_PROC_STATE_MISSING);
            rc = ORTE_ERR_PROC_STATE_MISSING;
            goto CLEANUP;
        }
        /** if we get here, then we are missing them both! report that too */
        ORTE_ERROR_LOG(ORTE_ERR_PROC_EXIT_STATUS_MISSING);
        ORTE_ERROR_LOG(ORTE_ERR_PROC_STATE_MISSING);
        rc = ORTE_ERR_PROC_STATE_MISSING; /** pick one to return */
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

    if (NULL != values) {
        for (i=0; i < cnt; i++) {
            OBJ_RELEASE(values[i]);
        }
        free(values);
    }

    return rc;
}
