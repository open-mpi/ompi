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

int orte_smr_base_set_proc_state(orte_process_name_t *proc,
                               orte_proc_state_t state,
                               int exit_status)
{
    orte_gpr_value_t *value;
    int rc;
    orte_jobid_t jobid;
    orte_vpid_t vpid;
    orte_exit_code_t exit_code;
    char *segment;

    jobid = proc->jobid;

    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                                    segment, 2, 0))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    
    vpid = proc->vpid;

    if (ORTE_VPID_MAX != vpid) {  /* check for wildcard case - leave tokens alone if so */
        if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&(value->tokens), &(value->num_tokens), proc))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            free(segment);
            return rc;
        }
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_STATE_KEY, ORTE_PROC_STATE, &state))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        free(segment);
        return rc;
    }

    exit_code = (orte_exit_code_t)exit_status;
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_PROC_EXIT_CODE_KEY, ORTE_EXIT_CODE, &exit_code))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        free(segment);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(value);

    /* check to see if we need to increment orte-standard counters */
    if (ORTE_PROC_STATE_LAUNCHED == state ||
        ORTE_PROC_STATE_AT_STG1 == state ||
        ORTE_PROC_STATE_AT_STG2 == state ||
        ORTE_PROC_STATE_AT_STG3 == state ||
        ORTE_PROC_STATE_FINALIZED == state ||
        ORTE_PROC_STATE_TERMINATED == state ||
        ORTE_PROC_STATE_ABORTED == state) {
            
        /* If we're setting ABORTED, we're also setting TERMINATED, so we
           need 2 keyvals.  Everything else only needs 1 keyval. */
    
        if (ORTE_PROC_STATE_ABORTED == state) {
            if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_AND,
                                                            segment, 2, 1))) {
                ORTE_ERROR_LOG(rc);
                free(segment);
                return rc;
            }
        } else {
            if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_KEYS_OR|ORTE_GPR_TOKENS_AND,
                                                            segment, 1, 1))) {
                ORTE_ERROR_LOG(rc);
                free(segment);
                return rc;
            }
        }
        
        value->tokens[0] = strdup(ORTE_JOB_GLOBALS);
    
        /* see which state we are in - let that determine the counter */
        switch (state) {
            case ORTE_PROC_STATE_LAUNCHED:
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_NUM_LAUNCHED, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                break;
                
            case ORTE_PROC_STATE_AT_STG1:
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_NUM_AT_STG1, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                break;
    
            case ORTE_PROC_STATE_AT_STG2:
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_NUM_AT_STG2, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                break;
    
            case ORTE_PROC_STATE_AT_STG3:
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_NUM_AT_STG3, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                break;
    
            case ORTE_PROC_STATE_FINALIZED:
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_NUM_FINALIZED, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                break;
    
            case ORTE_PROC_STATE_TERMINATED:
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_NUM_TERMINATED, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                break;
    
            case ORTE_PROC_STATE_ABORTED:
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_PROC_NUM_ABORTED, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[1]), ORTE_PROC_NUM_TERMINATED, ORTE_UNDEF, NULL))) {
                    ORTE_ERROR_LOG(rc);
                    goto cleanup;
                }
                break;
        }
        
        if (ORTE_SUCCESS != (rc = orte_gpr.increment_value(value))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(value);
            free(segment);
            return rc;
        }
    }

cleanup:
    /* all done */
    if (NULL != value) OBJ_RELEASE(value);
    free(segment);

    return rc;
}
