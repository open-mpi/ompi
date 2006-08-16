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

int orte_smr_base_set_job_state(orte_jobid_t jobid,
                              orte_job_state_t state)
{
    orte_gpr_value_t *value;
    int rc;

    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&value, ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND,
                                                    ORTE_JOBINFO_SEGMENT, 1, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if (ORTE_JOBID_MAX != jobid) {  /* check for wildcard case - leave tokens blank if so */
        if (ORTE_SUCCESS != (rc = orte_schema.get_job_tokens(&(value->tokens),
                                                &(value->num_tokens), jobid))) {
            ORTE_ERROR_LOG(rc);
            goto CLEANUP;
        }
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(value->keyvals[0]), ORTE_JOB_STATE_KEY, ORTE_JOB_STATE, &state))) {
        ORTE_ERROR_LOG(rc);
        goto CLEANUP;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.put(1, &value))) {
        ORTE_ERROR_LOG(rc);
    }

CLEANUP:
    /* cleanup */
    OBJ_RELEASE(value);

    /* all done */
    return rc;
}
