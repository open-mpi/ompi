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
 *
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <string.h>

#include "opal/util/output.h"

#include "orte/dss/dss.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/orte_wakeup.h"

/* orterun will only wakeup when all procs IN THE ROOT JOB report terminated. In some cases,
* such as when an orted fails to start, we have a situation where the root job's processes
* cannot report back as terminated or aborted. For those situations, we force the issue by
* deliberately causing the TERMINATE trigger on the root job to fire
*/
int orte_wakeup(orte_jobid_t job) {
    int rc;
    orte_jobid_t root;
    orte_vpid_t range;
    char *segment;
    orte_std_cntr_t num;
    char *tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    orte_data_value_t dval = ORTE_DATA_VALUE_EMPTY;
    
    /* first, let's ensure we cause any triggers on this job to fire */
    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(job, &range))) {
        ORTE_ERROR_LOG(rc);
    }
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
    }
    num = range;
    if (ORTE_SUCCESS != (rc = orte_dss.set(&dval, (void*)&num, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.put_1(ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                             segment, tokens, ORTE_PROC_NUM_TERMINATED, &dval))) {
        ORTE_ERROR_LOG(rc);
    }
    /* clear the dval for future use */
    dval.data = NULL;

    /* now let's get the root job - note, this may not actually happen IF the
     * prior trigger was enough to cause us to terminate the job
     */
    if (ORTE_SUCCESS != (rc = orte_ns.get_root_job(&root, job))) {
        ORTE_ERROR_LOG(rc);
    }

    /* if the root and the specified job are the same, stop here */
    if (root == job) {
        return ORTE_SUCCESS;
    }

    if (ORTE_SUCCESS != (rc = orte_ns.get_vpid_range(root, &range))) {
        ORTE_ERROR_LOG(rc);
    }
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, root))) {
        ORTE_ERROR_LOG(rc);
    }
    num = range;
    if (ORTE_SUCCESS != (rc = orte_dss.set(&dval, (void*)&num, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.put_1(ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                             segment, tokens, ORTE_PROC_NUM_TERMINATED, &dval))) {
        ORTE_ERROR_LOG(rc);
    }
    
    return ORTE_SUCCESS;
}
