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

#include "orte/dss/dss.h"
#include "orte/mca/rmaps/rmaps_types.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/base/pls_private.h"

/* Since we now send the "add_procs" command using xcast to all daemons
 * as our standard launch procedure, all we need do for launching on
 * existing daemons is correctly increment the launch counter so that
 * trigger will fire and the launch message will be sent
 */
int orte_pls_base_launch_on_existing_daemons(orte_job_map_t *map)
{
    orte_std_cntr_t num_reused;
    orte_data_value_t dval = ORTE_DATA_VALUE_EMPTY;
    char *trig_tokens[] = {
        ORTE_JOB_GLOBALS,
        NULL
    };
    char *to_launch_keys[] = {
        ORTE_PROC_NUM_LAUNCHED,
        NULL
    };
    int rc;
    
    /* check the number of new daemons vs the number of nodes in the job
     * if num_new_daemons < num_nodes, then we are reusing some existing
     * daemons and we need to increment the launch counter
     */
    if (map->num_nodes == map->num_new_daemons) {
        return ORTE_SUCCESS;
    }
    
    /* compute the number of daemons that are being reused */
    num_reused = map->num_nodes - map->num_new_daemons;
    
    /* setup the arithmetic operand */
    if (ORTE_SUCCESS != (rc = orte_dss.set(&dval, (void*)&num_reused, ORTE_STD_CNTR))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* update the counter */
    if (ORTE_SUCCESS != (rc = orte_gpr.arith(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                             "orte-job-0", trig_tokens, to_launch_keys,
                                             ORTE_DSS_ADD, &dval))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    return ORTE_SUCCESS;
}
