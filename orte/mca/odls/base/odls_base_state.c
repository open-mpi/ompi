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

#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/util/sys_info.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/smr/smr.h"
#include "orte/dss/dss.h"

#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


/*
 * Function for reporting the state and other process-related info
 * for newly spawned child processes
 */
int orte_odls_base_report_spawn(opal_list_t *children)
{
    opal_list_item_t *item;
    orte_odls_child_t *child;
    char **tokens, *segment;
    orte_std_cntr_t num_tokens;
    orte_gpr_addr_mode_t mode = ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR;
    orte_data_value_t dval = ORTE_DATA_VALUE_EMPTY;
    int rc;
    
    if (ORTE_SUCCESS != (rc = orte_gpr.begin_compound_cmd())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    for (item = opal_list_get_first(children);
         item != opal_list_get_end(children);
         item = opal_list_get_next(item)) {
        child = (orte_odls_child_t*)item;
        
        if (ORTE_PROC_STATE_LAUNCHED == child->state) {
            /* when we launch the child, we need to store the pid
             * in addition to setting the state. Be sure to store
             * the pid first, though, as setting the state can
             * cause triggers to fire
             */
            if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens, child->name))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, child->name->jobid))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tokens);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_dss.set(&dval, (void*)&(child->pid), ORTE_PID))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tokens);
                free(segment);
                return rc;
            }
            if (ORTE_SUCCESS != (rc = orte_gpr.put_1(mode, segment, tokens, ORTE_PROC_LOCAL_PID_KEY, &dval))) {
                ORTE_ERROR_LOG(rc);
                opal_argv_free(tokens);
                free(segment);
                return rc;
            }
            dval.data = NULL;
            opal_argv_free(tokens);
            free(segment);
            
            /* now set the process state to LAUNCHED */
        }
        if (ORTE_SUCCESS !=
            (rc = orte_smr.set_proc_state(child->name, ORTE_PROC_STATE_LAUNCHED, 0))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.exec_compound_cmd())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* All done */
    return ORTE_SUCCESS;
}
