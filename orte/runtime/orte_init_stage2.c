/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

/** @file **/

#include "orte_config.h"

#include "orte/orte_constants.h"

#include "opal/util/show_help.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/params.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/smr/smr.h"

#include "orte/runtime/orte_cr.h"

#include "orte/runtime/runtime.h"

int orte_init_stage2(char *trigger)
{
    int ret;
    char *error_str = NULL;

    if (orte_initialized) {
        return ORTE_SUCCESS;
    }

    /* register our contact info with the HNP.
     * NOTE: it is critical that this be done in stage2 so that
     * the embedded GPR actions can be "trapped" in a compound
     * command for later transmission to the HNP
     */
    if (ORTE_SUCCESS != (ret = orte_rml.register_contact_info())) {
        ORTE_ERROR_LOG(ret);
        error_str = "orte_rml.register_contact_info";
        goto return_error;
    }

    /* register a subscription to share RML contact info
     * between all processes in this job when the provided
     * trigger fires
     */
    if (NULL != trigger) {
        if (ORTE_SUCCESS != (ret = orte_rml.register_subscription(ORTE_PROC_MY_NAME->jobid, trigger))) {
            ORTE_ERROR_LOG(ret);
            error_str = "orte_rml.register_subscription";
            goto return_error;
        }        
    }
    
    /*
     * Initalize the CR setup
     * Note: Always do this, even in non-FT builds.
     * If we don't some user level tools may hang.
     */
    if (ORTE_SUCCESS != (ret = orte_cr_init())) {
        ORTE_ERROR_LOG(ret);
        error_str = "orte_cr_init";
        goto return_error;
    }

    /* Since we are now finished with init, change the state to running */
    orte_universe_info.state = ORTE_UNIVERSE_STATE_RUNNING;

    /* for singleton, need to fire the RUNNING gate
     * to ensure that everything in the rest of the system runs smoothly
     */
    if (orte_process_info.singleton) {
        if (ORTE_SUCCESS != (ret = orte_smr.set_proc_state(orte_process_info.my_name, ORTE_PROC_STATE_RUNNING, 0))) {
            ORTE_ERROR_LOG(ret);
            error_str = "singleton could not set RUNNING state";
            goto return_error;
        }
    }

    /* startup the receive if we are not the HNP - unless we are a singleton,
     * in which case we must start it up in case we do a comm_spawn!
     */
    if (orte_process_info.singleton || !orte_process_info.seed) {
        if (ORTE_SUCCESS != (ret = orte_rml_base_comm_start())) {
            ORTE_ERROR_LOG(ret);
            error_str = "orte_rml_base_comm_start";
            goto return_error;
        }
    }

    /* All done */
    orte_initialized = true;
    return ORTE_SUCCESS; 

return_error:
    opal_show_help("help-orte-runtime",
                   "orte_init:startup:internal-failure",
                   true, error_str, ORTE_ERROR_NAME(ret), ret);
    
    return ret;
}
