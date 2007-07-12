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

/** @file **/

#include "orte_config.h"

#include "orte/orte_constants.h"

#include "opal/runtime/opal.h"

#include "orte/mca/gpr/gpr.h"
#include "orte/mca/smr/smr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/runtime/runtime.h"

/**
 * Initialze and setup a process in the ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 */

int orte_system_init(bool infrastructure, bool barrier)
{
    int rc;

    if (ORTE_SUCCESS != (rc = orte_init_stage1(infrastructure))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* begin recording registry actions */
#if 0
    if (ORTE_SUCCESS != (rc = orte_gpr.begin_compound_cmd())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
#endif
    if (ORTE_SUCCESS != (rc = orte_init_stage2(ORTE_STARTUP_TRIGGER))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* indicate we are at the ORTE_STARTUP_COMPLETE state */
    if (ORTE_SUCCESS != (rc = orte_smr.set_proc_state(ORTE_PROC_MY_NAME,
                                                      ORTE_PROC_ORTE_STARTUP_COMPLETE, 0))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
#if 0
    /* send the information */
    if (ORTE_SUCCESS != (rc = orte_gpr.exec_compound_cmd())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
#endif
    /* if we want to wait for receipt of info and release, do so here */
    if (barrier) {
        if (ORTE_SUCCESS != (rc = orte_rml.xcast_gate(orte_gpr.deliver_notify_msg))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

