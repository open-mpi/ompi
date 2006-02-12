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
#include "orte/mca/errmgr/errmgr.h"

#include "opal/runtime/opal.h"
#include "orte/runtime/runtime.h"

/**
 * Initialze and setup a process in the ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 */

/* globals used by RTE */
int orte_debug_flag=(int)false;

int orte_init(bool infrastructure)
{
    int rc;

    if (ORTE_SUCCESS != (rc = opal_init())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_system_init(infrastructure))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* Since we are now finished with init, change the state to running */
    orte_universe_info.state = ORTE_UNIVERSE_STATE_RUNNING;

    return ORTE_SUCCESS;
}
