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
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/runtime/params.h"
#include "opal/runtime/opal.h"
#include "orte/runtime/runtime.h"

/**
 * Leave ORTE.
 *
 * @retval ORTE_SUCCESS Upon success.
 * @retval ORTE_ERROR Upon failure.
 *
 * This function performs 
 */
int orte_finalize(void)
{
    if (!orte_initialized) {
        return ORTE_SUCCESS;
    }

    /* We have now entered the finalization stage */
    orte_universe_info.state = ORTE_UNIVERSE_STATE_FINALIZE;

    /* finalize the orte system */
    orte_system_finalize();
    
    /* finalize the opal utilities */
    opal_finalize();

    orte_initialized = false;
    return ORTE_SUCCESS;
}

