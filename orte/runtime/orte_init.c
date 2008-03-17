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
 * Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
 *
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/** @file **/

#include "orte_config.h"
#include "orte/constants.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal.h"

#include "orte/mca/ess/base/base.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/util/proc_info.h"
#include "orte/util/sys_info.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_locks.h"

#include "orte/runtime/orte_cr.h"

#if OMPI_CC_USE_PRAGMA_IDENT
#pragma ident ORTE_IDENT_STRING
#elif OMPI_CC_USE_IDENT
#ident ORTE_IDENT_STRING
#else
static const char ident[] = ORTE_IDENT_STRING;
#endif

int orte_init(char flags)
{
    int ret;
    char *error = NULL;

    if (orte_initialized) {
        return ORTE_SUCCESS;
    }

    /* initialize the opal layer */
    if (ORTE_SUCCESS != (ret = opal_init())) {
        ORTE_ERROR_LOG(ret);
        return ret;
    }
    
    /* register handler for errnum -> string conversion */
    opal_error_register("ORTE", ORTE_ERR_BASE, ORTE_ERR_MAX, orte_err2str);

    /* setup the locks */
    if (ORTE_SUCCESS != (ret = orte_locks_init())) {
        error = "orte_locks_init";
        goto error;
    }
    
    /* Register all MCA Params */
    if (ORTE_SUCCESS != (ret = orte_register_params())) {
        error = "orte_register_params";
        goto error;
    }

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        error = "orte_sys_info";
        goto error;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        error = "orte_proc_info";
        goto error;
    }

    /* ensure we know the tool setting for when we finalize */
    if ((flags & ORTE_TOOL) || (flags & ORTE_TOOL_WITH_NAME)) {
        orte_process_info.tool = true;
    }
    
    /* Initialize the ORTE data type support */
    if (ORTE_SUCCESS != (ret = orte_dt_init())) {
        error = "orte_dt_init";
        goto error;
    }
    
    /* if I'm the HNP, make sure that the daemon flag is NOT set so that
     * components unique to non-HNP orteds can be selected and init
     * my basic storage elements
     */
    if (orte_process_info.hnp) {
        orte_process_info.daemon = false;
        if (ORTE_SUCCESS != (ret = orte_hnp_globals_init())) {
            error = "orte_hnp_globals_init";
            goto error;
        }
    }
    
    /*
     * Internal startup
     */
    if (ORTE_SUCCESS != (ret = orte_wait_init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_wait_init";
        goto error;
    }

    
    /* open the ESS and select the correct module for this environment */
    if (ORTE_SUCCESS != (ret = orte_ess_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_ess_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_base_select";
        goto error;
    }
    
    /* initialize the RTE for this environment */
    if (ORTE_SUCCESS != (ret = orte_ess.init(flags))) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ess_set_name";
        goto error;
    }
    
    /* All done */
    orte_initialized = true;
    return ORTE_SUCCESS;
    
error:
    opal_show_help("help-orte-runtime",
                   "orte_init:startup:internal-failure",
                   true, error, ORTE_ERROR_NAME(ret), ret);

    return ret;
}

