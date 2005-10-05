/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/orte_constants.h"

#include "opal/util/show_help.h"

#include "mca/errmgr/errmgr.h"

#include "mca/rml/rml.h"
#include "mca/ns/ns.h"
#include "mca/gpr/gpr.h"

#include "mca/rmgr/base/base.h"
#include "mca/soh/base/base.h"
#include "mca/iof/base/base.h"

#include "runtime/runtime.h"

int orte_init_stage2(void)
{
    int ret;
    char *error;

    /* 
     * Initialize the selected modules now that all components/name are available.
     */

    if (ORTE_SUCCESS != (ret = orte_rml.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_rml.init";
        goto error;
    }

    if (ORTE_SUCCESS != (ret = orte_ns.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_ns.init";
        goto error;
    }

    if (ORTE_SUCCESS != (ret = orte_gpr.init())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_gpr.init";
        goto error;
    }

    /*
     * setup I/O forwarding system
     */
    if (ORTE_SUCCESS != (ret = orte_iof_base_open())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_open";
        goto error;
    }
    if (ORTE_SUCCESS != (ret = orte_iof_base_select())) {
        ORTE_ERROR_LOG(ret);
        error = "orte_iof_base_select";
        goto error;
    }
    
     /* 
     * All done 
     */
error:
    if (ret != ORTE_SUCCESS) {
        opal_show_help("help-orte-runtime",
                       "orte_init:startup:internal-failure",
                       error, ret);
    }
    
    return ret;
}
