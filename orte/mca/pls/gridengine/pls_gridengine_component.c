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
 * Copyright (c) 2006      Sun Microsystems, Inc.  All rights reserved.
 *                         Use is subject to license terms.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */
/**
 * @file:
 * Part of the gridengine launcher.
 * See pls_gridengine.h for an overview of how it works.
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include "opal/util/path.h"
#include "opal/util/argv.h"
#include "opal/util/output.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"
#include "orte/mca/pls/base/pls_private.h"
#include "pls_gridengine.h"

/**
 * Public string showing the pls ompi_gridengine component version number
 */
const char *mca_pls_gridengine_component_version_string =
  "Open MPI gridengine pls MCA component version " ORTE_VERSION;


/**
 * Local functions
 */

/**
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */
orte_pls_gridengine_component_t mca_pls_gridengine_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_3_0,

        /* Component name and version */

        "gridengine",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */

        orte_pls_gridengine_component_open,
        orte_pls_gridengine_component_close
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        false
    },

    /* Initialization / querying functions */

    orte_pls_gridengine_component_init
    }
};

/**
orte_pls_gridengine_component_open - open component and register all parameters
@return error number
*/
int orte_pls_gridengine_component_open(void)
{
    mca_base_component_t *c = &mca_pls_gridengine_component.super.pls_version;

    mca_base_param_reg_int(c, "debug",
        "Enable debugging of gridengine pls component",
        false, false, 0, &mca_pls_gridengine_component.debug);
    mca_base_param_reg_int(c, "verbose",
        "Enable verbose output of the gridengine qrsh -inherit command",
        false, false, 0, &mca_pls_gridengine_component.verbose);
    mca_base_param_reg_int(c, "priority",
        "Priority of the gridengine pls component",
        false , false, 100, &mca_pls_gridengine_component.priority);
    mca_base_param_reg_string(c, "orted",
        "The command name that the gridengine pls component will invoke for the ORTE daemon",
        false, false, "orted", &mca_pls_gridengine_component.orted);

    return ORTE_SUCCESS;
}

/**
orte_pls_gridengine_component_close - close component and register all parameters
@return error number
*/
int orte_pls_gridengine_component_close(void)
{
    /* cleanup state */
    if (NULL != mca_pls_gridengine_component.orted) {
        free(mca_pls_gridengine_component.orted);
    }
    return ORTE_SUCCESS;
}

/**
orte_pls_gridengine_component_init - initialize component, check if we can run on this machine.
@return error number
*/
orte_pls_base_module_t *orte_pls_gridengine_component_init(int *priority)
{
    /* if we are not an HNP, then don't select us */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    if (NULL != getenv("SGE_ROOT") && NULL != getenv("ARC") &&
        NULL != getenv("PE_HOSTFILE") && NULL != getenv("JOB_ID")) {
        opal_output_verbose(10, orte_pls_base.pls_output,
            "pls:gridengine: available for selection");

        *priority = mca_pls_gridengine_component.priority;
        return &orte_pls_gridengine_module;
    }
    opal_output_verbose(10, orte_pls_base.pls_output,
            "pls:gridengine: NOT available for selection");
    return NULL;
}
