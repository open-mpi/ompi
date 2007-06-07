/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
 *                         reserved. 
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

#include "orte_config.h"

#include "pls_poe.h"

#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/orte_constants.h"
#include "orte/mca/pls/pls.h"
#include "orte/util/proc_info.h"


/*
 * Public string showing the pls ompi_poe component version number
 */
const char *mca_pls_poe_component_version_string =
  "Open MPI poe pls MCA component version " ORTE_VERSION;


/*
 * Local variable
 */


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_poe_component_t mca_pls_poe_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_3_0,

        /* Component name and version */

        "poe",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */

        orte_pls_poe_component_open,
        NULL
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        false
    },

    /* Initialization / querying functions */

    orte_pls_poe_component_init
    }
};

/**
orte_pls_poe_component_open - open component and register all parameters
@return error number
*/
int orte_pls_poe_component_open(void)
{
    char *param;
    mca_base_component_t *c = &mca_pls_poe_component.super.pls_version;

    mca_base_param_reg_int(c, "mp_retry",
                           "specifies the interval (in seconds) to wait before repeating the node request",
                           true, false, 0, &mca_pls_poe_component.mp_retry);
    mca_base_param_reg_int(c, "mp_retrycount",
                           "specifies the number of times the Partition Manager should make the request before returning",
                           true, false, 0, &mca_pls_poe_component.mp_retrycount);
    mca_base_param_reg_int(c, "mp_infolevel",
                           "specify the level of messages you want from POE (0-6)",
                           true, false, 0, &mca_pls_poe_component.mp_infolevel);
    mca_base_param_reg_string(c, "mp_labelio",
                           "Whether or not to label message output with task identifiers (yes or no)",
                           true, false, "no", &mca_pls_poe_component.mp_labelio);
    mca_base_param_reg_string(c, "mp_stdoutmode",
                           "standard output mode (ordered, unordered or taskID)",
                           true, false, "unordered", &mca_pls_poe_component.mp_stdoutmode);

    mca_base_param_reg_int(c, "debug",
                           "Whether or not to enable debugging output for the poe pls component (0 or 1)",
                           false, false, 0, &mca_pls_poe_component.debug);
    mca_base_param_reg_int(c, "verbose",
                           "Verbose level",
                           true, false, 0, &mca_pls_poe_component.verbose);
    mca_base_param_reg_int(c, "priority",
                           "Priority of the poe pls component",
                           false , false, 100, &mca_pls_poe_component.priority);
    mca_base_param_reg_string(c, "orted",
                           "The command name that the poe pls component will invoke for the ORTE daemon",
                           false, false, "orted", &mca_pls_poe_component.orted);
    mca_base_param_reg_string(c, "class",
                           "class (interactive or batch)",
                           true, false, "interactive", &mca_pls_poe_component.class);
    mca_base_param_reg_string(c, "resource_allocation",
                           "resource_allocation mode (hostfile or automatic)",
                           false, false, "hostfile", &mca_pls_poe_component.resource_allocation);
    mca_base_param_reg_string(c, "progenv",
                           "The command name that setup environment",
                           false, false, "env", &mca_pls_poe_component.env);
    mca_base_param_reg_string(c, "progpoe",
                           "The POE command",
                           false, false, "poe", &param);
    mca_pls_poe_component.argv = opal_argv_split(param, ' ');
    mca_pls_poe_component.argc = opal_argv_count(mca_pls_poe_component.argv);
    if (mca_pls_poe_component.argc > 0) {
        mca_pls_poe_component.path = strdup(mca_pls_poe_component.argv[0]);
        return ORTE_SUCCESS;
    } else {
        mca_pls_poe_component.path = NULL;
        return ORTE_ERR_BAD_PARAM;
    }


    return ORTE_SUCCESS;
}

/**
orte_pls_poe_component_init - initialize component, check if we can run on this machine.
@return error number
*/
orte_pls_base_module_t *orte_pls_poe_component_init(int *priority)
{
    
    /* if we are NOT an HNP, then don't select us */
    if (!orte_process_info.seed) {
        return NULL;
    }

    mca_pls_poe_component.path = opal_path_findv(mca_pls_poe_component.argv[0], 0, environ, NULL);
    if (NULL == mca_pls_poe_component.path) {
        return NULL;
    }
    mca_pls_poe_component.env = opal_path_findv(mca_pls_poe_component.env, 0, environ, NULL);
    if (NULL == mca_pls_poe_component.env) {
        return NULL;
    }
    *priority = mca_pls_poe_component.priority;
    return &orte_pls_poe_module;
}
