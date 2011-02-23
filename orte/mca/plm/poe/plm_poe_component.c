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
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011      IBM Corporation.  All rights reserved.
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
#include "orte/constants.h"

#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/util/opal_environ.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/plm/plm.h"
#include "orte/util/proc_info.h"

#include "plm_poe.h"


/*
 * Public string showing the plm ompi_poe component version number
 */
const char *mca_plm_poe_component_version_string =
  "Open MPI poe plm MCA component version " ORTE_VERSION;


/*
 * Local variable
 */


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_plm_poe_component_t mca_plm_poe_component = {
    {
        /* First, the mca_component_t struct containing meta information
           about the component itself */

        {
            ORTE_PLM_BASE_VERSION_2_0_0,

            /* Component name and version */
            "poe",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,

            /* Component open and close functions */
            orte_plm_poe_component_open,
            orte_plm_poe_component_close,
            orte_plm_poe_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


/**
orte_plm_poe_component_open - open component and register all parameters
@return error number
*/
int orte_plm_poe_component_open(void)
{
    char *param;
    mca_base_component_t *c = &mca_plm_poe_component.super.base_version;

    mca_base_param_reg_int(c, "mp_retry",
                           "specifies the interval (in seconds) to wait before repeating the node request",
                           true, false, 0, &mca_plm_poe_component.mp_retry);
    mca_base_param_reg_int(c, "mp_retrycount",
                           "specifies the number of times the Partition Manager should make the request before returning",
                           true, false, 0, &mca_plm_poe_component.mp_retrycount);
    mca_base_param_reg_int(c, "mp_infolevel",
                           "specify the level of messages you want from POE (0-6)",
                           true, false, 0, &mca_plm_poe_component.mp_infolevel);
    mca_base_param_reg_string(c, "mp_labelio",
                           "Whether or not to label message output with task identifiers (yes or no)",
                           true, false, "no", &mca_plm_poe_component.mp_labelio);
    mca_base_param_reg_string(c, "mp_stdoutmode",
                           "standard output mode (ordered, unordered or taskID)",
                           true, false, "unordered", &mca_plm_poe_component.mp_stdoutmode);

    mca_base_param_reg_int(c, "debug",
                           "Whether or not to enable debugging output for the poe plm component (0 or 1)",
                           false, false, 0, &mca_plm_poe_component.debug);
    mca_base_param_reg_int(c, "priority",
                           "Priority of the poe plm component",
                           false , false, 100, &mca_plm_poe_component.priority);
    mca_base_param_reg_string(c, "class",
                           "class (interactive or batch)",
                           true, false, "interactive", &mca_plm_poe_component.class);
    mca_base_param_reg_string(c, "resource_allocation",
                           "resource_allocation mode (hostfile or automatic)",
                           false, false, "hostfile", &mca_plm_poe_component.resource_allocation);
    mca_base_param_reg_string(c, "progenv",
                           "The command name that setup environment",
                           false, false, "env", &mca_plm_poe_component.env);
    mca_base_param_reg_string(c, "progpoe",
                           "The POE command",
                           false, false, "poe", &param);
    mca_plm_poe_component.argv = opal_argv_split(param, ' ');
    mca_plm_poe_component.argc = opal_argv_count(mca_plm_poe_component.argv);
    if (mca_plm_poe_component.argc > 0) {
        mca_plm_poe_component.path = strdup(mca_plm_poe_component.argv[0]);
        return ORTE_SUCCESS;
    } else {
        mca_plm_poe_component.path = NULL;
        return ORTE_ERR_BAD_PARAM;
    }


    return ORTE_SUCCESS;
}

int orte_plm_poe_component_query(mca_base_module_t **module, int *priority)
{
    
    /* if we are NOT an HNP, then don't select us */
    if (!ORTE_PROC_IS_HNP) {
        *priority = -1;
        *module = NULL;
        return ORTE_ERROR;
    }

    /* if we can't find the required elements, don't select us */
    mca_plm_poe_component.path = opal_path_findv(mca_plm_poe_component.argv[0], 0, environ, NULL);
    if (NULL == mca_plm_poe_component.path) {
        *priority = -1;
        *module = NULL;
        return ORTE_ERROR;
    }
    mca_plm_poe_component.env = opal_path_findv(mca_plm_poe_component.env, 0, environ, NULL);
    if (NULL == mca_plm_poe_component.env) {
        *priority = -1;
        *module = NULL;
        return ORTE_ERROR;
    }

    /* guess we are good to go */
    *priority = mca_plm_poe_component.priority;
    *module = (mca_base_module_t*) &orte_plm_poe_module;
    return ORTE_SUCCESS;
}

int orte_plm_poe_component_close(void)
{
    return ORTE_SUCCESS;
}
