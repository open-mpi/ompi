/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
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
 */
/**
 * @file
 * Resource allocation for Grid Engine
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/util/output.h"

#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_gridengine.h"

/*
 * Local functions
 */

static int orte_ras_gridengine_register(void);
static int orte_ras_gridengine_open(void);
static int orte_ras_gridengine_close(void);
static int orte_ras_gridengine_component_query(mca_base_module_t **module, int *priority);

static int orte_ras_gridengine_verbose;

orte_ras_gridengine_component_t mca_ras_gridengine_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        ORTE_RAS_BASE_VERSION_2_0_0,
        "gridengine",                /* MCA component name */
        ORTE_MAJOR_VERSION,          /* MCA component major version */
        ORTE_MINOR_VERSION,          /* MCA component minor version */
        ORTE_RELEASE_VERSION,        /* MCA component release version */
        orte_ras_gridengine_open,    /* component open  */
        orte_ras_gridengine_close,    /* component close */
        orte_ras_gridengine_component_query,
        orte_ras_gridengine_register
      },
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      }
    }
};

static int orte_ras_gridengine_register(void)
{
    mca_base_component_t *c = &mca_ras_gridengine_component.super.base_version;

    mca_ras_gridengine_component.priority = 100;
    (void) mca_base_component_var_register (c, "priority", "Priority of the gridengine ras component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_ras_gridengine_component.priority);

    orte_ras_gridengine_verbose = 0;
    (void) mca_base_component_var_register (c, "verbose", 
                                            "Enable verbose output for the gridengine ras component",
                                            MCA_BASE_VAR_TYPE_INT, NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_LOCAL, &orte_ras_gridengine_verbose);

    mca_ras_gridengine_component.show_jobid = false;
    (void) mca_base_component_var_register (c, "show_jobid", "Show the JOB_ID of the Grid Engine job",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0, OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY, &mca_ras_gridengine_component.show_jobid);

    return ORTE_SUCCESS;
}

/**
  * component open/close/init function
  */
static int orte_ras_gridengine_open(void)
{
    if (orte_ras_gridengine_verbose != 0) {
        mca_ras_gridengine_component.verbose = opal_output_open(NULL);
    } else {
        mca_ras_gridengine_component.verbose = -1;
    }

    return ORTE_SUCCESS;
}

static int orte_ras_gridengine_component_query(mca_base_module_t **module, int *priority)
{
    *priority = mca_ras_gridengine_component.priority;

    if (NULL != getenv("SGE_ROOT") && NULL != getenv("ARC") && 
        NULL != getenv("PE_HOSTFILE") && NULL != getenv("JOB_ID")) {
        OPAL_OUTPUT_VERBOSE((2, orte_ras_base_framework.framework_output,
                             "%s ras:gridengine: available for selection",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        *module = (mca_base_module_t *) &orte_ras_gridengine_module;
        return ORTE_SUCCESS;
    }
    OPAL_OUTPUT_VERBOSE((2, orte_ras_base_framework.framework_output,
                         "%s ras:gridengine: NOT available for selection",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    *module = NULL;
    return ORTE_ERROR;
}

/**
 *  Close all subsystems.
 */
static int orte_ras_gridengine_close(void)
{
    return ORTE_SUCCESS;
}
