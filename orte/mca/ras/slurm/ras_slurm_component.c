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
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"

#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_slurm.h"

/*
 * Local functions
 */
static int ras_slurm_register(void);
static int ras_slurm_open(void);
static int orte_ras_slurm_component_query(mca_base_module_t **module, int *priority);


orte_ras_base_component_t mca_ras_slurm_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        ORTE_RAS_BASE_VERSION_2_0_0,
        
        /* Component name and version */
        "slurm",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        ras_slurm_open,
        NULL,
        orte_ras_slurm_component_query,
        ras_slurm_register
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int ras_slurm_register(void)
{
    return ORTE_SUCCESS;
}

static int ras_slurm_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_ras_slurm_component_query(mca_base_module_t **module, int *priority)
{
    /* Are we running under a SLURM job? */

    if (NULL != getenv("SLURM_JOBID")) {
        OPAL_OUTPUT_VERBOSE((2, orte_ras_base_framework.framework_output,
                             "%s ras:slurm: available for selection",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        *module = (mca_base_module_t *) &orte_ras_slurm_module;
        /* since only one RM can exist on a cluster, just set
         * my priority to something - the other components won't
         * be responding anyway
         */
        *priority = 50;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */

    OPAL_OUTPUT_VERBOSE((2, orte_ras_base_framework.framework_output,
                         "%s ras:slurm: NOT available for selection",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    *module = NULL;
    *priority = 0;
    return ORTE_ERROR;
}
