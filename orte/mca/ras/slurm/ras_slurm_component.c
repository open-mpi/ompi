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
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/net.h"
#include "opal/opal_socket_errno.h"

#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_slurm.h"


/*
 * Local functions
 */
static int ras_slurm_open(void);
static int ras_slurm_close(void);
static int orte_ras_slurm_component_query(mca_base_module_t **module, int *priority);


orte_ras_slurm_component_t mca_ras_slurm_component = {
    {
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
            ras_slurm_close,
            orte_ras_slurm_component_query
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};


static int ras_slurm_open(void)
{
    int tmp;

    mca_base_param_reg_int(&mca_ras_slurm_component.super.base_version,
                           "dyn_allocate_timeout",
                           "Number of seconds to wait for Slurm dynamic allocation",
                           false, false, 30, &mca_ras_slurm_component.timeout);

    mca_base_param_reg_int(&mca_ras_slurm_component.super.base_version,
                           "enable_dyn_alloc",
                           "Whether or not dynamic allocations are enabled",
                           false, false, (int)false, &tmp);
    mca_ras_slurm_component.dyn_alloc_enabled = OPAL_INT_TO_BOOL(tmp);

    mca_base_param_reg_string(&mca_ras_slurm_component.super.base_version,
                              "config_file",
                              "Path to Slurm configuration file",
                              false, false, NULL, &mca_ras_slurm_component.config_file);

    mca_base_param_reg_int(&mca_ras_slurm_component.super.base_version,
                           "enable_rolling_alloc",
                           "Enable partial dynamic allocations",
                           false, false, (int)false, &tmp);
    mca_ras_slurm_component.rolling_alloc = OPAL_INT_TO_BOOL(tmp);


    return ORTE_SUCCESS;
}

static int ras_slurm_close(void)
{
    if (NULL != mca_ras_slurm_component.config_file) {
        free(mca_ras_slurm_component.config_file);
    }
    return ORTE_SUCCESS;
}

static int orte_ras_slurm_component_query(mca_base_module_t **module, int *priority)
{
    /* if I built, then slurm support is available. If
     * I am not in a Slurm allocation, and dynamic alloc
     * is not enabled, then I'll deal with that by returning
     * an appropriate status code upon allocation
     */

    OPAL_OUTPUT_VERBOSE((2, orte_ras_base.ras_output,
                         "%s ras:slurm: available for selection",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* since only one RM can exist on a cluster, just set
     * my priority to something - the other components won't
     * be responding anyway
     */
    *priority = 50;
    *module = (mca_base_module_t *) &orte_ras_slurm_module;
    return ORTE_SUCCESS;
}
