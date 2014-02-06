/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2007 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2007      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/base/base.h"
#include "opal/util/basename.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_tm.h"


/*
 * Local variables
 */
static int param_priority;


/*
 * Local functions
 */
static int ras_tm_register(void);
static int ras_tm_open(void);
static int orte_ras_tm_component_query(mca_base_module_t **module, int *priority);


orte_ras_tm_component_t mca_ras_tm_component = {
    {
        /* First, the mca_base_component_t struct containing meta
           information about the component itself */

        {
            ORTE_RAS_BASE_VERSION_2_0_0,
            
            /* Component name and version */
            "tm",
            ORTE_MAJOR_VERSION,
            ORTE_MINOR_VERSION,
            ORTE_RELEASE_VERSION,
            
            /* Component open and close functions */
            ras_tm_open,
            NULL,
            orte_ras_tm_component_query,
            ras_tm_register
        },
        {
            /* The component is checkpoint ready */
            MCA_BASE_METADATA_PARAM_CHECKPOINT
        }
    }
};

static int ras_tm_register(void)
{
    mca_base_component_t *c        = &mca_ras_tm_component.super.base_version;
    char *pbs_nodefile_env         = NULL;

    param_priority = 100;
    (void) mca_base_component_var_register(c, "priority", "Priority of the tm ras component",
                                           MCA_BASE_VAR_TYPE_INT, NULL, 0, 0,
                                           OPAL_INFO_LVL_9,
                                           MCA_BASE_VAR_SCOPE_READONLY,
                                           &param_priority);

    mca_ras_tm_component.nodefile_dir = NULL;

    /* try to detect the default directory */
    pbs_nodefile_env = getenv("PBS_NODEFILE");
    if (NULL != pbs_nodefile_env) {
        mca_ras_tm_component.nodefile_dir = opal_dirname(pbs_nodefile_env);
    }

    if (NULL == mca_ras_tm_component.nodefile_dir) {
        mca_ras_tm_component.nodefile_dir = strdup ("/var/torque/aux");
    }

    (void) mca_base_component_var_register (c, "nodefile_dir",
                                            "The directory where the PBS nodefile can be found",
                                            MCA_BASE_VAR_TYPE_STRING, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_ras_tm_component.nodefile_dir);

    /* for big SMP machines (e.g., those from SGI), listing the nodes
     * once/slot in the nodefile is extreme. In those cases, they may
     * choose to list each node once, but then provide an envar that
     * tells us how many cpus/node were allocated. Allow the user to
     * inform us that we are in such an environment
     */
    mca_ras_tm_component.smp_mode = false;
    (void) mca_base_component_var_register (c, "smp",
                                            "The Torque system is configured in SMP mode "
                                            "with the number of cpus/node given in the environment",
                                            MCA_BASE_VAR_TYPE_BOOL, NULL, 0, 0,
                                            OPAL_INFO_LVL_9,
                                            MCA_BASE_VAR_SCOPE_READONLY,
                                            &mca_ras_tm_component.smp_mode);

    return ORTE_SUCCESS;
}

static int ras_tm_open(void)
{
    return ORTE_SUCCESS;
}


static int orte_ras_tm_component_query(mca_base_module_t **module, int *priority)
{
    /* Are we running under a TM job? */
    if (NULL != getenv("PBS_ENVIRONMENT") &&
        NULL != getenv("PBS_JOBID")) {
        *priority = param_priority;
        *module = (mca_base_module_t *) &orte_ras_tm_module;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */
    *module = NULL;
    return ORTE_ERROR;
}
