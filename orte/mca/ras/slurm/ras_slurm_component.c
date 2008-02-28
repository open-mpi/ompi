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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"

#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/ras/base/ras_private.h"
#include "ras_slurm.h"


/*
 * Local variables
 */
static int param_priority;


/*
 * Local functions
 */
static int ras_slurm_open(void);
static orte_ras_base_module_t *ras_slurm_init(int*);


orte_ras_base_component_t mca_ras_slurm_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        /* Indicate that we are a ras v2.0.0 component (which also
           implies a specific MCA version) */
        
        ORTE_RAS_BASE_VERSION_2_0_0,
        
        /* Component name and version */
        
        "slurm",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        
        ras_slurm_open,
        NULL
    },
    
    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    
    ras_slurm_init
};


static int ras_slurm_open(void)
{
    param_priority = 
        mca_base_param_reg_int(&mca_ras_slurm_component.ras_version,
                               "priority",
                               "Priority of the slurm ras component",
                               false, false, 75, NULL);

    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *ras_slurm_init(int* priority)
{
    /* Are we running under a SLURM job? */

    if (NULL != getenv("SLURM_JOBID")) {
        mca_base_param_lookup_int(param_priority, priority);
        OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "%s ras:slurm: available for selection",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return &orte_ras_slurm_module;
    }

    /* Sadly, no */

    OPAL_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                         "%s ras:slurm: NOT available for selection",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return NULL;
}
