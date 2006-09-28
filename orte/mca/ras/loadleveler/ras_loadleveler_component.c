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
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"
#include "ras_loadleveler.h"


/*
 * Local variables
 */
static int param_priority;


/*
 * Local functions
 */
static int orte_ras_loadleveler_open(void);
static orte_ras_base_module_t *orte_ras_loadleveler_init(int*);


orte_ras_base_component_t mca_ras_loadleveler_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */
    {
        /* Indicate that we are a ras v1.3.0 component (which also
           implies a specific MCA version) */
        
        ORTE_RAS_BASE_VERSION_1_3_0,
        
        /* Component name and version */
        
        "loadleveler",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        
        orte_ras_loadleveler_open,
        NULL
    },
    
    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        false
    },
    
    orte_ras_loadleveler_init
};


static int orte_ras_loadleveler_open(void)
{
    /* for now we set the priority lower then the priority of the POE RAS
     * so that it is used whenever the LOADL_PROCESSOR_LIST is actually set */
    param_priority = 
        mca_base_param_reg_int(&mca_ras_loadleveler_component.ras_version,
                               "priority",
                               "Priority of the loadleveler ras component",
                               false, false, 90, NULL);

    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *orte_ras_loadleveler_init(int* priority)
{
    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    /* Are we running under a LOADLEVELER job? */
    if (NULL != getenv("LOADL_STEP_ID")) {
        mca_base_param_lookup_int(param_priority, priority);
        opal_output(orte_ras_base.ras_output,
                    "ras:loadleveler: available for selection with priority %d",
                    param_priority);
        return &orte_ras_loadleveler_module;
    }

    /* Sadly, no */
    opal_output(orte_ras_base.ras_output,
                "ras:loadleveler: NOT available for selection");
    return NULL;
}

