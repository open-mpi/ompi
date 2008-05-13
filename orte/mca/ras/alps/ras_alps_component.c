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
 * Copyright (c) 2008      UT-Battelle, LLC
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "orte/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/constants.h"
#include "orte/util/proc_info.h"
#include "ras_alps.h"


/*
 * Local variables
 */
static int param_priority;


/*
 * Local functions
 */
static int ras_alps_open(void);
static int orte_ras_alps_component_query(mca_base_module_t **module, int *priority);


orte_ras_base_component_t mca_ras_alps_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        /* Indicate that we are a ras v2.0.0 component (which also
           implies a specific MCA version) */
        
        ORTE_RAS_BASE_VERSION_2_0_0,
        
        /* Component name and version */
        
        "alps",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        
        ras_alps_open,
        NULL,
        orte_ras_alps_component_query
    },
    /* Next the MCA v1.0.0 component meta data */
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


static int ras_alps_open(void)
{
    param_priority = 
        mca_base_param_reg_int(&mca_ras_alps_component.base_version,
                               "priority",
                               "Priority of the alps ras component",
                               false, false, 75, NULL);

    return ORTE_SUCCESS;
}

static int orte_ras_alps_component_query(mca_base_module_t **module, int *priority)
{
    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.hnp) {
        *module = NULL;
        return ORTE_ERROR;
    }
    
    /* Are we running under a ALPS job? */

    if (NULL != getenv("BATCH_PARTITION_ID")) {
        mca_base_param_lookup_int(param_priority, priority);
        ORTE_OUTPUT_VERBOSE((1, orte_ras_base.ras_output,
                             "ras:alps: available for selection"));
        *module = (mca_base_module_t *) &orte_ras_alps_module;
        return ORTE_SUCCESS;
    }

    /* Sadly, no */

    orte_output(orte_ras_base.ras_output,
                "ras:alps: NOT available for selection");
    *module = NULL;
    return ORTE_ERROR;
}
