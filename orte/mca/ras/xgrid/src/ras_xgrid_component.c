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

#include "orte/orte_constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "ras_xgrid.h"
#include "opal/util/output.h"
#include "orte/util/proc_info.h"

/*
 * Local functions
 */
static int orte_ras_xgrid_component_open(void);
static int orte_ras_xgrid_component_close(void);
static orte_ras_base_module_t *orte_ras_xgrid_init(int*);


orte_ras_base_component_t mca_ras_xgrid_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        /* Indicate that we are a ras v1.3.0 component (which also
           implies a specific MCA version) */
        
        ORTE_RAS_BASE_VERSION_1_3_0,
        
        /* Component name and version */
        
        "xgrid",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,
        
        /* Component open and close functions */
        
        orte_ras_xgrid_component_open,
        orte_ras_xgrid_component_close
    },
    
    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        false
    },
    
    orte_ras_xgrid_init
};


static int
orte_ras_xgrid_component_open(void)
{
    mca_base_param_register_int("ras", "xgrid", "priority", NULL, 100);
    return ORTE_SUCCESS;
}


static int
orte_ras_xgrid_component_close(void)
{
    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *orte_ras_xgrid_init(int* priority)
{
    /* Are we running under a xgrid job? */
    int id = mca_base_param_find("ras", "xgrid", "priority");
    mca_base_param_lookup_int(id,priority);

    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.seed) {
        return NULL;
    }

    if (NULL != getenv("XGRID_CONTROLLER_HOSTNAME")) {
        opal_output(orte_ras_base.ras_output, 
                    "orte:ras:xgrid: available for selection");
        return &orte_ras_xgrid_module;
    }

    /* Sadly, no */
    opal_output(orte_ras_base.ras_output, 
                "orte:ras:xgrid: NOT available for selection");
    return NULL;
}
