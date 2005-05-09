/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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

#include "include/orte_constants.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/ras/xgrid/ras-xgrid-version.h"
#include "ras_xgrid.h"


/*
 * Local functions
 */
static orte_ras_base_module_t *ras_xgrid_init(int*);


orte_ras_base_component_1_0_0_t mca_ras_xgrid_component = {
    /* First, the mca_base_component_t struct containing meta
       information about the component itself */

    {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */
        
        ORTE_RAS_BASE_VERSION_1_0_0,
        
        /* Component name and version */
        
        "xgrid",
        MCA_ras_xgrid_MAJOR_VERSION,
        MCA_ras_xgrid_MINOR_VERSION,
        MCA_ras_xgrid_RELEASE_VERSION,
        
        /* Component open and close functions */
        
        NULL,
        NULL
    },
    
    /* Next the MCA v1.0.0 component meta data */
    {
        /* Whether the component is checkpointable or not */
        false
    },
    
    ras_xgrid_init
};


static orte_ras_base_module_t *ras_xgrid_init(int* priority)
{
    /* Are we running under a xgrid job? */
    int id = mca_base_param_register_int("ras","xgrid","priority",NULL,100);
    mca_base_param_lookup_int(id,priority);

    if (NULL != getenv("XGRID_CONTROLLER_HOSTNAME") &&
        NULL != getenv("XGRID_CONTROLLER_PASSWORD")) {
        ompi_output(orte_ras_base.ras_output, "ras:xgrid: available for selection");
        return &orte_ras_xgrid_module;
    }

    /* Sadly, no */
    ompi_output(orte_ras_base.ras_output, "ras:xgrid: NOT available for selection");
    return NULL;
}
