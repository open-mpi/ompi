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
#include "orte/constants.h"

#include "opal/mca/base/base.h"

#include "ras_bjs.h"

/*
 * Local functions
 */

static int ras_bjs_open(void);
static int ras_bjs_component_query(mca_base_module_t **module, int *priority);


orte_ras_base_component_t mca_ras_bjs_component = {
    {
        ORTE_RAS_BASE_VERSION_2_0_0,
        
        "bjs", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        
        /* Component open and close functions */
        ras_bjs_open,  /* component open  */
        NULL,  /* component close */
        ras_bjs_component_query
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    }
};


/**
  * component open/close/init function
  */
static int ras_bjs_open(void)
{
    return ORTE_SUCCESS;
}


static int ras_bjs_component_query(mca_base_module_t **module, int *priority)
{
    if(getenv("NODES") == NULL) {
        *module = NULL;
        return ORTE_ERR_NOT_AVAILABLE;
    }

    *priority = 10;
    *module = (mca_base_module_t *) &orte_ras_bjs_module;
    return ORTE_SUCCESS;
}

