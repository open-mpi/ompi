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
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "ras_poe.h"

/*
 * Local functions
 */

static int orte_ras_poe_open(void);
static int orte_ras_poe_close(void);
static orte_ras_base_module_t* orte_ras_poe_init(int* priority);


orte_ras_poe_component_t mca_ras_poe_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RAS_BASE_VERSION_1_0_0,
        "poe", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_ras_poe_open,  /* component open  */
        orte_ras_poe_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_ras_poe_init
    }
};

/**
  * component open/close/init function
  */
static int orte_ras_poe_open(void)
{
    mca_base_component_t *c = &mca_ras_poe_component.super.ras_version;

    mca_base_param_reg_int(c, "debug",
                           "Whether or not to enable debugging output for the poe ras component (0 or 1)",
                           false, false, 0, &mca_ras_poe_component.debug);

    mca_base_param_reg_int(c, "priority",
                           "Priority of the poe ras component",
                           false , false, 100, &mca_ras_poe_component.priority);

    return ORTE_SUCCESS;
}

static orte_ras_base_module_t *orte_ras_poe_init(int* priority)
{
    *priority = mca_ras_poe_component.priority;

    if ( NULL != getenv("LOADL_PID") ) {
        return &orte_ras_poe_module;
    }
    return NULL;
}

/**
 *  Close all subsystems.
 */
static int orte_ras_poe_close(void)
{
    return ORTE_SUCCESS;
}
