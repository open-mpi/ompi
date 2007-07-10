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
#include "orte/orte_constants.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"

#include "ras_lsf.h"

/*
 * Local functions
 */

static int orte_ras_lsf_open(void);
static int orte_ras_lsf_close(void);
static orte_ras_base_module_t* orte_ras_lsf_init(int* priority);


orte_ras_lsf_component_t mca_ras_lsf_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a ras v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_RAS_BASE_VERSION_1_3_0,

        "lsf", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_ras_lsf_open,  /* component open  */
        orte_ras_lsf_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      orte_ras_lsf_init
    }
};


/**
  * component open/close/init function
  */
static int orte_ras_lsf_open(void)
{
    mca_base_component_t *c = &mca_ras_lsf_component.super.ras_version;
    int tmp;
    
    mca_base_param_reg_int(c, "debug",
                           "Whether or not to enable debugging output for the LSF component (0 or 1)",
                           false, false, (int)false, &tmp);
    mca_ras_lsf_component.debug = OPAL_INT_TO_BOOL(tmp);
    
    mca_base_param_reg_int(c, "priority",
                           "Selection priority for LSF component",
                           false, false, 75, &mca_ras_lsf_component.priority);
    
    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *orte_ras_lsf_init(int* priority)
{
    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    /* check if lsf is running here */
    if (lsb_init() < 0) {
        /* nope, not here */
        return NULL;
    }
    
    *priority = mca_ras_lsf_component.priority;
    return &orte_ras_lsf_module;
}

/**
 *  Close all subsystems.
 */

static int orte_ras_lsf_close(void)
{
    return ORTE_SUCCESS;
}


