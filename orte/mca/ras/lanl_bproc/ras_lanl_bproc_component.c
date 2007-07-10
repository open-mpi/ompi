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

#include <sys/bproc.h>

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"

#include "ras_lanl_bproc.h"

/*
 * Local functions
 */

static int orte_ras_lanl_bproc_open(void);
static int orte_ras_lanl_bproc_close(void);
static orte_ras_base_module_t* orte_ras_lanl_bproc_init(int* priority);


orte_ras_lanl_bproc_component_t mca_ras_lanl_bproc_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a ras v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_RAS_BASE_VERSION_1_3_0,

        "lanl_bproc", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_ras_lanl_bproc_open,  /* component open  */
        orte_ras_lanl_bproc_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      orte_ras_lanl_bproc_init
    }
};


/**
  * component open/close/init function
  */
static int orte_ras_lanl_bproc_open(void)
{
    mca_base_component_t *c = &mca_ras_lanl_bproc_component.super.ras_version;
    int tmp;
    
    mca_base_param_reg_int(c, "debug",
                           "Whether or not to enable debugging output for the LANL-BPROC component (0 or 1)",
                           false, false, (int)false, &tmp);
    mca_ras_lanl_bproc_component.debug = OPAL_INT_TO_BOOL(tmp);
    
    /* we default to a negative priority so that we will *only* be selected
     * if directed by the user via -mca ras lanl_bproc or -mca ras_lanl_bproc_priority xxx
     */
    mca_base_param_reg_int(c, "priority",
                           "Selection priority for LANL-BPROC component",
                           false, false, -1, &mca_ras_lanl_bproc_component.priority);
    
    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *orte_ras_lanl_bproc_init(int* priority)
{
    int ret;
    struct bproc_version_t version;
    
    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    /* okay, we are in an HNP - now check to see if BProc is running here */
    ret =  bproc_version(&version);
    if (ret != 0) {
        return NULL;
    }
    
    *priority = mca_ras_lanl_bproc_component.priority;
    return &orte_ras_lanl_bproc_module;
}

/**
 *  Close all subsystems.
 */

static int orte_ras_lanl_bproc_close(void)
{
    return ORTE_SUCCESS;
}


