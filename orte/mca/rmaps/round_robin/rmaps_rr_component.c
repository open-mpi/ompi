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

#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "rmaps_rr.h"

/*
 * Local functions
 */

static int orte_rmaps_round_robin_open(void);
static int orte_rmaps_round_robin_close(void);
static orte_rmaps_base_module_t* orte_rmaps_round_robin_init(int* priority);


orte_rmaps_round_robin_component_t mca_rmaps_round_robin_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a rmaps v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_RMAPS_BASE_VERSION_1_3_0,

        "round_robin", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmaps_round_robin_open,  /* component open  */
        orte_rmaps_round_robin_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
          /* The component is checkpoint ready */
          MCA_BASE_METADATA_PARAM_CHECKPOINT
      },

      orte_rmaps_round_robin_init
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_round_robin_open(void)
{
    mca_base_param_reg_int(&mca_rmaps_round_robin_component.super.rmaps_version, "debug",
                           "Toggle debug output for Round Robin RMAPS component",
                           false, false, 1, 
                           &mca_rmaps_round_robin_component.debug);
    
    mca_base_param_reg_int(&mca_rmaps_round_robin_component.super.rmaps_version, "priority",
                           "Selection priority for Round Robin RMAPS component",
                           false, false, 1,
                           &mca_rmaps_round_robin_component.priority);

    return ORTE_SUCCESS;
}


static orte_rmaps_base_module_t* 
orte_rmaps_round_robin_init(int *priority)
{
    /* the RMAPS framework is -only- opened on HNP's,
     * so no need to check for that here
     */
    
    *priority = mca_rmaps_round_robin_component.priority;
    return &orte_rmaps_round_robin_module;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_round_robin_close(void)
{
    return ORTE_SUCCESS;
}


