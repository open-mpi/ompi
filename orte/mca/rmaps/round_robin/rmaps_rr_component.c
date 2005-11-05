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
#include "include/orte_constants.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/proc_info.h"
#include "opal/util/output.h"
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
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RMAPS_BASE_VERSION_1_0_0,

        "round_robin", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_rmaps_round_robin_open,  /* component open  */
        orte_rmaps_round_robin_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_rmaps_round_robin_init
    }
};


/**
  * component open/close/init function
  */
static int orte_rmaps_round_robin_open(void)
{
    size_t id;

    mca_base_param_reg_int(&mca_rmaps_round_robin_component.super.rmaps_version, "debug",
                           "Toggle debug output for Round Robin RMAPS component",
                           false, false, 1, 
                           &mca_rmaps_round_robin_component.debug);
    
    mca_base_param_reg_int(&mca_rmaps_round_robin_component.super.rmaps_version, "priority",
                           "Selection priority for Round Robin RMAPS component",
                           false, false, 1,
                           &mca_rmaps_round_robin_component.priority);

    /* JMS To be changed post-beta to LAM's C/N command line notation */

    id = mca_base_param_find("rmaps_base", NULL, "schedule_policy");
    if (0 > id) {
        id = mca_base_param_reg_string_name("rmaps_base", "schedule_policy",
                                            "Scheduling Policy for RMAPS. [slot | node]",
                                            false, false, "slot", 
                                            &mca_rmaps_round_robin_component.schedule_policy);
    }
    else {
        mca_base_param_lookup_string(id, &mca_rmaps_round_robin_component.schedule_policy);
    }

    return ORTE_SUCCESS;
}


static orte_rmaps_base_module_t* 
orte_rmaps_round_robin_init(int *priority)
{
    *priority = mca_rmaps_round_robin_component.priority;
    return &orte_rmaps_round_robin_module;
}

/**
 *  Close all subsystems.
 */

static int orte_rmaps_round_robin_close(void)
{
    if (NULL != mca_rmaps_round_robin_component.schedule_policy) {
        free(mca_rmaps_round_robin_component.schedule_policy);
    }

    return ORTE_SUCCESS;
}


