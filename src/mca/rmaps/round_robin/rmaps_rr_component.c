/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "util/output.h"
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
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
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
 *  Convience functions to lookup MCA parameter values.
 */

static  int orte_rmaps_round_robin_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("rmaps","round_robin",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}
 

/**
  * component open/close/init function
  */
static int orte_rmaps_round_robin_open(void)
{
    mca_rmaps_round_robin_component.debug = orte_rmaps_round_robin_param_register_int("debug",1);
    mca_rmaps_round_robin_component.priority = orte_rmaps_round_robin_param_register_int("priority",1);
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
    return ORTE_SUCCESS;
}


