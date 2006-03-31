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
#include "orte/mca/soh/xcpu/soh_xcpu.h"

/*
 * Local functions
 */

static int orte_soh_xcpu_open(void);
static int orte_soh_xcpu_close(void);
static orte_soh_base_module_t* orte_soh_xcpu_init(int*);

orte_soh_xcpu_component_t mca_soh_xcpu_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a xcpu soh v1.0.0 module (which also
           implies a specific MCA version) */

        ORTE_SOH_BASE_VERSION_1_0_0,

        "xcpu", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_soh_xcpu_open,  /* component open */
        orte_soh_xcpu_close  /* component close */
      },

      /* Next the MCA v1.0.0 module meta data */

      {
        /* Whether the module is checkpointable or not */

        false
      },

      orte_soh_xcpu_init
    }
};

/**
 * Utility function to register parameters
 */
static int orte_soh_xcpu_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("soh","xcpu",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

static int orte_soh_xcpu_open(void)
{
    mca_soh_xcpu_component.debug =
        orte_soh_xcpu_param_register_int("debug", 0);
    mca_soh_xcpu_component.priority =
        orte_soh_xcpu_param_register_int("priority", 1);
    /*fprintf(stdout, "soh_xcpu: open\n");*/
    return ORTE_SUCCESS;
}

static orte_soh_base_module_t* orte_soh_xcpu_init(int *priority)
{
    if (!orte_process_info.seed){
        fprintf(stderr, "soh_xcpu: no seed found\n");
	return NULL;
    }

    *priority = mca_soh_xcpu_component.priority;
    orte_soh_xcpu_module_init();/*do we need this???*/
    return &orte_soh_xcpu_module;
}

static int orte_soh_xcpu_close(void)
{
    fprintf(stdout, "soh_xcpu: close\n");
    return ORTE_SUCCESS;
}
