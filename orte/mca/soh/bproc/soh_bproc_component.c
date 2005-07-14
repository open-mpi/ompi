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
#include "util/proc_info.h"
#include "opal/util/output.h"
#include "mca/soh/bproc/soh_bproc.h"

/*
 * Local functions
 */

static int orte_soh_bproc_open(void);
static int orte_soh_bproc_close(void);
static orte_soh_base_module_t* orte_soh_bproc_init(int*);

orte_soh_bproc_component_t mca_soh_bproc_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a bproc soh v1.0.0 module (which also
           implies a specific MCA version) */

        ORTE_SOH_BASE_VERSION_1_0_0,

        "bproc", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        orte_soh_bproc_open,  /* component open */
        orte_soh_bproc_close  /* component close */
      },

      /* Next the MCA v1.0.0 module meta data */

      {
        /* Whether the module is checkpointable or not */

        false
      },

      orte_soh_bproc_init
    }
};

/**
 * Utility function to register parameters
 */
static int orte_soh_bproc_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("soh","bproc_soh",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/**
 *
 */

static int orte_soh_bproc_open(void)
{
    mca_soh_bproc_component.debug =
        orte_soh_bproc_param_register_int("debug", 1);
    mca_soh_bproc_component.priority =
        orte_soh_bproc_param_register_int("priority", 1);
    return OMPI_SUCCESS;
}

/**
 *
 */

static orte_soh_base_module_t* orte_soh_bproc_init(int *priority)
{
    if (!orte_process_info.seed)
	return NULL;

    *priority = mca_soh_bproc_component.priority;
    orte_soh_bproc_module_init();
    return &orte_soh_bproc_module;
}


/**
 *
 */

static int orte_soh_bproc_close(void)
{
    return OMPI_SUCCESS;
}


