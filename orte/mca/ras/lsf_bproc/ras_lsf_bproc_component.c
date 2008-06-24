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
#include "ras_lsf_bproc.h"

/*
 * Local functions
 */

static int orte_ras_lsf_bproc_open(void);
static int orte_ras_lsf_bproc_close(void);
static orte_ras_base_module_t* orte_ras_lsf_bproc_init(int* priority);


orte_ras_lsf_bproc_component_t mca_ras_lsf_bproc_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a ras v1.3.0 component (which also
           implies a specific MCA version) */

        ORTE_RAS_BASE_VERSION_1_3_0,

        "lsf_bproc", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_ras_lsf_bproc_open,  /* component open  */
        orte_ras_lsf_bproc_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_ras_lsf_bproc_init
    }
};


/**
 *  Convience functions to lookup MCA parameters
 */
static  int orte_ras_lsf_bproc_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("ras","lsf_bproc",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int orte_ras_lsf_bproc_open(void)
{
    mca_ras_lsf_bproc_component.debug = orte_ras_lsf_bproc_param_register_int("debug",1);
    mca_ras_lsf_bproc_component.priority = orte_ras_lsf_bproc_param_register_int("priority",-1);
    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *orte_ras_lsf_bproc_init(int* priority)
{
    /* if we are not an HNP, then we must not be selected */
    if (!orte_process_info.seed) {
        return NULL;
    }
    
    *priority = mca_ras_lsf_bproc_component.priority;
    return NULL;
}

/**
 *  Close all subsystems.
 */

static int orte_ras_lsf_bproc_close(void)
{
    return ORTE_SUCCESS;
}


