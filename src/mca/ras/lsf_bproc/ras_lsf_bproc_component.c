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
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_RAS_BASE_VERSION_1_0_0,

        "lsf_bproc", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
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

static char* orte_ras_lsf_bproc_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("ras","lsf_bproc",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                            
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
    mca_ras_lsf_bproc_component.priority = orte_ras_lsf_bproc_param_register_int("priority",100);
    return ORTE_SUCCESS;
}


static orte_ras_base_module_t *orte_ras_lsf_bproc_init(int* priority)
{
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


