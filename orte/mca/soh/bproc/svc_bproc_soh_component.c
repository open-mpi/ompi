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

#include "ompi_config.h"
#include "include/constants.h"

#include "svc_bproc_soh.h"


mca_svc_bproc_soh_component_t mca_svc_bproc_soh_component = {
    {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a bproc soh v1.0.0 module (which also
           implies a specific MCA version) */

        MCA_SVC_BASE_VERSION_1_0_0,

        "bproc_soh", /* MCA module name */
        ORTE_MAJOR_VERSION,  /* MCA module major version */
        ORTE_MINOR_VERSION,  /* MCA module minor version */
        ORTE_RELEASE_VERSION,  /* MCA module release version */
        mca_svc_bproc_soh_component_open,  /* component open */
        mca_svc_bproc_soh_component_close  /* component close */
      },

      /* Next the MCA v1.0.0 module meta data */

      {
        /* Whether the module is checkpointable or not */

        false
      },

      mca_svc_bproc_soh_component_init
    },
    0 /* exec_debug */
};

/**
 * Utility function to register parameters
 */
static inline int mca_svc_bproc_soh_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("svc","bproc_soh",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}

/**
 *
 */

int mca_svc_bproc_soh_component_open(void)
{
    mca_svc_bproc_soh_component.debug =
        mca_svc_bproc_soh_param_register_int("debug", 0);
    return OMPI_SUCCESS;
}

/**
 *
 */

mca_svc_base_module_t* mca_svc_bproc_soh_component_init(void)
{
    return &mca_svc_bproc_soh_module;
}


/**
 *
 */

int mca_svc_bproc_soh_component_close(void)
{
    return OMPI_SUCCESS;
}


