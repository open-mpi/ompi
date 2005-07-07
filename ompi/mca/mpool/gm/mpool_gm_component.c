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
#include "opal/util/output.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/allocator/base/base.h"
#include "mpool_gm.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include <unistd.h> 

/*
 * Local functions
 */
static int mca_mpool_gm_open(void);
static mca_mpool_base_module_t* mca_mpool_gm_init(
    struct mca_mpool_base_resources_t* resources);

mca_mpool_gm_component_t mca_mpool_gm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_MPOOL_BASE_VERSION_1_0_0,

        "gm", /* MCA component name */
        OMPI_MAJOR_VERSION,  /* MCA component major version */
        OMPI_MINOR_VERSION,  /* MCA component minor version */
        OMPI_RELEASE_VERSION,  /* MCA component release version */
        mca_mpool_gm_open,  /* component open  */
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_gm_init
    }
};



static char* mca_mpool_gm_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("mpool","gm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int mca_mpool_gm_open(void)
{
    return OMPI_SUCCESS;
}


/* Allocates a segment of memory and registers with GM, user_out returns the memory handle. */ 
static mca_mpool_base_module_t* mca_mpool_gm_init(
     struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_gm_module_t* gm_mpool; 
    gm_mpool = (mca_mpool_gm_module_t*)malloc(sizeof(mca_mpool_gm_module_t)); 
    mca_mpool_gm_module_init(gm_mpool); 
    gm_mpool->port = resources->port;
    return &gm_mpool->super;
}


