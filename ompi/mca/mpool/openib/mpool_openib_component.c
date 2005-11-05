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

#include "ompi_config.h"
#include "opal/util/output.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mpool_openib.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include <unistd.h> 
#include <malloc.h> 

/*
 * Local functions
 */
static int mca_mpool_openib_open(void);
static mca_mpool_base_module_t* mca_mpool_openib_init(
    struct mca_mpool_base_resources_t* resources);

mca_mpool_openib_component_t mca_mpool_openib_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */
          
          MCA_MPOOL_BASE_VERSION_1_0_0,
          
          "openib", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          mca_mpool_openib_open,  /* component open  */
          NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_openib_init
    }
};



static void mca_mpool_openib_registration_constructor( mca_mpool_openib_registration_t * registration ) 
{ 
    registration->base_reg.base = NULL; 
    registration->base_reg.bound = NULL; 
    registration->base_reg.flags = 0;

}

static void mca_mpool_openib_registration_destructor( mca_mpool_openib_registration_t * registration ) 
{ 
    registration->base_reg.base = NULL; 
    registration->base_reg.bound = NULL; 
    registration->base_reg.flags = 0;
} 


OBJ_CLASS_INSTANCE( 
                   mca_mpool_openib_registration_t, 
                   mca_mpool_base_registration_t, 
                   mca_mpool_openib_registration_constructor, 
                   mca_mpool_openib_registration_destructor
                   ); 





/**
  * component open/close/init function
  */
static int mca_mpool_openib_open(void)
{
 
    return OMPI_SUCCESS;
}

static mca_mpool_base_module_t* mca_mpool_openib_init(
     struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_openib_module_t* mpool_module; 
    mca_base_param_reg_string(&mca_mpool_openib_component.super.mpool_version,                                                                                                                                      
                              "rcache_name",                                                                                                                                                                    
                              "The name of the registration cache the mpool should use",                                                                                                                        
                              false,                                                                                                                                                                            
                              false,                                                                                                                                                                            
                              "rb",                                                                                                                                                                             
                              &(mca_mpool_openib_component.rcache_name));     

        
    mpool_module = (mca_mpool_openib_module_t*)malloc(sizeof(mca_mpool_openib_module_t)); 
    
    mca_mpool_openib_module_init(mpool_module); 
    
    mpool_module->resources = *resources;
    
    return &mpool_module->super;
}


