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
#include "mpool_mvapi.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include <unistd.h> 

/*
 * Local functions
 */
static int mca_mpool_mvapi_open(void);
static mca_mpool_base_module_t* mca_mpool_mvapi_init(
    struct mca_mpool_base_resources_t* resources);

mca_mpool_mvapi_component_t mca_mpool_mvapi_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */

          MCA_MPOOL_BASE_VERSION_1_0_0,
          
          "mvapi", /* MCA component name */
          OMPI_MAJOR_VERSION,  /* MCA component major version */
          OMPI_MINOR_VERSION,  /* MCA component minor version */
          OMPI_RELEASE_VERSION,  /* MCA component release version */
          mca_mpool_mvapi_open,  /* component open  */
          NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_mvapi_init
    }
};



static void mca_mpool_mvapi_registration_constructor( mca_mpool_mvapi_registration_t * registration ) 
{ 
    registration->base_reg.is_leave_pinned = false; 
}

static void mca_mpool_mvapi_registration_destructor( mca_mpool_mvapi_registration_t * registration ) 
{ 
    registration->base_reg.base = NULL; 
    registration->base_reg.bound = NULL; 
    registration->base_reg.is_leave_pinned=false; 

} 


OBJ_CLASS_INSTANCE( 
                   mca_mpool_mvapi_registration_t, 
                   mca_mpool_base_registration_t, 
                   mca_mpool_mvapi_registration_constructor, 
                   mca_mpool_mvapi_registration_destructor
                   ); 




/**
  * component open/close/init function
  */
static int mca_mpool_mvapi_open(void)
{
    /* register VAPI component parameters */
    
    /* get the page size for this architecture*/ 
    mca_mpool_mvapi_component.page_size = sysconf(_SC_PAGESIZE); 

    return OMPI_SUCCESS;
}

static mca_mpool_base_module_t* mca_mpool_mvapi_init(
     struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_mvapi_module_t* mpool_module; 
    long page_size = mca_mpool_mvapi_component.page_size; 

    mca_mpool_mvapi_component.page_size_log = 0; 
    while(page_size > 1){ 
        page_size = page_size >> 1; 
        mca_mpool_mvapi_component.page_size_log++; 
    }    
    
    mpool_module = (mca_mpool_mvapi_module_t*)malloc(sizeof(mca_mpool_mvapi_module_t)); 
    mca_mpool_mvapi_module_init(mpool_module); 
    
    mpool_module->hca_pd = *resources;
    return &mpool_module->super;
}


