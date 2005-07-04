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

        "vapi", /* MCA component name */
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
    registration->is_leave_pinned = false; 
}

static void mca_mpool_mvapi_registration_destructor( mca_mpool_mvapi_registration_t * registration ) 
{ 
    registration->base_reg.base = NULL; 
    registration->base_reg.bound = NULL; 
    registration->is_leave_pinned=false; 

} 


OBJ_CLASS_INSTANCE( 
                   mca_mpool_mvapi_registration_t, 
                   mca_mpool_base_registration_t, 
                   mca_mpool_mvapi_registration_constructor, 
                   mca_mpool_mvapi_registration_destructor
                   ); 




static char* mca_mpool_mvapi_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("mpool","vapi",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int mca_mpool_mvapi_open(void)
{
    /* register VAPI component parameters */
    
    mca_mpool_mvapi_component.vapi_allocator_name =
        mca_mpool_mvapi_param_register_string("allocator", "bucket");
    /* get the page size for this architecture*/ 
    mca_mpool_mvapi_component.page_size = sysconf(_SC_PAGESIZE); 

    return OMPI_SUCCESS;
}

/* Allocates a segment of memory and registers with IB, user_out returns the memory handle. */ 
void* mca_common_vapi_segment_alloc(
    struct mca_mpool_base_module_t* mpool,
    size_t* size, 
    mca_mpool_base_registration_t** registration)
{
    void* addr_malloc = (void*)malloc((*size) + mca_mpool_mvapi_component.page_size); 
    void* addr = (void*)  ALIGN_ADDR(addr_malloc, mca_mpool_mvapi_component.page_size_log); 
    if(OMPI_SUCCESS !=  mpool->mpool_register(mpool, addr, *size, registration)) { 
        free(addr_malloc);
        return NULL; 
    } 
    return addr; 
}

/* Allocates a segment of memory and registers with IB, user_out returns the memory handle. */ 
static mca_mpool_base_module_t* mca_mpool_mvapi_init(
     struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_mvapi_module_t* mpool_module; 
    mca_allocator_base_component_t* allocator_component;
    long page_size = mca_mpool_mvapi_component.page_size; 

    mca_mpool_mvapi_component.page_size_log = 0; 
    while(page_size > 1){ 
        page_size = page_size >> 1; 
        mca_mpool_mvapi_component.page_size_log++; 
    }    
    
    /* if specified allocator cannout be loaded - look for an alternative */
    allocator_component = mca_allocator_component_lookup(mca_mpool_mvapi_component.vapi_allocator_name);
    if(NULL == allocator_component) {
        if(opal_list_get_size(&mca_allocator_base_components) == 0) {
            mca_base_component_list_item_t* item = (mca_base_component_list_item_t*)
                opal_list_get_first(&mca_allocator_base_components);
            allocator_component = (mca_allocator_base_component_t*)item->cli_component;
            opal_output(0, "mca_mpool_mvapi_init: unable to locate allocator: %s - using %s\n",
                mca_mpool_mvapi_component.vapi_allocator_name, allocator_component->allocator_version.mca_component_name);
        } else {
            opal_output(0, "mca_mpool_mvapi_init: unable to locate allocator: %s\n",
                mca_mpool_mvapi_component.vapi_allocator_name);
            return NULL;
        }
    }
    
    mpool_module = (mca_mpool_mvapi_module_t*)malloc(sizeof(mca_mpool_mvapi_module_t)); 
    mca_mpool_mvapi_module_init(mpool_module); 
    
    /* setup allocator  TODO fix up */
    mpool_module->hca_pd = *resources;
    mpool_module->vapi_allocator = 
        allocator_component->allocator_init(true, mca_common_vapi_segment_alloc, NULL, &mpool_module->super);
    if(NULL == mpool_module->vapi_allocator) {
      opal_output(0, "mca_mpool_mvapi_init: unable to initialize allocator");
      return NULL;
    }
    return &mpool_module->super;
}


