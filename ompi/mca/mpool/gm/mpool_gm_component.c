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
#include "util/output.h"
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
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
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
    /* register GM component parameters */
    
    mca_mpool_gm_component.gm_allocator_name =
        mca_mpool_gm_param_register_string("allocator", "bucket");
    return OMPI_SUCCESS;
}

/* Allocates a segment of memory and registers with GM, user_out returns the memory handle. */ 
void* mca_common_gm_segment_alloc(
    struct mca_mpool_base_module_t* mpool,
    size_t* size, 
    mca_mpool_base_registration_t** registration)
{
    mca_mpool_gm_module_t* gm_mpool; 
    mca_mpool_base_registration_t* reg;
    void* addr = gm_dma_malloc(gm_mpool->gm_port, *size);
    if(NULL == addr)
        return NULL;
    if(NULL != registration) {
        reg = OBJ_NEW(mca_mpool_base_registration_t);
        reg->base = addr;
        reg->bound = reg->base + *size - 1;
        reg->mpool = mpool;
        *registration = reg;
    }
    return addr; 
}

/* Allocates a segment of memory and registers with IB, user_out returns the memory handle. */ 
static mca_mpool_base_module_t* mca_mpool_gm_init(
     struct mca_mpool_base_resources_t* resources)
{
    mca_mpool_gm_module_t* gm_mpool; 
    mca_allocator_base_component_t* allocator_component;

    /* if specified allocator cannout be loaded - look for an alternative */
    allocator_component = mca_allocator_component_lookup(mca_mpool_gm_component.gm_allocator_name);
    if(NULL == allocator_component) {
        if(opal_list_get_size(&mca_allocator_base_components) == 0) {
            mca_base_component_list_item_t* item = (mca_base_component_list_item_t*)
                opal_list_get_first(&mca_allocator_base_components);
            allocator_component = (mca_allocator_base_component_t*)item->cli_component;
            ompi_output(0, "[%d:%d] unable to locate allocator: %s - using %s\n",
                __FILE__, __LINE__,
                mca_mpool_gm_component.gm_allocator_name, 
                allocator_component->allocator_version.mca_component_name);
        } else {
            ompi_output(0, "[%s:%d] unable to locate allocator: %s\n",
                __FILE__, __LINE__,
                mca_mpool_gm_component.gm_allocator_name);
            return NULL;
        }
    }
    
    gm_mpool = (mca_mpool_gm_module_t*)malloc(sizeof(mca_mpool_gm_module_t)); 
    mca_mpool_gm_module_init(gm_mpool); 
    
    /* setup allocator  TODO fix up */
    gm_mpool->gm_port = resources->gm_port;
    gm_mpool->gm_allocator = 
      allocator_component->allocator_init(true, mca_common_gm_segment_alloc, NULL, &gm_mpool->super);
    if(NULL == gm_mpool->gm_allocator) {
        ompi_output(0, "[%s:%d] unable to initialize allocator", __FILE__, __LINE__);
        return NULL;
    }
    return &gm_mpool->super;
}


