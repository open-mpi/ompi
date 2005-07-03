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
#include "mpool_sm.h"
#include "mca/common/sm/common_sm_mmap.h"
#include "util/proc_info.h"
#include "util/sys_info.h"

/*
 * Local functions
 */
static int mca_mpool_sm_open(void);
static mca_mpool_base_module_t* mca_mpool_sm_init(
    struct mca_mpool_base_resources_t* resources);

mca_mpool_sm_component_t mca_mpool_sm_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_MPOOL_BASE_VERSION_1_0_0,

        "sm", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        mca_mpool_sm_open,  /* component open  */
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_sm_init
    }
};

static char* mca_mpool_sm_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("mpool","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static  int mca_mpool_sm_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("mpool","sm",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int mca_mpool_sm_open(void)
{
    /* register SM component parameters */
    mca_mpool_sm_component.sm_size =
        mca_mpool_sm_param_register_int("size", 512*1024*1024);
    mca_mpool_sm_component.sm_allocator_name =
        mca_mpool_sm_param_register_string("allocator", "bucket");
    return OMPI_SUCCESS;
}


static mca_mpool_base_module_t* mca_mpool_sm_init(
    struct mca_mpool_base_resources_t* resources)
{
    char *file_name;
    int len;
    mca_mpool_sm_module_t* mpool_module; 
    mca_allocator_base_component_t* allocator_component; 
    allocator_component = mca_allocator_component_lookup(
        mca_mpool_sm_component.sm_allocator_name);

    /* if specified allocator cannout be loaded - look for an alternative */
    if(NULL == allocator_component) {
        if(opal_list_get_size(&mca_allocator_base_components) == 0) {
            mca_base_component_list_item_t* item = (mca_base_component_list_item_t*)
                opal_list_get_first(&mca_allocator_base_components);
            allocator_component = (mca_allocator_base_component_t*)item->cli_component;
            ompi_output(0, "mca_mpool_sm_init: unable to locate allocator: %s - using %s\n",
                mca_mpool_sm_component.sm_allocator_name, allocator_component->allocator_version.mca_component_name);
        } else {
            ompi_output(0, "mca_mpool_sm_init: unable to locate allocator: %s\n",
                mca_mpool_sm_component.sm_allocator_name);
            return NULL;
        }
    }
    
    
    mpool_module = (mca_mpool_sm_module_t*)malloc(sizeof(mca_mpool_sm_module_t)); 
    mca_mpool_sm_module_init(mpool_module); 
    
    /* create initial shared memory mapping */
    len=asprintf(&file_name,"%s/shared_mem_pool.%s",
            orte_process_info.job_session_dir,
            orte_system_info.nodename);
    if ( 0 > len ) {
        return NULL;
    }

    if(NULL == 
            (mca_common_sm_mmap = 
             mca_common_sm_mmap_init(mca_mpool_sm_component.sm_size,
                 file_name,sizeof(mca_common_sm_mmap_t), 8 )
             )) 
    {
        free(file_name);
        ompi_output(0, "mca_mpool_sm_init: unable to create shared memory mapping");
        return NULL;
    }
    free(file_name);

    /* setup function pointers */


    /* setup allocator  TODO fix up */
    mpool_module->sm_allocator = 
      allocator_component->allocator_init(true,
                                          mca_common_sm_mmap_seg_alloc, NULL, NULL);
    if(NULL == mpool_module->sm_allocator) {
      ompi_output(0, "mca_mpool_sm_init: unable to initialize allocator");
        return NULL;
    }
   
    return &mpool_module->super;
}










