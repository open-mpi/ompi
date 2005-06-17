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
#include "mpool_vapi.h"
#include "util/proc_info.h"
#include "util/sys_info.h"
#include <unistd.h> 

/*
 * Local functions
 */
static int mca_mpool_vapi_open(void);
static mca_mpool_base_module_t* mca_mpool_vapi_init(void* user);

mca_mpool_vapi_component_t mca_mpool_vapi_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a mpool v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_MPOOL_BASE_VERSION_1_0_0,

        "vapi", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        mca_mpool_vapi_open,  /* component open  */
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_mpool_vapi_init
    }
};

static char* mca_mpool_vapi_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("mpool","vapi",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static  int mca_mpool_vapi_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("mpool","vapi",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int mca_mpool_vapi_open(void)
{
    /* register VAPI component parameters */
    
    mca_mpool_vapi_component.vapi_allocator_name =
        mca_mpool_vapi_param_register_string("allocator", "bucket");
    mca_mpool_vapi_component.page_size = sysconf(_SC_PAGESIZE); 

    return OMPI_SUCCESS;
}

/* Allocates a segment of memory and registers with IB, user_out returns the memory handle. */ 
void* mca_common_vapi_segment_alloc(size_t* size, void* user_in, void** user_out){
  
  mca_mpool_vapi_module_t * mpool_module = (mca_mpool_vapi_module_t*)user_in; 

  void* addr = (void*)malloc((*size)); 
  
  VAPI_mrw_t mr_in, mr_out;
  
  VAPI_ret_t ret; 
  mca_common_vapi_memhandle_t* mem_hndl; 
  memset(&mr_in, 0, sizeof(VAPI_mrw_t)); 
  memset(&mr_out, 0, sizeof(VAPI_mrw_t)); 
  

  *user_out = (void*) malloc(sizeof(mca_common_vapi_memhandle_t)); 
  
  mem_hndl = (mca_common_vapi_memhandle_t*) *user_out;  
  memset(mem_hndl, 0, sizeof(mca_common_vapi_memhandle_t*)); 
  mem_hndl->hndl = VAPI_INVAL_HNDL; 
  
  
  mr_in.acl = VAPI_EN_LOCAL_WRITE | VAPI_EN_REMOTE_WRITE;
  mr_in.l_key = 0;
  mr_in.r_key = 0;
  mr_in.pd_hndl = mpool_module->hca_pd.pd_tag;
  mr_in.size = *size;
  mr_in.start = (VAPI_virt_addr_t) (MT_virt_addr_t) addr;
  mr_in.type = VAPI_MR;
  

  ret = VAPI_register_mr(
                    mpool_module->hca_pd.hca, 
                    &mr_in, 
                    &mem_hndl->hndl, 
                    &mr_out
                    ); 
  
  if(VAPI_OK != ret){ 
      ompi_output(0, "error pinning vapi memory\n"); 
      return NULL; 
  }
  
  mem_hndl->l_key = mr_out.l_key; 
  mem_hndl->r_key = mr_out.r_key; 
  return addr; 
}

static mca_mpool_base_module_t* mca_mpool_vapi_init(void * user_in)
{
  
  mca_allocator_base_component_t* allocator_component = mca_allocator_component_lookup( 
                                           mca_mpool_vapi_component.vapi_allocator_name);
  
  /* if specified allocator cannout be loaded - look for an alternative */
    if(NULL == allocator_component) {
        if(ompi_list_get_size(&mca_allocator_base_components) == 0) {
            mca_base_component_list_item_t* item = (mca_base_component_list_item_t*)
                ompi_list_get_first(&mca_allocator_base_components);
            allocator_component = (mca_allocator_base_component_t*)item->cli_component;
            ompi_output(0, "mca_mpool_vapi_init: unable to locate allocator: %s - using %s\n",
                mca_mpool_vapi_component.vapi_allocator_name, allocator_component->allocator_version.mca_component_name);
        } else {
            ompi_output(0, "mca_mpool_vapi_init: unable to locate allocator: %s\n",
                mca_mpool_vapi_component.vapi_allocator_name);
            return NULL;
        }
    }
    
    mca_mpool_vapi_module_t* mpool_module; 
    mpool_module = (mca_mpool_vapi_module_t*)malloc(sizeof(mca_mpool_vapi_module_t)); 
    mca_mpool_vapi_module_init(mpool_module); 
    
    /* setup allocator  TODO fix up */
    mpool_module->hca_pd = *(mca_common_vapi_hca_pd_t*) user_in; 
    mpool_module->vapi_allocator = 
      allocator_component->allocator_init(true,
                                          mca_common_vapi_segment_alloc, NULL, mpool_module);
    if(NULL == mpool_module->vapi_allocator) {
      ompi_output(0, "mca_mpool_vapi_init: unable to initialize allocator");
      return NULL;
    }
   
    
    return &mpool_module->super;
}














