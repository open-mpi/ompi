#include "util/output.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/allocator/base/base.h"
#include "mpool_sm.h"
#include "mpool_sm_mmap.h"



mca_mpool_sm_component_t mca_mpool_sm_module = {
    {
    /* First, the mca_base_module_t struct containing meta information
       about the module itself */
                                                                                                                         
    {
    /* Indicate that we are a mpool v1.0.0 component (which also implies a
       specific MCA version) */
                                                                                                                         
    MCA_MPOOL_BASE_VERSION_1_0_0,
                                                                                                                         
    "sm", /* MCA component name */
    1,  /* MCA module major version */
    0,  /* MCA module minor version */
    0,  /* MCA module release version */
    mca_mpool_sm_open,  /* component open  */
    mca_mpool_sm_close  /* component close */
    },
                                                                                                                         
                                                                                                                         
    /* Next the MCA v1.0.0 module meta data */
                                                                                                                         
                                                                                                                         
    {
    /* Whether the module is checkpointable or not */
                                                                                                                         
                                                                                                                         
    false
    },
                                                                                                                         
    mca_mpool_sm_init
    }
};
                                                                                                                         
static inline char* mca_mpool_sm_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("mpool","sm",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
                                                                                                                          
static inline int mca_mpool_sm_param_register_int(
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
int mca_mpool_sm_open(void)
{
    /* register SM module parameters */
    mca_mpool_sm_module.sm_size =
        mca_mpool_sm_param_register_int("size", 512*1024*1024);
    mca_mpool_sm_module.sm_allocator_name =
        mca_mpool_sm_param_register_string("allocator", "bucket");
    return OMPI_SUCCESS;
}


int mca_mpool_sm_close(void)
{
    return OMPI_SUCCESS;
}


mca_mpool_t* mca_mpool_sm_init(bool *allow_multi_user_threads)
{
    mca_allocator_base_module_t* allocator_component = mca_allocator_component_lookup(
        mca_mpool_sm_module.sm_allocator_name);

    /* if specified allocator cannout be loaded - look for an alternative */
    if(NULL == allocator_component) {
        if(ompi_list_get_size(&mca_allocator_base_components) == 0) {
            mca_base_module_list_item_t* item = (mca_base_module_list_item_t*)
                ompi_list_get_first(&mca_allocator_base_components);
            allocator_component = (mca_allocator_base_module_t*)item->mli_module;
            ompi_output(0, "mca_mpool_sm_init: unable to locate allocator: %s - using %s\n",
                mca_mpool_sm_module.sm_allocator_name, allocator_component->allocator_version.mca_module_name);
        } else {
            ompi_output(0, "mca_mpool_sm_init: unable to locate allocator: %s\n",
                mca_mpool_sm_module.sm_allocator_name);
            return NULL;
        }
    }
    
    /* create initial shared memory mapping */
    if(NULL == (mca_mpool_sm_module.sm_mmap = mca_mpool_sm_mmap_init(mca_mpool_sm_module.sm_size))) {
        ompi_output(0, "mca_mpool_sm_init: unable to create shared memory mapping");
        return NULL;
    }

    /* setup allocator */
    mca_mpool_sm_module.sm_allocator = allocator_component->allocator_init(
        allow_multi_user_threads,
        mca_mpool_sm_mmap_alloc,
        NULL
        );
    if(NULL == mca_mpool_sm_module.sm_allocator) {
        ompi_output(0, "mca_mpool_sm_init: unable to initialize allocator");
        return NULL;
    }

    *allow_multi_user_threads = true;
    return &mca_mpool_sm;
}

