#include "mca/base/mca_base_param.h"
#include "mca/mpool/sm/mpool_sm.h"


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
    mca_mpool_sm_module.sm_min_size =
        mca_mpool_sm_param_register_int("min_size", 64*1024*1024);
    mca_mpool_sm_module.sm_max_size =
        mca_mpool_sm_param_register_int("max_size", 512*1024*1024);
    mca_mpool_sm_module.sm_allocator_name =
        mca_mpool_sm_param_register_string("allocator", "bucket");
    mca_mpool_sm_module.sm_segment = 1;
    mca_mpool_sm_module.sm_size = 0;
    return OMPI_SUCCESS;
}


int mca_mpool_sm_close(void)
{
    return OMPI_SUCCESS;
}


mca_mpool_t* mca_mpool_sm_init(bool *allow_multi_user_threads)
{
    return NULL;
}



