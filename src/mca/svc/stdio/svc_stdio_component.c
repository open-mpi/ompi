#include "svc_stdio.h"


mca_svc_base_component_t mca_svc_stdio_component = {
      /* First, the mca_base_module_t struct containing meta
         information about the module itself */
      {
        /* Indicate that we are a pml v1.0.0 module (which also
           implies a specific MCA version) */
                                                                                                                  
        MCA_SVC_BASE_VERSION_1_0_0,
                                                                                                                  
        "stdio", /* MCA module name */
        1,  /* MCA module major version */
        0,  /* MCA module minor version */
        0,  /* MCA module release version */
        mca_svc_stdio_component_open,  /* component open */
        mca_svc_stdio_component_close  /* component close */
      },
                                                                                                                  
      /* Next the MCA v1.0.0 module meta data */
                                                                                                                  
      {
        /* Whether the module is checkpointable or not */
                                                                                                                  
        false
      },
                                                                                                                  
      mca_svc_stdio_component_init
};


/**
 *
 */

int mca_svc_stdio_component_open(void)
{
    return OMPI_SUCCESS;
}

/**
 *
 */

mca_svc_base_module_t* mca_svc_stdio_component_init(void)
{
    return &mca_svc_stdio_module;
}


/**
 *
 */

int mca_svc_stdio_component_close(void)
{
    return OMPI_SUCCESS;
}


