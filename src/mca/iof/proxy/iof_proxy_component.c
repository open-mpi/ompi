/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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
#include "iof_proxy.h"

/*
 * Local functions
 */
static int mca_iof_proxy_open(void);
static mca_iof_base_module_t* mca_iof_proxy_init(int* priority, bool *allow_multi_user_threads);


mca_iof_proxy_component_t mca_iof_proxy_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        MCA_IOF_BASE_VERSION_1_0_0,

        "proxy", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        mca_iof_proxy_open,  /* component open  */
        NULL
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      mca_iof_proxy_init
    }
};

static char* mca_iof_proxy_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("iof","proxy",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}

static  int mca_iof_proxy_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("iof","proxy",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int mca_iof_proxy_open(void)
{
    return OMPI_SUCCESS;
}


static mca_iof_base_module_t* 
mca_iof_proxy_init(int* priority, bool *allow_multi_user_threads)
{
    *priority = 1;
    *allow_multi_user_threads = true;
    return &mca_iof_proxy_module;
}

