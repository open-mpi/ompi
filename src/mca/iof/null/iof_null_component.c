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
#include "util/proc_info.h"
#include "util/output.h"
#include "runtime/ompi_progress.h"
#include "mca/rml/rml.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "mca/iof/base/base.h"
#include "mca/iof/base/iof_base_endpoint.h"
#include "iof_null.h"

/*
 * Local functions
 */
static int orte_iof_null_open(void);
static int orte_iof_null_close(void);
static orte_iof_base_module_t* orte_iof_null_init(
    int* priority, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads);


orte_iof_null_component_t mca_iof_null_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_IOF_BASE_VERSION_1_0_0,

        "null", /* MCA component name */
        1,  /* MCA component major version */
        0,  /* MCA component minor version */
        0,  /* MCA component release version */
        orte_iof_null_open,  /* component open  */
        orte_iof_null_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_iof_null_init
    },
    false,
    {{NULL, 0}}
};

static  int orte_iof_null_param_register_int(
    const char* param_name,
    int default_value)
{
    int id = mca_base_param_register_int("iof","null",param_name,NULL,default_value);
    int param_value = default_value;
    mca_base_param_lookup_int(id,&param_value);
    return param_value;
}


/**
  * component open/close/init function
  */
static int orte_iof_null_open(void)
{
    mca_iof_null_component.null_debug = 
        orte_iof_null_param_register_int("debug", 1);
    mca_iof_null_component.null_debug = 
        orte_iof_null_param_register_int("override", 0);
    return OMPI_SUCCESS;
}


static orte_iof_base_module_t* 
orte_iof_null_init(int* priority, bool *allow_multi_user_threads, 
                   bool *have_hidden_threads)
{
    int param, override;

    param = mca_base_param_find("iof", "null", "override");
    mca_base_param_lookup_int(param, &override);

    /* Only be used in a PBS environment -- this component is
       currently *only* for debugging */

    if (0 != override ||
        (NULL != getenv("PBS_ENVIRONMENT") &&
         NULL != getenv("PBS_JOBID"))) {
        *priority = 50;
        *allow_multi_user_threads = true;
        *have_hidden_threads = false;

        return &orte_iof_null_module;
    } 

    return NULL;
}

/**
 *
 */

static int orte_iof_null_close(void)
{
    return ORTE_SUCCESS;
}


