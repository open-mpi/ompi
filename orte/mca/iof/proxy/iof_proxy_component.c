/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#include "orte_config.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"
#include "opal/runtime/opal_progress.h"
#include "orte/mca/rml/rml.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/mca/iof/base/base.h"
#include "orte/mca/iof/base/iof_base_endpoint.h"
#include "iof_proxy.h"
#include "iof_proxy_svc.h"

/*
 * Local functions
 */
static int orte_iof_proxy_open(void);
static int orte_iof_proxy_close(void);
static orte_iof_base_module_t* orte_iof_proxy_init(
    int* priority, 
    bool *allow_multi_user_threads,
    bool *have_hidden_threads);


/*
 * Local variables
 */
static bool initialized = false;


orte_iof_proxy_component_t mca_iof_proxy_component = {
    {
      /* First, the mca_base_component_t struct containing meta
         information about the component itself */

      {
        /* Indicate that we are a iof v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_IOF_BASE_VERSION_1_0_0,

        "proxy", /* MCA component name */
        ORTE_MAJOR_VERSION,  /* MCA component major version */
        ORTE_MINOR_VERSION,  /* MCA component minor version */
        ORTE_RELEASE_VERSION,  /* MCA component release version */
        orte_iof_proxy_open,  /* component open  */
        orte_iof_proxy_close  /* component close */
      },

      /* Next the MCA v1.0.0 component meta data */
      {
        /* Whether the component is checkpointable or not */
        false
      },

      orte_iof_proxy_init
    },
    false,
    /* {{NULL, 0}} - Let the compiler initialize it so it won't complain on Windows
	 * where the order of the length and the pointer is different.
	 */
};

#if 0
static char* orte_iof_proxy_param_register_string(
    const char* param_name,
    const char* default_value)
{
    char *param_value;
    int id = mca_base_param_register_string("iof","proxy",param_name,NULL,default_value);
    mca_base_param_lookup_string(id, &param_value);
    return param_value;
}
#endif

static  int orte_iof_proxy_param_register_int(
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
static int orte_iof_proxy_open(void)
{
    mca_iof_proxy_component.proxy_debug = orte_iof_proxy_param_register_int("debug",1);
    return ORTE_SUCCESS;
}


static orte_iof_base_module_t* 
orte_iof_proxy_init(int* priority, bool *allow_multi_user_threads, bool *have_hidden_threads)
{
    int rc;
    if(orte_process_info.seed == true)
        return NULL;

    *priority = 1;
    *allow_multi_user_threads = true;
    *have_hidden_threads = false;

    /* post receive with oob */
    mca_iof_proxy_component.proxy_iov[0].iov_base = NULL;
    mca_iof_proxy_component.proxy_iov[0].iov_len = 0;

    rc = orte_rml.recv_nb(
        ORTE_NAME_WILDCARD,
        mca_iof_proxy_component.proxy_iov,
        1,
        ORTE_RML_TAG_IOF_SVC,
        ORTE_RML_ALLOC|ORTE_RML_PERSISTENT,
        orte_iof_proxy_svc_recv,
        NULL
    );
    if(rc < 0) {
        opal_output(0, "orte_iof_proxy_init: unable to post non-blocking recv");
        return NULL;
    }
    initialized = true;
    return &orte_iof_proxy_module;
}

/**
 *
 */

static int orte_iof_proxy_close(void)
{
    int rc = ORTE_SUCCESS;

    if (initialized) {
        rc = orte_rml.recv_cancel(ORTE_NAME_WILDCARD, ORTE_RML_TAG_IOF_SVC);
    }
    return rc;
}


