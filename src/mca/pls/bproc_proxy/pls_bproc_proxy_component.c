/* -*- C -*-
 * 
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
 *
 */

#include "ompi_config.h"

#include "include/orte_constants.h"
#include "include/types.h"
#include "class/ompi_list.h"
#include "util/proc_info.h"
#include "mca/mca.h"
#include "mca/base/mca_base_param.h"
#include "mca/pls/base/base.h"
#include "pls_bproc_proxy.h"

/*
 * Struct of function pointers and all that to let us be initialized
 */
orte_pls_base_component_t mca_pls_bproc_proxy_component = {
  {
    ORTE_PLS_BASE_VERSION_1_0_0,

    "bproc_proxy", /* MCA component name */
    1,  /* MCA component major version */
    0,  /* MCA component minor version */
    0,  /* MCA component release version */
    orte_pls_bproc_proxy_component_open,  /* component open */
    orte_pls_bproc_proxy_component_close /* component close */
  },
  {
    false /* checkpoint / restart */
  },
  orte_pls_bproc_proxy_init    /* component init */
};

/*
 * Module variables handles
 */
static int param_priority;


int orte_pls_bproc_proxy_component_open(void)
{
   param_priority =
    mca_base_param_register_int("pls", "bproc_proxy", "priority", NULL, 20);
   return ORTE_SUCCESS;
}


int orte_pls_bproc_proxy_component_close(void)
{
    return ORTE_SUCCESS;
}


orte_pls_base_module_t* orte_pls_bproc_proxy_init(
    int *priority)
{
    if(orte_process_info.seed)
        return NULL;

    /* initialize the priority */
    mca_base_param_lookup_int(param_priority, priority);
    return &orte_pls_bproc_proxy_module;
}

