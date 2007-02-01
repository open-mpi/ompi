/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
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
/** @file:
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/util/proc_info.h"
#include "opal/util/output.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/pls/base/pls_private.h"
#include "pls_proxy.h"

/*
 * Struct of function pointers that need to be initialized
 */
orte_pls_base_component_t mca_pls_proxy_component = {
  {
    ORTE_PLS_BASE_VERSION_1_3_0,

    "proxy", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_pls_proxy_open,  /* module open */
    orte_pls_proxy_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  orte_pls_proxy_init    /* module init */
};

/*
 * setup the function pointers for the module
 */
static orte_pls_base_module_t orte_pls_proxy_module = {
    orte_pls_proxy_launch,
    orte_pls_proxy_terminate_job,
    orte_pls_proxy_terminate_orteds,
    orte_pls_proxy_terminate_proc,
    orte_pls_proxy_signal_job,
    orte_pls_proxy_signal_proc,
    orte_pls_proxy_cancel_operation,
    orte_pls_proxy_finalize
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/* the name of our replica */
orte_process_name_t *orte_pls_proxy_replica;

/*
 * Not much to do here.
 */
int orte_pls_proxy_open(void)
{
    return ORTE_SUCCESS;
}

/*
 * ditto for this one
 */
int orte_pls_proxy_close(void)
{
    return ORTE_SUCCESS;
}

orte_pls_base_module_t* orte_pls_proxy_init(int *priority)
{
    /* If we are an HNP, then don't pick us  */
    if (orte_process_info.seed) {
        return NULL;
    }
    
    /* define the replica for us to use - for now, just point
     * to the name service replica
     */
    orte_pls_proxy_replica = orte_process_info.ns_replica;
    
    initialized = true;
    *priority = 1;
    return &orte_pls_proxy_module;
}


/*
 * finalize routine
 */
int orte_pls_proxy_finalize(void)
{
    initialized = false;

    /* All done */

    return ORTE_SUCCESS;
}
