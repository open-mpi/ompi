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

#include "orte/mca/rds/base/rds_private.h"
#include "rds_proxy.h"

/*
 * Struct of function pointers that need to be initialized
 */
orte_rds_base_component_t mca_rds_proxy_component = {
  {
    ORTE_RDS_BASE_VERSION_1_3_0,

    "proxy", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_rds_proxy_open,  /* module open */
    orte_rds_proxy_close /* module close */
  },
  {
    false /* checkpoint / restart */
  },
  orte_rds_proxy_init,    /* module init */
  orte_rds_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
orte_rds_base_module_t orte_rds_proxy_module = {
    orte_rds_proxy_query,
    orte_rds_base_store_resource,
};

/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/* the name of our replica */
orte_process_name_t *orte_rds_proxy_replica;

/*
 * Not much to do here.
 */
int orte_rds_proxy_open(void)
{
    return ORTE_SUCCESS;
}

/*
 * ditto for this one
 */
int orte_rds_proxy_close(void)
{
    return ORTE_SUCCESS;
}

orte_rds_base_module_t* orte_rds_proxy_init(void)
{
    /* If we are an HNP, then don't pick us  */
    if (orte_process_info.seed) {
        return NULL;
    }
    
    /* define the replica for us to use - for now, just point
     * to the name service replica
     */
    orte_rds_proxy_replica = orte_process_info.ns_replica;
    
    initialized = true;
    return &orte_rds_proxy_module;
}


/*
 * finalize routine
 */
int orte_rds_proxy_finalize(void)
{
    initialized = false;

    /* All done */

    return ORTE_SUCCESS;
}
