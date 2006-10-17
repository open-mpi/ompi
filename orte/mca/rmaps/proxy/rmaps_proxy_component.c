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
 * The Open MPI General Purpose Registry - Proxy component
 *
 */

/*
 * includes
 */
#include "orte_config.h"

#include "orte/orte_constants.h"
#include "orte/orte_types.h"

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/util/proc_info.h"
#include "orte/mca/ns/ns_types.h"

#include "orte/mca/rmaps/rmaps.h"
#include "orte/mca/rmaps/base/rmaps_private.h"

#include "rmaps_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
orte_rmaps_base_component_t mca_rmaps_proxy_component = {
    {
    ORTE_RMAPS_BASE_VERSION_1_3_0,

    "proxy", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_rmaps_proxy_open,  /* module open */
    orte_rmaps_proxy_close /* module close */
    },
    {
    false /* checkpoint / restart */
    },
    orte_rmaps_proxy_component_init    /* module init */
};

/*
 * setup the function pointers for the module
 */
static orte_rmaps_base_module_t orte_rmaps_proxy = {
    orte_rmaps_proxy_map,
    orte_rmaps_base_get_job_map,
    orte_rmaps_base_get_node_map,
    orte_rmaps_proxy_finalize
};


/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/* local globals */
orte_rmaps_proxy_globals_t orte_rmaps_proxy_globals;

/*
 * Open the component
 */
int orte_rmaps_proxy_open(void)
{
    int id, tmp;

    id = mca_base_param_register_int("rmaps", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_rmaps_proxy_globals.debug = true;
    } else {
        orte_rmaps_proxy_globals.debug = false;
    }

    return ORTE_SUCCESS;
}

/*
 * Close the component
 */
int orte_rmaps_proxy_close(void)
{
    return ORTE_SUCCESS;
}

orte_rmaps_base_module_t*
orte_rmaps_proxy_component_init(int *priority)
{
    if (orte_rmaps_proxy_globals.debug) {
        opal_output(0, "rmaps_proxy_init called");
    }

    /* If we are an HNP or an orted, then don't pick us! */
    if (orte_process_info.seed || orte_process_info.daemon) {
        /* don't take me! */
        return NULL;
    }
    
    /* Return a module (choose an arbitrary, positive priority --
       it's only relevant compared to other components). */

    *priority = 10;

    /* define the replica for us to use - for now, just point
     * to the name service replica
     */
    orte_rmaps_proxy_globals.replica = orte_process_info.ns_replica;
    
    initialized = true;
    return &orte_rmaps_proxy;
}

/*
 * finalize routine
 */
int orte_rmaps_proxy_finalize(void)
{
    if (orte_rmaps_proxy_globals.debug) {
       opal_output(0, "[%lu,%lu,%lu] rmaps_proxy_finalize called",
                        ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    initialized = false;

    /* All done */
    return ORTE_SUCCESS;
}
