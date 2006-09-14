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

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_proxy.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_errmgr_base_component_t mca_errmgr_proxy_component = {
    {
    ORTE_ERRMGR_BASE_VERSION_1_3_0,

    "proxy", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_errmgr_proxy_open,  /* module open */
    orte_errmgr_proxy_close /* module close */
    },
    {
    false /* checkpoint / restart */
    },
    orte_errmgr_proxy_component_init,    /* module init */
    orte_errmgr_proxy_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static orte_errmgr_base_module_t orte_errmgr_proxy = {
    orte_errmgr_base_log,
    orte_errmgr_proxy_proc_aborted,
    orte_errmgr_proxy_incomplete_start,
    orte_errmgr_proxy_error_detected,
    orte_errmgr_proxy_register_job,
    orte_errmgr_proxy_abort,
    orte_errmgr_proxy_abort_procs_request
};


/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/* local globals */
orte_errmgr_proxy_globals_t orte_errmgr_proxy_globals;

/*
 * Open the component
 */
int orte_errmgr_proxy_open(void)
{
    int id, tmp;

    id = mca_base_param_register_int("errmgr", "proxy", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_errmgr_proxy_globals.debug = true;
    } else {
        orte_errmgr_proxy_globals.debug = false;
    }

    return ORTE_SUCCESS;
}

/*
 * Close the component
 */
int orte_errmgr_proxy_close(void)
{
    return ORTE_SUCCESS;
}

orte_errmgr_base_module_t*
orte_errmgr_proxy_component_init(bool *allow_multi_user_threads, bool *have_hidden_threads,
                            int *priority)
{
    if (orte_errmgr_proxy_globals.debug) {
        opal_output(0, "errmgr_proxy_init called");
    }

    /* If we are an HNP or an orted, then don't pick us! */
    if (orte_process_info.seed || orte_process_info.daemon) {
        /* don't take me! */
        return NULL;
    }
    
    /* Return a module (choose an arbitrary, positive priority --
       it's only relevant compared to other components). */

    *priority = 10;

    /* no part of OpenRTE allows or has threads */

    *allow_multi_user_threads = false;
    *have_hidden_threads = false;

    /* define the replica for us to use - for now, just point
     * to the name service replica
     */
    orte_errmgr_proxy_globals.replica = orte_process_info.ns_replica;
    
    initialized = true;
    return &orte_errmgr_proxy;
}

/*
 * finalize routine
 */
int orte_errmgr_proxy_finalize(void)
{
    if (orte_errmgr_proxy_globals.debug) {
       opal_output(0, "[%lu,%lu,%lu] errmgr_proxy_finalize called",
                        ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    initialized = false;

    /* All done */
    return ORTE_SUCCESS;
}
