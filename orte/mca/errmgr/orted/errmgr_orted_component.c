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

#include "errmgr_orted.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_errmgr_base_component_t mca_errmgr_orted_component = {
    {
    ORTE_ERRMGR_BASE_VERSION_1_3_0,

    "orted", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_errmgr_orted_open,  /* module open */
    orte_errmgr_orted_close /* module close */
    },
    {
    false /* checkpoint / restart */
    },
    orte_errmgr_orted_component_init,    /* module init */
    orte_errmgr_orted_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
static orte_errmgr_base_module_t orte_errmgr_orted = {
    orte_errmgr_base_log,
    orte_errmgr_orted_proc_aborted,
    orte_errmgr_orted_incomplete_start,
    orte_errmgr_orted_error_detected,
    orte_errmgr_orted_register_job,
    orte_errmgr_orted_abort,
    orte_errmgr_orted_abort_procs_request
};


/*
 * Whether or not we allowed this component to be selected
 */
static bool initialized = false;

/* local globals */
orte_errmgr_orted_globals_t orte_errmgr_orted_globals;


/*
 * Open the component
 */
int orte_errmgr_orted_open(void)
{
    int id, tmp;

    id = mca_base_param_register_int("errmgr", "orted", "debug", NULL, 0);
    mca_base_param_lookup_int(id, &tmp);
    if (tmp) {
        orte_errmgr_orted_globals.debug = true;
    } else {
        orte_errmgr_orted_globals.debug = false;
    }

    return ORTE_SUCCESS;
}

/*
 * Close the component
 */
int orte_errmgr_orted_close(void)
{
    return ORTE_SUCCESS;
}

orte_errmgr_base_module_t*
orte_errmgr_orted_component_init(bool *allow_multi_user_threads, bool *have_hidden_threads,
                            int *priority)
{
    if (orte_errmgr_orted_globals.debug) {
        opal_output(0, "errmgr_orted_init called");
    }

    /* If we are not a daemon, then this component is not for us! */
    if (!orte_process_info.daemon) {
        /* don't take me! */
        return NULL;
    }
    
    /* Return a module (choose an arbitrary, positive priority --
       it's only relevant compared to other components). */

    *priority = 10;

    /* no part of OpenRTE allows or has threads */

    *allow_multi_user_threads = false;
    *have_hidden_threads = false;

    /* define the HNP we should be talking to - for now,
     * just use the NS replica
     */
    orte_errmgr_orted_globals.replica = orte_process_info.ns_replica;
    
    initialized = true;
    return &orte_errmgr_orted;
}

/*
 * finalize routine
 */
int orte_errmgr_orted_finalize(void)
{
    if (orte_errmgr_orted_globals.debug) {
       opal_output(0, "[%lu,%lu,%lu] errmgr_orted_finalize called",
                        ORTE_NAME_ARGS(orte_process_info.my_name));
    }

    initialized = false;

    /* All done */
    return ORTE_SUCCESS;
}
