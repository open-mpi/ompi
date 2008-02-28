/* -*- C -*-
 *
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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
#include "orte/constants.h"
#include "orte/types.h"

#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/rml/rml.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "errmgr_default.h"


/*
 * Struct of function pointers that need to be initialized
 */
mca_errmgr_base_component_t mca_errmgr_default_component = {
    {
    ORTE_ERRMGR_BASE_VERSION_1_3_0,

    "default", /* MCA module name */
    ORTE_MAJOR_VERSION,  /* MCA module major version */
    ORTE_MINOR_VERSION,  /* MCA module minor version */
    ORTE_RELEASE_VERSION,  /* MCA module release version */
    orte_errmgr_default_open,  /* module open */
    orte_errmgr_default_close /* module close */
    },
    {
        /* The component is checkpoint ready */
        MCA_BASE_METADATA_PARAM_CHECKPOINT
    },
    orte_errmgr_default_component_init,    /* module init */
    orte_errmgr_default_finalize /* module shutdown */
};

/*
 * setup the function pointers for the module
 */
orte_errmgr_base_module_t orte_errmgr_default = {
    orte_errmgr_default_proc_aborted,
    orte_errmgr_default_incomplete_start,
    orte_errmgr_default_register_callback,
    orte_errmgr_base_error_abort
};


/*
 * Open the component
 */
int orte_errmgr_default_open(void)
{
    return ORTE_SUCCESS;
}

/*
 * Close the component
 */
int orte_errmgr_default_close(void)
{
    return ORTE_SUCCESS;
}

orte_errmgr_base_module_t*
orte_errmgr_default_component_init(int *priority)
{
    /* If we are not an HNP, then don't pick us! */
    if (!orte_process_info.hnp) {
        /* don't take me! */
        return NULL;
    }
    
    /* Return a module (choose an arbitrary, positive priority --
       it's only relevant compared to other components). */

    *priority = 10;

    return &orte_errmgr_default;
}

/*
 * finalize routine
 */
int orte_errmgr_default_finalize(void)
{
    /* All done */
    return ORTE_SUCCESS;
}
