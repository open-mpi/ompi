/*
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/mca/base/mca_base_param.h"
#include "orte/util/proc_info.h"
#include "orte/orte_constants.h"
#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/process/pls_process.h"


/*
 * Public string showing the pls ompi_process component version number
 */
const char *mca_pls_process_component_version_string =
  "Open MPI process pls MCA component version " ORTE_VERSION;


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_pls_process_component_t mca_pls_process_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a pls v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_PLS_BASE_VERSION_1_0_0,

        /* Component name and version */

        "process",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */

        orte_pls_process_component_open,
        orte_pls_process_component_close
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        true
    },

    /* Initialization / querying functions */

    orte_pls_process_component_init
    }
};



int orte_pls_process_component_open(void)
{
    mca_base_component_t *c = &mca_pls_process_component.super.pls_version;

    /* initialize globals */
    OBJ_CONSTRUCT(&mca_pls_process_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_pls_process_component.cond, opal_condition_t);

    /* lookup parameters */
    mca_base_param_reg_int(c, "reap", 
                           "Whether to wait to reap all children before finalizing or not",
                           false, false, 1, &mca_pls_process_component.reap);
    mca_base_param_reg_int(c, "reap_timeout", 
                           "When killing children processes, first send a SIGTERM, then wait at least this timeout (in seconds), then send a SIGKILL",
                           false, false, 0, &mca_pls_process_component.timeout_before_sigkill);
    mca_base_param_reg_int(c, "priority", 
                           "Priority of this component",
                           false, false, 1, &mca_pls_process_component.priority);
    mca_base_param_reg_int(c, "debug", 
                           "Whether to enable debugging output or not",
                           false, false, 0, &mca_pls_process_component.debug);
    if (mca_pls_process_component.debug == 0) {
        int id = mca_base_param_register_int("debug",NULL,NULL,NULL,0);
        int value;
        mca_base_param_lookup_int(id,&value);
        mca_pls_process_component.debug = (value > 0) ? 1 : 0;
    }
    return ORTE_SUCCESS;
}


orte_pls_base_module_t *orte_pls_process_component_init(int *priority)
{
    /* Only return a module if we're in the orted */
#if 0
    if (orte_process_info.daemon) {
        *priority = mca_pls_process_component.priority;
        return &orte_pls_process_module;
    } else {
        return NULL;
    }
#endif
    *priority = mca_pls_process_component.priority;
    return &orte_pls_process_module;
}


int orte_pls_process_component_close(void)
{
    OBJ_DESTRUCT(&mca_pls_process_component.lock);
    OBJ_DESTRUCT(&mca_pls_process_component.cond);
    return ORTE_SUCCESS;
}

