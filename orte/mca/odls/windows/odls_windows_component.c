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
#include "orte/mca/odls/odls.h"
#include "orte/mca/odls/process/odls_process.h"


/*
 * Public string showing the odls ompi_process component version number
 */
const char *mca_odls_process_component_version_string =
  "Open MPI process odls MCA component version " ORTE_VERSION;


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_odls_process_component_t mca_odls_process_component = {
    {
    /* First, the mca_component_t struct containing meta information
       about the component itself */

    {
        /* Indicate that we are a odls v1.0.0 component (which also
           implies a specific MCA version) */

        ORTE_ODLS_BASE_VERSION_1_0_0,

        /* Component name and version */

        "process",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */

        orte_odls_process_component_open,
        orte_odls_process_component_close
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        true
    },

    /* Initialization / querying functions */

    orte_odls_process_component_init
    }
};



int orte_odls_process_component_open(void)
{
    mca_base_component_t *c = &mca_odls_process_component.super.odls_version;

    /* initialize globals */
    OBJ_CONSTRUCT(&mca_odls_process_component.lock, opal_mutex_t);
    OBJ_CONSTRUCT(&mca_odls_process_component.cond, opal_condition_t);

    /* lookup parameters */
    mca_base_param_reg_int(c, "reap", 
                           "Whether to wait to reap all children before finalizing or not",
                           false, false, 1, &mca_odls_process_component.reap);
    mca_base_param_reg_int(c, "reap_timeout", 
                           "When killing children processes, first send a SIGTERM, then wait at least this timeout (in seconds), then send a SIGKILL",
                           false, false, 0, &mca_odls_process_component.timeout_before_sigkill);
    mca_base_param_reg_int(c, "priority", 
                           "Priority of this component",
                           false, false, 1, &mca_odls_process_component.priority);
    mca_base_param_reg_int(c, "debug", 
                           "Whether to enable debugging output or not",
                           false, false, 0, &mca_odls_process_component.debug);
    if (mca_odls_process_component.debug == 0) {
        int id = mca_base_param_register_int("debug",NULL,NULL,NULL,0);
        int value;
        mca_base_param_lookup_int(id,&value);
        mca_odls_process_component.debug = (value > 0) ? 1 : 0;
    }
    return ORTE_SUCCESS;
}


orte_odls_base_module_t *orte_odls_process_component_init(int *priority)
{
    /* Only return a module if we're in the orted */
#if 0
    if (orte_process_info.daemon) {
        *priority = mca_odls_process_component.priority;
        return &orte_odls_process_module;
    } else {
        return NULL;
    }
#endif
    *priority = mca_odls_process_component.priority;
    return &orte_odls_process_module;
}


int orte_odls_process_component_close(void)
{
    OBJ_DESTRUCT(&mca_odls_process_component.lock);
    OBJ_DESTRUCT(&mca_odls_process_component.cond);
    return ORTE_SUCCESS;
}

