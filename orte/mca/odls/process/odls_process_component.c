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

/* Instantiate the component globals */
orte_odls_process_globals_t orte_odls_process;


/* instance the app_context list object */
OBJ_CLASS_INSTANCE(odls_process_app_context_t,
                   opal_list_item_t,
                   NULL, NULL);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_odls_base_component_t mca_odls_process_component = {
    /* First, the mca_component_t struct containing meta information
    about the component itself */
    {
        /* Indicate that we are a odls v1.3.0 component (which also
        implies a specific MCA version) */
        
        ORTE_ODLS_BASE_VERSION_1_3_0,
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

    orte_odls_process_component_init,
    orte_odls_process_component_finalize
};

bool orte_odls_process_debug = false;

int orte_odls_process_component_open(void)
{
    int tmp;
    /* initialize globals */
    OBJ_CONSTRUCT(&orte_odls_process.mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_odls_process.cond, opal_condition_t);
    OBJ_CONSTRUCT(&orte_odls_process.children, opal_list_t);

    /* lookup parameters */
    mca_base_param_reg_int( &mca_odls_process_component.version, "debug",
                           "Whether or not to enable debugging output for the process odls component (0 or 1)",
                           false, false, false, &tmp);
    orte_odls_process_debug = OPAL_INT_TO_BOOL(tmp);
    return ORTE_SUCCESS;
}

orte_odls_base_module_t *orte_odls_process_component_init(int *priority)
{
    /* the base open/select logic protects us against operation when
     * we are NOT in a daemon, so we don't have to check that here
     */
    
    *priority = 1;
    
    return &orte_odls_process_module;
}

int orte_odls_process_component_close(void)
{
    OBJ_DESTRUCT(&orte_odls_process.mutex);
    OBJ_DESTRUCT(&orte_odls_process.cond);
    OBJ_DESTRUCT(&orte_odls_process.children);
    return ORTE_SUCCESS;
}

int orte_odls_process_component_finalize(void)
{
    opal_list_item_t *item;
    
    /* cleanup state */
    while (NULL != (item = opal_list_remove_first(&orte_odls_process.children))) {
        OBJ_RELEASE(item);
    }
    
    return ORTE_SUCCESS;
}
