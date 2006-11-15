/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#include "orte_config.h"
#include "orte/orte_constants.h"

#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <ctype.h>

#include "opal/util/argv.h"
#include "opal/util/path.h"
#include "opal/util/basename.h"
#include "opal/util/show_help.h"
#include "opal/mca/base/mca_base_param.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"

#include "orte/mca/odls/odls.h"
#include "orte/mca/pls/base/pls_private.h"

#include "orte/mca/odls/default/odls_default.h"

/* Instantiate the component globals */
orte_odls_default_globals_t orte_odls_default;


/* instance the app_context list object */
OBJ_CLASS_INSTANCE(odls_default_app_context_t,
                   opal_list_item_t,
                   NULL, NULL);


/*
 * Instantiate the public struct with all of our public information
 * and pointers to our public functions in it
 */

orte_odls_base_component_t mca_odls_default_component = {
    /* First, the mca_component_t struct containing meta information
    about the component itself */
    {
        /* Indicate that we are a odls v1.3.0 component (which also
        implies a specific MCA version) */
        
        ORTE_ODLS_BASE_VERSION_1_3_0,
        /* Component name and version */

        "default",
        ORTE_MAJOR_VERSION,
        ORTE_MINOR_VERSION,
        ORTE_RELEASE_VERSION,

        /* Component open and close functions */

        orte_odls_default_component_open,
        orte_odls_default_component_close
    },

    /* Next the MCA v1.0.0 component meta data */

    {
        /* Whether the component is checkpointable or not */

        true
    },

    /* Initialization / querying functions */

    orte_odls_default_component_init,
    orte_odls_default_finalize
};



int orte_odls_default_component_open(void)
{
    /* initialize globals */
    OBJ_CONSTRUCT(&orte_odls_default.mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_odls_default.cond, opal_condition_t);
    OBJ_CONSTRUCT(&orte_odls_default.children, opal_list_t);

    return ORTE_SUCCESS;
}


orte_odls_base_module_t *orte_odls_default_component_init(int *priority)
{
    /* the base open/select logic protects us against operation when
     * we are NOT in a daemon, so we don't have to check that here
     */
    
    /* we have built some logic into the configure.m4 file that checks
     * to see if we have "fork" support and only builds this component
     * if we do. Hence, we only get here if we CAN build - in which
     * case, we definitely should be considered for selection
     */
    *priority = 1; /* let others override us - we are the default */
    
    return &orte_odls_default_module;
}


int orte_odls_default_component_close(void)
{
    OBJ_DESTRUCT(&orte_odls_default.mutex);
    OBJ_DESTRUCT(&orte_odls_default.cond);
    OBJ_DESTRUCT(&orte_odls_default.children);
    return ORTE_SUCCESS;
}

int orte_odls_default_finalize(void)
{
    opal_list_item_t *item;
    
    /* cleanup state */
    while (NULL != (item = opal_list_remove_first(&orte_odls_default.children))) {
        OBJ_RELEASE(item);
    }
    
    return ORTE_SUCCESS;
}
