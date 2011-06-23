/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2010-2011 Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"

#include "opal/util/opal_environ.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"
#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "orte/mca/errmgr/base/static-components.h"

/*
 * Globals
 */
opal_list_t orte_errmgr_base_components_available;

orte_errmgr_base_t orte_errmgr_base;

orte_errmgr_base_component_t orte_errmgr_base_selected_component;

orte_errmgr_fault_callback_t *fault_cbfunc;

/* Public module provides a wrapper around previous functions */
orte_errmgr_base_module_t orte_errmgr = {
    NULL, /* init     */
    NULL, /* finalize */
    orte_errmgr_base_log,
    orte_errmgr_base_abort,
    orte_errmgr_base_abort_peers,
    orte_errmgr_base_update_state,
    NULL, /* predicted_fault     */
    NULL, /* suggest_map_targets */
    NULL, /* ft_event            */
    orte_errmgr_base_register_migration_warning
};

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_errmgr_base_open(void)
{
    OPAL_TRACE(5);

    /* Only pass this way once */
    if( orte_errmgr_base.initialized ) {
        return ORTE_SUCCESS;
    }

    orte_errmgr_base.output = opal_output_open(NULL);

    /*
     * Open up all available components
     */
    if (ORTE_SUCCESS != 
        mca_base_components_open("errmgr",
                                 orte_errmgr_base.output,
                                 mca_errmgr_base_static_components, 
                                 &orte_errmgr_base_components_available,
                                 true)) {
        return ORTE_ERROR;
    }
    
    orte_errmgr_base.initialized = true;
    
    return ORTE_SUCCESS;
}
