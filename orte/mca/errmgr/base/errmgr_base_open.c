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

#include "orte/mca/errmgr/base/base.h"
#include "orte/mca/errmgr/base/errmgr_private.h"

#include "orte/mca/errmgr/base/static-components.h"

/*
 * Globals
 */
int  orte_errmgr_base_output  = -1;
bool orte_errmgr_base_enable_recovery = false;
bool orte_errmgr_base_shutting_down = false;
bool orte_errmgr_initialized = false;
opal_list_t orte_errmgr_base_components_available;

orte_errmgr_base_t orte_errmgr_base;

/* Public module provides a wrapper around previous functions */
orte_errmgr_API_t orte_errmgr = {
    orte_errmgr_base_log,
    orte_errmgr_base_update_state,
    orte_errmgr_base_predicted_fault,
    orte_errmgr_base_suggest_map_targets,
    orte_errmgr_base_abort
};

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_errmgr_base_open(void)
{
    int value;

    OPAL_TRACE(5);

    /* Only pass this way once */
    if( orte_errmgr_base.initialized ) {
        return ORTE_SUCCESS;
    }

    OBJ_CONSTRUCT(&orte_errmgr_base.modules, opal_pointer_array_t);
    opal_pointer_array_init(&orte_errmgr_base.modules, 3, INT_MAX, 1);
    
    orte_errmgr_base.output = opal_output_open(NULL);

    mca_base_param_reg_int_name("errmgr",
                                "base_enable_recovery",
                                "If the ErrMgr recovery components should be enabled."
                                " [Default = disabled]",
                                false, false,
                                0, &value);
    orte_errmgr_base.enable_recovery = OPAL_INT_TO_BOOL(value);

    mca_base_param_reg_int_name("errmgr",
                                "max_global_restarts",
                                "Max number of times to relocate a failed process to a new node",
                                false, false,
                                -1, &orte_errmgr_base.max_global_restarts);
    
    mca_base_param_reg_int_name("errmgr",
                                "max_local_restarts",
                                "Max number of times to locally restart a failed process before relocating it to a new node",
                                false, false,
                                -1, &orte_errmgr_base.max_local_restarts);
    
    if (orte_errmgr_base.enable_recovery) {
        if (orte_errmgr_base.max_global_restarts < 0 ) {
            orte_errmgr_base.max_global_restarts = 3;
        }
        if (orte_errmgr_base.max_local_restarts < 0) {
            orte_errmgr_base.max_local_restarts = 3;
        }
    } else {
        if (orte_errmgr_base.max_local_restarts > 0 ||
            orte_errmgr_base.max_global_restarts > 0) {
            orte_errmgr_base.enable_recovery = true;
        }
    }
    
    /*
     * A flag to indicate that orterun is shutting down, so skip the recovery
     * logic.
     */
    orte_errmgr_base.shutting_down = false;

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
