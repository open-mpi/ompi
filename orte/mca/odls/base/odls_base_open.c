/*
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


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"
#include "opal/util/argv.h"

#include "opal/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/plm/plm_types.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"

#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/base/odls_private.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/odls/base/static-components.h"

/*
 * Instantiate globals
 */
orte_odls_base_module_t orte_odls;


/* instance the child list object */
static void orte_odls_child_constructor(orte_odls_child_t *ptr)
{
    ptr->name = NULL;
    ptr->local_rank = ORTE_VPID_INVALID;
    ptr->pid = 0;
    ptr->app_idx = -1;
    ptr->alive = false;
    /* set the default state to "failed to start" so
     * we can correctly report should something
     * go wrong during launch
     */
    ptr->state = ORTE_PROC_STATE_FAILED_TO_START;
    ptr->exit_code = 0;
    ptr->cpu_set = 0xffffffff;
    ptr->rml_uri = NULL;

}
static void orte_odls_child_destructor(orte_odls_child_t *ptr)
{
    if (NULL != ptr->name) free(ptr->name);
    if (NULL != ptr->rml_uri) free(ptr->rml_uri);
}
OBJ_CLASS_INSTANCE(orte_odls_child_t,
                   opal_list_item_t,
                   orte_odls_child_constructor,
                   orte_odls_child_destructor);

/*
 * Framework global variables
 */
orte_odls_base_t orte_odls_base;
orte_odls_globals_t orte_odls_globals;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_odls_base_open(void)
{
    /* Debugging / verbose output.  Always have stream open, with
        verbose set by the mca open system... */
    orte_odls_globals.output = opal_output_open(NULL);

    mca_base_param_reg_int_name("odls", "base_sigkill_timeout",
                                "Time to wait for a process to die after issuing a kill signal to it",
                                false, false, 1, &orte_odls_globals.timeout_before_sigkill);

    /* initialize globals */
    OBJ_CONSTRUCT(&orte_odls_globals.mutex, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_odls_globals.cond, opal_condition_t);
    OBJ_CONSTRUCT(&orte_odls_globals.children, opal_list_t);
    
    /* Open up all available components */

    if (ORTE_SUCCESS != 
        mca_base_components_open("odls", orte_odls_globals.output,
                                    mca_odls_base_static_components, 
                                    &orte_odls_base.available_components, true)) {
        return ORTE_ERROR;
    }

    /* are there components available for use ?  - 
     * orte_odls_base.available_components is always initialized */
    if(0 < opal_list_get_size(&(orte_odls_base.available_components))) {
        orte_odls_base.components_available = true;
    } else {
        orte_odls_base.components_available = false;
    }
    
    /* All done */
    
    return ORTE_SUCCESS;
}
