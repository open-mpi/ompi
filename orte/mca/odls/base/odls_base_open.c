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
#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "opal/util/trace.h"

#include "orte/dss/dss.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/smr/smr_types.h"

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
    ptr->pid = 0;
    ptr->app_idx = -1;
    ptr->alive = false;
    ptr->state = ORTE_PROC_STATE_UNDEF;
}
static void orte_odls_child_destructor(orte_odls_child_t *ptr)
{
    if (NULL != ptr->name) free(ptr->name);
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
    int param, value, rc;
    orte_data_type_t tmp;

    OPAL_TRACE(5);
    
    /* Debugging / verbose output */
    
    param = mca_base_param_reg_int_name("odls", "base_verbose",
                                        "Verbosity level for the odls framework",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_odls_globals.output = opal_output_open(NULL);
    } else {
        orte_odls_globals.output = -1;
    }

    mca_base_param_reg_int_name("odls", "base_sigkill_timeout",
                                "Time to wait for a process to die after issuing a kill signal to it",
                                false, false, 1, &orte_odls_globals.timeout_before_sigkill);
    
    /* register the daemon cmd data type */
    tmp = ORTE_DAEMON_CMD;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_odls_pack_daemon_cmd,
                                                     orte_odls_unpack_daemon_cmd,
                                                     (orte_dss_copy_fn_t)orte_odls_copy_daemon_cmd,
                                                     (orte_dss_compare_fn_t)orte_odls_compare_daemon_cmd,
                                                     (orte_dss_size_fn_t)orte_odls_size_daemon_cmd,
                                                     (orte_dss_print_fn_t)orte_odls_print_daemon_cmd,
                                                     (orte_dss_release_fn_t)orte_odls_std_release,
                                                     ORTE_DSS_UNSTRUCTURED,
                                                     "ORTE_DAEMON_CMD", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
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
