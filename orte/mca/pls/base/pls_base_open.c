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
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/pls/pls.h"
#include "orte/mca/pls/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/pls/base/static-components.h"


/*
 * Global variables
 */
orte_pls_base_t orte_pls_base;

/*
 * The default module
 */
orte_pls_base_module_t orte_pls;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_pls_base_open(void)
{
    int value;
    
    /* Debugging / verbose output.  Always have stream open, with
       verbose set by the mca open system... */
    orte_pls_base.pls_output = opal_output_open(NULL);
    
    /* init selected to be false */
    orte_pls_base.selected = false;

    /* initialize the condition variables for orted comm */
    OBJ_CONSTRUCT(&orte_pls_base.orted_cmd_lock, opal_mutex_t);
    OBJ_CONSTRUCT(&orte_pls_base.orted_cmd_cond, opal_condition_t);

    /* check for reuse of daemons */
    mca_base_param_reg_int_name("pls", "base_reuse_daemons",
                                "If nonzero, reuse daemons to launch dynamically spawned processes.  If zero, do not reuse daemons (default)",
                                false, false, (int)false, &value);
    if (false == value) {
        orte_pls_base.reuse_daemons = false;
    } else {
        orte_pls_base.reuse_daemons = true;
    }
    
    /* check for timing requests */
    mca_base_param_reg_int_name("orte", "timing",
                                "Request that critical timing loops be measured",
                                false, false, 0, &value);
    if (value != 0) {
        orte_pls_base.timing = true;
    } else {
        orte_pls_base.timing = false;
    }
    
    /* Open up all the components that we can find */

    if (ORTE_SUCCESS != 
        mca_base_components_open("pls", orte_pls_base.pls_output,
                                 mca_pls_base_static_components, 
                                 &orte_pls_base.available_components, true)) {
       return ORTE_ERROR;
    }
    
    /* All done */

    return ORTE_SUCCESS;
}
