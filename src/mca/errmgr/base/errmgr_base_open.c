/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "include/orte_constants.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/base/mca_base_param.h"
#include "util/output.h"

#include "mca/errmgr/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/errmgr/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
int orte_errmgr_base_output = -1;
orte_errmgr_base_module_t orte_errmgr = {
    orte_errmgr_base_log,
    orte_errmgr_base_proc_aborted,
    orte_errmgr_base_incomplete_start,
    orte_errmgr_base_error_detected,
    orte_errmgr_base_register_job,
    orte_errmgr_base_abort
};
bool orte_errmgr_base_selected = false;
ompi_list_t orte_errmgr_base_components_available;
mca_errmgr_base_component_t orte_errmgr_base_selected_component;
bool orte_errmgr_initialized = false;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_errmgr_base_open(void)
{
    if (!orte_errmgr_initialized) { /* ensure we only do this once */
      
        /* Open up all available components */
    
        if (ORTE_SUCCESS != 
            mca_base_components_open("errmgr", 0, mca_errmgr_base_static_components, 
                                   &orte_errmgr_base_components_available)) {
            return ORTE_ERROR;
        }
    
        /* setup output for debug messages */
        if (!ompi_output_init) {  /* can't open output */
            return ORTE_ERROR;
        }
    
        if (0 > orte_errmgr_base_output)
            orte_errmgr_base_output = ompi_output_open(NULL);
      
        orte_errmgr_initialized = true;
    }
    
  /* All done */

  return ORTE_SUCCESS;
}
