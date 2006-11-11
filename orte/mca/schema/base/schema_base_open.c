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
 */

#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_constants.h"

#include "opal/util/output.h"

#include "orte/mca/schema/base/base.h"



/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_module_t struct.
 */

#include "orte/mca/schema/base/static-components.h"

/*
 * globals
 */

orte_schema_base_module_t orte_schema = {
    orte_schema_base_get_proc_tokens,
    orte_schema_base_get_node_tokens,
    orte_schema_base_get_job_tokens,
    orte_schema_base_get_cell_tokens,
    orte_schema_base_get_job_segment_name,
    orte_schema_base_extract_jobid_from_segment_name,
    orte_schema_base_get_std_trigger_name,
    orte_schema_base_check_std_trigger_name,
    orte_schema_base_extract_jobid_from_std_trigger_name,
    orte_schema_base_get_std_subscription_name
};



/*
 * Global variables for framework
 */
int orte_schema_base_output;
bool orte_schema_base_selected = false;
opal_list_t orte_schema_base_components_available;
mca_schema_base_component_t orte_schema_base_selected_component;
bool orte_schema_initialized = false;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_schema_base_open(void)
{
    int param, value;

    if (!orte_schema_initialized) {
        /* Debugging / verbose output */
        
        param = mca_base_param_reg_int_name("schema_base", "verbose",
                                            "Verbosity level for the schema framework",
                                            false, false, 0, &value);
        if (value != 0) {
            orte_schema_base_output = opal_output_open(NULL);
        } else {
            orte_schema_base_output = -1;
        }
    
        /* Open up all available components */
    
        if (ORTE_SUCCESS != 
            mca_base_components_open("schema", orte_schema_base_output,
                                     mca_schema_base_static_components, 
                                     &orte_schema_base_components_available, true)) {
            return ORTE_ERROR;
        }
    
        orte_schema_initialized = true;
    }
    
    /* All done */
    
    return ORTE_SUCCESS;
}
