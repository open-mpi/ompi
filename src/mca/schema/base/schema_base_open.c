/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
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
#include "include/orte_constants.h"
#include "include/constants.h"

#include "util/output.h"

#include "mca/schema/base/base.h"



/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_module_t struct.
 */

#include "mca/schema/base/static-components.h"

/*
 * globals
 */

orte_schema_base_module_t orte_schema = {
    orte_schema_base_get_proc_tokens,
    orte_schema_base_get_node_tokens,
    orte_schema_base_get_cell_tokens,
    orte_schema_base_get_job_segment_name,
    orte_schema_base_extract_jobid_from_segment_name
};



/*
 * Global variables for framework
 */
int orte_schema_base_output;
orte_schema_base_module_t orte_schema;
bool orte_schema_base_selected = false;
ompi_list_t orte_schema_base_components_available;
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
        
        param = mca_base_param_register_int("schema", "base", "verbose",
                                            NULL, 0);
        mca_base_param_lookup_int(param, &value);
        if (value != 0) {
            orte_schema_base_output = ompi_output_open(NULL);
        } else {
            orte_schema_base_output = -1;
        }
    
        /* Open up all available components */
    
        if (OMPI_SUCCESS != 
            mca_base_components_open("schema", 0, mca_schema_base_static_components, 
                                     &orte_schema_base_components_available, true)) {
            return ORTE_ERROR;
        }
    
        orte_schema_initialized = true;
    }
    
    /* All done */
    
    return ORTE_SUCCESS;
}
