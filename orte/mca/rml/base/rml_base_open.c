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
#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"
#include "orte/mca/oob/oob.h"
#include "orte/mca/oob/base/base.h"
#include "orte/mca/rml/base/base.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rml/base/static-components.h"


/*
 * Global variables
 */

orte_rml_base_t orte_rml_base;
orte_rml_module_t orte_rml;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rml_base_open(void)
{
    int id;
    int int_value;
    int rc;

    /* Initialize globals */
    OBJ_CONSTRUCT(&orte_rml_base.rml_components, opal_list_t);
    
    /* lookup common parameters */
    id = mca_base_param_reg_int_name("rml_base", "debug",
                                     "Verbosity level for the rml famework",
                                     false, false, 0, &int_value);
    orte_rml_base.rml_debug = int_value;
    if (int_value) {
        orte_rml_base.rml_output = opal_output_open(NULL);
    } else {
        orte_rml_base.rml_output = -1;
    }

    /* Open up all available components */
    if ((rc = mca_base_components_open("rml", orte_rml_base.rml_output,
                                       mca_rml_base_static_components, 
            &orte_rml_base.rml_components, true)) != ORTE_SUCCESS) {
        return rc;
    }
    return ORTE_SUCCESS;
}

