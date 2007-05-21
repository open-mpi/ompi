/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
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

#include "orte/dss/dss.h"
#include "orte/mca/errmgr/errmgr.h"
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
orte_rml_component_t orte_rml_component;

/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rml_base_open(void)
{
    int id;
    int int_value;
    int rc;
    char *rml_wrapper = NULL;
    orte_data_type_t tmp;

    /* Initialize globals */
    OBJ_CONSTRUCT(&orte_rml_base.rml_components, opal_list_t);
    
    /* lookup common parameters */
    id = mca_base_param_reg_int_name("rml_base", "debug",
                                     "Verbosity level for the rml famework",
                                     false, false, 0, &int_value);
    if (0 != int_value) {
        orte_rml_base.rml_output = opal_output_open(NULL);
    } else {
        orte_rml_base.rml_output = -1;
    }
    orte_rml_base.rml_debug = int_value;
    opal_output_set_verbosity(orte_rml_base.rml_output, int_value);

    /* register the base system types with the DPS */
    tmp = ORTE_RML_TAG;
    if (ORTE_SUCCESS != (rc = orte_dss.register_type(orte_rml_base_pack_tag,
                                                     orte_rml_base_unpack_tag,
                                                     (orte_dss_copy_fn_t)orte_rml_base_copy_tag,
                                                     (orte_dss_compare_fn_t)orte_rml_base_compare_tags,
                                                     (orte_dss_size_fn_t)orte_rml_base_size_tag,
                                                     (orte_dss_print_fn_t)orte_rml_base_print_tag,
                                                     (orte_dss_release_fn_t)orte_rml_base_std_obj_release,
                                                     ORTE_DSS_UNSTRUCTURED,
                                                     "ORTE_RML_TAG", &tmp))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    /* 
     * Which RML Wrapper component to use, if any
     *  - NULL or "" = No wrapper
     *  - ow. select that specific wrapper component
     */
    mca_base_param_reg_string_name("rml", "wrapper",
                                   "Use a Wrapper component around the selected RML component",
                                   false, false,
                                   NULL, &rml_wrapper);
    if( NULL != rml_wrapper) {
        free(rml_wrapper);
    }
    
    /* Open up all available components */
    if (ORTE_SUCCESS != (rc = mca_base_components_open("rml",
                                          orte_rml_base.rml_output,
                                          mca_rml_base_static_components, 
                                          &orte_rml_base.rml_components,
                                          true)) ) {
        return rc;
    }
    return ORTE_SUCCESS;
}

