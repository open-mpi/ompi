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
#include "orte/mca/ras/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/ras/base/static-components.h"


/*
 * Global variables
 */
orte_ras_base_module_t orte_ras;
orte_ras_base_t orte_ras_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_ras_base_open(void)
{
    int value;

    /* Debugging / verbose output */

    orte_ras_base.ras_output = opal_output_open(NULL);
    mca_base_param_reg_int_name("ras_base", "verbose", 
                                "Enable debugging for the RAS framework (nonzero = enabled)",
                                false, false, 0, &value);
    if (value != 0) {
        orte_ras_base.ras_output = opal_output_open(NULL);
    } else {
        orte_ras_base.ras_output = -1;
    }

    /* Defaults */

    orte_ras_base.ras_opened_valid = false;
    orte_ras_base.ras_available_valid = false;

    /* Open up all available components */

    if (ORTE_SUCCESS != 
        mca_base_components_open("ras", orte_ras_base.ras_output,
                                 mca_ras_base_static_components, 
                                 &orte_ras_base.ras_opened, true)) {
        return ORTE_ERROR;
    }

    /* All done */

    orte_ras_base.ras_opened_valid = true;
    return ORTE_SUCCESS;
}

