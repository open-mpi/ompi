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
#include "opal/util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pls/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/pls/base/static-components.h"


/*
 * Global variables
 */
orte_pls_base_t orte_pls_base;


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_pls_base_open(void)
{
    int param, value;

    /* Debugging / verbose output */

    param = mca_base_param_register_int("pls", "base", "verbose", NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_pls_base.pls_output = opal_output_open(NULL);
    } else {
        orte_pls_base.pls_output = -1;
    }

    orte_pls_base.pls_opened_valid = false;
    orte_pls_base.pls_available_valid = false;

    /* Open up all the components that we can find */

    if (ORTE_SUCCESS != 
        mca_base_components_open("pls", 0, mca_pls_base_static_components, 
                                 &orte_pls_base.pls_opened, true)) {
       return ORTE_ERROR;
    }
    orte_pls_base.pls_opened_valid = true;

    /* All done */

    return ORTE_SUCCESS;
}
