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

#include "mca/rds/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/rds/base/static-components.h"

/*
 * globals
 */

/*
 * Global variables
 */
orte_rds_base_t orte_rds_base;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rds_base_open(void)
{
    int param, value;

    /* Debugging / verbose output */

    orte_rds_base.rds_output = ompi_output_open(NULL);
    param = mca_base_param_register_int("rds", "base", "verbose", NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_rds_base.rds_output = ompi_output_open(NULL);
    } else {
        orte_rds_base.rds_output = -1;
    }

    /* Open up all available components */

    if (ORTE_SUCCESS != 
        mca_base_components_open("rds", 0, mca_rds_base_static_components, 
                                 &orte_rds_base.rds_components)) {
        return ORTE_ERROR;
    }
    OBJ_CONSTRUCT(&orte_rds_base.rds_selected, ompi_list_t);

    /* All done */

    return ORTE_SUCCESS;
}

