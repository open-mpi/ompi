/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */


#include "orte_config.h"
#include "orte/constants.h"

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/dss/dss_types.h"

#include "orte/mca/db/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * dbments and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "orte/mca/db/base/static-components.h"

orte_db_base_module_t orte_db;
orte_db_base_t orte_db_base;

int orte_db_base_open(void)
{
    orte_db_base.output = opal_output_open(NULL);
    
    OBJ_CONSTRUCT(&orte_db_base.available_components, opal_list_t);

    /* Open up all available components */
    if (ORTE_SUCCESS != 
        mca_base_components_open("db", orte_db_base.output, mca_db_base_static_components, 
                                 &orte_db_base.available_components,
                                 true)) {
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}
