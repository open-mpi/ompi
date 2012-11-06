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

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "orte/mca/db/base/base.h"

extern opal_list_t orte_db_base_components_available;

int
orte_db_base_close(void)
{
    if (NULL != orte_db.finalize) {
        orte_db.finalize();
    }
    
    mca_base_components_close(orte_db_base.output, 
                              &orte_db_base.available_components, NULL);

    OBJ_DESTRUCT(&orte_db_base.available_components);

    /* Close the framework output */
    opal_output_close (orte_db_base.output);
    orte_db_base.output = -1;

    return ORTE_SUCCESS;
}

