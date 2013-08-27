/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * Copyright (c) 2012      Los Alamos National Security, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <stdio.h>

#include "opal/mca/mca.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "opal/mca/base/mca_base_component_repository.h"

#include "opal/mca/db/base/base.h"

extern opal_list_t opal_db_base_components_available;

int
opal_db_base_close(void)
{
    if (NULL != opal_db.finalize) {
        opal_db.finalize();
    }
    
    mca_base_components_close(opal_db_base.output, 
                              &opal_db_base.available_components, NULL);

    OBJ_DESTRUCT(&opal_db_base.available_components);

    /* Close the framework output */
    opal_output_close (opal_db_base.output);
    opal_db_base.output = -1;

    return OPAL_SUCCESS;
}

