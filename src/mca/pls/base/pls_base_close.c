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

#include "ompi_config.h"

#include <stdio.h>

#include "include/orte_constants.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pls/base/base.h"


int orte_pls_base_close(void)
{
    ompi_list_item_t* item;

    /* Finalize all available modules */

    while (NULL != 
           (item = ompi_list_remove_first(&orte_pls_base.pls_available))) {
        orte_pls_base_cmp_t* cmp = (orte_pls_base_cmp_t*) item;
        ompi_output(orte_pls_base.pls_output,
                    "orte:base:close: finalizing module %s",
                    cmp->component->pls_version.mca_component_name);
        if (NULL != cmp->module->finalize) {
            cmp->module->finalize();
        }
        OBJ_RELEASE(cmp);
    }

    /* Close all remaining open components */

    mca_base_components_close(orte_pls_base.pls_output, 
                              &orte_pls_base.pls_opened, NULL);

    return ORTE_SUCCESS;
}

