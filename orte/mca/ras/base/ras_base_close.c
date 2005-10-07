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

#include <stdio.h>

#include "include/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/ras/base/base.h"


int orte_ras_base_finalize(void)
{
    opal_list_item_t* item;

    /* Finalize all available modules */
    if (orte_ras_base.ras_available_valid) {
        while (NULL != 
               (item = opal_list_remove_first(&orte_ras_base.ras_available))) {
            orte_ras_base_cmp_t* cmp = (orte_ras_base_cmp_t*)item;
            cmp->module->finalize();
            OBJ_RELEASE(cmp);
        }
        OBJ_DESTRUCT(&orte_ras_base.ras_available);
    }

    return ORTE_SUCCESS;
}


int orte_ras_base_close(void)
{
    /* Close all remaining available components (may be one if this is a
       Open RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(orte_ras_base.ras_output, 
                              &orte_ras_base.ras_opened, NULL);
  
    return ORTE_SUCCESS;
}

