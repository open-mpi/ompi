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

#include <stdio.h>

#include "include/orte_constants.h"

#include "mca/mca.h"
#include "mca/base/base.h"

#include "mca/ras/base/base.h"


int orte_ras_base_close(void)
{
    ompi_list_item_t* item;
                                                                                                                      
    /* Finalize all available modules */
    while((item = ompi_list_remove_first(&orte_ras_base.ras_available)) != NULL) {
        orte_ras_base_cmp_t* cmp = (orte_ras_base_cmp_t*)item;
        cmp->module->finalize();
        OBJ_RELEASE(cmp);
    }
                                                                                                                      
    /* Close all remaining available components (may be one if this is a
       Open RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(orte_ras_base.ras_output, 
                              &orte_ras_base.ras_opened, NULL);
  
    OBJ_DESTRUCT(&orte_ras_base.ras_available);
    return ORTE_SUCCESS;
}

