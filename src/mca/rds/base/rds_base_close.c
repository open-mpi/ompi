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
#include "mca/rds/base/base.h"


int orte_rds_base_close(void)
{
    ompi_list_item_t* item;

    /* Finalize all selected modules */
    while((item = ompi_list_remove_first(&orte_rds_base.rds_selected)) != NULL) {
        orte_rds_base_selected_t* selected = (orte_rds_base_selected_t*)item;
        selected->module->finalize();
        OBJ_RELEASE(selected);
    }
    
    /* Close all remaining available components (may be one if this is a
       Open RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(orte_rds_base.rds_output, 
                              &orte_rds_base.rds_components, NULL);

    OBJ_DESTRUCT(&orte_rds_base.rds_selected);
    return ORTE_SUCCESS;
}

