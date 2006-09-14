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

#include <stdio.h>

#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "orte/mca/rds/base/base.h"


int orte_rds_base_finalize(void)
{
    opal_list_item_t* item;

    /* if we are using the "null" component, then do nothing */
    if (orte_rds_base.no_op_selected) {
        return ORTE_SUCCESS;
    }
    
    /* Finalize all selected modules */
    while((item = opal_list_remove_first(&orte_rds_base.rds_selected)) != NULL) {
        orte_rds_base_selected_t* selected = (orte_rds_base_selected_t*)item;
        selected->component->rds_fini();
        OBJ_RELEASE(selected);
    }
    return ORTE_SUCCESS;
}
    
int orte_rds_base_close(void)
{
    /* if we are using the "null" component, then do nothing */
    if (orte_rds_base.no_op_selected) {
        return ORTE_SUCCESS;
    }

    /* Close all remaining available components (may be one if this is a
       Open RTE program, or [possibly] multiple if this is ompi_info) */

    mca_base_components_close(orte_rds_base.rds_output, 
                              &orte_rds_base.rds_components, NULL);

    OBJ_DESTRUCT(&orte_rds_base.rds_selected);
    return ORTE_SUCCESS;
}

