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
#include "mca/rds/base/base.h"


/**
 * Function for querying all loaded components.
 */
int orte_rds_base_query(void)
{
    ompi_list_item_t* item;
                                                                                                            
    /* Query all selected modules */
    for(item =  ompi_list_get_first(&orte_rds_base.rds_selected);
        item != ompi_list_get_end(&orte_rds_base.rds_selected);
        item =  ompi_list_get_next(item)) {
        orte_rds_base_selected_t* selected = (orte_rds_base_selected_t*)item;
        int rc = selected->module->query();
        if(rc != ORTE_SUCCESS)
            return rc;
    }
    return ORTE_SUCCESS;
}

