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
#include "orte/orte_constants.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/mca/rds/base/rds_private.h"
#include "orte/mca/rds/base/base.h"


/**
 * Function for querying all loaded components.
 */
int orte_rds_base_query(orte_jobid_t job)
{
    opal_list_item_t* item;

    /* Query all selected modules */
    for(item =  opal_list_get_first(&orte_rds_base.rds_selected);
        item != opal_list_get_end(&orte_rds_base.rds_selected);
        item =  opal_list_get_next(item)) {
        orte_rds_base_selected_t* selected = (orte_rds_base_selected_t*)item;
        int rc = selected->module->query(job);
        if(rc != ORTE_SUCCESS)
            return rc;
    }
    return ORTE_SUCCESS;
}

