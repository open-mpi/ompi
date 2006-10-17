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
#include "opal/util/output.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rds/base/rds_private.h"
#include "orte/mca/rds/base/base.h"


OBJ_CLASS_INSTANCE(orte_rds_base_selected_t,
                   opal_list_item_t, NULL, NULL);


/**
 * Function for selecting all available modules.
 */
int orte_rds_base_select(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_rds_base_component_t *component;
    orte_rds_base_module_t *module = NULL;
    int rc;

    /* if we are using the "null" component, then do nothing */
    if (orte_rds_base.no_op_selected) {
        return ORTE_SUCCESS;
    }

    /* Iterate through all the available components */
  
    for (item = opal_list_get_first(&orte_rds_base.rds_components);
         item != opal_list_get_end(&orte_rds_base.rds_components);
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rds_base_component_t *) cli->cli_component;

        /* Call the component's init function and see if it wants to be
         * selected
         */
        module = component->rds_init();

        /* If we got a non-NULL module back, then the component wants to
         * be selected.   
         */
        if (NULL != module) {
            orte_rds_base_selected_t* selected = OBJ_NEW(orte_rds_base_selected_t);
            selected->module = module;
            selected->component = component;
            opal_list_append(&orte_rds_base.rds_selected, &selected->super);
        } 
    }

    if (opal_list_is_empty(&orte_rds_base.rds_selected)) {
        opal_output(orte_rds_base.rds_output,
                    "rda:select: no components available!");
        return ORTE_ERROR;
    }

    /* if we are an HNP, issue non-blocking receive for call_back function */
    if (orte_process_info.seed) {
        if (ORTE_SUCCESS != (rc = orte_rds_base_comm_start())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    return ORTE_SUCCESS;
}

