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


OBJ_CLASS_INSTANCE(orte_rds_base_selected_t,
                   ompi_list_item_t, NULL, NULL);


/**
 * Function for selecting all available modules.
 */
int orte_rds_base_select(void)
{
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_rds_base_component_t *component;
    orte_rds_base_module_t *module;

    /* Iterate through all the available components */
  
    for (item = ompi_list_get_first(&orte_rds_base.rds_components);
         item != ompi_list_get_end(&orte_rds_base.rds_components);
         item = ompi_list_get_next(item)) {
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
            ompi_list_append(&orte_rds_base.rds_selected, &selected->super);
        } 
    }

    if (ompi_list_is_empty(&orte_rds_base.rds_selected)) {
        ompi_output(orte_rds_base.rds_output,
                    "rda:select: no components available!");
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

