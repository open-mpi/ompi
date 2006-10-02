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

#include <string.h>

#include "orte/orte_constants.h"
#include "opal/util/output.h"
#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"

#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/mca/rmaps/base/rmaps_private.h"
#include "orte/mca/rmaps/base/base.h"


/*
 * Local functions
 */
static void cmp_constructor(orte_rmaps_base_cmp_t *cmp);
static void cmp_destructor(orte_rmaps_base_cmp_t *cmp);
OBJ_CLASS_INSTANCE(orte_rmaps_base_cmp_t, opal_list_item_t,
                   cmp_constructor, cmp_destructor);

static int compare(opal_list_item_t **a, opal_list_item_t **b);


/*
 * Function for selecting one component from all those that are
 * available.
 */
int orte_rmaps_base_find_available(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_rmaps_base_component_t *component;
    orte_rmaps_base_module_t *module;
    orte_rmaps_base_cmp_t *cmp;
    int priority, rc;
    
    /* construct the list to hold any available components */
    OBJ_CONSTRUCT(&orte_rmaps_base.rmaps_available, opal_list_t);

    /* Query all the opened components and see if they want to run */
    for (item = opal_list_get_first(&orte_rmaps_base.rmaps_opened); 
         opal_list_get_end(&orte_rmaps_base.rmaps_opened) != item; 
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rmaps_base_component_t *) cli->cli_component;
        opal_output(orte_rmaps_base.rmaps_output,
                    "orte:base:open: querying component %s", 
                    component->rmaps_version.mca_component_name);
        
        /* Call the component's init function and see if it wants to be
            selected */
        
        module = component->rmaps_init(&priority);
        
        /* If we got a non-NULL module back, then the component wants
            to be considered for selection */
        
        if (NULL != module) {
            opal_output(orte_rmaps_base.rmaps_output,
                        "orte:base:open: component %s returns priority %d", 
                        component->rmaps_version.mca_component_name,
                        priority);
            
            cmp = OBJ_NEW(orte_rmaps_base_cmp_t);
            cmp->component = component;
            cmp->module = module;
            cmp->priority = priority;
            
            opal_list_append(&orte_rmaps_base.rmaps_available, &cmp->super);
        } else {
            opal_output(orte_rmaps_base.rmaps_output,
                        "orte:base:open: component %s does NOT want to be considered for selection", 
                        component->rmaps_version.mca_component_name);
        }
    }
    
    /* Sort the resulting available list in priority order */
    opal_list_sort(&orte_rmaps_base.rmaps_available, compare);

    /* if we are an HNP, start the receive function */
    if (orte_process_info.seed) {
        if (ORTE_SUCCESS != (rc = orte_rmaps_base_comm_start())) {
            return rc;
        }
    }

    /* all done */
    return ORTE_SUCCESS;
}

static void cmp_constructor(orte_rmaps_base_cmp_t *cmp)
{
    cmp->component = NULL;
    cmp->module = NULL;
    cmp->priority = -1;
}


static void cmp_destructor(orte_rmaps_base_cmp_t *cmp)
{
    cmp_constructor(cmp);
}


static int compare(opal_list_item_t **a, opal_list_item_t **b)
{
    orte_rmaps_base_cmp_t *aa = *((orte_rmaps_base_cmp_t **) a);
    orte_rmaps_base_cmp_t *bb = *((orte_rmaps_base_cmp_t **) b);
    
    if (aa->priority > bb->priority) {
        return 1;
    } else if (aa->priority == bb->priority) {
        return 0;
    } else {
        return -1;
    }
}
