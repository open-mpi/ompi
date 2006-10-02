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

#include "orte/mca/ras/base/ras_private.h"
#include "orte/mca/ras/base/base.h"


/*
 * Local functions
 */
static void orte_ras_base_cmp_constructor(orte_ras_base_cmp_t *cmp);
static int compare(opal_list_item_t **a, opal_list_item_t **b);


/*
 * Global variables
 */
OBJ_CLASS_INSTANCE(orte_ras_base_cmp_t, 
                   opal_list_item_t,
                   orte_ras_base_cmp_constructor, 
                   NULL);


/*
 * Find all available RAS components and sort them according to
 * priority
 */
int orte_ras_base_find_available(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_ras_base_component_t *component;
    orte_ras_base_module_t *module;
    int priority, rc;
    orte_ras_base_cmp_t *cmp;

    orte_ras_base.ras_available_valid = false;
    
    if (orte_ras_base.ras_opened_valid) {
        OBJ_CONSTRUCT(&orte_ras_base.ras_available, opal_list_t);
        orte_ras_base.ras_available_valid = true;

        for (item = opal_list_get_first(&orte_ras_base.ras_opened);
             opal_list_get_end(&orte_ras_base.ras_opened) != item;
             item = opal_list_get_next(item)) {
            cli = (mca_base_component_list_item_t *) item;
            component = (orte_ras_base_component_t *) cli->cli_component;
            opal_output(orte_ras_base.ras_output,
                        "orte:ras:base:open: querying component %s",
                        component->ras_version.mca_component_name);

            /* Call the component's init function and see if it wants to be
               selected */

            module = component->ras_init(&priority);

            /* If we got a non-NULL module back, then the component wants
               to be considered for selection */

            if (NULL != module) {
                opal_output(orte_ras_base.ras_output,
                            "orte:ras:base:open: component %s returns priority %d",
                            component->ras_version.mca_component_name,
                            priority);

                cmp = OBJ_NEW(orte_ras_base_cmp_t);
                cmp->component = component;
                cmp->module = module;
                cmp->priority = priority;

                opal_list_append(&orte_ras_base.ras_available, &cmp->super);
            } else {
                opal_output(orte_ras_base.ras_output,
                            "orte:ras:base:open: component %s does NOT want to be considered for selection",
                            component->ras_version.mca_component_name);
            }
        }

        /* Sort the resulting available list in priority order */
        opal_list_sort(&orte_ras_base.ras_available, compare);

        /* if we are an HNP, start the receive */
        if (orte_process_info.seed) {
            if (ORTE_SUCCESS  != (rc = orte_ras_base_comm_start())) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
    }
    
    return ORTE_SUCCESS;
}



static void orte_ras_base_cmp_constructor(orte_ras_base_cmp_t *cmp)
{
    cmp->component = NULL;
    cmp->module = NULL;
    cmp->priority = -1;
}

/*
 * Need to make this an *opposite* compare (this is invoked by qsort)
 * so that we get the highest priority first (i.e., so the sort is
 * highest->lowest, not lowest->highest)
 */
static int compare(opal_list_item_t **a, opal_list_item_t **b)
{
    orte_ras_base_cmp_t *aa = *((orte_ras_base_cmp_t **) a);
    orte_ras_base_cmp_t *bb = *((orte_ras_base_cmp_t **) b);

    if (bb->priority > aa->priority) {
        return 1;
    } else if (bb->priority == aa->priority) {
        return 0;
    } else {
        return -1;
    }
}
