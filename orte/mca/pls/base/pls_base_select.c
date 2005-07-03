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

#include "include/orte_constants.h"
#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pls/base/base.h"


/*
 * Local functions
 */

static orte_pls_base_module_t *select_preferred(char *name);
static orte_pls_base_module_t *select_any(void);

static int compare(opal_list_item_t **a, opal_list_item_t **b);
static void cmp_constructor(orte_pls_base_cmp_t *cmp);
static void cmp_destructor(orte_pls_base_cmp_t *cmp);


/*
 * Global variables
 */
OBJ_CLASS_INSTANCE(orte_pls_base_cmp_t, opal_list_item_t,
                   cmp_constructor, cmp_destructor);



/*
 * Function for selecting one component from all those that are
 * available.
 */
orte_pls_base_module_t* orte_pls_base_select(char *preferred)
{
    /* Construct the empty list */

    OBJ_CONSTRUCT(&orte_pls_base.pls_available, opal_list_t);
    orte_pls_base.pls_available_valid = true;

    /* Now - did we want a specific one? */

    if (NULL != preferred) {
        return select_preferred(preferred);
    } else {
        return select_any();
    }
}


static orte_pls_base_module_t *select_preferred(char *name)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_pls_base_component_t *component;
    orte_pls_base_module_t *module;
    orte_pls_base_cmp_t *cmp;
    int priority;

    /* Look for a matching selected name */

    opal_output(orte_pls_base.pls_output,
                "orte:base:select: looking for component %s", name);
    for (item = opal_list_get_first(&orte_pls_base.pls_opened);
         item != opal_list_get_end(&orte_pls_base.pls_opened);
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_pls_base_component_t *) cli->cli_component;

        /* If we found it, call the component's init function to see
           if we get a module back */

        if (0 == strcmp(name, 
                        component->pls_version.mca_component_name)) {
            opal_output(orte_pls_base.pls_output,
                        "orte:base:select: found module for compoent %s", name);
            module = component->pls_init(&priority);

            /* If we got a non-NULL module back, then the component wants
               to be considered for selection */
            
            if (NULL != module) {
                opal_output(orte_pls_base.pls_output,
                            "orte:base:open: component %s returns priority %d", 
                            component->pls_version.mca_component_name,
                            priority);

                cmp = OBJ_NEW(orte_pls_base_cmp_t);
                cmp->component = component;
                cmp->module = module;
                cmp->priority = priority;

                opal_list_append(&orte_pls_base.pls_available, &cmp->super);
                return module;
            }
        }
    }

    /* Didn't find a matching name */

    opal_output(orte_pls_base.pls_output,
                "orte:base:select: did not find module for compoent %s", name);
    return NULL;
}


static orte_pls_base_module_t *select_any(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_pls_base_component_t *component;
    orte_pls_base_module_t *module;
    orte_pls_base_cmp_t *cmp;
    int priority;

    /* Query all the opened components and see if they want to run */

    for (item = opal_list_get_first(&orte_pls_base.pls_opened); 
         opal_list_get_end(&orte_pls_base.pls_opened) != item; 
         item = opal_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_pls_base_component_t *) cli->cli_component;
        opal_output(orte_pls_base.pls_output,
                    "orte:base:open: querying component %s", 
                    component->pls_version.mca_component_name);

        /* Call the component's init function and see if it wants to be
           selected */

        module = component->pls_init(&priority);

        /* If we got a non-NULL module back, then the component wants
           to be considered for selection */

        if (NULL != module) {
            opal_output(orte_pls_base.pls_output,
                        "orte:base:open: component %s returns priority %d", 
                        component->pls_version.mca_component_name,
                        priority);

            cmp = OBJ_NEW(orte_pls_base_cmp_t);
            cmp->component = component;
            cmp->module = module;
            cmp->priority = priority;

            opal_list_append(&orte_pls_base.pls_available, &cmp->super);
        } else {
            opal_output(orte_pls_base.pls_output,
                        "orte:base:open: component %s does NOT want to be considered for selection", 
                        component->pls_version.mca_component_name);
        }
    }

    /* If the list is empty, return NULL */

    if (opal_list_is_empty(&orte_pls_base.pls_available)) {
        opal_output(orte_pls_base.pls_output,
                    "orte:base:select: no components available!");
        return NULL;
    }

    /* Sort the resulting available list in priority order */

    opal_list_sort(&orte_pls_base.pls_available, compare);

    /* Otherwise, return the first item (it's already sorted in
       priority order) */

    item = opal_list_get_first(&orte_pls_base.pls_available);
    cmp = (orte_pls_base_cmp_t *) item;
    opal_output(orte_pls_base.pls_output,
                "orte:base:select: highest priority component: %s",
                cmp->component->pls_version.mca_component_name);
    return cmp->module;
}


/*
 * Need to make this an *opposite* compare (this is invoked by qsort)
 * so that we get the highest priority first (i.e., so the sort is
 * highest->lowest, not lowest->highest)
 */
static int compare(opal_list_item_t **a, opal_list_item_t **b)
{
    orte_pls_base_cmp_t *aa = *((orte_pls_base_cmp_t **) a);
    orte_pls_base_cmp_t *bb = *((orte_pls_base_cmp_t **) b);

    if (bb->priority > aa->priority) {
        return 1;
    } else if (bb->priority == aa->priority) {
        return 0;
    } else {
        return -1;
    }
}

static void cmp_constructor(orte_pls_base_cmp_t *cmp)
{
    cmp->component = NULL;
    cmp->module = NULL;
    cmp->priority = -1;
}


static void cmp_destructor(orte_pls_base_cmp_t *cmp)
{
    cmp_constructor(cmp);
}
