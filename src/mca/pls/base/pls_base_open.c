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
#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pls/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * module's public mca_base_module_t struct.
 */

#include "mca/pls/base/static-components.h"


/*
 * Local functions
 */
static void cmp_constructor(orte_pls_base_cmp_t *cmp);
static void cmp_destructor(orte_pls_base_cmp_t *cmp);
static int compare(ompi_list_item_t **a, ompi_list_item_t **b);

/*
 * Global variables
 */
orte_pls_base_t orte_pls_base;
OBJ_CLASS_INSTANCE(orte_pls_base_cmp_t, ompi_list_item_t,
                   cmp_constructor, cmp_destructor);


/**
 * Function for finding and opening either all MCA modules, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_pls_base_open(void)
{
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_pls_base_component_t *component;
    orte_pls_base_module_t *module;
    int param, priority, value;
    orte_pls_base_cmp_t *cmp;

    /* Debugging / verbose output */

    param = mca_base_param_register_int("pls", "base", "verbose", NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_pls_base.pls_output = ompi_output_open(NULL);
    } else {
        orte_pls_base.pls_output = -1;
    }

    /* Open up all the components that we can find */

    if (ORTE_SUCCESS != 
        mca_base_components_open("pls", 0, mca_pls_base_static_components, 
                                 &orte_pls_base.pls_opened)) {
       return ORTE_ERROR;
    }

    /* Query all the opened components and see if they want to run */

    OBJ_CONSTRUCT(&orte_pls_base.pls_available, ompi_list_t);
    for (item = ompi_list_get_first(&orte_pls_base.pls_opened); 
         ompi_list_get_end(&orte_pls_base.pls_opened) != item; 
         item = ompi_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_pls_base_component_t *) cli->cli_component;
        ompi_output(orte_pls_base.pls_output,
                    "orte:base:open: querying component %s", 
                    component->pls_version.mca_component_name);

        /* Call the component's init function and see if it wants to be
           selected */

        module = component->pls_init(&priority);

        /* If we got a non-NULL module back, then the component wants
           to be considered for selection */

        if (NULL != module) {
            ompi_output(orte_pls_base.pls_output,
                        "orte:base:open: component %s returns priority %d", 
                        component->pls_version.mca_component_name,
                        priority);

            cmp = OBJ_NEW(orte_pls_base_cmp_t);
            cmp->component = component;
            cmp->module = module;
            cmp->priority = priority;

            ompi_list_append(&orte_pls_base.pls_available, &cmp->super);
        } else {
            ompi_output(orte_pls_base.pls_output,
                        "orte:base:open: component %s does NOT want to be considered for selection", 
                        component->pls_version.mca_component_name);
        }
    }

    /* Sort the resulting available list in priority order */

    ompi_list_sort(&orte_pls_base.pls_available, compare);
    /* All done */

    return ORTE_SUCCESS;
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


/*
 * Need to make this an *opposite* compare (this is invoked by qsort)
 * so that we get the highest priority first (i.e., so the sort is
 * highest->lowest, not lowest->highest)
 */
static int compare(ompi_list_item_t **a, ompi_list_item_t **b)
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
