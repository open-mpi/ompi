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
#include "mca/base/mca_base_param.h"
#include "util/output.h"
#include "mca/ras/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/ras/base/static-components.h"

/**
 * Local functions.
 */

static void orte_ras_base_cmp_constructor(orte_ras_base_cmp_t *cmp)
{
    cmp->component = NULL;
    cmp->module = NULL;
    cmp->priority = -1;
}

static void orte_ras_base_cmp_destructor(orte_ras_base_cmp_t *cmp)
{
}

/*
 * Need to make this an *opposite* compare (this is invoked by qsort)
 * so that we get the highest priority first (i.e., so the sort is
 * highest->lowest, not lowest->highest)
 */
static int compare(ompi_list_item_t **a, ompi_list_item_t **b)
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


/*
 * Global variables
 */
orte_ras_base_t orte_ras_base;
OBJ_CLASS_INSTANCE(
    orte_ras_base_cmp_t, 
    ompi_list_item_t,
    orte_ras_base_cmp_constructor, 
    orte_ras_base_cmp_destructor);


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_ras_base_open(void)
{
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_ras_base_component_t *component;
    orte_ras_base_module_t *module;
    int param, priority, value;
    orte_ras_base_cmp_t *cmp;

    /* Debugging / verbose output */

    orte_ras_base.ras_output = ompi_output_open(NULL);
    param = mca_base_param_register_int("ras", "base", "verbose", NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_ras_base.ras_output = ompi_output_open(NULL);
    } else {
        orte_ras_base.ras_output = -1;
    }

    /* Open up all available components */

    if (ORTE_SUCCESS != 
        mca_base_components_open("ras", 0, mca_ras_base_static_components, 
                                 &orte_ras_base.ras_opened)) {
        return ORTE_ERROR;
    }
    OBJ_CONSTRUCT(&orte_ras_base.ras_available, ompi_list_t);

    for (item = ompi_list_get_first(&orte_ras_base.ras_opened);
         ompi_list_get_end(&orte_ras_base.ras_opened) != item;
         item = ompi_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_ras_base_component_t *) cli->cli_component;
        ompi_output(orte_ras_base.ras_output,
                    "orte:base:open: querying component %s",
                    component->ras_version.mca_component_name);

        /* Call the component's init function and see if it wants to be
           selected */

        module = component->ras_init(&priority);

        /* If we got a non-NULL module back, then the component wants
           to be considered for selection */

        if (NULL != module) {
            ompi_output(orte_ras_base.ras_output,
                        "orte:base:open: component %s returns priority %d",
                        component->ras_version.mca_component_name,
                        priority);
                                                                                                                                    
            cmp = OBJ_NEW(orte_ras_base_cmp_t);
            cmp->component = component;
            cmp->module = module;
            cmp->priority = priority;

            ompi_list_append(&orte_ras_base.ras_available, &cmp->super);
        } else {
            ompi_output(orte_ras_base.ras_output,
                        "orte:base:open: component %s does NOT want to be considered for selection",
                        component->ras_version.mca_component_name);
        }
    }

    /* Sort the resulting available list in priority order */
    ompi_list_sort(&orte_ras_base.ras_available, compare);

    /* All done */
    return ORTE_SUCCESS;
}

