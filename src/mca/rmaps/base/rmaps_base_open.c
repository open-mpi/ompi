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

#include "mca/rmaps/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/rmaps/base/static-components.h"


/*
 * Local functions
 */
static void cmp_constructor(orte_rmaps_base_cmp_t *cmp);
static void cmp_destructor(orte_rmaps_base_cmp_t *cmp);
static int compare(ompi_list_item_t **a, ompi_list_item_t **b);

/*
 * Global variables
 */
orte_rmaps_base_t orte_rmaps_base;
OBJ_CLASS_INSTANCE(orte_rmaps_base_cmp_t, ompi_list_item_t,
                   cmp_constructor, cmp_destructor);


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmaps_base_open(void)
{
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_rmaps_base_component_t *component;
    orte_rmaps_base_module_t *module;
    int param, priority, value;
    orte_rmaps_base_cmp_t *cmp;

    /* Debugging / verbose output */

    param = mca_base_param_register_int("rmaps", "base", "verbose", NULL, 0);
    mca_base_param_lookup_int(param, &value);
    if (value != 0) {
        orte_rmaps_base.rmaps_output = ompi_output_open(NULL);
    } else {
        orte_rmaps_base.rmaps_output = -1;
    }

    /* Open up all the components that we can find */

    if (ORTE_SUCCESS != 
        mca_base_components_open("rmaps", 0, mca_rmaps_base_static_components, 
                                 &orte_rmaps_base.rmaps_opened)) {
       return ORTE_ERROR;
    }

    /* Query all the opened components and see if they want to run */

    OBJ_CONSTRUCT(&orte_rmaps_base.rmaps_available, ompi_list_t);
    for (item = ompi_list_get_first(&orte_rmaps_base.rmaps_opened); 
         ompi_list_get_end(&orte_rmaps_base.rmaps_opened) != item; 
         item = ompi_list_get_next(item)) {
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_rmaps_base_component_t *) cli->cli_component;
        ompi_output(orte_rmaps_base.rmaps_output,
                    "orte:base:open: querying component %s", 
                    component->rmaps_version.mca_component_name);

        /* Call the component's init function and see if it wants to be
           selected */

        module = component->rmaps_init(&priority);

        /* If we got a non-NULL module back, then the component wants
           to be considered for selection */

        if (NULL != module) {
            ompi_output(orte_rmaps_base.rmaps_output,
                        "orte:base:open: component %s returns priority %d", 
                        component->rmaps_version.mca_component_name,
                        priority);

            cmp = OBJ_NEW(orte_rmaps_base_cmp_t);
            cmp->component = component;
            cmp->module = module;
            cmp->priority = priority;

            ompi_list_append(&orte_rmaps_base.rmaps_available, &cmp->super);
        } else {
            ompi_output(orte_rmaps_base.rmaps_output,
                        "orte:base:open: component %s does NOT want to be considered for selection", 
                        component->rmaps_version.mca_component_name);
        }
    }

    /* Sort the resulting available list in priority order */

    ompi_list_sort(&orte_rmaps_base.rmaps_available, compare);

    /* All done */

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


static int compare(ompi_list_item_t **a, ompi_list_item_t **b)
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
