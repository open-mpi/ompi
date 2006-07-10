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
 * Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
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
#include "opal/mca/base/mca_base_param.h"
#include "opal/util/output.h"

#include "orte/mca/rmaps/base/base.h"


/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "orte/mca/rmaps/base/static-components.h"


/*
 * Local functions
 */
static void cmp_constructor(orte_rmaps_base_cmp_t *cmp);
static void cmp_destructor(orte_rmaps_base_cmp_t *cmp);
static int compare(opal_list_item_t **a, opal_list_item_t **b);

/*
 * Global variables
 */
orte_rmaps_base_t orte_rmaps_base;
OBJ_CLASS_INSTANCE(orte_rmaps_base_cmp_t, opal_list_item_t,
                   cmp_constructor, cmp_destructor);


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int orte_rmaps_base_open(void)
{
    opal_list_item_t *item;
    mca_base_component_list_item_t *cli;
    orte_rmaps_base_component_t *component;
    orte_rmaps_base_module_t *module;
    int param, priority, value;
    orte_rmaps_base_cmp_t *cmp;
    char *policy;

    /* Debugging / verbose output */

    param = mca_base_param_reg_int_name("rmaps", "base_verbose",
                                        "Verbosity level for the rmaps framework",
                                        false, false, 0, &value);
    if (value != 0) {
        orte_rmaps_base.rmaps_output = opal_output_open(NULL);
    } else {
        orte_rmaps_base.rmaps_output = -1;
    }

    /* Are we scheduling by node or by slot? */

    param = mca_base_param_reg_string_name("rmaps", "base_schedule_policy",
                                           "Scheduling Policy for RMAPS. [slot | node]",
                                           false, false, "slot", &policy);
    if (0 == strcmp(policy, "node")) {
        mca_base_param_set_string(param, "node");
    }

    /* Should we schedule on the local node or not? */

    mca_base_param_reg_int_name("rmaps", "base_schedule_local",
                                "If nonzero, allow scheduling MPI applications on the same node as mpirun (default).  If zero, do not schedule any MPI applications on the same node as mpirun",
                                false, false, 1, &value);

    /* Should we oversubscribe or not? */
    
    mca_base_param_reg_int_name("rmaps", "base_no_oversubscribe",
                                "If nonzero, then do not allow oversubscription of nodes - mpirun will return an error if there aren't enough nodes to launch all processes without oversubscribing",
                                false, false, 0, &value);
    if (0 == value) {
        orte_rmaps_base.oversubscribe = true;  /** default condition */
    } else {
        orte_rmaps_base.oversubscribe = false;
    }
    
    
    /* Open up all the components that we can find */

    if (ORTE_SUCCESS != 
        mca_base_components_open("rmaps", orte_rmaps_base.rmaps_output,
                                 mca_rmaps_base_static_components, 
                                 &orte_rmaps_base.rmaps_opened, true)) {
       return ORTE_ERROR;
    }

    /* Query all the opened components and see if they want to run */

    OBJ_CONSTRUCT(&orte_rmaps_base.rmaps_available, opal_list_t);
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
