/*
 * Copyright (c) 2007      Los Alamos National Security, LLC.
 *                         All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orte_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_component_repository.h"
#include "orte/mca/routed/routed.h"
#include "orte/mca/routed/base/base.h"
#include "orte/mca/errmgr/errmgr.h"


/* The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct. */
#include "orte/mca/routed/base/static-components.h"


int               orte_routed_base_output = -1;
orte_routed_module_t orte_routed;
opal_list_t       orte_routed_base_components;

static orte_routed_component_t *active_component = NULL;
static bool                  component_open_called = false;

int
orte_routed_base_open(void)
{
    int ret;

    /* setup the output stream */
    orte_routed_base_output = opal_output_open(NULL);

    /* Initialize globals */
    OBJ_CONSTRUCT(&orte_routed_base_components, opal_list_t);
    
    /* Open up all available components */
    ret = mca_base_components_open("routed",
                                   orte_routed_base_output,
                                   mca_routed_base_static_components, 
                                   &orte_routed_base_components,
                                   true);
    component_open_called = true;

    return ret;
}


int
orte_routed_base_select(void)
{
    opal_list_item_t *item;

    int selected_priority = -1;
    orte_routed_component_t *selected_component = NULL;
    orte_routed_module_t *selected_module = NULL;
    
    for (item = opal_list_get_first(&orte_routed_base_components);
         item != opal_list_get_end(&orte_routed_base_components) ;
         item = opal_list_get_next(item)) {
        mca_base_component_list_item_t *cli;
        orte_routed_component_t* component;
 
        cli = (mca_base_component_list_item_t *) item;
        component = (orte_routed_component_t *) cli->cli_component;

        opal_output_verbose(10, orte_routed_base_output, 
                            "orte_routed_base_select: initializing %s component %s",
                            component->routed_version.mca_type_name,
                            component->routed_version.mca_component_name);

        if (NULL == component->routed_init) {
            opal_output_verbose(10, orte_routed_base_output, 
                                "orte_routed_base_select: no init function; ignoring component");
        } else {
            int priority = 0;

            orte_routed_module_t* module = component->routed_init(&priority);
            if (NULL == module) {
                opal_output_verbose(10, orte_routed_base_output,
                                    "orte_routed_base_select: init returned failure");
                continue;
            }

            if (priority > selected_priority) {
                /* Otherwise this is a normal module and subject to normal selection */
                if (NULL != selected_module && NULL != selected_module->finalize) {
                    selected_module->finalize();
                }

                selected_priority = priority;
                selected_component = component;
                selected_module = module;
            }
        }
    }

    /* 
     * Unload all components that were not selected
     */
    mca_base_components_close(-1,
                              &orte_routed_base_components,
                              &selected_component->routed_version);

    /* setup reference to selected module */
    if (NULL != selected_module) {
        orte_routed = *selected_module;
        active_component = selected_component;
    }

    if (NULL == selected_component) return ORTE_ERROR;
    
    return ORTE_SUCCESS;
}


int
orte_routed_base_close(void)
{
    /* shutdown any remaining opened components */
    if (component_open_called) {
        mca_base_components_close(orte_routed_base_output, 
                                  &orte_routed_base_components, NULL);
    }

    OBJ_DESTRUCT(&orte_routed_base_components);

    return ORTE_SUCCESS;
}
