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

#include "ompi_config.h"

#include "util/output.h"
#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/iof/iof.h"
#include "mca/iof/base/base.h"

mca_iof_base_module_t mca_iof;


/**
 * Call the init function on all available components to find out if
 * they want to run.  Select the single component with the highest 
 * priority.
 */
int mca_iof_base_select(bool *allow_multi_user_threads, bool* have_hidden_threads)
{
    ompi_list_item_t *item;
    mca_base_component_list_item_t *cli;
    int selected_priority = -1;
    mca_iof_base_component_t *selected_component = NULL;
    mca_iof_base_module_t *selected_module = NULL;
    bool selected_allow_user;
    bool selected_have_hidden;
 
    /* Traverse the list of opened modules; call their init functions. */
    for(item = ompi_list_get_first(&mca_iof_base.iof_components_opened);
        item != ompi_list_get_end(&mca_iof_base.iof_components_opened);
        item = ompi_list_get_next(item)) {
        mca_iof_base_component_t* component;
 
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_iof_base_component_t *) cli->cli_component;

        ompi_output_verbose(10, mca_iof_base.iof_output, 
            "mca_iof_base_select: initializing %s component %s",
            component->iof_version.mca_type_name,
            component->iof_version.mca_component_name);

        if (NULL == component->iof_init) {
          ompi_output_verbose(10, mca_iof_base.iof_output, 
              "mca_iof_base_select: no init function; ignoring component");
        } else {
            bool allow_user;
            bool have_hidden;
            int priority;
            mca_iof_base_module_t* module = component->iof_init(&priority, &allow_user, &have_hidden);

            /* If the component didn't initialize, remove it from the opened
               list and remove it from the component repository */
            if (NULL == module) {
                ompi_output_verbose(10, mca_iof_base.iof_output,
                    "mca_iof_base_select: init returned failure");
                continue;
            }

            if(priority > selected_priority) {
                selected_priority = priority;
                selected_component = component;
                selected_module = module;
                selected_allow_user = allow_user;
                selected_have_hidden = have_hidden;
            }
        }
    }

    /* unload all components that were not selected */
    item = ompi_list_get_first(&mca_iof_base.iof_components_opened);
    while(item != ompi_list_get_end(&mca_iof_base.iof_components_opened)) {
        ompi_list_item_t* next = ompi_list_get_next(item);
        mca_iof_base_component_t* component;
        cli = (mca_base_component_list_item_t *) item;
        component = (mca_iof_base_component_t *) cli->cli_component;
        if(component != selected_component) {
            ompi_output_verbose(10, mca_iof_base.iof_output,
                "mca_iof_base_select: module %s unloaded",
                component->iof_version.mca_component_name);
            mca_base_component_repository_release((mca_base_component_t *) component);
            ompi_list_remove_item(&mca_iof_base.iof_components_opened, item);
            OBJ_RELEASE(item);
        }
        item = next;
    }

    /* setup reference to selected module */
    if(NULL != selected_module) {
        *allow_multi_user_threads = selected_allow_user;
        *have_hidden_threads = selected_have_hidden;
        mca_iof = *selected_module;
    }
    return OMPI_SUCCESS;
}

