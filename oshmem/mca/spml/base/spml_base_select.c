/*
 * Copyright (c) 2013      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "oshmem_config.h"

#include <string.h>

#include "opal/util/show_help.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"

#include "orte/mca/errmgr/errmgr.h"

#include "oshmem/util/oshmem_util.h"
#include "oshmem/constants.h"
#include "oshmem/mca/spml/spml.h"
#include "oshmem/mca/spml/base/base.h"

#include "ompi/mca/bml/base/base.h"


typedef struct opened_component_t {
    opal_list_item_t super;
    mca_spml_base_component_t *om_component;
} opened_component_t;


/**
 * Function for selecting one component from all those that are
 * available.
 *
 * Call the init function on all available components and get their
 * priorities.  Select the component with the highest priority.  All
 * other components will be closed and unloaded.  The selected component
 * will have all of its function pointers saved and returned to the
 * caller.
 */
int mca_spml_base_select(bool enable_progress_threads, bool enable_mpi_threads)
{
    int i, priority = 0, best_priority = 0, num_spml = 0;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_spml_base_component_t *component = NULL, *best_component = NULL;
    mca_spml_base_module_t *module = NULL, *best_module = NULL;
    opal_list_t opened;
    opened_component_t *om = NULL;
    bool found_spml;

    /* Traverse the list of available components; call their init
       functions. */

    best_priority = -1;
    best_component = NULL;
    module = NULL;
    OBJ_CONSTRUCT(&opened, opal_list_t);
    OPAL_LIST_FOREACH(cli, &oshmem_spml_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_spml_base_component_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to be included */
        found_spml = false;
        for( i = 0; i < opal_pointer_array_get_size(&mca_spml_base_spml); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_spml_base_spml, i);
            if( NULL == tmp_val) {
                continue;
            }

            if(0 == strncmp(component->spmlm_version.mca_component_name,
                            tmp_val, strlen(component->spmlm_version.mca_component_name)) ) {
                found_spml = true;
                break;
            }
        }

        if (!found_spml
                && opal_pointer_array_get_size(&mca_spml_base_spml)) {
            SPML_VERBOSE( 10,
                         "select: component %s not in the include list",
                         component->spmlm_version.mca_component_name);

            continue;
        }

        /* if there is no init function - ignore it */
        if (NULL == component->spmlm_init) {
            SPML_VERBOSE( 10,
                         "select: no init function; ignoring component %s",
                         component->spmlm_version.mca_component_name);
            continue;
        }

        /* this is a spml that could be considered */
        num_spml++;

        /* Init component to get its priority */
        SPML_VERBOSE( 10,
                     "select: initializing %s component %s",
                     component->spmlm_version.mca_type_name, component->spmlm_version.mca_component_name);
        priority = best_priority;
        module = component->spmlm_init(&priority,
                                       enable_progress_threads,
                                       enable_mpi_threads);
        if (NULL == module) {
            SPML_VERBOSE( 10,
                         "select: init returned failure for component %s",
                         component->spmlm_version.mca_component_name);
            continue;
        }

        SPML_VERBOSE( 10, "select: init returned priority %d", priority);

        if (priority > best_priority) {
            best_priority = priority;
            best_component = component;
            best_module = module;
        }

        om = (opened_component_t*) malloc(sizeof(opened_component_t));
        if (NULL == om) {
            return OSHMEM_ERR_OUT_OF_RESOURCE;
        }
        OBJ_CONSTRUCT(om, opal_list_item_t);
        om->om_component = component;
        opal_list_append(&opened, (opal_list_item_t*) om);
    }

    /* Finished querying all components.  Check for the bozo case. */

    if (NULL == best_component) {
        opal_show_help("help-oshmem-memheap.txt",
                       "find-available:none-found",
                       true,
                       "spml");
        for (i = 0; i < opal_pointer_array_get_size(&mca_spml_base_spml); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_spml_base_spml,
                                                           i);
            if (NULL == tmp_val) {
                continue;
            }
            orte_errmgr.abort(1, "SPML %s cannot be selected", tmp_val);
        }
        if (0 == i) {
            orte_errmgr.abort(2,
                              "No spml component available.  This shouldn't happen.");
        }
    }

    SPML_VERBOSE( 10,
                 "selected %s best priority %d\n",
                 best_component->spmlm_version.mca_component_name, best_priority);

    /* Finalize all non-selected components */
    for (item = opal_list_remove_first(&opened);
         NULL != item;
         item = opal_list_remove_first(&opened)) {
        om = (opened_component_t *) item;

        if (om->om_component != best_component) {
            /* Finalize */

            if (NULL != om->om_component->spmlm_finalize) {

                /* Blatently ignore the return code (what would we do to
                 recover, anyway?  This component is going away, so errors
                 don't matter anymore) */

                om->om_component->spmlm_finalize();
                SPML_VERBOSE(10,
                             "select: component %s not selected / finalized",
                             om->om_component->spmlm_version.mca_component_name);
            }
        }
        OBJ_DESTRUCT( om);
        free(om);
    }
    OBJ_DESTRUCT( &opened);

    /* Save the winner */

    mca_spml_base_selected_component = *best_component;
    mca_spml = *best_module;
    SPML_VERBOSE( 10,
                 "select: component %s selected",
                 mca_spml_base_selected_component.spmlm_version.mca_component_name);

    /* This base function closes, unloads, and removes from the
     available list all unselected components.  The available list will
     contain only the selected component. */

    mca_base_components_close(oshmem_spml_base_framework.framework_output,
                              &oshmem_spml_base_framework.framework_components,
                              (mca_base_component_t *) best_component);

    /* All done */

    return OSHMEM_SUCCESS;
}
