/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012      Los Alamos National Security, LLC.  All rights
 *                         reserved.
 * Copyright (c) 2013-2014 Intel, Inc. All rights reserved
 * Copyright (c) 2015 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2020      Sandia National Laboratories. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include <string.h>

#include "opal/class/opal_list.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"
#include "opal/runtime/opal_progress.h"
#include "ompi/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/runtime/opal.h"
#include "opal/mca/pmix/pmix-internal.h"

#include "ompi/constants.h"
#include "ompi/mca/part/part.h"
#include "ompi/mca/part/base/base.h"
#include "ompi/proc/proc.h"

typedef struct opened_component_t {
  opal_list_item_t super;
  mca_part_base_component_t *om_component;
} opened_component_t;

static bool modex_reqd=false;

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
int mca_part_base_select(bool enable_progress_threads,
                        bool enable_mpi_threads)
{
    int i, priority = 0, best_priority = 0, num_part = 0;
    opal_list_item_t *item = NULL;
    mca_base_component_list_item_t *cli = NULL;
    mca_part_base_component_t *component = NULL, *best_component = NULL;
    mca_part_base_module_t *module = NULL, *best_module = NULL;
    opal_list_t opened;
    opened_component_t *om = NULL;
    bool found_part;

    /* Traverse the list of available components; call their init
       functions. */

    best_priority = -1;
    best_component = NULL;
    module = NULL;
    OBJ_CONSTRUCT(&opened, opal_list_t);
    OPAL_LIST_FOREACH(cli, &ompi_part_base_framework.framework_components, mca_base_component_list_item_t) {
        component = (mca_part_base_component_t *) cli->cli_component;

        /* if there is an include list - item must be in the list to be included */
        found_part = false;
        for( i = 0; i < opal_pointer_array_get_size(&mca_part_base_part); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_part_base_part, i);
            if( NULL == tmp_val) {
                continue;
            }

            if(0 == strncmp(component->partm_version.mca_component_name,
                            tmp_val, strlen(component->partm_version.mca_component_name)) ) {
                found_part = true;
                break;
            }
        }

        if(!found_part && opal_pointer_array_get_size(&mca_part_base_part)) {
            opal_output_verbose( 10, ompi_part_base_framework.framework_output,
                                     "select: component %s not in the include list",
                                     component->partm_version.mca_component_name );

            continue;
        }

        /* if there is no init function - ignore it */
        if (NULL == component->partm_init) {
            opal_output_verbose( 10, ompi_part_base_framework.framework_output,
                                 "select: no init function; ignoring component %s",
                                 component->partm_version.mca_component_name );
            continue;
        }

        /* this is a part that could be considered */
        num_part++;

        /* Init component to get its priority */
        opal_output_verbose( 10, ompi_part_base_framework.framework_output,
                             "select: initializing %s component %s",
                             component->partm_version.mca_type_name,
                             component->partm_version.mca_component_name );
        priority = best_priority;
        module = component->partm_init(&priority, enable_progress_threads,
                                      enable_mpi_threads);
        if (NULL == module) {
            opal_output_verbose( 10, ompi_part_base_framework.framework_output,
                                 "select: init returned failure for component %s",
                                 component->partm_version.mca_component_name );
            continue;
        }

        opal_output_verbose( 10, ompi_part_base_framework.framework_output,
                             "select: init returned priority %d", priority );
        if (priority > best_priority) {
            best_priority = priority;
            best_component = component;
            best_module = module;
        }

        om = (opened_component_t*)malloc(sizeof(opened_component_t));
        if (NULL == om) {
            return OMPI_ERR_OUT_OF_RESOURCE;
        }
        OBJ_CONSTRUCT(om, opal_list_item_t);
        om->om_component = component;
        opal_list_append(&opened, (opal_list_item_t*) om);
    }

    /* Finished querying all components.  Check for the bozo case. */

    if( NULL == best_component ) {
        opal_show_help("help-mca-base.txt", "find-available:none found",
                       true, "part",
                       opal_process_info.nodename,
                       "part");
        for( i = 0; i < opal_pointer_array_get_size(&mca_part_base_part); i++) {
            char * tmp_val = NULL;
            tmp_val = (char *) opal_pointer_array_get_item(&mca_part_base_part, i);
            if( NULL == tmp_val) {
                continue;
            }
            ompi_rte_abort(1, "PART %s cannot be selected", tmp_val);
        }
        if(0 == i) {
            ompi_rte_abort(2, "No part component available.  This shouldn't happen.");
        }
    }

    opal_output_verbose( 10, ompi_part_base_framework.framework_output,
                         "selected %s best priority %d\n",
                         best_component->partm_version.mca_component_name, best_priority);

    /* if more than one PART could be considered, then we still need the
     * modex since we cannot know which one will be selected on all procs
     */
    if (1 < num_part) {
        modex_reqd = true;
    }

    /* Finalize all non-selected components */

    for (item = opal_list_remove_first(&opened);
         NULL != item;
         item = opal_list_remove_first(&opened)) {
        om = (opened_component_t *) item;

        if (om->om_component != best_component
            ) {
            /* Finalize */

            if (NULL != om->om_component->partm_finalize) {

                /* Blatantly ignore the return code (what would we do to
                   recover, anyway?  This component is going away, so errors
                   don't matter anymore) */

                om->om_component->partm_finalize();
                opal_output_verbose(10, ompi_part_base_framework.framework_output,
                                    "select: component %s not selected / finalized",
                                    om->om_component->partm_version.mca_component_name);
            }
        }
        OBJ_DESTRUCT( om );
        free(om);
    }
    OBJ_DESTRUCT( &opened );

    /* Save the winner */

    mca_part_base_selected_component = *best_component;
    mca_part = *best_module;
    opal_output_verbose( 10, ompi_part_base_framework.framework_output,
                         "select: component %s selected",
                         mca_part_base_selected_component.partm_version.mca_component_name );

    /* This base function closes, unloads, and removes from the
       available list all unselected components.  The available list will
       contain only the selected component. */

    mca_base_components_close(ompi_part_base_framework.framework_output,
                              &ompi_part_base_framework.framework_components,
                              (mca_base_component_t *) best_component);

    /* register the winner's callback */
    if( NULL != mca_part.part_progress ) {
        opal_progress_register(mca_part.part_progress);
    }

    /* All done */

    return OMPI_SUCCESS;
}

